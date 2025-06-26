{-
  Given a typechecked AST, emits high level Hiss assembly.
-}

module Codegen (codegen, Block (Block, label, code)) where

import Syntax.AST (Expr (..), Decl (Decl), Binding (..), Name, getIdent, Program (Program), UnaryOp (..), BinOp (..), declGetName)
import Control.Monad.State.Lazy (State, MonadState (get, put), execState)
import Semantic.Types (Type)
import Data.List (elemIndex, find)
import Control.Monad (forM_)
import Bytecode ( Instr(..), Label, Const (..) )
import Data.Maybe (fromJust)

data Block = Block { label :: Label, code :: [Instr] }
  deriving Show

data Ctx = Ctx { blocks :: [Block], locals :: [Name Type], globals :: [Name Type] }

-- | Emit monad. Emit a encapsulates some computation that reads current codegen state, possibly emits some code, and returns a value of type a.
type Emit a = State Ctx a

-- | Fetches the current block.
currentBlock :: Emit Block
currentBlock = do
    ctx <- get
    let bs = blocks ctx
    case bs of
        [] -> error "compiler bug: currentBlock called with no blocks"
        b:_ -> return b

updateCurrentBlock :: (Block -> Block) -> Emit ()
updateCurrentBlock f = do
  ctx <- get
  let bs = blocks ctx
  let bs' = case bs of
        [] -> error "compiler bug: emitInstr called with no blocks"
        x : xs -> f x : xs
  put $ ctx {blocks = bs'}

emitInstr :: Instr -> Emit ()
emitInstr i = updateCurrentBlock $ \b -> b { code = code b ++ [i] }

mkBlock :: Label -> Block
mkBlock label = Block { label=label, code=[] }

beginBlock :: Label -> Emit ()
beginBlock label = do
  ctx <- get
  put ctx { blocks=mkBlock label:blocks ctx }

runEmit :: Emit a -> Ctx -> [Block]
runEmit e c = blocks $ execState e c

class Emitter n where
    emit :: n -> Emit ()

emitLoad :: Name Type -> Emit ()
emitLoad n = do
    ctx <- get
    -- First, try to find index of variable in locals.
    case elemIndex n (locals ctx) of
      Just v -> emitInstr $ LoadV v
      -- Next, try to find index of variable in globals.
      Nothing -> case elemIndex n (globals ctx) of
        Just g -> emitInstr $ LoadG g
        -- We shouldn't ever reach this case. If we do, it's a compiler bug.
        Nothing -> error $ "compiler bug: cannot resolve variable reference " <> show n

instance Emitter (Expr Type) where
  -- Literal expressions: push a constant to the stack.
  emit (EInt _ i) = emitInstr $ PushC $ Int $ fromIntegral i
  emit (EBool _ b) = emitInstr $ PushC $ Bool b
  -- Variable reference
  emit (EVar _ n) = emitLoad n
  -- Function application
  emit (EFunApp _ funExp argExps) = do
    -- Evaluate each of the function arguments
    forM_ argExps emit
    -- Evaluate the function itself
    emit funExp
    -- Emit call instruction
    emitInstr Call
  -- Unary operator
  emit (EUnaryOp _ op expr) = do
    -- Evaluate the operand
    emit expr
    -- Emit code for the operator
    emit op
  -- Binary opeartor
  emit (EBinOp _ expr1 op expr2) = do
    -- Evaluate right operand
    emit expr2
    -- Evaluate left operand
    emit expr1
    -- Emit code for the operator
    emit op
  emit (ELetIn _ (ValBinding _ _) _ _) = error "compiler bug: let..in does not generate code"
  emit (EIf _ condExpr thenExpr elseExpr) = do
    -- Evaluate condition
    emit condExpr
    curLabel <- label <$> currentBlock
    -- Create then/else/finally blocks
    let thenLabel = curLabel ++ "_then"
    let elseLabel = curLabel ++ "_else"
    let finallyLabel = curLabel ++ "_finally"
    -- TODO: should consider making it so that each block has single exit point
    -- Jump to 'then' if condition holds
    emitInstr $ Br thenLabel
    -- Otherwise, jump to 'else'
    emitInstr $ Jmp elseLabel
    beginBlock elseLabel
    emit elseExpr
    -- Jump to 'finally'
    emitInstr $ Jmp finallyLabel
    beginBlock thenLabel
    emit thenExpr
    emitInstr $ Jmp finallyLabel
    beginBlock finallyLabel

  emit (EParen _ expr) = emit expr
  emit _ = error "compiler bug: unimplemented"

instance Emitter UnaryOp where
  emit Not = do
    -- Operand is already on stack
    -- Booleans are represented by integers, so NOT is just subtraction from one
    emitInstr $ PushC $ Int 1
    emitInstr ISub

instance Emitter BinOp where
  emit Add = emitInstr IAdd
  emit Sub = emitInstr ISub
  emit Mult = emitInstr IMul
  emit Div = emitInstr IDiv
  emit _ = error "compiler bug: unimplemented"

setLocals :: [Name Type] -> Emit ()
setLocals ls = do
  ctx <- get
  put ctx { locals = ls }

instance Emitter (Decl Type) where
  emit (Decl _ (FuncBinding _ name args) expr) = do
    setLocals args
    beginBlock $ getIdent name
    emit expr
    emitInstr Ret
  emit _ = pure ()

setGlobals :: [Name Type] -> Emit ()
setGlobals gs = do
  ctx <- get
  put ctx { globals = gs }

emitInitializer :: Decl Type -> Emit ()
emitInitializer (Decl _ (FuncBinding _ name args) _) = do
  ctx <- get
  -- Push constant for this function
  emitInstr $ PushC $ Func (length args) (getIdent name)
  -- Store it in globals
  let globalIdx = fromJust $ elemIndex name $ globals ctx
  emitInstr $ StoreG globalIdx
emitInitializer (Decl _ (ValBinding _ name) expr) = do
  ctx <- get
  -- Emit code to compute value
  emit expr
  -- Store in globals
  let globalIdx = fromJust $ elemIndex name $ globals ctx
  emitInstr $ StoreG globalIdx

instance Emitter (Program Type) where
  emit (Program _ decls) = do
    setGlobals $ map declGetName decls
    -- Emit function bodies
    forM_ decls emit
    -- Emit initializer block
    beginBlock "_init"
    -- Emit initializers for global variables
    forM_ decls emitInitializer
    -- Find declaration of 'main' function
    let main = fromJust $ find (("main"==) . getIdent) $ map declGetName decls
    -- Load/call the main function
    emitLoad main
    emitInstr Call
    -- Print the result
    emitInstr Print
    -- Halt the program
    emitInstr Halt

codegen :: Program Type -> [Block]
codegen program = runEmit (emit program) initialCtx
  where initialCtx = Ctx { blocks = [], locals = [], globals = [] }