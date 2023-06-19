module Semantic.Names (collectNames, resolveVars) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (MonadState (get, put), State, evalState, gets, modify)
import Data.Map (Map)
import Data.Map qualified as Map (fromList, insert, lookup)
import Data.Set (Set)
import Data.Set qualified as Set (empty, singleton, union)
import Error (HissError (SemanticError))
import Syntax (getLineCol, start)
import Syntax.AST (Annotated (getAnn), Binding (..), Decl (..), Expr (..), Name (..), Program (Program), declGetName, getIdent)
import Syntax.Lexer (Range (..))

-- | Collects Names referenced in an expression and all of its descendants.
collectNames :: Expr a -> Set (Name a)
collectNames (EBool _ _) = Set.empty
collectNames (EInt _ _) = Set.empty
collectNames (EVar _ n) = Set.singleton n
collectNames (EResolvedVar _ _ n) = Set.singleton n
collectNames (EFunApp _ f args) = collectNames f `Set.union` foldl Set.union Set.empty (map collectNames args)
collectNames (EUnaryOp _ _ e1) = collectNames e1
collectNames (EBinOp _ e1 _ e2) = collectNames e1 `Set.union` collectNames e2
collectNames (ELetIn _ _ e1 e2) = collectNames e1 `Set.union` collectNames e2
collectNames (EIf _ e1 e2 e3) = collectNames e1 `Set.union` collectNames e2 `Set.union` collectNames e3
collectNames (EParen _ e1) = collectNames e1

-- | Table mapping Names to the lexical depth in which they were declared.
type NameTable = Map (Name Range) Int

-- | State object for variable resolution.
data ResolveCtx = ResolveCtx
  { -- | Current lexical depth.
    depth :: Int,
    -- | Table mapping names to the lexical depth in which they were declared.
    nameTable :: NameTable
  }

modifyDepth :: (Int -> Int) -> Resolve ()
modifyDepth f = modify (\ctx -> ctx {depth = f (depth ctx)})

-- | Inserts a Name in nameTable at current depth. Returns old value of nameTable.
putName :: Name Range -> Resolve NameTable
putName n = do
  -- get current name table and lexical depth
  table <- gets nameTable
  d <- gets depth
  -- insert name at current depth
  let table' = Map.insert n d table
  -- update table
  modify (\ctx -> ctx {nameTable = table'})
  -- return old table
  return table

-- | Inserts a Name in nameTable at current depth.
putName_ :: Name Range -> Resolve ()
putName_ n = void $ putName n

-- | Resolves a program's variable references and returns modified AST.
resolveVars :: Program Range -> Either HissError (Program Range)
resolveVars prog@(Program _ decls) = evalState (runExceptT $ doResolveVars prog) ctx
  where
    -- initial resolution context
    ctx =
      ResolveCtx
        { depth = 0,
          -- include globals in name table at depth 0
          nameTable = Map.fromList $ map ((,0) . declGetName) decls
        }

-- | Resolve monad.
type Resolve = ExceptT HissError (State ResolveCtx)

doResolveVars :: Program Range -> Resolve (Program Range)
doResolveVars (Program r decls) = Program r <$> mapM declResolveVars decls

declResolveVars :: Decl Range -> Resolve (Decl Range)
declResolveVars (Decl r binding expr) = Decl r binding <$> resolvedExpr
  where
    resolvedExpr = do
      -- get resolution context
      ctx <- get

      -- does binding create new scope?
      case binding of
        -- value bindings do not create new scope
        ValBinding _ _ -> return ()
        -- function bindings create new scope
        FuncBinding _ _ args -> do
          modifyDepth (+ 1) -- enter new scope
          mapM_ putName args -- insert function args in name table

      -- resolve decl RHS
      expr' <- exprResolveVars expr
      -- restore context
      put ctx
      -- return resolved RHS
      return expr'

exprResolveVars :: Expr Range -> Resolve (Expr Range)
-- let..in expression: update name table and increment scope
exprResolveVars (ELetIn r binding value body) = do
  -- let <binding> = <value> in <body>
  ctx <- get

  case binding of
    ValBinding _ n -> do
      -- val bindings cannot be recursive,
      -- so we resolve value before we create the new scope
      value' <- exprResolveVars value

      -- create new scope where name is defined
      modifyDepth (+ 1)
      putName_ n

      body' <- exprResolveVars body

      -- restore old context
      put ctx

      return $ ELetIn r binding value' body'
    FuncBinding _ func args -> do
      -- func bindings can be recursive

      -- create new scope where name is defined
      modifyDepth (+ 1)
      putName_ func

      -- create another scope where args are defined
      ctx' <- get
      modifyDepth (+ 1)
      mapM_ putName args

      -- resolve value (function body)
      value' <- exprResolveVars value

      put ctx'

      body' <- exprResolveVars body

      put ctx
      return $ ELetIn r binding value' body'
-- for variables: use name table to resolve
exprResolveVars (EVar r n) = do
  -- lookup name in table
  table <- gets nameTable
  case n `Map.lookup` table of
    Nothing -> throwError $ SemanticError $ "Name error: Use of undeclared name '" <> getIdent n <> "' at line " <> show line <> ", column " <> show col
      where
        (line, col) = getLineCol $ start $ getAnn n
    Just d -> return $ EResolvedVar r d n
-- should never need to call exprResolveVars on an already-resolved AST
exprResolveVars (EResolvedVar {}) = error "Compiler bug: exprResolveVars called on EResolvedVar"
-- most expressions: recursively resolve subexpressions
exprResolveVars (EFunApp r fun args) = do
  -- resolve fun
  fun' <- exprResolveVars fun
  -- resolve args
  args' <- mapM exprResolveVars args
  return $ EFunApp r fun' args'
exprResolveVars (EUnaryOp r op operand) = EUnaryOp r op <$> exprResolveVars operand
exprResolveVars (EBinOp r e1 op e2) = do
  -- resolve LHS
  e1' <- exprResolveVars e1
  -- resolve RHS
  e2' <- exprResolveVars e2
  return $ EBinOp r e1' op e2'
exprResolveVars (EIf r cond thenExpr elseExpr) = do
  -- resolve cond
  cond' <- exprResolveVars cond
  -- resolve then
  thenExpr' <- exprResolveVars thenExpr
  -- resolve else
  elseExpr' <- exprResolveVars elseExpr
  return $ EIf r cond' thenExpr' elseExpr'
exprResolveVars (EParen r e) = EParen r <$> exprResolveVars e
-- other expressions: return unmodified
exprResolveVars expr = return expr