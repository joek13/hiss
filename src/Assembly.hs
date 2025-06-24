module Assembly (assemble, writeAssembly) where

import Bytecode ( Const (Int, Bool, Func), Instr (..), Comparison (..) )
import Codegen (Block (Block), codegen)
import qualified Codegen ( Block (label, code) )
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Writer.Lazy (Writer, execWriter, MonadWriter (tell))
import Control.Monad (forM_)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Data.List (elemIndex)
import Error (HissError)
import Syntax.AST (Program (Program))
import Semantic.Types (Type)

-- | An assembled program.
data Assembly = Assembly { constants :: [Const], blocks :: [Block] }

-- | Finds unique constants in the provided blocks.
collectConstants :: [Block] -> Set Const
collectConstants = foldl f Set.empty
    where f consts block = consts `Set.union` collectConstants' (Codegen.code block)

collectConstants' :: [Instr] -> Set Const
collectConstants' = foldl f Set.empty
    where f consts instr = consts `Set.union` collectConstant instr

collectConstant :: Instr -> Set Const
collectConstant (PushC c) = Set.singleton c
collectConstant _ = Set.empty

-- | Writes out an assembly in .hissa format. This isn't expected to fail, but it returns Either for consistency with other passes.
writeAssembly :: Assembly -> Either HissError [String]
writeAssembly asm = Right $ execWriter $ runReaderT writeAssembly' asm

-- | 'Assemble a' represents an action that can read the current assembly, emit some Hiss assembly code, and return a value of type a.
type Assemble a = ReaderT Assembly (Writer [String]) a

writeLine :: String -> Assemble ()
writeLine = tell.pure

indent :: String -> String
indent = ("\t"<>)

writeAssembly' :: Assemble ()
writeAssembly' = do
    asm <- ask
    -- Write program constants.
    writeLine ".constants {"
    forM_ (constants asm) writeConstant
    writeLine "}"
    writeLine ""
    -- Write program code.
    writeLine ".code {"
    forM_ (blocks asm) writeBlock
    writeLine "}"

writeConstant :: Const -> Assemble ()
writeConstant (Int i) = writeLine $ indent $ "hint " <> show i
writeConstant (Bool True) = writeLine $ indent "hint 1"
writeConstant (Bool False) = writeLine $ indent "hint 0"
writeConstant (Func arity label) = writeLine $ indent $ "hfunc " <> show arity <> " $" <> label

writeBlock :: Block -> Assemble ()
writeBlock Block { Codegen.label, Codegen.code } = do
    writeLine $ label <> ":"
    forM_ code writeInstr

writeInstr :: Instr -> Assemble ()
writeInstr i = f i >>= writeLine . indent
    where
        f :: Instr -> Assemble String
        f NoOp = pure "noop"
        f (PushC c) = do
            asm <- ask
            -- Find index of this constant in the constant table
            let constIdx = elemIndex c $ constants asm
            case constIdx of
                Just idx -> pure $ "pushc " <> show idx
                Nothing -> error $ "compiler bug: cannot find constant " <> show c
        f Pop = pure "pop"
        f (LoadV v) = pure $ "loadv " <> show v
        f (StoreV v) = pure $ "storev " <> show v
        f (LoadG g) = pure $ "loadg " <> show g
        f (StoreG g) = pure $ "storeg " <> show g
        f Halt = pure "halt"
        f Call = pure "call"
        f Ret = pure "ret"
        f (Br target) = pure $ "br $" <> target
        f (Jmp target) = pure $ "jmp $" <> target
        f IAdd = pure "iadd"
        f ISub = pure "isub"
        f IMul = pure "imul"
        f IDiv = pure "idiv"
        f IAnd = pure "iand"
        f IOr = pure "ior"
        f (ICmp Eq) = pure "icmp eq"
        f (ICmp NEq) = pure "icmp neq"
        f (ICmp Lt) = pure "icmp lt"
        f (ICmp LEq) = pure "icmp leq"
        f (ICmp Gt) = pure "icmp gt"
        f (ICmp GEq) = pure "icmp geq"
        f Print = pure "print"

-- Constant for the initializer function.
initConst :: Const
initConst = Func 0 "_init"

-- | Assembles a program.
assemble :: Program Type -> Either HissError Assembly
assemble program = 
    let bs = reverse $ codegen program
        cs = (Set.toAscList . collectConstants) bs
    in
        Right $ Assembly { constants = initConst:cs, blocks = bs }