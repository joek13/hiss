{-
  Print program bytecode using .hissa format.
-}

module Codegen.Assembly (writeAssembly) where

import Codegen.Program (Program (..))
import Codegen.Bytecode (Instr (..), Comparison (..), Const (..))
import Codegen.Emit (Block(..))
import Error (HissError)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.Writer.Lazy (Writer, execWriter, MonadWriter (tell))
import Control.Monad (forM_)
import Data.List (elemIndex)

writeAssembly :: Program -> Either HissError [String]
writeAssembly asm = Right $ execWriter $ runReaderT writeAssembly' asm

-- | 'Assemble a' represents an action that can read the current program, emit some Hiss assembly code, and return a value of type a.
type Assemble a = ReaderT Program (Writer [String]) a

writeLine :: String -> Assemble ()
writeLine = tell.pure

indent :: String -> String
indent = ("\t"<>)

writeAssembly' :: Assemble ()
writeAssembly' = do
    prog <- ask
    -- Write program constants.
    writeLine ".constants {"
    forM_ (constants prog) writeConstant
    writeLine "}"
    writeLine ""
    -- Write program code.
    writeLine ".code {"
    forM_ (blocks prog) writeBlock
    writeLine "}"

writeConstant :: Const -> Assemble ()
writeConstant (Int i) = writeLine $ indent $ "hint " <> show i
writeConstant (Bool True) = writeLine $ indent "hint 1"
writeConstant (Bool False) = writeLine $ indent "hint 0"
writeConstant (Func arity label) = writeLine $ indent $ "hfunc " <> show arity <> " $" <> label

writeBlock :: Block -> Assemble ()
writeBlock Block { label, code } = do
    writeLine $ label <> ":"
    forM_ code writeInstr

writeInstr :: Instr -> Assemble ()
writeInstr i = f i >>= writeLine . indent
    where
        f :: Instr -> Assemble String
        f NoOp = pure "noop"
        f (PushC c) = do
            prog <- ask
            -- Find index of this constant in the constant table
            let constIdx = elemIndex c $ constants prog
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