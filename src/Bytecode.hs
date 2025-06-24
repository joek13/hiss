module Bytecode (Label, Instr (..), Comparison (..), Const (..)) where 

type Label = String

-- | Bytecode instructions.
data Instr
  = NoOp
  | PushC Const
  | Pop
  | LoadV Int
  | StoreV Int
  | LoadG Int
  | StoreG Int
  | Halt
  | Call
  | Ret
  | Br Label
  | Jmp Label
  | IAdd
  | ISub
  | IMul
  | IDiv
  | IAnd
  | IOr
  | ICmp Comparison
  | Print
  deriving Show

data Comparison =
    Eq
    | NEq
    | Lt
    | LEq
    | Gt
    | GEq
    deriving Show

data Const =
    Int Int
    | Bool Bool
    | Func Int Label
    deriving (Eq, Ord, Show)