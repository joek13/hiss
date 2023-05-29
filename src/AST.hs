module AST (Name (..), BinOp (..), LetBinding (..), Exp (..), info) where

import Data.Maybe (fromJust)
import Data.Monoid (getFirst)

-- Unsafely grabs the information field from a Foldable, e.g., an AST node.
info :: Foldable t => t a -> a
info = fromJust . getFirst . foldMap pure

data Name a = Name a String
  deriving (Show, Foldable)

data BinOp
  = Add
  | Sub
  | Mult
  | Div
  deriving (Eq, Show)

-- Let binding
-- Given by a name, and, in the case of a function, one or more argument names.
data LetBinding a = LetBinding a (Name a) [Name a]
  deriving (Show, Foldable)

data Exp a
  = EInt a Integer -- <int>
  | EVar a (Name a) -- <var>
  | EBinOp a (Exp a) BinOp (Exp a) -- <exp1> <binop> <exp2>
  | ELetIn a (LetBinding a) (Exp a) (Exp a) -- let <binding> = <exp1> in <exp2>
  | EIf a (Exp a) (Exp a) (Exp a) -- if <exp1> then <exp2> else <exp3>
  | EParen a (Exp a) -- (<exp1>)
  deriving (Show, Foldable)