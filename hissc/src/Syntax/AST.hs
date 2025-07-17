module Syntax.AST
  ( Annotated (getAnn),
    Program (..),
    Decl (..),
    Name (..),
    BinOp (..),
    UnaryOp (..),
    Binding (..),
    Expr (..),
    progDecls,
    getIdent,
    declGetName,
    stripAnns,
    bindingGetName
  )
where

import Control.Monad (void)
import Data.Ord (comparing)

-- | Class for a syntax node annotated with, e.g., source range information.
class Annotated n where
  getAnn :: n a -> a
  -- ^ Gets an object's annotation.

-- | Strips annotations from an AST node.
stripAnns :: Functor n => n a -> n ()
stripAnns = void

-- | A Hiss program, given by zero or more top-level declarations.
data Program a = Program a [Decl a]
  deriving (Eq, Show, Functor, Foldable)

-- | Gets a program's list of declarations.
progDecls :: Program a -> [Decl a]
progDecls (Program _ decls) = decls

instance Annotated Program where
  getAnn (Program a _) = a

-- | Top-level Hiss declaration.
data Decl a = Decl a (Binding a) (Expr a)
  deriving (Eq, Show, Functor, Foldable)

instance Annotated Decl where
  getAnn (Decl a _ _) = a

-- | Gets Name bound by a declaration.
declGetName :: Decl a -> Name a
declGetName (Decl _ b _) = bindingGetName b

-- | Hiss binary operators.
data BinOp
  = -- | Addition operator
    Add
  | -- | Subtraction operator
    Sub
  | -- | Multiplication operator
    Mult
  | -- | Integer division operator
    Div
  | -- | Equals comparison operator
    Equals
  | -- | Not-equals comparison operator
    NotEquals
  | -- | Less-than comparison operator
    LessThan
  | -- | Less-than-or-equal-to comparison operator
    LessEqual
  | -- | Greater-than comparison operator
    GreaterThan
  | -- | Greater-than-or-equal-to comparison operator
    GreaterEqual
  | -- | Logical AND boolean operator
    And
  | -- | Logical OR boolean operator
    Or
  deriving (Eq, Show)

-- | Hiss unary operators.
data UnaryOp
  = -- | Logical NOT operator.
    Not
  deriving (Eq, Show)

-- | Hiss expression type.
data Expr a
  = -- | Integer literal.
    EInt a Integer
  | -- | Boolean literal.
    EBool a Bool
  | -- | Variable reference.
    EVar a (Name a)
  | -- | Function application.
    EFunApp
      a
      (Expr a)
      -- ^ Function expression
      [Expr a]
      -- ^ Zero or more argument expressions
  | -- | Unary operator.
    EUnaryOp a UnaryOp (Expr a)
  | -- | Binary operator.
    EBinOp a (Expr a) BinOp (Expr a)
  | -- | Let..in expression.
    ELetIn a (Binding a) (Expr a) (Expr a)
  | -- | If-then-else expression.
    EIf a (Expr a) (Expr a) (Expr a)
  | -- | Parenthesized expression.
    EParen a (Expr a)
  deriving (Eq, Show, Functor, Foldable)

instance Annotated Expr where
  getAnn (EInt a _) = a
  getAnn (EBool a _) = a
  getAnn (EVar a _) = a
  getAnn (EFunApp a _ _) = a
  getAnn (EUnaryOp a _ _) = a
  getAnn (EBinOp a _ _ _) = a
  getAnn (ELetIn a _ _ _) = a
  getAnn (EIf a _ _ _) = a
  getAnn (EParen a _) = a

-- | Variable name.
data Name a = Name a String
  deriving (Show, Functor, Foldable)

-- Names compare by their identifiers.
instance Eq (Name a) where
  (Name _ s1) == (Name _ s2) = s1 == s2

instance Ord (Name a) where
  compare = comparing getIdent

instance Annotated Name where
  getAnn (Name a _) = a

-- | Gets identifier for a Name.
getIdent :: Name a -> String
getIdent (Name _ s) = s

-- | Variable binding.
-- E.g., `let <binding> = <exp> in <exp>`. The LHS is a binding.
data Binding a
  = -- | Value binding. Value bindings are just names. e.g., 'x' in 'let x = 5 in ...'
    ValBinding a (Name a)
  | -- | Function binding. Function bindings are names with zero or more arguments. e.g., 'f(a,b)' in 'let f(a,b) = a + b in ...'
    FuncBinding a (Name a) [Name a]
  deriving (Eq, Show, Functor, Foldable)

instance Annotated Binding where
  getAnn (ValBinding a _) = a
  getAnn (FuncBinding a _ _) = a

-- | Gets the name bound by a Binding.
bindingGetName :: Binding a -> Name a
bindingGetName (ValBinding _ n) = n
bindingGetName (FuncBinding _ n _) = n