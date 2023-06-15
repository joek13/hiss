module Syntax.AST (Program, Decl (..), Name (..), BinOp (..), UnaryOp (..), Binding (..), Exp (..), FunApp (..), getAnn, mapAnn, stripAnns, getIdent, declGetName, declMapAnn, declStripAnns) where

import Data.Maybe (fromJust)
import Data.Monoid (getFirst)
import Data.Ord (comparing)

-- a Hiss program is given by zero or more top-level declarations.
type Program a = [Decl a]

-- Top-level declaration
data Decl a = Decl a (Binding a) (Exp a)
  deriving (Eq, Show, Foldable)

-- Gets Name bound by a declaration.
declGetName :: Decl a -> Name a
declGetName (Decl _ b _) = bindingGetName b

declMapAnn :: (a -> b) -> Decl a -> Decl b
declMapAnn f (Decl a b e) = Decl (f a) (bindingMapAnn f b) (mapAnn f e)

declStripAnns :: Decl a -> Decl ()
declStripAnns = declMapAnn (const ())

data BinOp
  = Add
  | Sub
  | Mult
  | Div
  | Equals
  | NotEquals
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | And
  | Or
  deriving (Eq, Show)

data UnaryOp
  = Not
  deriving (Eq, Show)

-- AST expression.
-- For Exp and other AST node types, we include a type argument a, which we can use
-- to annotate the syntax tree with, e.g., source program range information.
data Exp a
  = EInt a Integer -- <int>
  | EBool a Bool -- <bool>
  | EVar a (Name a) -- <var>
  | EFunApp a (FunApp a) -- <exp1> <exp2> (<exp3> <exp4> ...)
  | EUnaryOp a UnaryOp (Exp a) -- <op> <exp1>
  | EBinOp a (Exp a) BinOp (Exp a) -- <exp1> <binop> <exp2>
  | ELetIn a (Binding a) (Exp a) (Exp a) -- let <binding> = <exp1> in <exp2>
  | EIf a (Exp a) (Exp a) (Exp a) -- if <exp1> then <exp2> else <exp3>
  | EParen a (Exp a) -- (<exp1>)
  deriving (Eq, Show, Foldable)

-- Unsafely grabs annotation field from an AST node.
getAnn :: Foldable t => t a -> a
getAnn = fromJust . getFirst . foldMap pure

-- Recursively strips annotations from an AST.
stripAnns :: Exp a -> Exp ()
stripAnns = mapAnn (const ())

-- Recursively applies f to each AST node's annotation.
mapAnn :: (a -> b) -> Exp a -> Exp b
mapAnn f (EBool a b) = EBool (f a) b
mapAnn f (EInt a i) = EInt (f a) i
mapAnn f (EVar a n) = EVar (f a) (nameMapAnn f n)
mapAnn f (EFunApp a funApp) = EFunApp (f a) (funAppMapAnn f funApp)
mapAnn f (EUnaryOp a op e1) = EUnaryOp (f a) op (mapAnn f e1)
mapAnn f (EBinOp a e1 op e2) = EBinOp (f a) (mapAnn f e1) op (mapAnn f e2)
mapAnn f (ELetIn a lb e1 e2) = ELetIn (f a) (bindingMapAnn f lb) (mapAnn f e1) (mapAnn f e2)
mapAnn f (EIf a e1 e2 e3) = EIf (f a) (mapAnn f e1) (mapAnn f e2) (mapAnn f e3)
mapAnn f (EParen a e1) = EParen (f a) (mapAnn f e1)

-- Name of a variable.
data Name a = Name a String
  deriving (Show, Foldable)

-- Gets identifier for a Name.
getIdent :: Name a -> String
getIdent (Name _ s) = s

-- Names compare by their string value.
instance Eq (Name a) where
  (Name _ s1) == (Name _ s2) = s1 == s2

instance Ord (Name a) where
  compare = comparing getIdent

-- Applies f to a Name's annotation.
nameMapAnn :: (a -> b) -> Name a -> Name b
nameMapAnn f (Name a n) = Name (f a) n

-- Variable binding, e.g., `let <binding> = <exp> in <exp>`
data Binding a
  = ValBinding a (Name a) -- Value bindings are just names. e.g., 'x' in 'let x = 5 in ...'
  | FuncBinding a (Name a) [Name a] -- Function bindings are names with zero or more arguments. e.g., 'f(a,b)' in 'let f(a,b) = a + b in ...'
  deriving (Eq, Show, Foldable)

-- Recursively applies f to a Binding's annotation and the annotations of its Names.
bindingMapAnn :: (a -> b) -> Binding a -> Binding b
bindingMapAnn f (ValBinding a n) = ValBinding (f a) (nameMapAnn f n)
bindingMapAnn f (FuncBinding a n args) = FuncBinding (f a) (nameMapAnn f n) (map (nameMapAnn f) args)

-- Gets the bound name of a Binding.
bindingGetName :: Binding a -> Name a
bindingGetName (ValBinding _ n) = n
bindingGetName (FuncBinding _ n _) = n

data FunApp a = FunApp a (Exp a) [Exp a]
  deriving (Eq, Show, Foldable)

funAppMapAnn :: (a -> b) -> FunApp a -> FunApp b
funAppMapAnn f (FunApp a fun args) = FunApp (f a) (mapAnn f fun) (map (mapAnn f) args)