module Syntax.AST (Name (..), BinOp (..), LetBinding (..), Exp (..), FunApp (..), getAnn, stripAnns) where

import Data.Maybe (fromJust)
import Data.Monoid (getFirst)

data BinOp
  = Add
  | Sub
  | Mult
  | Div
  deriving (Eq, Show)

-- AST expression.
-- For Exp and other AST node types, we include a type argument a, which we can use
-- to annotate the syntax tree with, e.g., source program range information.
data Exp a
  = EInt a Integer -- <int>
  | EVar a (Name a) -- <var>
  | EFunApp a (FunApp a) -- <exp1> <exp2> (<exp3> <exp4> ...)
  | EBinOp a (Exp a) BinOp (Exp a) -- <exp1> <binop> <exp2>
  | ELetIn a (LetBinding a) (Exp a) (Exp a) -- let <binding> = <exp1> in <exp2>
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
mapAnn f (EInt a i) = EInt (f a) i
mapAnn f (EVar a n) = EVar (f a) (nameMapAnn f n)
mapAnn f (EFunApp a funApp) = EFunApp (f a) (funAppMapAnn f funApp)
mapAnn f (EBinOp a e1 op e2) = EBinOp (f a) (mapAnn f e1) op (mapAnn f e2)
mapAnn f (ELetIn a lb e1 e2) = ELetIn (f a) (letBindingMapAnn f lb) (mapAnn f e1) (mapAnn f e2)
mapAnn f (EIf a e1 e2 e3) = EIf (f a) (mapAnn f e1) (mapAnn f e2) (mapAnn f e3)
mapAnn f (EParen a e1) = EParen (f a) (mapAnn f e1)

-- Name of a variable.
data Name a = Name a String
  deriving (Eq, Show, Foldable)

-- Applies f to a Name's annotation.
nameMapAnn :: (a -> b) -> Name a -> Name b
nameMapAnn f (Name a n) = Name (f a) n

-- Let binding (e.g., `flip f x y`)
-- Given by a name, and, in the case of a function, one or more argument names.
data LetBinding a = LetBinding a (Name a) [Name a]
  deriving (Eq, Show, Foldable)

-- Recursively applies f to a LetBinding's annotation and the annotations of its Names.
letBindingMapAnn :: (a -> b) -> LetBinding a -> LetBinding b
letBindingMapAnn f (LetBinding a n args) = LetBinding (f a) (nameMapAnn f n) (map (nameMapAnn f) args)

data FunApp a = FunApp a (Exp a) [Exp a]
  deriving (Eq, Show, Foldable)

funAppMapAnn :: (a -> b) -> FunApp a -> FunApp b
funAppMapAnn f (FunApp a fun args) = FunApp (f a) (mapAnn f fun) (map (mapAnn f) args)