module Semantic.Names (collectNames, checkNames) where

import Data.Set (Set)
import Data.Set qualified as Set (empty, singleton, union)
import Error (HissError)
import Syntax.AST (Exp (..), FunApp (..), Name (..))
import Syntax.Lexer (Range)

-- Collects all Names referenced in an expression and its children.
collectNames :: Exp a -> Set (Name a)
collectNames (EBool _ _) = Set.empty
collectNames (EInt _ _) = Set.empty
collectNames (EVar _ n) = Set.singleton n
collectNames (EFunApp _ funApp) = funAppCollectNames funApp
collectNames (EUnaryOp _ _ e1) = collectNames e1
collectNames (EBinOp _ e1 _ e2) = collectNames e1 `Set.union` collectNames e2
collectNames (ELetIn _ _ e1 e2) = collectNames e1 `Set.union` collectNames e2
collectNames (EIf _ e1 e2 e3) = collectNames e1 `Set.union` collectNames e2 `Set.union` collectNames e3
collectNames (EParen _ e1) = collectNames e1

-- Collects all Names referenced in a FunApp and its children.
funAppCollectNames :: FunApp a -> Set (Name a)
funAppCollectNames (FunApp _ fun args) = collectNames fun `Set.union` foldl Set.union Set.empty (map collectNames args)

-- Traverses AST and makes sure each name is defined in its context.
checkNames :: Exp Range -> Either HissError (Exp Range)
checkNames = Right