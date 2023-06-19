module Semantic.Names (collectNames) where

import Data.Set (Set)
import Data.Set qualified as Set (empty, singleton, union)
import Syntax.AST (Expr (..), Name (..))

-- | Collects Names referenced in an expression and all of its descendants.
collectNames :: Expr a -> Set (Name a)
collectNames (EBool _ _) = Set.empty
collectNames (EInt _ _) = Set.empty
collectNames (EVar _ n) = Set.singleton n
collectNames (EFunApp _ f args) = collectNames f `Set.union` foldl Set.union Set.empty (map collectNames args)
collectNames (EUnaryOp _ _ e1) = collectNames e1
collectNames (EBinOp _ e1 _ e2) = collectNames e1 `Set.union` collectNames e2
collectNames (ELetIn _ _ e1 e2) = collectNames e1 `Set.union` collectNames e2
collectNames (EIf _ e1 e2 e3) = collectNames e1 `Set.union` collectNames e2 `Set.union` collectNames e3
collectNames (EParen _ e1) = collectNames e1