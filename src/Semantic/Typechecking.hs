{-
  Frontend for typechecking functionality.

  For definitions, see Semantic.Types.
  For type inference rules, see Semantic.Types.Constraints.
-}
module Semantic.Typechecking (infer, infer') where

import Data.Map qualified as Map
import Error (HissError)
import Semantic.Types (Substitutable (apply), Type, TypeEnv, getTy, relabel)
import Semantic.Types.Constraints (runInfer, solve)
import Semantic.Types.Constraints qualified as Constraints (infer)
import Syntax.AST (Expr)
import Syntax.Lexer (Range)

infer :: TypeEnv -> Expr Range -> Either HissError Type
infer env expr = do
  -- infer type and collect constraints
  (expr', cs) <- runInfer env (Constraints.infer expr)
  let ty = getTy expr'
  -- solve for type substitutions
  subst <- solve cs
  -- apply substitutions to the type
  let solved = apply subst ty
  -- and relabel vars
  let relabeled = relabel solved
  return relabeled

infer' :: Expr Range -> Either HissError Type
infer' = infer Map.empty