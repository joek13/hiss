module Semantic.Typechecking (infer, infer') where

import Data.Map qualified as Map
import Debug.Trace (trace)
import Error (HissError)
import Semantic.Types (Substitutable (apply), Type, TypeEnv)
import Semantic.Types.Constraints (runInfer, solve)
import Semantic.Types.Constraints qualified as Constraints (infer)
import Syntax.AST (Expr)
import Syntax.Lexer (Range)

{-
    Frontend to typechecking functionality.
-}

infer :: TypeEnv -> Expr Range -> Either HissError Type
infer env expr = do
  -- infer type and collect constraints
  (ty, cs) <- runInfer env (Constraints.infer expr)
  -- solve for type substitutions
  subst <- solve cs
  -- apply substitutions to the type
  return $ apply subst ty

infer' :: Expr Range -> Either HissError Type
infer' = infer Map.empty