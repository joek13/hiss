module Semantic.Types.Solver where

import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Error (HissError (SemanticError))
import Semantic.Types (Subst, Substitutable (apply, freeVars), Type (TVar), Var, compose)
import Semantic.Types.Constraints (Constraint (TyEq))

-- | Constraint solver monad.
type Solve =
  StateT
    (Subst, [Constraint]) -- current substitution, remaining constraints
    (Except HissError) -- can throw HissError

unify :: Type -> Type -> Solve (Subst, [Constraint])
unify t1 t2 | t1 == t2 = return (Map.empty, [])
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify t1 t2 = throwError $ SemanticError $ "Type error: cannot unify types " <> show t1 <> " and " <> show t2

bind :: Var -> Type -> Solve (Subst, [Constraint])
bind v t
  | t == TVar v = return (Map.empty, [])
  | occursIn v t = throwError $ SemanticError "Type error: cannot construct infinite type"
  | otherwise = return (Map.singleton v t, [])

occursIn :: Var -> Type -> Bool
occursIn v t = v `Set.member` freeVars t

solver :: Solve Subst
solver = do
  (subst, cs) <- get
  case cs of
    [] -> return subst
    (TyEq (t1, t2) : cs') -> do
      (s1, cs1) <- unify t1 t2
      put (s1 `compose` subst, cs1 ++ map (apply s1) cs')
      solver

solve :: [Constraint] -> Either HissError Subst
solve cs = do
  (subst, _s) <- runExcept $ runStateT solver (Map.empty, cs)
  return subst
