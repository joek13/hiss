module Semantic.Types.Solver where

import Control.Monad (when)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.State (MonadState (get, put), StateT (runStateT))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Error (HissError (SemanticError))
import Semantic.Types (Subst, Substitutable (apply, freeVars), Type (TFunc, TVar), Var, compose)
import Semantic.Types.Constraints (Constraint (TyEq))

-- | Constraint solver monad.
type Solve =
  StateT
    (Subst, [Constraint]) -- current substitution, remaining constraints
    (Except HissError) -- can throw HissError

-- | Unifies two types. Returns a substitution that makes the types equal.
unify :: Type -> Type -> Solve (Subst, [Constraint])
-- no unification necessary
unify t1 t2 | t1 == t2 = return (Map.empty, [])
-- should substitute instances of v with t
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
-- check that return type and argument types match
unify ty1@(TFunc args1 ret1) ty2@(TFunc args2 ret2) = do
  when (length args1 /= length args2) $
    throwError $
      SemanticError $
        "Type error: cannot unify types " <> show ty1 <> " and " <> show ty2 <> " because they have different arities"

  unifyMany (ret1 : args1) (ret2 : args2)
-- otherwise, unification fails
unify t1 t2 = throwError $ SemanticError $ "Type error: cannot unify types " <> show t1 <> " and " <> show t2

-- | Unifies many types.
-- Errors with "compiler bug" if the two lists have different sizes.
unifyMany :: [Type] -> [Type] -> Solve (Subst, [Constraint])
unifyMany [] [] = return (Map.empty, [])
unifyMany (ty1 : tys1) (ty2 : tys2) = do
  (subst1, cs1) <- unify ty1 ty2
  (subst2, cs2) <- unifyMany (map (apply subst1) tys1) (map (apply subst1) tys2)
  return (subst2 `compose` subst1, cs1 ++ cs2)
unifyMany _ _ = error "Compiler bug: unifyMany called on lists of different sizes"

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
