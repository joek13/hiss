{- Hindley-Milner type system based on https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter7/poly/src/Infer.hs

   Other references:
   - https://github.com/sdiehl/write-you-a-haskell/blob/master/006_hindley_milner.md
   - https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
-}

module Semantic.Types (Type (..), Cons (..), Scheme (..), Substitutable (..), Var (..), Subst, TypeEnv, compose, varNames) where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map (delete, findWithDefault, foldl, map, union)
import Data.Set (Set)
import Data.Set qualified as Set (difference, empty, fromList, singleton, union)
import Syntax.AST (Name)
import Syntax.Lexer (Range)

-- | Hiss type variable.
newtype Var = Var String
  deriving (Eq, Show, Ord)

-- infinite list of type variable names - [a, ..., z, a1, ..., z1, a2, ...]
varNames :: [String]
varNames = [letter ++ num | letter <- map pure ['a' .. 'z'], num <- "" : map show [(1 :: Integer) ..]]

-- | Hiss type.
data Type
  = -- | Concrete type
    TCons Cons
  | -- | Type variable
    TVar Var
  | -- | Function type
    TFunc
      [Type]
      -- ^ Argument types
      Type
      -- ^ Return type
  deriving (Eq)

instance Show Type where
  show (TCons cons) = show cons
  show (TVar (Var v)) = v
  show (TFunc args ret) = "(" <> intercalate "," (map show args) <> ") -> " <> show ret

-- | Type schemes, aka polytypes.
-- A type scheme specifies a family of monomorphic types.
data Scheme
  = -- | Universal quantifier over zero or more type variables.
    ForAll [Var] Type

-- | Concrete types.
data Cons
  = CInt
  | CBool
  deriving (Eq)

instance Show Cons where
  show CInt = "int"
  show CBool = "bool"

-- | Substitution map.
type Subst = Map Var Type

-- | Typing environment.
-- Maps variable names to bound type schemes.
type TypeEnv = Map (Name Range) Scheme

-- | Class for anything containing type variables that we can substitute away.
class Substitutable a where
  -- | Applies a substitution.
  apply :: Subst -> a -> a

  -- | Collects free type variables.
  freeVars :: a -> Set Var

instance Substitutable Type where
  apply _ (TCons t) = TCons t
  apply s t@(TVar v) = Map.findWithDefault t v s
  apply s (TFunc args ret) = TFunc (map (apply s) args) (apply s ret)

  freeVars (TVar t) = Set.singleton t
  freeVars (TFunc args ret) = foldl Set.union (freeVars ret) (map freeVars args)
  freeVars _ = Set.empty

instance Substitutable Scheme where
  -- applies substitution to the inner type, ignoring those bound by the quantifier
  apply s (ForAll vs t) = ForAll vs $ apply s' t
    where
      s' = foldl (flip Map.delete) s vs

  -- free vars of the inner type minus those bound by the quantifier
  freeVars (ForAll vs t) = freeVars t `Set.difference` Set.fromList vs

instance Substitutable TypeEnv where
  -- applies subst to each type scheme
  apply s = Map.map (apply s)

  -- union of each type scheme's free type variables
  freeVars env = Map.foldl Set.union Set.empty (Map.map freeVars env)

-- | Composes two substitutions.
-- s1 `compose` s2 contains the substitutions from both s1 and s2,
-- and s1's substitutions are applied over s2's types.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- -- | Infers type of an expression.
-- infer :: TypeEnv -> Expr Range -> Infer (Subst, Type)
-- infer env expr = case expr of
--   EInt _ _ -> return (Map.empty, TCons CInt)
--   EBool _ _ -> return (Map.empty, TCons CBool)
--   EVar _ n -> lookupName env n
--   EUnaryOp _ op subexpr -> inferUnaryOp env op subexpr
--   EBinOp _ expr1 op expr2 -> inferBinaryOp env expr1 op expr2
--   ELetIn _ binding val body ->
--     case binding of
--       ValBinding _ name -> do
--         (s1, t1) <- infer env val
--         let env' = Map.insert name (generalize env t1) env
--         (s2, t2) <- infer (apply s1 env') body
--         return (s1 `compose` s2, t2)
--       FuncBinding _ _ _ -> error "Typing rules for function bindings are unimplemented!"
--   EIf _ condExpr thenExpr elseExpr -> do
--     (s1, t1) <- infer env condExpr
--     (s2, t2) <- infer env thenExpr
--     (s3, t3) <- infer env elseExpr
--     s4 <- unify t1 (TCons CBool)
--     s5 <- unify t2 t3
--     return (foldr1 compose [s1, s2, s3, s4, s5], t2)
--   EParen _ subexpr -> infer env subexpr

-- inferUnaryOp :: TypeEnv -> UnaryOp -> Expr Range -> Infer (Subst, Type)
-- inferUnaryOp env op expr = case op of
--   Not -> do
--     (s1, t1) <- infer env expr
--     tv <- fresh
--     s2 <- unify t1 (TCons CBool)
--     s3 <- unify tv (TCons CBool)
--     return (s1 `compose` s2 `compose` s3, apply s3 tv)

-- inferBinaryOp :: TypeEnv -> Expr Range -> BinOp -> Expr Range -> Infer (Subst, Type)
-- inferBinaryOp env expr1 op expr2 = case op of
--   Add -> inferArithmetic
--   Sub -> inferArithmetic
--   Mult -> inferArithmetic
--   Div -> inferArithmetic
--   Equals -> inferComparison
--   NotEquals -> inferComparison
--   GreaterThan -> inferComparison
--   GreaterEqual -> inferComparison
--   LessThan -> inferComparison
--   LessEqual -> inferComparison
--   And -> inferLogical
--   Or -> inferLogical
--   where
--     -- int -> int -> int
--     inferArithmetic = do
--       (s1, t1) <- infer env expr1
--       (s2, t2) <- infer (apply s1 env) expr2
--       s3 <- unify t1 (TCons CInt)
--       s4 <- unify t2 (TCons CInt)
--       return (foldr1 compose [s1, s2, s3, s4], TCons CInt)
--     -- int -> int -> bool
--     inferComparison = do
--       (s1, t1) <- infer env expr1
--       (s2, t2) <- infer (apply s1 env) expr2
--       s3 <- unify t1 (TCons CInt)
--       s4 <- unify t2 (TCons CInt)
--       return (foldr1 compose [s1, s2, s3, s4], TCons CBool)
--     -- bool -> bool -> bool
--     inferLogical = do
--       (s1, t1) <- infer env expr1
--       (s2, t2) <- infer (apply s1 env) expr2
--       s3 <- unify t1 (TCons CBool)
--       s4 <- unify t2 (TCons CBool)
--       return (foldr1 compose [s1, s2, s3, s4], TCons CBool)