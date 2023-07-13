{-
  Hiss type definitions.

  For type inference rules, see Semantic.Types.Constraints.
-}
module Semantic.Types (Type (..), Cons (..), Scheme (..), Substitutable (..), Var (..), Subst, TypeEnv, TypedExpr, compose, varNames, relabel, getTy, emptyEnv) where

import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.Bifunctor qualified
import Data.Map (Map)
import Data.Map qualified as Map (delete, empty, findWithDefault, foldl, insert, lookup, map, union)
import Data.Set (Set)
import Data.Set qualified as Set (difference, empty, fromList, singleton, union)
import Syntax.AST (Annotated (getAnn), Decl, Expr, Name, Program)
import Syntax.Lexer (Range)

-- | Hiss type variable.
newtype Var = Var String
  deriving (Eq, Show, Ord)

-- infinite list of type variable names - [a, ..., z, a1, ..., z1, a2, ...]
varNames :: [String]
varNames = [letter ++ num | num <- "" : map show [(1 :: Integer) ..], letter <- map pure ['a' .. 'z']]

-- | Hiss type.
data Type
  = -- | Unit type
    TUnit
  | -- | Concrete type
    TCons Cons
  | -- | Type variable
    TVar Var
  | -- | Function type
    TFunc
      Type
      -- ^ Argument type
      Type
      -- ^ Return type
  deriving (Eq)

instance Show Type where
  show TUnit = "()"
  show (TCons cons) = show cons
  show (TVar (Var v)) = v
  -- arrow is right associative, so need to parenthesize left argument if it is a function
  show (TFunc arg@TFunc {} ret) = "(" <> show arg <> ") -> " <> show ret
  show (TFunc arg ret) = show arg <> " -> " <> show ret

-- | An expression with associated range and type.
type TypedExpr = Expr (Range, Type)

-- | Gets the type of a typed AST node.
getTy :: Annotated t => t (Range, Type) -> Type
getTy = snd . getAnn

-- | Type schemes, aka polytypes.
-- A type scheme specifies a family of monomorphic types.
data Scheme
  = -- | Universal quantifier over zero or more type variables.
    ForAll [Var] Type
  deriving (Eq, Show)

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

emptyEnv :: TypeEnv
emptyEnv = Map.empty

-- | Class for anything containing type variables that we can substitute away.
class Substitutable a where
  -- | Applies a substitution.
  apply :: Subst -> a -> a

  -- | Collects free type variables.
  freeVars :: a -> Set Var

instance Substitutable Type where
  apply _ TUnit = TUnit
  apply _ (TCons t) = TCons t
  apply s t@(TVar v) = Map.findWithDefault t v s
  apply s (TFunc arg ret) = TFunc (apply s arg) (apply s ret)

  freeVars (TVar t) = Set.singleton t
  freeVars (TFunc arg ret) = freeVars arg `Set.union` freeVars ret
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

instance Substitutable TypedExpr where
  -- apply subst to type of this expr and that of all of its subexprs
  apply s = fmap (Data.Bifunctor.second (apply s))

  -- union of free vars of the type of this expr and all of its subexprs
  freeVars expr = foldl Set.union Set.empty (fmap (freeVars . snd) expr)

instance Substitutable (Decl (Range, Type)) where
  apply s = fmap (Data.Bifunctor.second (apply s))
  freeVars decl = foldl Set.union Set.empty (fmap (freeVars . snd) decl)

instance Substitutable (Program (Range, Type)) where
  apply s = fmap (Data.Bifunctor.second (apply s))
  freeVars decl = foldl Set.union Set.empty (fmap (freeVars . snd) decl)

-- | Composes two substitutions.
-- s1 `compose` s2 contains the substitutions from both s1 and s2,
-- and s1's substitutions are applied over s2's types.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | Relabel's a type's variables so that they start with 'a' and continue alphabetically.
relabel :: Type -> Type
relabel ty = evalState (relabel' ty) (0, Map.empty)
  where
    relabelVar :: Var -> State (Int, Map Var Var) Var
    relabelVar v = do
      (ctr, vmap) <- get
      case v `Map.lookup` vmap of
        Just v' -> return v'
        Nothing -> do
          let v' = Var $ varNames !! ctr
          let ctr' = ctr + 1
          let vmap' = Map.insert v v' vmap
          put (ctr', vmap')
          return v'
    relabel' (TVar v) = do
      v' <- relabelVar v
      return $ TVar v'
    relabel' (TFunc t1 t2) = do
      t1' <- relabel' t1
      t2' <- relabel' t2
      return $ TFunc t1' t2'
    relabel' t = return t