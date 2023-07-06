module Semantic.Types.Constraints where

import Control.Monad.Except (Except, runExcept)
import Control.Monad.RWS (MonadReader (ask), MonadWriter (tell), RWST (runRWST), gets, modify)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Error (HissError)
import Semantic.Types
  ( Cons (CBool, CInt),
    Scheme (..),
    Substitutable (..),
    Type (TCons, TVar),
    TypeEnv,
    Var (Var),
    varNames,
  )
import Syntax.AST (Expr (EBool, EInt, EVar), Name, getIdent)
import Syntax.Lexer

-- | Given a type env and name, looks up its type.
lookupName :: Name Range -> Infer Type
lookupName name = do
  env <- ask
  case Map.lookup name env of
    Nothing -> error $ "Compiler bug: lookup called on missing variable " <> getIdent name
    Just scheme -> do
      -- let polymorphism: instantiate a new type at each usage
      instantiate scheme

newtype Counter = Counter {count :: Int}

initCounter :: Counter
initCounter = Counter {count = 0}

-- | Type equality constraint.
newtype Constraint = TyEq (Type, Type)

instance Substitutable Constraint where
  apply s1 (TyEq (t1, t2)) = TyEq (apply s1 t1, apply s1 t2)
  freeVars (TyEq (t1, t2)) = freeVars t1 `Set.union` freeVars t2

-- | Type inference monad.
type Infer =
  RWST
    TypeEnv -- reads type environment
    [Constraint] -- writes constraints
    Counter -- stateful unique counter
    (Except HissError) -- throws HissError

-- | Instantiates a fresh type variable.
fresh :: Infer Type
fresh = do
  i <- gets count
  modify $ Counter . (+ 1) . count -- increment counter
  return $ TVar (Var $ varNames !! i) -- index into list of type variable names

-- | Instantiates a type scheme, replacing its bound variables
-- with fresh type variables.
instantiate :: Scheme -> Infer Type
instantiate (ForAll vars ty) = do
  vars' <- mapM (const fresh) vars
  let subst = Map.fromList $ zip vars vars'
  return $ apply subst ty

-- | Generalizes a type, closing over its free variables
generalize :: Type -> Infer Scheme
generalize ty = do
  env <- ask
  let vars = Set.toList $ freeVars ty `Set.difference` freeVars env
  return $ ForAll vars ty

-- | Appends constraint that t1 == t2.
constrain :: Type -> Type -> Infer ()
constrain t1 t2 = tell [TyEq (t1, t2)]

infer :: Expr Range -> Infer Type
infer expr = case expr of
  EInt _ _ -> return $ TCons CInt
  EBool _ _ -> return $ TCons CBool
  EVar _ n -> lookupName n
  _ -> error "Unimplemented!"

runInfer :: TypeEnv -> Infer a -> Either HissError (a, [Constraint])
runInfer env m = do
  (a, _s, w) <- runExcept $ runRWST m env initCounter
  return (a, w)