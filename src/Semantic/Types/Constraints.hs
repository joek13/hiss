{-
    Functionality for performing type inference, generating type constraints, and solving generated constraints.
-}
module Semantic.Types.Constraints (runInfer, infer, solve) where

import Control.Monad (replicateM, when)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.RWS (MonadReader (ask, local), MonadState (get, put), MonadWriter (tell), RWST (runRWST), gets, modify)
import Control.Monad.State (StateT (runStateT))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Error (HissError (SemanticError))
import Semantic.Types
  ( Cons (CBool, CInt),
    Scheme (..),
    Subst,
    Substitutable (..),
    Type (TCons, TFunc, TVar),
    TypeEnv,
    Var (Var),
    compose,
    varNames,
  )
import Syntax.AST (BinOp (..), Binding (..), Expr (..), Name, UnaryOp (..), getIdent)
import Syntax.Lexer (Range)

-- | Given a name, looks up its type in current typing environment.
lookupName :: Name Range -> Infer Type
lookupName name = do
  env <- ask
  case Map.lookup name env of
    Nothing -> error $ "Compiler bug: lookup called on missing variable " <> getIdent name
    Just scheme -> do
      {-
        this achieves half of let-polymorphism
        a polytype is freshly instantiated upon every usage
      -}
      instantiate scheme

newtype Counter = Counter {count :: Int}

initCounter :: Counter
initCounter = Counter {count = 0}

-- | Type constraint.
newtype Constraint
  = -- | Type equality constraint that fst == snd.
    TyEq (Type, Type)
  deriving (Eq, Show)

instance Substitutable Constraint where
  apply s1 (TyEq (t1, t2)) = TyEq (apply s1 t1, apply s1 t2) -- substitute LHS and RHS
  freeVars (TyEq (t1, t2)) = freeVars t1 `Set.union` freeVars t2 -- union of freeVars of LHS and RHS

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

-- | Binds a name to a scheme and performs an Infer action in the modified environment.
bindEnv :: (Name Range, Scheme) -> Infer a -> Infer a
bindEnv (name, scheme) = local (Map.insert name scheme)

-- | Binds many names to corresponding type schemes and performs Infer action in the modified environment.
bindEnvMany :: [(Name Range, Scheme)] -> Infer a -> Infer a
bindEnvMany pairs = local (\e -> foldl (\m (n, s) -> Map.insert n s m) e pairs)

-- | Instantiates a type scheme, replacing its bound variables with fresh type variables.
instantiate :: Scheme -> Infer Type
instantiate (ForAll vars ty) = do
  vars' <- mapM (const fresh) vars
  let subst = Map.fromList $ zip vars vars'
  return $ apply subst ty

-- | Generalizes over a type with respect to a given typing environment,
-- closing over its free variables.
generalize :: TypeEnv -> Type -> Scheme
generalize env ty =
  let vars = Set.toList $ freeVars ty `Set.difference` freeVars env
   in ForAll vars ty

-- | Appends constraint that t1 == t2.
constrain :: Type -> Type -> Infer ()
constrain t1 t2 = tell [TyEq (t1, t2)]

-- | Infers type of an expression in current typing environment.
-- Returns the inferred type and emits constraints to be solved.
infer :: Expr Range -> Infer Type
infer expr = case expr of
  -- typing literals is easy
  EInt _ _ -> return $ TCons CInt
  EBool _ _ -> return $ TCons CBool
  -- to type variables, we just look up and instantiate the matching type scheme
  EVar _ n -> lookupName n
  EFunApp _ funExpr argExprs -> do
    funTy <- infer funExpr -- inferred type of the function
    argTys <- mapM infer argExprs -- list of inferred arguent types

    -- fresh variable to represent return type
    retTy <- fresh
    let funTy' = TFunc argTys retTy -- funExpr SHOULD have this type -- matching argument/ret types

    -- enforce that funExpr has the type we expect
    constrain funTy funTy'

    return retTy
  EUnaryOp _ op subexpr -> inferUnary op subexpr
  EBinOp _ expr1 op expr2 -> inferBinary op expr1 expr2
  ELetIn _ binding valExpr inExpr -> case binding of
    ValBinding _ n -> do
      {-
      TODO: rethink this
      not necessarily the case that value bindings are monomorphic
      e.g.,

        let f(x) = x in let g = f in let h = g(0) in g(false)

      doesn't type because g is bound as a value instead of a function
      and can't take advantage of let-polymorphism
      even though g has type (a) -> a
      -}
      valTy <- infer valExpr
      let valSc = ForAll [] valTy
      bindEnv (n, valSc) (infer inExpr)
    FuncBinding _ funcName argNames -> do
      {-
         ugh!
         we have to break out of this monad for a second.
         to implement let-polymorphism, we want to make sure to infer constraints for a function's arguments
         before we generalize over unconstrained arguments.

         to do this, we essentially call a sub-inference (that doesn't affect our state) and solve its constraints.
         then, we use the solved substitution to generate a scheme that only generalizes over unconstrained terms.
       -}
      env <- ask
      case runInfer env (inferFunc funcName argNames valExpr) of
        Left err -> throwError err
        Right (funcTy, cs) -> do
          case solve cs of
            Left err -> throwError err
            Right subst -> do
              let scheme = generalize (apply subst env) (apply subst funcTy)
              bindEnv (funcName, scheme) $ local (apply subst) (infer inExpr)
  EIf _ condExpr thenExpr elseExpr -> do
    -- condition must be bool
    condTy <- infer condExpr
    constrain condTy (TCons CBool)

    -- then/else must have matching type
    thenTy <- infer thenExpr
    elseTy <- infer elseExpr
    constrain thenTy elseTy

    return thenTy
  EParen _ subexpr -> infer subexpr

inferUnary :: UnaryOp -> Expr Range -> Infer Type
inferUnary op expr = case op of
  Not -> do
    exprTy <- infer expr
    constrain exprTy (TCons CBool)
    return $ TCons CBool

inferBinary :: BinOp -> Expr Range -> Expr Range -> Infer Type
inferBinary op expr1 expr2 = case op of
  Add -> inferArith
  Sub -> inferArith
  Mult -> inferArith
  Div -> inferArith
  Equals -> inferCmp
  NotEquals -> inferCmp
  GreaterThan -> inferCmp
  GreaterEqual -> inferCmp
  LessThan -> inferCmp
  LessEqual -> inferCmp
  And -> inferLogical
  Or -> inferLogical
  where
    -- int -> int -> int
    inferArith = do
      expr1Ty <- infer expr1
      expr2Ty <- infer expr2
      constrain expr1Ty (TCons CInt)
      constrain expr2Ty (TCons CInt)
      return $ TCons CInt
    -- int -> int -> bool
    inferCmp = do
      expr1Ty <- infer expr1
      expr2Ty <- infer expr2
      constrain expr1Ty (TCons CInt)
      constrain expr2Ty (TCons CInt)
      return $ TCons CBool
    -- bool -> bool -> bool
    inferLogical = do
      expr1Ty <- infer expr1
      expr2Ty <- infer expr2
      constrain expr1Ty (TCons CBool)
      constrain expr2Ty (TCons CBool)
      return $ TCons CBool

inferFunc :: Name Range -> [Name Range] -> Expr Range -> Infer Type
inferFunc funcName argNames defnExpr = do
  -- create fresh type variables for each function argument
  argTys <- replicateM (length argNames) fresh
  let argScs = map (ForAll []) argTys

  -- fresh variable for ret type
  retTy <- fresh

  let funcTy = TFunc argTys retTy -- construct the function type
  let funcSc = ForAll [] funcTy
  let argPairs = zip argNames argScs -- list of (arg name, arg scheme)

  -- infer the return type of the function from the body
  retTy' <-
    bindEnvMany
      ((funcName, funcSc) : argPairs) -- include itself since functions can be recursive
      (infer defnExpr)
  constrain retTy retTy'

  return funcTy

runInfer :: TypeEnv -> Infer a -> Either HissError (a, [Constraint])
runInfer env m = do
  (a, _s, w) <- runExcept $ runRWST m env initCounter
  return (a, w)

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

-- | Checks if a type var occurs within a given type.
occursIn :: Var -> Type -> Bool
occursIn v t = v `Set.member` freeVars t

-- | Constraint solver.
solver :: Solve Subst
solver = do
  (subst, cs) <- get -- (incremental solution, remaining constraints)
  case cs of
    [] -> return subst
    -- take one constraint and solve it
    (TyEq (t1, t2) : cs') -> do
      (s1, cs1) <- unify t1 t2
      put (s1 `compose` subst, cs1 ++ map (apply s1) cs')
      solver

solve :: [Constraint] -> Either HissError Subst
solve cs = do
  (subst, _s) <- runExcept $ runStateT solver (Map.empty, cs)
  return subst