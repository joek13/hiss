{-
  Type inference, constraint generation, and solving.

  Based off of code from https://github.com/sdiehl/write-you-a-haskell/blob/master/006_hindley_milner.md.
  Other referencees:
  - https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
-}
module Semantic.Types.Constraints (runInfer, infer, inferDecl, inferProgram, solve, generalize) where

import Control.Monad (replicateM, zipWithM_)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.RWS (MonadReader (ask, local), MonadState (get, put), MonadWriter (tell), RWST (runRWST), gets, modify)
import Control.Monad.State (StateT (runStateT))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Error (HissError (SemanticError))
import Semantic.Dependencies
import Semantic.Types
  ( Cons (CBool, CInt),
    Scheme (..),
    Subst,
    Substitutable (..),
    Type (..),
    TypeEnv,
    TypedExpr,
    Var (Var),
    compose,
    getTy,
    varNames,
  )
import Syntax.AST (Annotated (getAnn), BinOp (..), Binding (..), Decl (..), Expr (..), Name, Program (..), UnaryOp (..), declGetName, getIdent, stripAnns)
import Syntax.Lexer (Range)

-- | Given a name, looks up its type in current typing environment.
lookupName :: Name Range -> Infer Type
lookupName name = do
  env <- ask
  case Map.lookup (stripAnns name) env of
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
bindEnv :: (Name (), Scheme) -> Infer a -> Infer a
bindEnv (name, scheme) = local (Map.insert name scheme)

-- | Binds many names to corresponding type schemes and performs Infer action in the modified environment.
bindEnvMany :: [(Name (), Scheme)] -> Infer a -> Infer a
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
-- Returns type annotated expression and emits constraints to be solved.
infer :: Expr Range -> Infer TypedExpr
infer expr = case expr of
  -- typing literals is easy
  EInt _ _ -> return $ fmap (,TCons CInt) expr
  EBool _ _ -> return $ fmap (,TCons CBool) expr
  -- to type variables, we just look up and instantiate the matching type scheme
  EVar _ n -> do
    ty <- lookupName n
    return $ fmap (,ty) expr
  EFunApp r funExpr argExprs -> do
    -- infer function type
    funExpr' <- infer funExpr
    let funTy = getTy funExpr'

    -- funExpr SHOULD have this type
    retTy <- fresh
    argExprs' <- mapM infer argExprs
    let argTys = map getTy argExprs'

    let funTy' = mkCurried retTy argTys

    -- enforce that funExpr has the type we expect
    constrain funTy funTy'

    return $ EFunApp (r, retTy) funExpr' argExprs'
  EUnaryOp {} -> inferUnary expr
  EBinOp {} -> inferBinary expr
  ELetIn r binding valExpr inExpr -> case binding of
    ValBinding _ n -> do
      {-
        note that this code means value bindings are always monomorphic, even if you're just declaring a synonym of a polymorphic type.
        e.g., `let f(x) = x in let g = f in let h = g(false) in let i = g(0)` does not typecheck, since h is monomorphic.
        will possibly have to rethink this limitation, but for right now it is enforced by a test in TypecheckingSpec ("monomorphizes value declarations")
      -}
      valExpr' <- infer valExpr
      let valTy = getTy valExpr'

      let valSc = ForAll [] valTy
      inExpr' <- bindEnv (stripAnns n, valSc) (infer inExpr)
      let inTy = getTy inExpr'

      -- bindings don't have types, but it needs an annotation - we use unit
      let binding' = fmap (,TUnit) binding

      return $ ELetIn (r, inTy) binding' valExpr' inExpr'
    FuncBinding _ funcName argNames -> do
      {-
        to implement let-polymorphism, we want to infer and solve constraints for a function's arguments
        then, we generalize over unconstrained type variables in the function type

        to accomplish this, we launch a 'sub-inference' and immediately solve its collected constraints
      -}
      env <- ask
      ctr <- get
      case runInfer' ctr env (inferFunc funcName argNames valExpr) of
        Left err -> throwError err
        Right ((funcTy, valExpr'), ctr', cs) -> do
          -- update counter so that type vars are unique across all runInfers
          put ctr'
          {-
            propagate constraints in case the sub-inference constrains an existing variable
            e.g., `add(x) = let inner(y) = x + y in inner` would drop constraint x :: int
           -}
          tell cs
          -- solve function constraints
          case solve cs of
            Left err -> throwError err
            Right subst -> do
              -- apply solved constraints and generalize over unconstrained variables
              let scheme = generalize (apply subst env) (apply subst funcTy)

              inExpr' <- bindEnv (stripAnns funcName, scheme) $ local (apply subst) (infer inExpr)
              let inTy = getTy inExpr'

              -- dummy annotation for binding
              let binding' = fmap (,TUnit) binding

              return $ ELetIn (r, inTy) binding' valExpr' inExpr'
  EIf r condExpr thenExpr elseExpr -> do
    -- condition must be bool
    condExpr' <- infer condExpr
    let condTy = getTy condExpr'

    constrain condTy (TCons CBool)

    -- then/else must have matching type
    thenExpr' <- infer thenExpr
    let thenTy = getTy thenExpr'
    elseExpr' <- infer elseExpr
    let elseTy = getTy elseExpr'

    constrain thenTy elseTy

    return $ EIf (r, thenTy) condExpr' thenExpr' elseExpr'
  EParen _ subexpr -> infer subexpr

-- | Given a return type and list of argument types,
-- constructs the corresponding curried function type.
-- I.e., converts from `(a,b,c) -> e` to `a -> (b -> (c -> e))`
mkCurried :: Type -> [Type] -> Type
mkCurried ret [] = TFunc TUnit ret
mkCurried ret args = foldr TFunc ret args

inferUnary :: Expr Range -> Infer TypedExpr
inferUnary (EUnaryOp r op expr) = case op of
  Not -> do
    expr' <- infer expr
    let exprTy = getTy expr'

    constrain exprTy (TCons CBool)

    return $ EUnaryOp (r, TCons CBool) op expr'
inferUnary _ = error "Compiler bug: inferUnary called on something other than EUnaryOp"

inferBinary :: Expr Range -> Infer TypedExpr
inferBinary (EBinOp r expr1 op expr2) = case op of
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
      expr1' <- infer expr1
      let expr1Ty = getTy expr1'
      expr2' <- infer expr2
      let expr2Ty = getTy expr2'

      constrain expr1Ty (TCons CInt)
      constrain expr2Ty (TCons CInt)

      return $ EBinOp (r, TCons CInt) expr1' op expr2'
    -- int -> int -> bool
    inferCmp = do
      expr1' <- infer expr1
      let expr1Ty = getTy expr1'
      expr2' <- infer expr2
      let expr2Ty = getTy expr2'

      constrain expr1Ty (TCons CInt)
      constrain expr2Ty (TCons CInt)

      return $ EBinOp (r, TCons CBool) expr1' op expr2'
    -- bool -> bool -> bool
    inferLogical = do
      expr1' <- infer expr1
      let expr1Ty = getTy expr1'
      expr2' <- infer expr2
      let expr2Ty = getTy expr2'

      constrain expr1Ty (TCons CBool)
      constrain expr2Ty (TCons CBool)

      return $ EBinOp (r, TCons CBool) expr1' op expr2'
inferBinary _ = error "Compiler bug: inferBinary called on something other than EBinOp"

-- | Given a function's name, the names of its arguments, and its body expr,
-- infers the type of the function. Returns (inferred function type, typed body expr).
inferFunc :: Name Range -> [Name Range] -> Expr Range -> Infer (Type, TypedExpr)
inferFunc funcName argNames defnExpr = do
  let argNames' = map stripAnns argNames
  -- create fresh type variables for each function argument
  argTys <- replicateM (length argNames) fresh
  let argScs = map (ForAll []) argTys

  -- fresh variable for the function type
  funcTy <- fresh
  let funcSc = ForAll [] funcTy
  let argPairs = zip argNames' argScs -- list of (arg name, arg scheme)

  -- infer the return type of the function from the body
  defnExpr' <-
    bindEnvMany
      ((stripAnns funcName, funcSc) : argPairs) -- include itself since functions can be recursive
      (infer defnExpr)

  let retTy = getTy defnExpr'

  let funcTy' = mkCurried retTy argTys

  constrain funcTy funcTy'

  return (funcTy, defnExpr')

-- | Infers a value declaration, or a polymorphic func declaration.
inferDecl :: Decl Range -> Infer (Decl (Range, Type))
inferDecl decl@(Decl _ (ValBinding {}) _) = inferValDecl decl
inferDecl decl@(Decl _ (FuncBinding {}) _) = inferFuncDeclPoly decl

-- | Infers type of a value declaration.
--   Compiler bug if a function declaration is provided.
inferValDecl :: Decl Range -> Infer (Decl (Range, Type))
inferValDecl (Decl a binding@(ValBinding {}) defn) = do
  -- binding has type TUnit
  let binding' = fmap (,TUnit) binding
  defn' <- infer defn
  -- decl has type of thing declared
  return $ Decl (a, getTy defn') binding' defn'
inferValDecl _ = error "Compiler bug: inferValDecl called on something other than a value decl"

-- | Infers type of a function declaration monomorphically.
--   I.e., without trying to solve and generalize over constrained variables.
--   Compiler bug if a value declaration is provided.
inferFuncDeclMono :: Decl Range -> Infer (Decl (Range, Type))
inferFuncDeclMono (Decl a binding@(FuncBinding _ funcName argNames) defnExpr) = do
  -- binding has type TUnit
  let binding' = fmap (,TUnit) binding

  -- infer function type monomorphically
  (funcTy, defnExpr') <- inferFunc funcName argNames defnExpr

  -- return annotated tree
  return $ Decl (a, funcTy) binding' defnExpr'
inferFuncDeclMono _ = error "Compiler bug: inferFuncDeclPoly called on something other than a func decl"

-- | Infers type of a function declaration polymorphically.
--   Runs a "sub-inference" to solve any constrained variables in the function's definition.
--   Other variables are left free and can be generalized to produce a polymorphic function.
--   Compiler bug if a value declaration is provided.
inferFuncDeclPoly :: Decl Range -> Infer (Decl (Range, Type))
inferFuncDeclPoly (Decl a binding@(FuncBinding _ funcName argNames) defnExpr) = do
  -- binding has type TUnit
  let binding' = fmap (,TUnit) binding
  env <- ask
  ctr <- get
  case runInfer' ctr env (inferFunc funcName argNames defnExpr) of
    Left err -> throwError err
    Right ((funcTy, defnExpr'), ctr', cs) -> do
      -- update counter so that type vars are unique across all runInfers
      put ctr'
      {-
        propagate constraints in case the sub-inference constrains an existing variable
        e.g., `add(x) = let inner(y) = x + y in inner` would drop constraint x :: int
        -}
      tell cs
      -- solve function constraints
      case solve cs of
        Left err -> throwError err
        Right subst -> do
          let funcTy' = apply subst funcTy

          -- decl has type of thing declared
          return $ Decl (a, funcTy') binding' defnExpr'
inferFuncDeclPoly _ = error "Compiler bug: inferFuncDeclPoly called on something other than a func decl"

-- | Infers type of a program in current typing environment.
-- Returns type annotated program and emits constraints.
inferProgram :: Program Range -> Infer (Program (Range, Type))
inferProgram prog = do
  {-
    complexity here because our type system does not permit polymorphic, mutually recursive functions.
    this is to keep the implementation simple and because, i think, the type system would be undecidable otherwise.
    (see https://suif.stanford.edu/~brm/reading/p253-henglein.pdf)

    our solution is to group into mutually dependent sets and handle two cases separately:
    1. singleton groups: decls for values, nonrecursive functions, and non-mutually recursive functions
    2. groups with >= 2 decls: decls for mutually recursive functions

    in the first case, we can proceed typing the decl as usual: infer the type of x, bind x to the inferred type, continue
    in the second case, we introduce fresh type variables for each decl in the group (forces them to be monomorphic), infer
      the type of each function, and then constrain inferred type to be same as the type variable
  -}
  -- group decls according to dependency graph
  let declGroups = groupDecls prog
  -- process each group in order, inferring type and adding to environment
  declGroups' <- doInferGroups declGroups
  -- return the type annotated program
  return $ Program (getAnn prog, TUnit) (concat declGroups')
  where
    -- singleton decl group: no mutual recursion
    doInferGroups ([decl@(Decl _ binding _)] : groups) = do
      decl' <- inferDecl decl
      env <- ask
      let sc = case binding of
            ValBinding {} -> ForAll [] (getTy decl')
            FuncBinding {} -> (generalize env . getTy) decl'
      groups' <- bindEnv ((stripAnns . declGetName) decl, sc) (doInferGroups groups)
      return $ [decl'] : groups'
    -- >= 2 mutually recursive functions
    doInferGroups (group : groups) = do
      -- create type variables for each function
      groupTys <- replicateM (length group) fresh
      let groupScs = map (ForAll []) groupTys

      -- list of [(name, scheme)]
      let funcNames = map (stripAnns . declGetName) group
      let funcScPairs = zip funcNames groupScs

      -- note: compiler bug if we give inferFuncDeclMono a val decl
      -- however if we run Semantic.Dependencies.reorderDecls first programs with mutually recursive values are thrown out
      group' <- bindEnvMany funcScPairs (mapM inferFuncDeclMono group)
      let funcTys = map getTy group'

      zipWithM_ constrain groupTys funcTys

      groups' <- bindEnvMany funcScPairs (doInferGroups groups)
      return $ group' : groups'
    doInferGroups [] = return []

runInfer :: TypeEnv -> Infer a -> Either HissError (a, [Constraint])
runInfer env m = do
  (a, _ctr, cs) <- runInfer' initCounter env m
  return (a, cs)

runInfer' :: Counter -> TypeEnv -> Infer a -> Either HissError (a, Counter, [Constraint])
runInfer' ctr env m = runExcept $ runRWST m env ctr

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
unify (TFunc arg1 ret1) (TFunc arg2 ret2) = do
  unifyMany [ret1, arg1] [ret2, arg2]
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