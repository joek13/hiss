module Semantic.Types.Constraints where

import Control.Monad (replicateM)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.RWS (MonadReader (ask, local), MonadWriter (tell), RWST (runRWST), gets, modify)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Error (HissError)
import Semantic.Types
  ( Cons (CBool, CInt),
    Scheme (..),
    Substitutable (..),
    Type (TCons, TFunc, TVar),
    TypeEnv,
    Var (Var),
    varNames,
  )
import Syntax.AST (BinOp (..), Binding (..), Expr (..), Name, UnaryOp (..), getIdent)
import Syntax.Lexer (Range)

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
  deriving (Eq, Show)

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

-- | Binds a name to a scheme and performs an Infer action in the modified environment.
bindEnv :: (Name Range, Scheme) -> Infer a -> Infer a
bindEnv (name, scheme) = local (Map.insert name scheme)

bindEnvMany :: [(Name Range, Scheme)] -> Infer a -> Infer a
bindEnvMany pairs = local (\e -> foldl (\m (n, s) -> Map.insert n s m) e pairs)

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
  EFunApp _ funExpr argExprs -> do
    funTy <- infer funExpr -- type of the function
    argTys <- mapM infer argExprs -- list of arguent types

    -- fresh variable to represent return type
    retTy <- fresh
    -- funExpr *should* have this type. matching argument types, return type
    let funTy' = TFunc argTys retTy
    constrain funTy funTy'

    return retTy
  EUnaryOp _ op subexpr -> inferUnary op subexpr
  EBinOp _ expr1 op expr2 -> inferBinary op expr1 expr2
  ELetIn _ binding valExpr inExpr -> case binding of
    ValBinding _ n -> do
      valTy <- infer valExpr
      valSc <- generalize valTy
      bindEnv (n, valSc) (infer inExpr)
    FuncBinding _ funcName argNames -> do
      funcSc <- inferFunc funcName argNames valExpr
      bindEnv (funcName, funcSc) (infer inExpr)
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
    inferArith = do
      expr1Ty <- infer expr1
      expr2Ty <- infer expr2
      constrain expr1Ty (TCons CInt)
      constrain expr2Ty (TCons CInt)
      return $ TCons CInt
    inferCmp = do
      expr1Ty <- infer expr1
      expr2Ty <- infer expr2
      constrain expr1Ty (TCons CInt)
      constrain expr2Ty (TCons CInt)
      return $ TCons CBool
    inferLogical = do
      expr1Ty <- infer expr1
      expr2Ty <- infer expr2
      constrain expr1Ty (TCons CBool)
      constrain expr2Ty (TCons CBool)
      return $ TCons CBool

inferFunc :: Name Range -> [Name Range] -> Expr Range -> Infer Scheme
inferFunc funcName argNames defnExpr = do
  -- create fresh type variables for each function argument
  argTys <- replicateM (length argNames) fresh
  let argScs = map (ForAll []) argTys

  -- fresh variable for ret type
  retTy <- fresh

  let funcTy = TFunc argTys retTy -- construct the function type
  let funcSc = ForAll [] funcTy
  let argPairs = zip argNames argScs -- list of (arg name, arg schema)

  -- infer the return type of the function from the body
  retTy' <-
    bindEnvMany
      ((funcName, funcSc) : argPairs) -- include itself since functions can be recursive
      (infer defnExpr)
  constrain retTy retTy'

  return funcSc

runInfer :: TypeEnv -> Infer a -> Either HissError (a, [Constraint])
runInfer env m = do
  (a, _s, w) <- runExcept $ runRWST m env initCounter
  return (a, w)