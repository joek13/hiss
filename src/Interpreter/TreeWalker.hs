module Interpreter.TreeWalker (eval, HissValue) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Lazy (State, evalState, get, put, zipWithM_)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (empty, fromList, insert, lookup, restrictKeys, union)
import Data.Set ((\\))
import Data.Set qualified as Set (fromList)
import Error (HissError (RuntimeError))
import Syntax.AST (BinOp (..), Exp (..), FunApp (..), Binding (..), Name (..), UnaryOp (..), getIdent, stripAnns)
import Semantic.Names (collectNames)

data HissValue
  = Int Integer
  | Bool Bool
  | Func
      Environment -- captured environment (bindings of referenced names)
      [Name ()] -- argument names
      (Exp ()) -- function body
  deriving (Eq)

showType :: HissValue -> String
showType (Int _) = "int"
showType (Bool _) = "bool"
showType (Func {}) = "function"

instance Show HissValue where
  show (Int x) = show x
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Func _ args _) = "(function of " <> intercalate "," (map getIdent args) <> ")"

-- An Environment maps names to their value.
type Environment = Map (Name ()) HissValue

-- Hiss evaluation monad.
type Hiss = ExceptT HissError (State Environment)

-- Evaluates an AST.
eval :: Exp a -> Either HissError HissValue
eval e = evalState (runExceptT $ eval' e') Map.empty
  where
    e' = stripAnns e

eval' :: Exp () -> Hiss HissValue
-- literals evaluate to themselves
eval' (EBool () b) = return (Bool b)
eval' (EInt () x) = return (Int x)
eval' (EUnaryOp () op e1) = do
  v1 <- eval' e1
  evalUnaryOp op v1
eval' (EBinOp () e1 op e2) = do
  -- evaluates left then right
  v1 <- eval' e1
  v2 <- eval' e2
  evalBinOp op v1 v2
eval' (EParen () e1) = eval' e1
eval' (EVar () n) = do
  -- looks up variable in current environment
  env <- get
  case Map.lookup n env of
    Just val -> return val
    Nothing -> throwError (RuntimeError $ "Name error: name " <> getIdent n <> " undefined in current environment")
eval' (ELetIn () b valExp inExp) = do
  case b of
    -- val binding
    ValBinding () n -> do
      val <- eval' valExp -- compute value
      env <- insertBinding n val -- bind n to val

      -- evaluate inExp in new environment
      inVal <- eval' inExp

      -- restore old environment
      put env
      return inVal
    -- function binding
    FuncBinding () n args -> do
      -- names referenced inside the closure (minus the function arguments)
      let capturedNames = collectNames valExp \\ Set.fromList args

      -- get captured values' associated bindings
      env <- get
      let capturedEnv = Map.restrictKeys env capturedNames

      -- create and bind function object
      let func = Func capturedEnv args valExp
      insertBinding_ n func

      -- evaluate inExp in new environment
      inVal <- eval' inExp

      -- restore old environment
      put env
      return inVal
eval' (EFunApp () funApp) = evalFunApp funApp
eval' (EIf () condExp thenExp elseExp) = do
  condVal <- eval' condExp
  case condVal of
    Bool True -> eval' thenExp
    Bool False -> eval' elseExp
    _ -> throwError (RuntimeError "Type error: if-then-else condition must be a bool")

evalUnaryOp :: UnaryOp -> HissValue -> Hiss HissValue
evalUnaryOp Not (Bool b) = pure $ Bool (not b)
evalUnaryOp Not x = throwError (RuntimeError $ "Type error: operator " <> show Not <> " cannot be applied to argument of type " <> showType x)

evalBinOp :: BinOp -> HissValue -> HissValue -> Hiss HissValue
evalBinOp op (Int a) (Int b) = case op of
  Add -> pure $ Int (a + b)
  Sub -> pure $ Int (a - b)
  Mult -> pure $ Int (a * b)
  Div -> pure $ Int (a `div` b)
  Equals -> pure $ Bool (a == b)
  NotEquals -> pure $ Bool (a /= b)
  LessThan -> pure $ Bool (a < b)
  LessEqual -> pure $ Bool (a <= b)
  GreaterThan -> pure $ Bool (a > b)
  GreaterEqual -> pure $ Bool (a >= b)
  _ -> throwError (RuntimeError $ "Type error: operator " <> show op <> " cannot be applied to arguments of type int,int")
evalBinOp op (Bool a) (Bool b) = case op of
  Equals -> pure $ Bool (a == b)
  NotEquals -> pure $ Bool (a /= b)
  And -> pure $ Bool (a && b)
  Or -> pure $ Bool (a || b)
  _ -> throwError (RuntimeError $ "Type error: operator " <> show op <> " cannot be applied to arguments of type bool,bool")
evalBinOp op x y = throwError (RuntimeError $ "Type error: operator " <> show op <> " cannot be applied to arguments of type " <> showType x <> "," <> showType y)

-- Binds 'name' to 'val' in current environment and returns the old environment.
insertBinding :: Name () -> HissValue -> Hiss Environment
insertBinding name val = do
  env <- get
  let env' = Map.insert name val env
  put env'
  return env

-- Binds 'name' to 'val' in current environment.
insertBinding_ :: Name () -> HissValue -> Hiss ()
insertBinding_ name val = void (insertBinding name val)

-- Merges an environment with the current environment and returns the old environment.
-- Prefers new bindings over current ones in case of duplicate names.
mergeEnv :: Environment -> Hiss Environment
mergeEnv env' = do
  env <- get
  put (env' `Map.union` env)
  return env

evalFunApp :: FunApp () -> Hiss HissValue
evalFunApp (FunApp () fun argExps) = do
  funVal <- eval' fun
  case funVal of
    (Func captured argNames body) -> do
      case compare (length argExps) (length argNames) of
        EQ -> do
          -- all arguments present: apply the function
          -- evaluate arguments
          argVals <- mapM eval' argExps
          env <- mergeEnv captured
          -- insert binding for each function argument
          zipWithM_ insertBinding argNames argVals
          -- evaluate function body
          retVal <- eval' body
          -- restore old environment
          put env
          return retVal
        LT -> do
          -- partial application: create a new closure
          let nArgs = length argExps -- number of args provided
          argVals <- mapM eval' argExps -- evaluate the provided args
          let partialEnv = Map.fromList (zip argNames argVals) :: Environment
          let captured' = partialEnv `Map.union` captured -- update captured environment with bindings for provided args
          let argNames' = drop nArgs argNames -- drop provided args from resulting closure
          return (Func captured' argNames' body)
        GT -> throwError (RuntimeError $ "Type error: function expects " <> show (length argNames) <> " arguments, but " <> show (length argExps) <> " were provided.")
    x -> throwError (RuntimeError $ "Type error: " <> show x <> " is not a function")