module Interpreter.TreeWalker (eval, HissValue) where

import Control.Monad (void)
import Control.Monad.State.Lazy (State, evalState, get, put, zipWithM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map (empty, insert, lookup, restrictKeys, union)
import Data.Set ((\\))
import Data.Set qualified as Set (fromList)
import Syntax.AST (BinOp (..), Exp (..), FunApp (..), LetBinding (..), Name (..), collectNames, getIdent, stripAnns)

data HissValue
  = Int Integer
  | Func
      Environment -- captured environment (bindings of referenced names)
      [Name ()] -- argument names
      (Exp ()) -- function body
  deriving (Eq, Show)

-- An Environment maps names to their value.
type Environment = Map (Name ()) HissValue

-- Hiss evaluation monad.
type Hiss = State Environment

-- Evaluates an AST.
eval :: Exp a -> HissValue
eval e = evalState (eval' e') Map.empty
  where
    e' = stripAnns e

eval' :: Exp () -> Hiss HissValue
-- literals evaluate to themselves
eval' (EInt () x) = return (Int x)
eval' (EBinOp () e1 op e2) = do
  -- evaluates left then right
  v1 <- eval' e1
  v2 <- eval' e2
  return (evalBinOp op v1 v2)
eval' (EParen () e1) = eval' e1
eval' (EVar () n) = do
  -- looks up variable in current environment
  env <- get
  case Map.lookup n env of
    Just val -> return val
    Nothing -> error $ "Name error: name " <> getIdent n <> " undefined in current environment"
eval' (ELetIn () lb valExp inExp) = do
  case lb of
    -- variable binding
    LetBinding () n [] -> do
      val <- eval' valExp -- compute value
      env <- insertBinding n val -- bind n to val

      -- evaluate inExp in new environment
      inVal <- eval' inExp

      -- restore old environment
      put env
      return inVal
    -- function binding
    LetBinding () n args -> do
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
    Int 0 -> eval' elseExp
    Int _ -> eval' thenExp
    _ -> error "Type error: if-then-else condition must be an integer"

evalBinOp :: BinOp -> HissValue -> HissValue -> HissValue
evalBinOp Add (Int a) (Int b) = Int (a + b)
evalBinOp Sub (Int a) (Int b) = Int (a - b)
evalBinOp Mult (Int a) (Int b) = Int (a * b)
evalBinOp Div (Int a) (Int b) = Int (a `div` b)
evalBinOp op _ _ = error $ "Type error: operator " <> show op <> " can only be applied to integers"

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
-- Prefers current bindings over new ones in case of duplicate names.
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
        LT -> error "Partial function application is unimplemented!"
        GT -> error $ "Type error: function expects " <> show (length argNames) <> " arguments, but " <> show (length argExps) <> " were provided."
    x -> error $ "Type error: " <> show x <> " is not a function"