module Interpreter.TreeWalker (eval, HissValue) where

import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Map (Map, empty, insert, lookup)
import Syntax.AST (BinOp (..), Exp (..), LetBinding (..), Name (..), getIdent, stripAnns)

data HissValue
  = Int Integer
  | Func (Name ()) [Name ()] (Exp ())
  deriving (Eq, Show)

type Environment = Map (Name ()) HissValue

-- Hiss evaluation monad.
type Hiss = State Environment

eval :: Exp a -> HissValue
eval e = evalState (eval' e') empty
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
  case Data.Map.lookup n env of
    Just val -> return val
    Nothing -> error $ "Name error: name " <> getIdent n <> " undefined in current environment"
eval' (ELetIn () lb valExp inExp) = do
  case lb of
    LetBinding () n [] -> do
      val <- eval' valExp -- compute value
      env <- insertBinding n val -- bind n to val

      -- evaluate inExp in new environment
      inVal <- eval' inExp

      -- restore old environment
      put env
      return inVal
    _ -> error "Function declarations are unimplemented!"
eval' _ = error "Unimplemented!"

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
  let env' = insert name val env
  put env'
  return env