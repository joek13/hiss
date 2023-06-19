-- | Experimental tree-walk interpreter.
--   Not currently well-tested, but useful for playing around while we develop the backend.
module Interpreter.TreeWalker (interp, eval, globalEnv, insertDecl, HissValue (..), Environment) where

import Control.Monad (foldM, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Lazy (State, evalState, get, put, zipWithM_)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map (empty, fromList, insert, lookup, restrictKeys, union)
import Data.Set ((\\))
import Data.Set qualified as Set (fromList)
import Error (HissError (RuntimeError))
import Semantic.Names (collectNames)
import Syntax.AST (BinOp (..), Binding (..), Decl (..), Expr (..), Name (..), Program, UnaryOp (..), getIdent, progDecls, stripAnns)

-- | Hiss runtime value.
data HissValue
  = -- | Integer value
    Int Integer
  | -- | Boolean value
    Bool Bool
  | -- | Function value (closure).
    Func
      Environment
      -- ^ closure's captured environment
      (Name ())
      -- ^ function name
      [Name ()]
      -- ^ argument names
      (Expr ())
      -- ^ function body
  deriving (Eq)

-- | Shows the type of a HissValue.
showType :: HissValue -> String
showType (Int _) = "int"
showType (Bool _) = "bool"
showType (Func {}) = "function"

instance Show HissValue where
  show (Int x) = show x
  show (Bool True) = "true"
  show (Bool False) = "false"
  show (Func _ n [] _) = "(function '" <> getIdent n <> "' of ())"
  show (Func _ n args _) = "(function '" <> getIdent n <> "' of " <> intercalate "," (map getIdent args) <> ")"

-- | Table mapping Names to HissValues.
type Environment = Map (Name ()) HissValue

-- | Hiss evaluation monad.
type Hiss = ExceptT HissError (State Environment)

-- | Updates environment with a new Decl.
insertDecl :: Environment -> Decl () -> Either HissError Environment
insertDecl env (Decl _ (ValBinding _ name) body) = do
  value <- eval env body -- evaluate RHS in current env
  return $ Map.insert name value env -- update env with new binding
insertDecl env (Decl _ (FuncBinding _ name args) body) = do
  {-
    note: top level functions do not capture any vars, since they can only reference globals
    we avoid scope confusion by banning shadowing of global variables
  -}
  let func = Func Map.empty name args body
  return $ Map.insert name func env

-- | Constructs global environment for a Program.
globalEnv :: Program a -> Either HissError Environment
globalEnv prog = do
  foldM insertDecl Map.empty (map stripAnns $ progDecls prog)

-- | Interprets a program.
interp :: Program a -> Either HissError HissValue
interp prog = do
  env <- globalEnv prog
  -- lookup and evaluate main function
  case Name () "main" `Map.lookup` env of
    Just (Func _ _ [] body) -> eval env body
    Just (Func _ _ args _) -> Left (RuntimeError $ "Type error: function 'main' must have zero arguments, not " <> (show . length) args)
    Just x -> Left (RuntimeError $ "Type error: 'main' must be declared as function, not " <> showType x)
    Nothing -> Left (RuntimeError "Name error: missing function 'main'")

-- | Evaluates an expression in a given environment.
eval :: Environment -> Expr a -> Either HissError HissValue
eval env e = evalState (runExceptT $ eval' $ stripAnns e) env

eval' :: Expr () -> Hiss HissValue
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
    Nothing -> throwError (RuntimeError $ "Name error: name '" <> getIdent n <> "' undefined in current environment")
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
      let func = Func capturedEnv n args valExp
      insertBinding_ n func

      -- evaluate inExp in new environment
      inVal <- eval' inExp

      -- restore old environment
      put env
      return inVal
eval' (EFunApp () fun argExps) = do
  funVal <- eval' fun
  case funVal of
    func@(Func captured name argNames body) -> do
      case compare (length argExps) (length argNames) of
        EQ -> do
          -- all arguments present: apply the function
          -- evaluate arguments
          argVals <- mapM eval' argExps
          env <- mergeEnv captured
          -- insert binding for each function argument
          zipWithM_ insertBinding argNames argVals
          -- insert binding for function itself (enables recursive calls)
          insertBinding_ name func
          -- evaluate function body
          retVal <- eval' body
          -- restore old environment
          put env
          return retVal
        LT -> do
          {-
          we handle partial application of f creating a new closure f' whose:
            * captured env contains supplied args of f
            * args are the unsupplied args of f
          -}
          let nArgs = length argExps -- number of args provided
          argVals <- mapM eval' argExps -- evaluate the provided args

          -- create environment that maps supplied args to their values
          let partialEnv = Map.fromList (zip argNames argVals) :: Environment
          -- merge it with original environment
          let captured' = partialEnv `Map.union` captured

          {-
          we do not want to overwrite the binding of f to its own body
          so we append '$partial' to the name to make it unique
          -}
          let name' = Name () $ getIdent name <> "$partial"
          -- finally, we drop the supplied args from the resulting closure
          let argNames' = drop nArgs argNames
          return $ Func captured' name' argNames' body
        GT -> throwError (RuntimeError $ "Type error: function '" <> getIdent name <> "' expects " <> show (length argNames) <> " arguments, but " <> show (length argExps) <> " were provided.")
    x -> throwError (RuntimeError $ "Type error: " <> show x <> " is not a function")
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

-- | Binds 'name' to 'val' in current environment and returns the old environment.
insertBinding :: Name () -> HissValue -> Hiss Environment
insertBinding name val = do
  env <- get
  let env' = Map.insert name val env
  put env'
  return env

-- | Binds 'name' to 'val' in current environment.
insertBinding_ :: Name () -> HissValue -> Hiss ()
insertBinding_ name val = void (insertBinding name val)

-- | Merges an environment with the current environment and returns the old environment.
--   Prefers new bindings over current ones in case of duplicate names.
mergeEnv :: Environment -> Hiss Environment
mergeEnv env' = do
  env <- get
  put (env' `Map.union` env)
  return env