module Semantic.Names (collectNames, expCheckNames) where

import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set (empty, insert, member, singleton, union)
import Error (HissError (SemanticError))
import Syntax.AST (Binding (..), Exp (..), FunApp (..), Name (..))
import Syntax.Lexer (AlexPosn (..), Range (..))

-- Collects all Names referenced in an expression and its children.
collectNames :: Exp a -> Set (Name a)
collectNames (EBool _ _) = Set.empty
collectNames (EInt _ _) = Set.empty
collectNames (EVar _ n) = Set.singleton n
collectNames (EFunApp _ funApp) = funAppCollectNames funApp
collectNames (EUnaryOp _ _ e1) = collectNames e1
collectNames (EBinOp _ e1 _ e2) = collectNames e1 `Set.union` collectNames e2
collectNames (ELetIn _ _ e1 e2) = collectNames e1 `Set.union` collectNames e2
collectNames (EIf _ e1 e2 e3) = collectNames e1 `Set.union` collectNames e2 `Set.union` collectNames e3
collectNames (EParen _ e1) = collectNames e1

-- Collects all Names referenced in a FunApp and its children.
funAppCollectNames :: FunApp a -> Set (Name a)
funAppCollectNames (FunApp _ fun args) = collectNames fun `Set.union` foldl Set.union Set.empty (map collectNames args)

-- Traverses an expression and make sure each name is defined in its context.
expCheckNames :: Exp Range -> Either HissError (Exp Range)
expCheckNames = expCheckNames' Set.empty

expCheckNames' :: NameSet -> Exp Range -> Either HissError (Exp Range)
-- uses State monad to track currently declared names as we traverse the AST.
expCheckNames' defined ast = case evalState (checkNames' ast) defined of
  [] -> Right ast -- no errors: return AST unchanged
  msg : _ -> Left (SemanticError msg) -- errors: return first error

-- Set of Names annotated with Range information.
type NameSet = Set (Name Range)

-- Checks if name is declared in current scope.
checkName :: Name Range -> State NameSet (Maybe String)
checkName name@(Name r str) = do
  declaredNames <- get
  if name `Set.member` declaredNames
    then return Nothing
    else return $ Just $ "Use of undeclared name '" <> str <> "' at line " <> show line <> ", column " <> show column
  where
    Range (AlexPn _ line column) _ = r

-- Recursively checks that names in the expression and all of its children are declared.
checkNames' :: Exp Range -> State NameSet [String]
checkNames' (EInt _ _) = return []
checkNames' (EBool _ _) = return []
-- EVar: check that the variable is declared
checkNames' (EVar _ name) = maybeToList <$> checkName name
checkNames' (EFunApp _ (FunApp _ fun args)) = concat <$> mapM checkNames' (fun : args)
checkNames' (EUnaryOp _ _ e1) = checkNames' e1
checkNames' (EBinOp _ e1 _ e2) = concat <$> mapM checkNames' [e1, e2]
checkNames' (ELetIn _ (ValBinding _ name) e1 e2) = do
  -- let <name> = <e1> in <e2>
  -- name should be declared in e1 (e.g. recursive binding) and e2
  declaredNames <- get
  putName name
  errs <- mapM checkNames' [e1, e2]
  put declaredNames
  return $ concat errs
checkNames' (ELetIn _ (FuncBinding _ name args) e1 e2) = do
  -- let <name>(<args>) = <e1> in <e2>
  -- both name and args are declared in e1
  -- but only name is declared in e2
  declaredNames <- get
  putName name -- mark name as declared
  errs2 <- checkNames' e2
  mapM_ putName args -- mark args as declared
  errs1 <- checkNames' e1
  put declaredNames -- restore previously declared names
  return $ errs1 ++ errs2
checkNames' (EIf _ e1 e2 e3) = concat <$> mapM checkNames' [e1, e2, e3]
checkNames' (EParen _ e1) = checkNames' e1

putName :: Name Range -> State NameSet ()
putName n = do
  declaredNames <- get
  put $ n `Set.insert` declaredNames
  return ()