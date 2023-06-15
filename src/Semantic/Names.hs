module Semantic.Names (collectNames, expCheckNames, progCheckNames) where

import Control.Monad.State (MonadState (get, put), State, evalState)
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set (empty, fromList, insert, member, singleton, union)
import Error (HissError (SemanticError))
import Syntax.AST (Binding (..), Decl (..), Exp (..), FunApp (..), Name (..), Program, declGetName, getAnn, getIdent)
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

-- Collects top-level declared names in a program, ensuring that they are unique.
-- Returns HissError if a duplicate is found.
progCollectDeclNames :: Program Range -> Either HissError (Set (Name Range))
progCollectDeclNames prog = foldl f (Right Set.empty) (map declGetName prog)
  where
    f (Right names) name@(Name r s)
      | name `Set.member` names = Left (SemanticError $ "Name '" <> s <> "' redeclared at line " <> show line <> ", column " <> show column)
      | otherwise = Right (name `Set.insert` names)
      where
        Range _ (AlexPn _ line column) = r
    f (Left err) _ = Left err

-- Checks all the names in a program and makes sure they are defined in cnotext.
progCheckNames :: Program Range -> Either HissError (Program Range)
progCheckNames prog = do
  globals <- progCollectDeclNames prog
  -- given prog's top level declarations,
  -- checks the body of each decl for undeclared names.
  case concatMap (checkDecl (globals, globals)) prog of
    [] -> Right prog -- no errors: return program unchanged
    msg : _ -> Left (SemanticError msg) -- errors: return first one
  where
    -- checks name of a given declaration given (globals, already-declared names)
    checkDecl decl (Decl _ (ValBinding _ _) body) = expCheckNames' decl body
    checkDecl (globals, _) (Decl _ (FuncBinding _ _ args) body) = expCheckNames' (globals, globals `Set.union` args') body
      where
        args' = Set.fromList args

-- Traverses an expression and make sure each name is defined in its context.
expCheckNames :: Exp Range -> Either HissError (Exp Range)
expCheckNames ast = case expCheckNames' (Set.empty, Set.empty) ast of
  [] -> Right ast -- no errors: return exp unchanged
  msg : _ -> Left (SemanticError msg) -- errors: return first one

expCheckNames' :: NameSet -> Exp Range -> [String]
-- uses State monad to track currently declared names as we traverse the AST.
expCheckNames' defined ast = evalState (checkNames' ast) defined

-- (globals, defined)
type NameSet = (Set (Name Range), Set (Name Range))

-- Checks if name is declared in current scope.
checkName :: Name Range -> State NameSet (Maybe String)
checkName name@(Name r str) = do
  (_, declaredNames) <- get
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
  (globals, declared) <- get
  -- check e1 (without new binding)
  errs1 <- checkNames' e1
  -- are we shadowing a global?
  errs0 <- checkDeclShadowsGlobal name
  -- insert new binding
  putName name
  -- check e2 (with new binding)
  errs2 <- checkNames' e2
  -- restore old environment
  put (globals, declared)
  return $ errs0 ++ errs1 ++ errs2
checkNames' (ELetIn _ (FuncBinding _ name args) e1 e2) = do
  -- let <name>(<args>) = <e1> in <e2>
  -- both name and args are declared in e1
  -- but only name is declared in e2
  (globals, declared) <- get
  errs0 <- checkDeclShadowsGlobal name
  putName name -- mark name as declared
  errs2 <- checkNames' e2
  mapM_ putName args -- mark args as declared
  errs1 <- checkNames' e1
  put (globals, declared) -- restore previously declared names
  return $ errs0 ++ errs1 ++ errs2
checkNames' (EIf _ e1 e2 e3) = concat <$> mapM checkNames' [e1, e2, e3]
checkNames' (EParen _ e1) = checkNames' e1

checkDeclShadowsGlobal :: Name Range -> State NameSet [String]
checkDeclShadowsGlobal name = do
  (globals, _) <- get
  if name `Set.member` globals
    then
      let Range _ (AlexPn _ line column) = getAnn name
       in return ["Let binding of '" <> getIdent name <> "' at line " <> show line <> ", column " <> show column <> " shadows global binding"]
    else return []

putName :: Name Range -> State NameSet ()
putName n = do
  (globals, declaredNames) <- get
  put (globals, n `Set.insert` declaredNames)
  return ()