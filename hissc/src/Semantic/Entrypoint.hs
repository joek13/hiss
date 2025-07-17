module Semantic.Entrypoint (checkEntrypoint) where

import Data.Foldable (find)
import Error (HissError (SemanticError))
import Syntax.AST (Binding (..), Decl (Decl), Name (Name), Program, declGetName, progDecls)

checkEntrypoint :: Program a -> Either HissError (Program a)
checkEntrypoint prog =
  let isMainDecl d = declGetName d == Name undefined "main"
      mainDecl = find isMainDecl (progDecls prog)
   in case mainDecl of
        Just (Decl _ (FuncBinding _ _ []) _) -> return prog
        Just (Decl _ (FuncBinding _ _ args) _) -> Left $ SemanticError $ "Function 'main' must have 0 arguments, not " <> (show . length) args
        Just _ -> Left $ SemanticError "'main' must be declared as a function"
        Nothing -> Left $ SemanticError "Missing function 'main'"