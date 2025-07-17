{-
  Generates Hiss bytecode from a program AST.
-}

module Codegen (codegen) where

import qualified Syntax.AST
import Semantic.Types (Type)
import Codegen.Emit(emitCode)
import Codegen.Program (Program, mkProgram)
import Error (HissError)

codegen :: Syntax.AST.Program Type -> Either HissError Program
codegen = Right . mkProgram . emitCode