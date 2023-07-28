module Semantic.DependenciesSpec (spec) where

import Error
import Semantic.Dependencies (reorderDecls)
import Syntax (parseProgram)
import Syntax.AST (Name (..), Program, declGetName, progDecls, stripAnns)
import Syntax.Lexer (Range (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Util (fromRight)

prog1 :: Program Range -- simple cyclic program
prog1 = fromRight $ parseProgram "a = b \n b = c \n c = a"

prog2 :: Program Range -- cyclic program containing functions
prog2 = fromRight $ parseProgram "a = f() \n f() = a"

prog3 :: Program Range -- acyclic program with mutually recursive functions
prog3 = fromRight $ parseProgram "f() = g() + a \n g() = f() + b \n a = 1 \n b = 1"

spec :: Spec
spec = do
  describe "reorderDecls" $ do
    it "emits error on a simple value cycle" $
      reorderDecls prog1 `shouldBe` Left (SemanticError "Definition of 'a' at line 1, column 1 is cyclic")
    it "emits error on a value cycle through a function" $
      reorderDecls prog2 `shouldBe` Left (SemanticError "Definition of 'a' at line 1, column 1 is cyclic")
    it "correctly re-orders declarations of a program with mutually recursive functions" $
      map (stripAnns . declGetName) <$> (progDecls <$> reorderDecls prog3)
        `shouldBe` Right
          [ Name () "b",
            Name () "a",
            Name () "f",
            Name () "g"
          ]