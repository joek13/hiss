module Semantic.DependenciesSpec (spec) where

import Data.Set qualified as Set (fromList)
import Error
import Semantic.Dependencies (groupDecls, reorderDecls)
import Syntax (parseProgram)
import Syntax.AST (Name (..), Program, declGetName, getIdent, progDecls, stripAnns)
import Syntax.Lexer (Range (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Util (fromRight)

prog1 :: Program Range
prog1 = fromRight $ parseProgram "a = 1 \n b = 2 \n c = 3"

prog2 :: Program Range
prog2 = fromRight $ parseProgram "f() = g() \n g() = f()"

prog3 :: Program Range
prog3 = fromRight $ parseProgram "a = 43 \n f() = i() \n g() = f() \n h() = f() \n i() = g() + h() \n b = 42"

prog4 :: Program Range -- simple cyclic program
prog4 = fromRight $ parseProgram "a = b \n b = c \n c = a"

prog5 :: Program Range -- cyclic program containing functions
prog5 = fromRight $ parseProgram "a = f() \n f() = a"

prog6 :: Program Range -- acyclic program with mutually recursive functions
prog6 = fromRight $ parseProgram "f() = g() + a \n g() = f() + b \n a = 1 \n b = 1"

spec :: Spec
spec = do
  describe "groupDecls" $ do
    -- uses Set throughout because the order of declarations within each group is not defined
    it "correctly groups decls without any dependencies" $
      map (Set.fromList . map (getIdent . declGetName)) (groupDecls prog1) `shouldBe` map Set.fromList [["c"], ["b"], ["a"]]
    it "correctly groups decls with mutual recursion" $
      map (Set.fromList . map (getIdent . declGetName)) (groupDecls prog2) `shouldBe` [Set.fromList ["f", "g"]]
    it "correctly groups decls with more complicated mutual recursion" $
      map (Set.fromList . map (getIdent . declGetName)) (groupDecls prog3) `shouldBe` map Set.fromList [["b"], ["f", "i", "g", "h"], ["a"]]
  describe "reorderDecls" $ do
    it "emits error on a simple value cycle" $
      reorderDecls prog4 `shouldBe` Left (SemanticError "Definition of 'a' at line 1, column 1 is cyclic")
    it "emits error on a value cycle through a function" $
      reorderDecls prog5 `shouldBe` Left (SemanticError "Definition of 'a' at line 1, column 1 is cyclic")
    it "correctly re-orders declarations of a program with mutually recursive functions" $
      map (stripAnns . declGetName) <$> (progDecls <$> reorderDecls prog6)
        `shouldBe` Right
          [ Name () "b",
            Name () "a",
            Name () "f",
            Name () "g"
          ]