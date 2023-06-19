module Semantic.NamesSpec (spec) where

import Data.Set qualified as Set (fromList)
import Error
import Semantic.Names (checkNames, collectNames)
import Syntax (parseExpression, parseProgram)
import Syntax.AST (Expr, Name (..), Program)
import Syntax.Lexer (AlexPosn (..), Range (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Util (fromRight)

exp1 :: Expr Range
exp1 = fromRight $ parseExpression "x + y - z"

exp2 :: Expr Range
exp2 = fromRight $ parseExpression "let f(x,y) = z in f(0)"

prog1 :: Program Range
prog1 = fromRight $ parseProgram "x = y + 1 \n y = 1"

prog2 :: Program Range
prog2 = fromRight $ parseProgram "inc(x) = x + 1 \n main = x"

prog3 :: Program Range
prog3 = fromRight $ parseProgram "f(x) = x \n f(x) = x + 1"

prog4 :: Program Range
prog4 = fromRight $ parseProgram "x = 1 \n x = 2"

spec :: Spec
spec = do
  describe "collectNames" $ do
    it "collects names in a simple expression" $
      collectNames exp1 `shouldBe` Set.fromList [Name (Range (AlexPn 0 1 1) (AlexPn 1 1 2)) "x", Name (Range (AlexPn 4 1 5) (AlexPn 5 1 6)) "y", Name (Range (AlexPn 8 1 9) (AlexPn 9 1 10)) "z"]
    it "does not collect function parameters" $
      collectNames exp2 `shouldBe` Set.fromList [Name (Range (AlexPn 17 1 18) (AlexPn 18 1 19)) "f", Name (Range (AlexPn 12 1 13) (AlexPn 13 1 14)) "z"]
  describe "checkNames" $ do
    it "hoists top-level declarations" $
      -- i.e., you can use "y" in the definition of "x" even though "y" is declared after
      checkNames prog1 `shouldBe` Right prog1
    it "does not allow use of function arguments outside function" $
      checkNames prog2 `shouldBe` Left (SemanticError "Name error: Use of undeclared name 'x' at line 2, column 9")
    it "emits error on redeclaration of function" $
      checkNames prog3 `shouldBe` Left (SemanticError "Name error: Global 'f' redeclared at line 2, column 2")
    it "emits error on redeclaration of value" $
      checkNames prog4 `shouldBe` Left (SemanticError "Name error: Global 'x' redeclared at line 2, column 2")