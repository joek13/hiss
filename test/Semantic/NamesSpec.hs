module Semantic.NamesSpec (spec) where

import Data.Set qualified as Set (fromList)
import Error (HissError (SemanticError))
import Semantic.Names (expCheckNames, progCheckNames, collectNames)
import Syntax (parseExpression, parseProgram)
import Syntax.AST (Exp, Name (..), Program)
import Syntax.Lexer (AlexPosn (..), Range (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Util (fromRight)

exp1 :: Exp Range
exp1 = fromRight $ parseExpression "x + y - z"

exp2 :: Exp Range
exp2 = fromRight $ parseExpression "let f(x,y) = z in f(0)"

exp3 :: Exp Range
exp3 = fromRight $ parseExpression "let f(x,y) = x in f(0,0)"

exp4 :: Exp Range
exp4 = fromRight $ parseExpression "let f(x,y) = x in x"

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
  describe "expCheckNames" $ do
    it "allows use of declared names" $
      expCheckNames exp3 `shouldBe` Right exp3
    it "does not allow use of function arguments outside function" $
      expCheckNames exp4 `shouldBe` Left (SemanticError "Use of undeclared name 'x' at line 1, column 19")
  describe "progCheckNames" $ do
    it "hoists top-level declarations" $
      -- i.e., you can use "y" in the definition of "x" even though "y" is declared after
      progCheckNames prog1 `shouldBe` Right prog1
    it "does not allow use of function arguments outside function" $ 
      progCheckNames prog2 `shouldBe` Left (SemanticError "Use of undeclared name 'x' at line 2, column 9")
    it "emits error on redeclaration of function" $
      progCheckNames prog3 `shouldBe` Left (SemanticError "Name 'f' redeclared at line 2, column 3")
    it "emits error on redeclaration of value" $
      progCheckNames prog4 `shouldBe` Left (SemanticError "Name 'x' redeclared at line 2, column 3")