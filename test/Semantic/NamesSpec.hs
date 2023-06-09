module Semantic.NamesSpec (spec) where

import Data.Set qualified as Set (fromList)
import Error (HissError (SemanticError))
import Semantic.Names (checkNames, collectNames)
import Syntax (parseExpression)
import Syntax.AST (Exp, Name (..))
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

spec :: Spec
spec = do
  describe "collectNames" $ do
    it "collects names in a simple expression" $
      collectNames exp1 `shouldBe` Set.fromList [Name (Range (AlexPn 0 1 1) (AlexPn 1 1 2)) "x", Name (Range (AlexPn 4 1 5) (AlexPn 5 1 6)) "y", Name (Range (AlexPn 8 1 9) (AlexPn 9 1 10)) "z"]
    it "does not collect function parameters" $
      collectNames exp2 `shouldBe` Set.fromList [Name (Range (AlexPn 17 1 18) (AlexPn 18 1 19)) "f", Name (Range (AlexPn 12 1 13) (AlexPn 13 1 14)) "z"]
  describe "checkNames" $ do
    it "allows use of declared names" $
      checkNames exp3 `shouldBe` Right exp3
    it "does not allow use of function arguments outside function" $
      checkNames exp4 `shouldBe` Left (SemanticError "Use of undeclared name 'x' at line 1, column 19")