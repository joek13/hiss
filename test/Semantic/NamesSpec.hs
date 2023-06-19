module Semantic.NamesSpec (spec) where

import Data.Set qualified as Set (fromList)
import Semantic.Names (collectNames)
import Syntax (parseExpression)
import Syntax.AST (Expr, Name (..))
import Syntax.Lexer (AlexPosn (..), Range (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Util (fromRight)

exp1 :: Expr Range
exp1 = fromRight $ parseExpression "x + y - z"

exp2 :: Expr Range
exp2 = fromRight $ parseExpression "let f(x,y) = z in f(0)"

spec :: Spec
spec = do
  describe "collectNames" $ do
    it "collects names in a simple expression" $
      collectNames exp1 `shouldBe` Set.fromList [Name (Range (AlexPn 0 1 1) (AlexPn 1 1 2)) "x", Name (Range (AlexPn 4 1 5) (AlexPn 5 1 6)) "y", Name (Range (AlexPn 8 1 9) (AlexPn 9 1 10)) "z"]
    it "does not collect function parameters" $
      collectNames exp2 `shouldBe` Set.fromList [Name (Range (AlexPn 17 1 18) (AlexPn 18 1 19)) "f", Name (Range (AlexPn 12 1 13) (AlexPn 13 1 14)) "z"]