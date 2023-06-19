module Syntax.ASTSpec (spec) where

import Syntax.AST (BinOp (Sub), Binding (..), Expr (..), Name (..), getAnn)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Name" $ do
    -- ignore annotation when comparing Names
    it "compare on identifier value" $ do
      (Name (42 :: Integer) "abcde") == (Name 42 "abcde") `shouldBe` True
      (Name (42 :: Integer) "abcde") == (Name 43 "abcde") `shouldBe` True
      (Name (42 :: Integer) "abcde") == (Name 42 "fghij") `shouldBe` False
