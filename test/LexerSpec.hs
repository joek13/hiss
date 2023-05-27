module LexerSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "alexScanTokens" $ do
    it "dummy test case" $
      1 `shouldBe` 1

-- TODO: tests for failed lex