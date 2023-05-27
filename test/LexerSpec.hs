module LexerSpec (spec) where

import Lexer (Token (Equals, Id, Int, LParen, Plus, RParen, Star), alexScanTokens)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "alexScanTokens" $ do
    it "lexes a simple expression" $ do
      alexScanTokens "1 + (2 * 3)" `shouldBe` [Int 1, Plus, LParen, Int 2, Star, Int 3, RParen]
    it "ignores text after comments" $ do
      alexScanTokens "f x y = g y x // f x y = g x x" `shouldBe` [Id "f", Id "x", Id "y", Equals, Id "g", Id "y", Id "x"]

-- TODO: tests for failed lex