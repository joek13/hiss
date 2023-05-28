module LexerSpec (spec) where

import Lexer (Lexeme, Token (..), lexString, tok, val)
import Test.Hspec (Spec, describe, it, shouldBe)

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

-- synonym for rightToMaybe
unwrapLexemes :: Either String [Lexeme] -> Maybe [Lexeme]
unwrapLexemes = rightToMaybe

-- takes lex result and discards position information,
-- giving list of (tok, val) pairs
discardPosns :: Either String [Lexeme] -> Maybe [(Token, String)]
discardPosns res = map (\l -> (tok l, val l)) <$> unwrapLexemes res

spec :: Spec
spec = do
  describe "alexScanTokens" $ do
    it "fails to lex opened block comment" $
      lexString "/* this comment is open" `shouldBe` Left "Unexpected EOF (open block comment)"

    it "fails to lex unexpectedly closed block comment" $
      lexString "/* this comment closes twice */ */" `shouldBe` Left "Unexpected closing block comment"

    it "successfully lexes nested block comments" $
      discardPosns (lexString "/* this comment is /* nested */ and that's okay */") `shouldBe` Just [(EOF, "")]

    it "lexes a simple program" $
      discardPosns (lexString "let f cond = if cond then 42 else -42 in f 0")
        `shouldBe` Just
          [ (Let, "let"),
            (Ident, "f"),
            (Ident, "cond"),
            (Equals, "="),
            (If, "if"),
            (Ident, "cond"),
            (Then, "then"),
            (Int, "42"),
            (Else, "else"),
            (Minus, "-"),
            (Int, "42"),
            (In, "in"),
            (Ident, "f"),
            (Int, "0"),
            (EOF, "")
          ]