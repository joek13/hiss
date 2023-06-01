module Syntax.LexerSpec (spec) where

import Syntax.Lexer (AlexPosn (..), Lexeme (..), Range (..), Token (..), lexString, mergeRange)
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
  describe "mergeRange" $ do
    it "merges disjoint ranges correctly" $
      (Range (AlexPn 0 1 1) (AlexPn 6 1 7))
        `mergeRange` (Range (AlexPn 8 2 2) (AlexPn 14 2 8))
        `shouldBe` (Range (AlexPn 0 1 1) (AlexPn 14 2 8))
    it "merges contained ranges correctly" $
      (Range (AlexPn 0 1 1) (AlexPn 6 1 7))
        `mergeRange` (Range (AlexPn 2 1 3) (AlexPn 5 1 6))
        `shouldBe` (Range (AlexPn 0 1 1) (AlexPn 6 1 7))

  describe "lexString" $ do
    it "fails to lex opened block comment" $
      lexString "/* this comment is open" `shouldBe` Left "Unexpected EOF (open block comment)"

    it "fails to lex unexpectedly closed block comment" $
      lexString "/* this comment closes twice */ */" `shouldBe` Left "Unexpected closing block comment"

    it "successfully lexes nested block comments" $
      discardPosns (lexString "/* this comment is /* nested */ and that's okay */") `shouldBe` Just [(EOF, "")]

    it "lexes booleans" $
      discardPosns (lexString "true || false") `shouldBe` Just [(Syntax.Lexer.True, "true"), (Or, "||"), (Syntax.Lexer.False, "false"), (EOF, "")]

    it "successfully tracks token range" $
      let getPosns res = map rng <$> unwrapLexemes res
       in getPosns (lexString "ident1\n ident2")
            `shouldBe` Just
              [ Range (AlexPn 0 1 1) (AlexPn 6 1 7), -- range for first identifier
                Range (AlexPn 8 2 2) (AlexPn 14 2 8), -- range for second identifier
                Range (AlexPn 14 2 8) (AlexPn 14 2 8) -- (empty) range for EOF
              ]

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