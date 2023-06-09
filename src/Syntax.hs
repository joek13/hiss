module Syntax (parseProgram, parseExpression) where

import Error (HissError (SyntaxError))
import Syntax.AST (Exp, Program)
import Syntax.Lexer (Range, runAlex)
import Syntax.Parser (parseExp, parseProg)

-- Wraps Lexer.parseProgram and returns HissError in case of failure
parseProgram :: String -> Either HissError (Program Range)
parseProgram inp = case runAlex inp parseProg of
  Left msg -> Left (SyntaxError msg)
  Right ast -> Right ast

-- Wraps Lexer.parseExp and returns HissError in case of failure
parseExpression :: String -> Either HissError (Exp Range)
parseExpression inp = case runAlex inp parseExp of
  Left msg -> Left (SyntaxError msg)
  Right ast -> Right ast