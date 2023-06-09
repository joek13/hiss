module Syntax (parseExpression) where

import Error (HissError (SyntaxError))
import Syntax.AST (Exp)
import Syntax.Lexer (Range, runAlex)
import Syntax.Parser (parseExp)

-- Wraps Lexer.parseExp and returns HissError in case of failure
parseExpression :: String -> Either HissError (Exp Range)
parseExpression inp = case runAlex inp parseExp of
  Left msg -> Left (SyntaxError msg)
  Right ast -> Right ast