module Syntax (parseString) where

import Error (HissError (SyntaxError))
import Syntax.AST (Exp)
import Syntax.Lexer (Range, runAlex)
import Syntax.Parser (parseHiss)

-- Wraps Lexer.parseHiss and returns HissError in case of an error.
parseString :: String -> Either HissError (Exp Range)
parseString inp = case runAlex inp parseHiss of
  Left msg -> Left (SyntaxError msg)
  Right ast -> Right ast