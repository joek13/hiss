module Syntax (parseProgram, parseExpression, parseDeclaration, parseDeclOrExp, start, stop, getLineCol) where

import Error (HissError (SyntaxError))
import Syntax.AST (Decl, Expr, Program)
import Syntax.Lexer (AlexPosn (AlexPn), Range (Range), runAlex)
import Syntax.Parser (parseDecl, parseExpr, parseProg)

-- | Gets a Range's start position.
start :: Range -> AlexPosn
start (Range p _) = p

-- | Gets a Range's stop position.
stop :: Range -> AlexPosn
stop (Range _ p) = p

-- | Gets the line and column of an AlexPosn.
getLineCol :: AlexPosn -> (Int, Int)
getLineCol (AlexPn l c _) = (l, c)

-- Wraps Lexer.parseProgram and returns HissError in case of failure
parseProgram :: String -> Either HissError (Program Range)
parseProgram inp = case runAlex inp parseProg of
  Left msg -> Left (SyntaxError msg)
  Right prog -> Right prog

-- Wraps Lexer.parseExp and returns HissError in case of failure
parseExpression :: String -> Either HissError (Expr Range)
parseExpression inp = case runAlex inp parseExpr of
  Left msg -> Left (SyntaxError msg)
  Right ast -> Right ast

-- Wraps Lexer.parseDecl and returns HissError in case of failure
parseDeclaration :: String -> Either HissError (Decl Range)
parseDeclaration inp = case runAlex inp parseDecl of
  Left msg -> Left (SyntaxError msg)
  Right ast -> Right ast

-- | Tries to parse 'str' as a declaration.
-- | If that fails, tries to parse 'str' as an expression.
parseDeclOrExp :: String -> Either HissError (Either (Decl Range) (Expr Range))
parseDeclOrExp inp = case parseDeclaration inp of
  Right decl -> return (Left decl)
  _ -> do
    expr <- parseExpression inp
    return (Right expr)