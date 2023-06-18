module Syntax (parseProgram, parseExpression, parseDeclaration, parseDeclOrExp) where

import Error (HissError (SyntaxError))
import Syntax.AST (Decl, Expr, Program)
import Syntax.Lexer (Range, runAlex)
import Syntax.Parser (parseDecl, parseExpr, parseProg)

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