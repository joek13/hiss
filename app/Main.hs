module Main (main) where

import Interpreter.TreeWalker qualified as Interpreter (eval)
import Syntax.Lexer (runAlex)
import Syntax.Parser (parseHiss)

main :: IO ()
main = do
  inp <- getContents
  let res = runAlex inp parseHiss
  case res of
    Right ast -> do
      print $ Interpreter.eval ast
    Left msg -> do
      print msg