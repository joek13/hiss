module Main (main) where

import Syntax.AST (stripAnns)
import Syntax.Lexer (runAlex)
import Syntax.Parser (parseHiss)

main :: IO ()
main = do
  inp <- getContents
  let res = runAlex inp parseHiss
  case res of
    Right ast -> do
      print $ stripAnns $ ast
    Left msg -> do
      print msg