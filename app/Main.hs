module Main (main) where

import Lexer qualified (lexString)

main :: IO ()
main = do
  inp <- getContents
  print $ Lexer.lexString inp
