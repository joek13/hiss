module Main (main) where

import Lexer (alexScanTokens)

main :: IO ()
main = do
  inp <- getContents
  print $ alexScanTokens inp
