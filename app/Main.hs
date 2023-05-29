module Main (main) where

import Lexer (runAlex)
import Parser (parseHiss)

main :: IO ()
main = do
  inp <- getContents
  print $ runAlex inp parseHiss