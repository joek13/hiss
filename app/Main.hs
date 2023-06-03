module Main (main) where

import Error (HissError, showErr)
import Interpreter.TreeWalker (HissValue)
import Interpreter.TreeWalker qualified as Interpreter (eval)
import Syntax (parseString)

runProg :: String -> Either HissError HissValue
runProg inp = do
  ast <- parseString inp
  Interpreter.eval ast

main :: IO ()
main = do
  inp <- getContents
  case runProg inp of
    Left err -> print $ showErr err
    Right out -> print out