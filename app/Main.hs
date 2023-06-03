module Main (main) where

import Error (HissError, showErr)
import Interpreter.TreeWalker (HissValue)
import Interpreter.TreeWalker qualified as Interpreter (eval)
import Semantic.Names (checkNames)
import Syntax (parseString)

runProg :: String -> Either HissError HissValue
runProg inp = do
  -- syntax
  ast <- parseString inp
  -- semantic
  ast' <- checkNames ast
  -- evaluate
  Interpreter.eval ast'

main :: IO ()
main = do
  inp <- getContents
  case runProg inp of
    Left err -> print $ showErr err
    Right out -> print out