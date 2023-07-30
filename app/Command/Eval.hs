module Command.Eval (evalOptsParser, doEval, doEval') where

import Command (Command (Eval), EvalOptions (..))
import Error (HissError, showErr)
import Interpreter.TreeWalker (HissValue, interp)
import Options.Applicative (Parser, ParserInfo, argument, help, helper, info, metavar, progDesc, str, (<**>))
import Semantic.Dependencies (reorderDecls)
import Semantic.Names (checkNames)
import Syntax (parseProgram)

parser :: Parser Command
parser = Eval . EvalOptions <$> argument str (metavar "FILE" <> help "Source Hiss program")

evalOptsParser :: ParserInfo Command
evalOptsParser = info (parser <**> helper) (progDesc "Evaluate a Hiss program")

doEval' :: String -> Either HissError HissValue
doEval' source = do
  ast <-
    parseProgram source -- parse/lex program
      >>= checkNames
      >>= reorderDecls

  -- interpret the program
  interp ast

doEval :: EvalOptions -> IO ()
doEval opts = do
  let fileName = evalSourceFile opts
  source <- readFile fileName
  case doEval' source of
    Right value -> print value
    Left err -> print $ showErr err