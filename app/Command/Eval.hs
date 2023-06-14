module Command.Eval (evalOptsParser, doEval, doEval') where

import Command (Command (Eval), EvalOptions (..))
import Debug.Trace (trace)
import Error (HissError, showErr)
import Interpreter.TreeWalker (HissValue, interp)
import Options.Applicative (Parser, ParserInfo, argument, help, helper, info, metavar, progDesc, str, (<**>))
import Semantic.Entrypoint (checkEntrypoint)
import Semantic.Names (progCheckNames)
import Syntax (parseProgram)
import Syntax.AST (getAnn)

parser :: Parser Command
parser = Eval . EvalOptions <$> argument str (metavar "FILE" <> help "Source Hiss program")

evalOptsParser :: ParserInfo Command
evalOptsParser = info (parser <**> helper) (progDesc "Evaluate a Hiss program")

doEval' :: String -> Either HissError HissValue
doEval' source = do
  ast <-
    parseProgram source -- parse/lex program
      >>= progCheckNames -- check names
      >>= checkEntrypoint -- check main func

  -- interpret the program
  interp ast

doEval :: EvalOptions -> IO ()
doEval opts = do
  let fileName = optSourceFile opts
  source <- readFile fileName
  case doEval' source of
    Right value -> print value
    Left err -> print $ showErr err