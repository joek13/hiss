module Command.Eval (evalOptsParser, doEval) where

import Command (Command (Eval), EvalOptions (..))
import Error (HissError, showErr)
import Interpreter.TreeWalker (HissValue, eval)
import qualified Interpreter.TreeWalker as I (HissValue (Bool))
import Options.Applicative (Parser, ParserInfo, argument, help, helper, info, metavar, progDesc, str, (<**>))
import Semantic.Names (progCheckNames)
import Syntax (parseProgram)

parser :: Parser Command
parser = Eval . EvalOptions <$> argument str (metavar "FILE" <> help "Source Hiss program")

evalOptsParser :: ParserInfo Command
evalOptsParser = info (parser <**> helper) (progDesc "Evaluate a Hiss program")

doEval' :: String -> Either HissError HissValue
doEval' source = do
  -- lex/parse input
  ast <- parseProgram source
  -- check names
  ast' <- progCheckNames ast

  Right (I.Bool True) -- TODO: actually eval!!

doEval :: EvalOptions -> IO ()
doEval opts = do
  let fileName = optSourceFile opts
  source <- readFile fileName
  case doEval' source of
    Right value -> print value
    Left err -> print $ showErr err