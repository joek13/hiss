module Command.Repl (replOptsParser, doRepl) where

import Command (Command (Repl), ReplOptions (..))
import Data.List (intercalate)
import Data.Map.Strict (assocs)
import Error (HissError, showErr)
import Interpreter.TreeWalker (Environment, HissValue, baseEnv, eval)
import Options.Applicative (Parser, ParserInfo, argument, help, helper, info, metavar, progDesc, str, (<**>))
import Semantic.Names (progCheckNames)
import Syntax (parseExpression, parseProgram)
import Syntax.AST (getIdent)
import System.IO (hFlush, stdout)

parser :: Parser Command
parser = Repl . ReplOptions <$> argument str (metavar "FILE" <> help "Source Hiss program")

replOptsParser :: ParserInfo Command
replOptsParser = info (parser <**> helper) (progDesc "Load a Hiss program and start a REPL")

loadProg :: String -> Either HissError Environment
loadProg src = parseProgram src >>= progCheckNames >>= baseEnv

parseAndEval :: Environment -> String -> Either HissError HissValue
parseAndEval env expr = do
  ast <- parseExpression expr
  eval env ast

doRepl' :: Environment -> IO ()
doRepl' env = do
  putStr "> "
  hFlush stdout
  expr <- getLine
  case expr of
    "" -> do
      putStrLn "Enter an expression, ':h' for help, or :q to quit"
      doRepl' env
    ":q" -> do
      -- quit
      putStrLn "See you next time!"
    ":e" -> do
      -- show current environment
      putStr "{"
      putStr $ intercalate ", " (map showBinding (assocs env))
      putStrLn "}"
      doRepl' env
      where
        showBinding (n, v) = getIdent n <> ": " <> show v
    ":h" -> do
      -- show help message
      putStrLn "== hiss repl =="
      putStrLn ""
      putStrLn "Enter an expression to evaluate or one of the following commands:"
      putStrLn "  :h - show this message"
      putStrLn "  :e - show current environment"
      putStrLn "  :q - quit"
      putStrLn ""
      doRepl' env
    _ -> case parseAndEval env expr of
      -- evaluate expression
      Right val -> do
        print val
        doRepl' env
      Left err -> do
        print $ showErr err
        doRepl' env

doRepl :: ReplOptions -> IO ()
doRepl opts = do
  let fileName = replSourceFile opts
  source <- readFile fileName
  case loadProg source of
    Right env -> do
      putStrLn $ "Loaded file " <> fileName
      doRepl' env
    Left err -> print $ showErr err