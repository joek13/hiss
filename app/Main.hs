module Main (main) where

import Command (Command (..))
import Command.Eval (doEval, evalOptsParser)
import Command.Repl (doRepl, replOptsParser)
import Command.Codegen (doCodegen, codegenOptsParser)
import Options.Applicative (Parser, ParserInfo, command, execParser, fullDesc, header, helper, info, progDesc, subparser, (<**>))

-- Global hissc options.
newtype Options = Options {optCommand :: Command}

optsParser' :: Parser Options
optsParser' =
  Options
    <$> subparser
      ( command "eval" evalOptsParser
          <> command "repl" replOptsParser
          <> command "codegen" codegenOptsParser
      )

optsParser :: ParserInfo Options
optsParser =
  info
    (optsParser' <**> helper)
    (fullDesc <> progDesc "Compiler for the Hiss programming language." <> header "hissc - Hiss compiler")

main :: IO ()
main = do
  opts <- execParser optsParser
  case optCommand opts of
    Eval evalOpts -> doEval evalOpts
    Repl replOpts -> doRepl replOpts
    Codegen codegenOpts -> doCodegen codegenOpts