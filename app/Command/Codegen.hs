module Command.Codegen (codegenOptsParser, doCodegen) where

import Command (Command (Codegen), CodegenOptions (..))
import Error (HissError, showErr)
import Options.Applicative (Parser, ParserInfo, argument, help, helper, info, metavar, progDesc, str, (<**>))
import Semantic.Dependencies (reorderDecls)
import Semantic.Names (checkNames)
import Semantic.Typechecking (Typecheck (..))
import Syntax (parseProgram)
import Assembly (assemble, writeAssembly)
import Control.Monad (forM_ )

parser :: Parser Command
parser = Codegen . CodegenOptions <$> argument str (metavar "FILE" <> help "Source Hiss program")

codegenOptsParser :: ParserInfo Command
codegenOptsParser = info (parser <**> helper) (progDesc "Perform codegen for a Hiss program")

doCodegen' :: String -> Either HissError [String]
doCodegen' source = parseProgram source -- parse/lex program
      >>= checkNames
      >>= reorderDecls
      >>= typecheck
      >>= assemble . fmap snd
      >>= writeAssembly

doCodegen :: CodegenOptions -> IO ()
doCodegen opts = do
  let fileName = codegenSourceFile opts
  source <- readFile fileName
  case doCodegen' source of
    Right value -> forM_ value putStrLn
    Left err -> print $ showErr err