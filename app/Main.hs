module Main (main) where

import Error ( HissError, HissError, showErr )
import Codegen.Program (Program)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, helper, info, progDesc, (<**>))
import Options.Applicative.Builder (value, argument, long, short, metavar, help, str, option, flag, ReadM, maybeReader)
import Semantic.Dependencies (reorderDecls)
import Semantic.Names (checkNames)
import Semantic.Typechecking (Typecheck (..))
import Syntax (parseProgram)
import Codegen (codegen)
import qualified Data.ByteString as B
import Codegen.Assembly (writeAssembly)
import Assembler (assemble)


data OutputFormat = Assembly | Bytecode

-- Function that derives an output path from a format and input path.
type OutPath = OutputFormat -> String -> String

-- Global hissc options.
data Options = Options { inputPath :: String, outPath :: OutPath , format :: OutputFormat }

fileName :: String -> String
fileName = reverse . takeWhile (/= '/') . reverse

removeExtension :: String -> String
removeExtension = reverse . drop 1 . dropWhile (/= '.') . reverse

defaultOutPath :: OutPath
defaultOutPath Assembly = (<>".hissa") . removeExtension . fileName
defaultOutPath Bytecode = (<>".hissc") . removeExtension . fileName

readOutPath :: ReadM OutPath
readOutPath = maybeReader f
  where f s = Just $ curry $ const s

parser :: Parser Options
parser = Options
  <$> argument str (metavar "INPATH" <> help "Path to input Hiss program.")
  <*> option readOutPath (long "output" <> short 'o' <> metavar "OUTPATH" <> value defaultOutPath <> help "Output path. (Default: derived from input filename.)")
  <*> flag Bytecode Assembly (long "asm" <> short 'a' <> help "Emit Hiss assembly. (Default: emit bytecode.)")


parserInfo :: ParserInfo Options
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> progDesc "Compiler for the Hiss programming language." <> header "hissc - Hiss compiler")

compile :: String -> Either HissError Program
compile source =
  -- Lex/parse program.
  parseProgram source
      -- Check validity of program identifiers.
      >>= checkNames
      -- Reorder program declarations so values are always declared before they are used.
      >>= reorderDecls
      -- Run type check.
      >>= typecheck
      -- Generate code and strip range information.
      >>= codegen . fmap snd

write :: OutputFormat -> String -> Program -> IO ()
write Assembly outPath prog = 
  let result = writeAssembly prog in
  case result of 
    Right asm -> do
      writeFile outPath (unlines asm)
      putStrLn $ "Wrote assembly to " <> outPath
    Left err -> putStrLn $ showErr err
write Bytecode outPath prog = 
  let result = assemble prog in
  case result of 
    Right bytecode ->  do
      B.writeFile outPath bytecode
      putStrLn $ "Wrote bytecode to " <> outPath
    Left err -> putStrLn $ showErr err

main :: IO ()
main = do
  opts <- execParser parserInfo
  let sourcePath = inputPath opts
  source <- readFile sourcePath
  case compile source of
    Left err -> putStrLn $ showErr err
    Right program -> do
      putStrLn $ "Successfully compiled " <> sourcePath
      let fmt = format opts
      let out = outPath opts fmt (inputPath opts)
      write fmt out program