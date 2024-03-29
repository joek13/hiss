module Command.Repl (replOptsParser, doRepl) where

import Command (Command (Repl), ReplOptions (..))
import Data.List (intercalate)
import Data.Map qualified as Map (insert, keys, lookup)
import Data.Map.Strict (assocs)
import Data.Set qualified as Set (fromList, insert)
import Error (HissError, showErr)
import Interpreter.TreeWalker (Environment, HissValue, eval, globalEnv, insertDecl)
import Options.Applicative (Parser, ParserInfo, argument, help, helper, info, metavar, progDesc, str, (<**>))
import Semantic.Dependencies (reorderDecls)
import Semantic.Names (checkNames, declCheckNames, exprCheckNames, runNameCheck, throwIfShadowsGlobal)
import Semantic.Typechecking (Typecheck (..), getTypeEnv)
import Semantic.Types (Type, TypeEnv, emptyEnv, getTy)
import Semantic.Types.Constraints (generalize)
import Syntax (parseDeclOrExp, parseProgram)
import Syntax.AST (Name (..), declGetName, getIdent, stripAnns)
import Syntax.Lexer (AlexPosn (..), Range (..))
import System.IO (hFlush, stdout)

parser :: Parser Command
parser = Repl . ReplOptions <$> argument str (metavar "FILE" <> help "Source Hiss program")

replOptsParser :: ParserInfo Command
replOptsParser = info (parser <**> helper) (progDesc "Load a Hiss program and start a REPL")

loadProg :: String -> Either HissError ReplEnv
loadProg src = do
  ast <-
    parseProgram src
      >>= checkNames
      >>= reorderDecls
      >>= typecheck

  valEnv <- globalEnv ast
  let tyEnv = getTypeEnv ast
  return (valEnv, tyEnv)

type ReplEnv = (Environment, TypeEnv)

printEnv :: ReplEnv -> IO ()
printEnv env =
  let (valEnv, tyEnv) = env
      showBinding (n, v) = case Map.lookup n tyEnv of
        Nothing -> getIdent n <> "=" <> show v
        Just sc -> getIdent n <> "=" <> show v <> " :: " <> show sc
   in do
        putStr "{"
        putStr $ intercalate ", " (map showBinding (assocs valEnv))
        putStrLn "}"

doRepl' :: ReplEnv -> IO ()
doRepl' env = do
  let (valEnv, tyEnv) = env
  putStr "> "
  hFlush stdout
  inp <- getLine
  case inp of
    "" -> do
      putStrLn "Enter an expression, ':h' for help, or :q to quit"
      doRepl' env
    ":q" -> do
      -- quit
      putStrLn "See you next time!"
    ":e" -> do
      -- show current environment
      printEnv env
      doRepl' env
    ":h" -> do
      -- show help message
      putStrLn "== hiss repl =="
      putStrLn ""
      putStrLn "Enter a declaration, an expression, or one of the following commands:"
      putStrLn "  :h - show this message"
      putStrLn "  :e - show current environment"
      putStrLn "  :q - quit"
      putStrLn ""
      doRepl' env
    _ -> case go inp of
      -- 'twas an expression
      Right (Right val, ty, env') -> do
        putStrLn $ show val <> " :: " <> show ty
        doRepl' env'
      -- 'twas a declaration
      Right (Left name, ty, env') -> do
        putStrLn $ getIdent name <> " :: " <> show ty
        doRepl' env'
      Left err -> do
        putStrLn (showErr err)
        doRepl' env
      where
        go :: String -> Either HissError (Either (Name Range) HissValue, Type, ReplEnv)
        go src = do
          declOrExp <- parseDeclOrExp src
          -- since we're missing range information, just attach a dummy range
          let globals = Set.fromList $ map (fmap (const dummyRange)) $ Map.keys valEnv
          case declOrExp of
            Right expr -> do
              -- check that names we use are declared
              runNameCheck
                globals
                (exprCheckNames expr)
              -- typecheck expression in current environment
              expr' <- typecheckEnv tyEnv expr
              -- eval expression in current environment
              val <- eval valEnv expr'
              -- return results with env unchanged
              return (Right val, getTy expr', env)
            Left decl -> do
              -- check that we do not shadow existing globals
              runNameCheck globals (throwIfShadowsGlobal $ declGetName decl)
              -- and that all names are defined
              runNameCheck (Set.insert (declGetName decl) globals) (declCheckNames decl)
              -- typecheck decl in current env
              decl' <- typecheckEnv tyEnv decl
              -- add decl to current environment and evaluate
              valEnv' <- insertDecl valEnv (stripAnns decl)
              -- add type of decl to typing environment
              let tyEnv' = Map.insert ((stripAnns . declGetName) decl') ((generalize emptyEnv . getTy) decl') tyEnv
              let env' = (valEnv', tyEnv')
              return (Left $ (declGetName . fmap fst) decl', getTy decl', env')

doRepl :: ReplOptions -> IO ()
doRepl opts = do
  let fileName = replSourceFile opts
  source <- readFile fileName
  case loadProg source of
    Right env -> do
      putStrLn $ "Loaded file " <> fileName
      doRepl' env
    Left err -> putStrLn (showErr err)

dummyRange :: Range
dummyRange = Range (AlexPn 0 0 0) (AlexPn 0 0 0)