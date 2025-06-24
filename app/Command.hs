module Command (EvalOptions (..), ReplOptions (..), CodegenOptions (..), Command (..)) where

-- Eval command options.
newtype EvalOptions = EvalOptions {evalSourceFile :: String}

-- Repl command options.
newtype ReplOptions = ReplOptions {replSourceFile :: String}

-- Codegen command options.
newtype CodegenOptions = CodegenOptions { codegenSourceFile :: String }

-- Command union.
data Command
  = Eval EvalOptions
  | Repl ReplOptions
  | Codegen CodegenOptions