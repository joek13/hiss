module Command (EvalOptions (..), ReplOptions (..), Command (..)) where

-- Eval command options.
newtype EvalOptions = EvalOptions {evalSourceFile :: String}

-- Repl command options.
newtype ReplOptions = ReplOptions {replSourceFile :: String}

-- Command union.
data Command
  = Eval EvalOptions
  | Repl ReplOptions