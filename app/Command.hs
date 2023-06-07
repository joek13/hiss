module Command (EvalOptions (..), Command (..)) where

-- Eval command options.
newtype EvalOptions = EvalOptions {optSourceFile :: String}

-- Command union.
newtype Command = Eval EvalOptions