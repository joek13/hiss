module Error (HissError (..), showErr) where

data HissError
  = SyntaxError String
  | SemanticError String
  | RuntimeError String
  deriving (Eq, Show)

showErr :: HissError -> String
showErr (SyntaxError msg) = "Syntax error: " <> msg
showErr (SemanticError msg) = "Semantic error: " <> msg
showErr (RuntimeError msg) = "Runtime error: " <> msg