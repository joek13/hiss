{
module Parser (parseHiss) where
import qualified Lexer as L
}

%name parseHiss
%tokentype { L.Lexeme } 
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.Lexeme { L.tok = L.EOF } }

%%

empty : {}

{

parseError :: L.Lexeme -> L.Alex a
parseError _ = do
    (L.AlexPn _ line col, _, _, _) <- L.alexGetInput
    L.alexError $ "Parse error at line " <> show line <> ", column " <> show col

lexer :: (L.Lexeme -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

}