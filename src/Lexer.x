{
module Lexer (lexString, Lexeme) where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
@id = ($alpha|\_) [$alpha $digit \_ \']*

tokens :-
-- ignore whitespace
<0>     $white+                    ;

-- line comments
<0>     "//".*                     ;
-- TODO: block comments

<0>     \(                         { mkLexeme LParen }
<0>     \)                         { mkLexeme RParen }

{
-- Token types.
data Token = EOF
           | LParen
           | RParen
    deriving (Eq, Show)

-- Smallest lexical unit.
data Lexeme = Lexeme
    { position :: Maybe AlexPosn -- position of token in source program, or Nothing if EOF
    , tok      :: Token          -- token type
    , val      :: String         -- string value of token
    }
    deriving (Eq, Show)

-- Given a token type, returns AlexAction which constructs that token.
mkLexeme :: Token -> AlexAction Lexeme
mkLexeme t (p,_,_,str) len = return $ Lexeme { position = Just p
                                             , tok = t
                                             , val = (take len str)
                                             }

-- Custom user state
data AlexUserState = AlexUserState 
    { commentDepth :: Int    -- current comment nesting level
    , stringBuf    :: String -- value of current string
    }

-- Initial user state
alexInitUserState = AlexUserState { commentDepth = 0, stringBuf = "" }

alexEOF = do
    pure $ Lexeme { position = Nothing, tok = EOF, val = "" }

lexString :: String -> Either String [Lexeme]
lexString input = runAlex input go
    where
        go = do
            output <- alexMonadScan
            if tok output == EOF
                then pure [output]
                else (output:) <$> go

}