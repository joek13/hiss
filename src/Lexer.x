{
module Lexer (lexString, Lexeme, Token(..), tok, val, position) where
import Control.Monad (when)
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
@id = ($alpha|\_) [$alpha $digit \_ \']*

tokens :-
-- ignore whitespace
<0>     $white+                    ;

-- line comments
<0>     "//".*                     ;
-- block comments
<0>     "/*"                       { nestComment `andBegin` comment }
<0>     "*/"                       { \_ _ -> alexError "Unexpected closing block comment" }
<comment> "/*"                     { nestComment }
<comment> "*/"                     { unnestComment }
<comment> (.|\n)                   ;

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
    d <- getCommentDepth
    when (d /= 0) $ 
        alexError "Unexpected EOF (open block comment)"
    pure $ Lexeme { position = Nothing, tok = EOF, val = "" }

-- get/set comment depth
getCommentDepth :: Alex Int
getCommentDepth = commentDepth <$> alexGetUserState

setCommentDepth :: Int -> Alex ()
setCommentDepth d = do
    ust <- alexGetUserState
    alexSetUserState ust { commentDepth = d }

incCommentDepth :: Alex ()
incCommentDepth = do
    d <- getCommentDepth
    setCommentDepth $ d + 1

decCommentDepth :: Alex ()
decCommentDepth = do
    d <- getCommentDepth
    setCommentDepth $ d - 1

-- increase comment depth
nestComment :: AlexAction Lexeme
nestComment inp len = do
    incCommentDepth
    skip inp len

-- decrease comment depth
unnestComment :: AlexAction Lexeme
unnestComment inp len = do
    decCommentDepth
    d <- getCommentDepth
    when (d == 0) $
        alexSetStartCode 0
    skip inp len

-- lexes a string
lexString :: String -> Either String [Lexeme]
lexString input = runAlex input go
    where
        go = do
            output <- alexMonadScan
            if tok output == EOF
                then pure [output]
                else (output:) <$> go

}