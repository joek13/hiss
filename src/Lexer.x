{
module Lexer (Lexeme(..), Token(..), Alex, AlexPosn(..), Range(..), alexMonadScan, lexString, runAlex, alexGetInput, alexError, mergeRange) where
import Control.Monad (when)
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
@id = ($alpha|\_) [$alpha $digit \_ \']*

tokens :-
-- ignore whitespace
<0>       $white+                    ;

-- line comments
<0>       "//".*                     ;
-- block comments
<0>       "/*"                       { nestComment `andBegin` comment }
<0>       "*/"                       { \_ _ -> alexError "Unexpected closing block comment" }
<comment> "/*"                       { nestComment }
<comment> "*/"                       { unnestComment }
<comment> (.|\n)                     ; -- ignore everything else inside a comment

-- tokens
<0>       \(                         { mkLexeme LParen }
<0>       \)                         { mkLexeme RParen }
<0>       "+"                        { mkLexeme Plus }
<0>       "-"                        { mkLexeme Minus }
<0>       "*"                        { mkLexeme Star }
<0>       "/"                        { mkLexeme Slash }
<0>       "="                        { mkLexeme Equals }
<0>       "let"                      { mkLexeme Let }
<0>       "in"                       { mkLexeme In }
<0>       "if"                       { mkLexeme If }
<0>       "then"                     { mkLexeme Then }
<0>       "else"                     { mkLexeme Else }
<0>       $digit+                    { mkLexeme Int }
<0>       @id                        { mkLexeme Ident}

{
-- Token types.
data Token = EOF
           | LParen
           | RParen
           | Plus
           | Minus
           | Star
           | Slash
           | Equals
           | Let
           | In
           | If
           | Then
           | Else
           | Int
           | Ident
    deriving (Eq, Show)

-- Range of positions. (Range start stop) <-> [start, stop)
data Range = Range AlexPosn AlexPosn
    deriving (Eq, Show)

-- Smallest lexical unit.
data Lexeme = Lexeme
    { rng            :: Range          -- range of token in source program
    , tok            :: Token          -- token type
    , val            :: String         -- string value of token
    }
    deriving (Eq, Show)

mkRange :: AlexInput -> Int -> Range
mkRange (start, _, _, str) len = Range start stop
    where stop = foldl alexMove start $ take len str

-- Merges two ranges. a `mergeRange` b is the smallest range that completely contains a and b.
mergeRange :: Range -> Range -> Range
mergeRange (Range a b) (Range c d) = Range (first a c) (second b d)
    where offset (AlexPn i _ _) = i
          first x y  | offset x < offset y = x
                     | otherwise           = y
          second x y | offset x > offset y = x
                     | otherwise           = y

-- Given a token type, returns AlexAction which constructs that token.
mkLexeme :: Token -> AlexAction Lexeme
mkLexeme t inp@(_,_,_,str) len = return $ Lexeme { rng = mkRange inp len
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
    (posn, _, _, _) <- alexGetInput
    pure $ Lexeme { rng = Range posn posn, tok = EOF, val = "" }

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