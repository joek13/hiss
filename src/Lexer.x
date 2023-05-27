{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+                        ;
    "//".*                         ;
    \(                             { \_ -> LParen }
    \)                             { \_ -> RParen }
    \+                             { \_ -> Plus }
    \-                             { \_ -> Minus }
    \*                             { \_ -> Star }
    \/                             { \_ -> Slash }
    =                              { \_ -> Eq }
    $digit+                        { \s -> Int (read s) }
    \"[^\"]+\"                     { \s -> Str s }
    $alpha [$alpha $digit \_ \']*  { \s -> Id s }

{
data Token
    = Plus
    | Minus
    | Star
    | Slash
    | LParen
    | RParen
    | Eq
    | Int Int
    | Id String
    | Str String
    deriving (Eq, Show)
}
