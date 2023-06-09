{
module Syntax.Parser ( parseExp, parseProg ) where
import Syntax.Lexer ( Lexeme(..), Range(..), AlexPosn(..), Alex, mergeRange, alexError, alexGetInput, alexMonadScan )
import qualified Syntax.Lexer as T (Token (..))
import Syntax.AST( Decl(..), Exp(..), Name(..), UnaryOp(..), BinOp(..), Binding(..), FunApp(..), getAnn )
import Data.Maybe (fromJust)
}

%name parseExp exp
%name parseProg prog
%tokentype { Lexeme } 
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Lexeme { tok = T.EOF } }
%expect 0 -- compile error if shift/reduce or reduce/reduce conflicts exist

-- associativity and operator precedence
%right 'else'
%right 'in' 
%left '==' '!='
%left '&&' '||'
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%token
    '('                    { Lexeme{ tok = T.LParen } }
    ')'                    { Lexeme{ tok = T.RParen } }
    ','                    { Lexeme{ tok = T.Comma } }
    '+'                    { Lexeme{ tok = T.Plus } }
    '-'                    { Lexeme{ tok = T.Minus } }
    '*'                    { Lexeme{ tok = T.Star } }
    '/'                    { Lexeme{ tok = T.Slash } }
    '='                    { Lexeme{ tok = T.Equals } }
    '=='                   { Lexeme{ tok = T.DblEquals }}
    '!='                   { Lexeme{ tok = T.NotEquals }}
    '<'                    { Lexeme{ tok = T.LessThan } }
    '>'                    { Lexeme{ tok = T.GreaterThan } }
    '<='                   { Lexeme{ tok = T.LessEqual } }
    '>='                   { Lexeme{ tok = T.GreaterEqual } }
    '&&'                   { Lexeme{ tok = T.And} }
    '||'                   { Lexeme{ tok = T.Or } }
    '!'                    { Lexeme{ tok = T.Not } }
    'let'                  { Lexeme{ tok = T.Let } }
    'in'                   { Lexeme{ tok = T.In } }
    'if'                   { Lexeme{ tok = T.If } }
    'then'                 { Lexeme{ tok = T.Then } }
    'else'                 { Lexeme{ tok = T.Else } }
    'true'                 { Lexeme{ tok = T.True } }
    'false'                { Lexeme{ tok = T.False } }
    int                    { Lexeme{ tok = T.Int } }
    ident                  { Lexeme{ tok = T.Ident } }


%%
-- a Hiss program consists of zero or more declarations
prog : {- empty -}                         { [] }
     | decls                               { $1 }

-- one or more declarations
decls : decl                               { [$1] }
      | decls decl                         { $2:$1 }

-- a top-level declaration
decl : binding '=' exp                     { mkDecl $1 $3 }

-- a Hiss expression
exp  : atom                                { $1 }
     | '!' atom                            { mkUnaryOpExp $1 $2 }
     | funApp                              { mkFunAppExp $1 }
     | 'let' binding '=' exp 'in' exp      { mkLetInExp $2 $4 $6}
     | 'if' exp 'then' exp 'else' exp      { mkIfExp $2 $4 $6 }
     | exp '*' exp                         { mkBinOpExp $1 Mult $3 }
     | exp '/' exp                         { mkBinOpExp $1 Div $3 }
     | exp '+' exp                         { mkBinOpExp $1 Add $3 }
     | exp '-' exp                         { mkBinOpExp $1 Sub $3 }
     | exp '==' exp                        { mkBinOpExp $1 Equals $3 }
     | exp '!=' exp                        { mkBinOpExp $1 NotEquals $3 }
     | exp '<' exp                         { mkBinOpExp $1 LessThan $3 }
     | exp '>' exp                         { mkBinOpExp $1 GreaterThan $3 }
     | exp '<=' exp                        { mkBinOpExp $1 LessEqual $3 }
     | exp '>=' exp                        { mkBinOpExp $1 GreaterEqual $3 }
     | exp '&&' exp                        { mkBinOpExp $1 And $3 }
     | exp '||' exp                        { mkBinOpExp $1 Or $3 }

-- smallest expression: some literal or a parenthesized expression
atom : 'true'                              { mkBoolExp $1 }
     | 'false'                             { mkBoolExp $1 }
     | int                                 { mkIntExp $1 }
     | ident                               { mkVarExp $1 }
     | '(' exp ')'                         { mkParenExp $1 $2 $3 }

-- either a value binding (just a name) or a func binding (a func name and zero or more arg names)
binding : ident                            { mkValBinding $1 }
        | ident '(' funBindingArgs ')'     { mkFuncBinding $1 $3 }

funBindingArgs : {- empty -}               { [] :: [Lexeme] }
               | ident                     { [$1] }
               | funBindingArgs ',' ident  { $3 : $1 }

funApp : atom '(' funArgs ')'              { mkFunApp $1 $3 }

funArgs : {- empty -}                      { [] :: [Exp Range] }
        | exp                              { [$1] }
        | funArgs ',' exp                  { $3 : $1 }

{
mkDecl :: Binding Range -> Exp Range -> Decl Range
mkDecl b e1 = Decl r b e1
    where r = (getAnn b) `mergeRange` (getAnn e1)

mkUnaryOpExp :: Lexeme -> Exp Range -> Exp Range
mkUnaryOpExp Lexeme{ tok=T.Not, rng=r1 } e1 = EUnaryOp r1 Not e1
    where r = r1 `mergeRange` (getAnn e1)

mkLetInExp :: Binding Range -> Exp Range -> Exp Range -> Exp Range
mkLetInExp binding e1 e2 = ELetIn r binding e1 e2
    where r = (getAnn binding) `mergeRange` (getAnn e1) `mergeRange` (getAnn e2) 

mkIfExp :: Exp Range -> Exp Range -> Exp Range -> Exp Range
mkIfExp e1 e2 e3 = EIf r e1 e2 e3
    where r = (getAnn e1) `mergeRange` (getAnn e2) `mergeRange` (getAnn e3)

mkBoolExp :: Lexeme -> Exp Range
mkBoolExp Lexeme { tok=T.True, rng=r } = EBool r True
mkBoolExp Lexeme { tok=T.False, rng=r } = EBool r False
mkBoolExp _ = error "Compiler bug: mkBoolExp called with a non-bool lexeme"

mkIntExp :: Lexeme -> Exp Range
mkIntExp Lexeme { tok=T.Int, val=str, rng=r } = EInt r (read str)
mkIntExp _ = error "Compiler bug: mkIntExp called with a non-int lexeme"

mkVarExp :: Lexeme -> Exp Range
mkVarExp Lexeme { rng = rng, tok = tok, val = val }
    = EVar rng (Name rng val)

mkFunAppExp :: FunApp Range -> Exp Range
mkFunAppExp funApp = EFunApp (getAnn funApp) funApp

mkBinOpExp :: Exp Range -> BinOp -> Exp Range -> Exp Range
mkBinOpExp e1 op e2 = EBinOp r e1 op e2
    where r = (getAnn e1) `mergeRange` (getAnn e2)

mkParenExp :: Lexeme -> Exp Range -> Lexeme -> Exp Range
mkParenExp Lexeme { rng = lRng } e1 Lexeme { rng = rRng } = EParen r (e1)
    where r = lRng `mergeRange` rRng

-- Creates a value binding
mkValBinding :: Lexeme -> Binding Range
mkValBinding Lexeme { tok = T.Ident, val=name, rng=r } = ValBinding r (Name r name)
mkLetBinding _ = error "Compiler bug: mkValBinding called with a non-identifier lexeme"

-- Converts an identifier to a Name.
mkName :: Lexeme -> Name Range
mkName Lexeme { tok=T.Ident, val=name, rng=r } = Name r name
mkName _ = error "Compiler bug: mkName called with a non-identifier lexeme"

-- Creates a function binding
mkFuncBinding :: Lexeme -> [Lexeme] -> Binding Range
mkFuncBinding Lexeme { tok = T.Ident, val=name, rng=r1 } argLexemes = FuncBinding r' (Name r1 name) (reverse args)
    where args = map mkName argLexemes
          r' = r1 `mergeRange` (foldl1 mergeRange (map getAnn args))

-- Creates a function application with a function name and one argument
mkFunApp :: Exp Range -> [Exp Range] -> FunApp Range
mkFunApp e1 es = FunApp r e1 (reverse es)
    where r = (getAnn e1) `mergeRange` (foldl1 mergeRange (map getAnn es))

parseError :: Lexeme -> Alex a
parseError _ = do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    alexError $ "Parse error at line " <> show line <> ", column " <> show col

lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}