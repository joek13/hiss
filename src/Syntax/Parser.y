{
module Syntax.Parser ( parseExpr, parseProg, parseDecl ) where
import Syntax.Lexer ( Lexeme(..), Range(..), AlexPosn(..), Alex, mergeRange, alexError, alexGetInput, alexMonadScan )
import qualified Syntax.Lexer as T (Token (..))
import Syntax.AST( Program(..), Decl(..), Expr(..), Name(..), UnaryOp(..), BinOp(..), Binding(..), getAnn )
import Data.Maybe (fromJust)
}

%name parseExpr expr
%name parseProg prog
%name parseDecl decl
%tokentype { Lexeme } 
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Lexeme { tok = T.EOF } }
%expect 0 -- compile error if shift/reduce or reduce/reduce conflicts exist

-- associativity and operator precedence
%right 'else'
%right 'in' 
%left '&&' '||'
%left '==' '!='
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
prog : {- empty -}                         { emptyProg }
     | decls                               { mkProg $1 }

-- one or more declarations
decls : decl                               { [$1] }
      | decls decl                         { $2:$1 }

-- a top-level declaration
decl : binding '=' expr                     { mkDecl $1 $3 }

-- a Hiss expression
expr  : atom                                { $1 }
      | '!' atom                            { mkUnaryOpExpr $1 $2 }
      | atom '(' funArgs ')'                { mkFunAppExpr $1 $3 $4 }
      | 'let' binding '=' expr 'in' expr    { mkLetInExpr $2 $4 $6}
      | 'if' expr 'then' expr 'else' expr   { mkIfExpr $2 $4 $6 }
      | expr '*' expr                       { mkBinOpExpr $1 Mult $3 }
      | expr '/' expr                       { mkBinOpExpr $1 Div $3 }
      | expr '+' expr                       { mkBinOpExpr $1 Add $3 }
      | expr '-' expr                       { mkBinOpExpr $1 Sub $3 }
      | expr '==' expr                      { mkBinOpExpr $1 Equals $3 }
      | expr '!=' expr                      { mkBinOpExpr $1 NotEquals $3 }
      | expr '<' expr                       { mkBinOpExpr $1 LessThan $3 }
      | expr '>' expr                       { mkBinOpExpr $1 GreaterThan $3 }
      | expr '<=' expr                      { mkBinOpExpr $1 LessEqual $3 }
      | expr '>=' expr                      { mkBinOpExpr $1 GreaterEqual $3 }
      | expr '&&' expr                      { mkBinOpExpr $1 And $3 }
      | expr '||' expr                      { mkBinOpExpr $1 Or $3 }

-- smallest expression: some literal or a parenthesized expression
atom : 'true'                              { mkBoolExpr $1 }
     | 'false'                             { mkBoolExpr $1 }
     | int                                 { mkIntExpr $1 }
     | ident                               { mkVarExpr $1 }
     | '(' expr ')'                        { mkParenExpr $1 $2 $3 }

-- either a value binding (just a name) or a func binding (a func name and zero or more arg names)
binding : ident                            { mkValBinding $1 }
        | ident '(' funBindingArgs ')'     { mkFuncBinding $1 $3 }

funBindingArgs : {- empty -}               { [] :: [Lexeme] }
               | ident                     { [$1] }
               | funBindingArgs ',' ident  { $3 : $1 }

funArgs : {- empty -}                      { [] :: [Expr Range] }
        | expr                             { [$1] }
        | funArgs ',' expr                 { $3 : $1 }

{
emptyProg :: Program Range
emptyProg = Program (Range (AlexPn 0 0 0) (AlexPn 0 0 0)) []

mkProg :: [Decl Range] -> Program Range
mkProg decls = Program r (reverse decls)
    where r = foldl1 mergeRange (map getAnn decls)

mkDecl :: Binding Range -> Expr Range -> Decl Range
mkDecl b e1 = Decl r b e1
    where r = (getAnn b) `mergeRange` (getAnn e1)

mkUnaryOpExpr :: Lexeme -> Expr Range -> Expr Range
mkUnaryOpExpr Lexeme{ tok=T.Not, rng=r1 } e1 = EUnaryOp r1 Not e1
    where r = r1 `mergeRange` (getAnn e1)

mkLetInExpr :: Binding Range -> Expr Range -> Expr Range -> Expr Range
mkLetInExpr binding e1 e2 = ELetIn r binding e1 e2
    where r = (getAnn binding) `mergeRange` (getAnn e1) `mergeRange` (getAnn e2) 

mkIfExpr :: Expr Range -> Expr Range -> Expr Range -> Expr Range
mkIfExpr e1 e2 e3 = EIf r e1 e2 e3
    where r = (getAnn e1) `mergeRange` (getAnn e2) `mergeRange` (getAnn e3)

mkBoolExpr :: Lexeme -> Expr Range
mkBoolExpr Lexeme { tok=T.True, rng=r } = EBool r True
mkBoolExpr Lexeme { tok=T.False, rng=r } = EBool r False
mkBoolExpr _ = error "Compiler bug: mkBoolExpr called with a non-bool lexeme"

mkIntExpr :: Lexeme -> Expr Range
mkIntExpr Lexeme { tok=T.Int, val=str, rng=r } = EInt r (read str)
mkIntExpr _ = error "Compiler bug: mkIntExpr called with a non-int lexeme"

mkVarExpr :: Lexeme -> Expr Range
mkVarExpr Lexeme { rng = rng, tok = tok, val = val }
    = EVar rng (Name rng val)

mkFunAppExpr :: Expr Range -> [Expr Range] -> Lexeme -> Expr Range
mkFunAppExpr e1 es Lexeme { rng=r } = EFunApp r' e1 (reverse es)
    where r' | length es == 0 = (getAnn e1) `mergeRange` r
             | otherwise      = (getAnn e1) `mergeRange` (foldl1 mergeRange (map getAnn es)) `mergeRange` r

mkBinOpExpr :: Expr Range -> BinOp -> Expr Range -> Expr Range
mkBinOpExpr e1 op e2 = EBinOp r e1 op e2
    where r = (getAnn e1) `mergeRange` (getAnn e2)

mkParenExpr :: Lexeme -> Expr Range -> Lexeme -> Expr Range
mkParenExpr Lexeme { rng = lRng } e1 Lexeme { rng = rRng } = EParen r (e1)
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
          r' | length args == 0 = r1
             | otherwise        = r1 `mergeRange` (foldl1 mergeRange (map getAnn args))

parseError :: Lexeme -> Alex a
parseError _ = do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    alexError $ "Parse error at line " <> show line <> ", column " <> show col

lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}