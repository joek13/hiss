{
module Syntax.Parser ( parseHiss ) where
import Syntax.Lexer ( Lexeme(..), Range(..), AlexPosn(..), Alex, mergeRange, alexError, alexGetInput, alexMonadScan )
import qualified Syntax.Lexer as T (Token (..))
import Syntax.AST( Exp(..), Name(..), UnaryOp(..), BinOp(..), LetBinding(..), FunApp(..), getAnn )
import Data.Maybe (fromJust)
}

%name parseHiss
%tokentype { Lexeme } 
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Lexeme { tok = T.EOF } }
%expect 0 -- compile error if shift/reduce or reduce/reduce conflicts exist

-- associativity and operator precedence
%right 'else'
%right 'in' 
%left '&&' '||'
%nonassoc '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%token
    '('                    { Lexeme{ tok = T.LParen } }
    ')'                    { Lexeme{ tok = T.RParen } }
    '+'                    { Lexeme{ tok = T.Plus } }
    '-'                    { Lexeme{ tok = T.Minus } }
    '*'                    { Lexeme{ tok = T.Star } }
    '/'                    { Lexeme{ tok = T.Slash } }
    '='                    { Lexeme{ tok = T.Equals } }
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

exp  : exp1                                { $1 }
     | '!' atom                            { mkUnaryOpExp $1 $2 }
     | funApp                              { mkFunAppExp $1 }
     | 'let' letBinding '=' exp 'in' exp   { mkLetInExp $2 $4 $6}
     | 'if' exp 'then' exp 'else' exp      { mkIfExp $2 $4 $6 }

exp1 : atom                                { $1 }
     | exp '*' exp                         { mkBinOpExp $1 Mult $3 }
     | exp '/' exp                         { mkBinOpExp $1 Div $3 }
     | exp '+' exp                         { mkBinOpExp $1 Add $3 }
     | exp '-' exp                         { mkBinOpExp $1 Sub $3 }
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

-- a let binding is simply one or more names
letBinding : ident                         { mkLetBinding $1 }
           | letBinding ident              { letBindingAppendArg $1 $2 }

-- using atom here ensures two things:
-- function application binds more tightly than any binary operators (e.g., f a + f b = (f a) + (f b) )
-- function application is left associative (e.g., f a b c = f applied to a,b,c )
funApp : atom atom %shift                  { mkFunApp $1 $2 }
       | funApp atom %shift                { funAppAppendArg $1 $2 }

{
mkUnaryOpExp :: Lexeme -> Exp Range -> Exp Range
mkUnaryOpExp Lexeme{ tok=T.Not, rng=r1 } e1 = EUnaryOp r1 Not e1
    where r = r1 `mergeRange` (getAnn e1)

mkLetInExp :: LetBinding Range -> Exp Range -> Exp Range -> Exp Range
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

-- Creates a let binding with a name and no arguments.
mkLetBinding :: Lexeme -> LetBinding Range
mkLetBinding Lexeme { tok=T.Ident, val=name, rng=r } = LetBinding r (Name r name) []
mkLetBinding _ = error "Compiler bug: mkLetBinding called with a non-identifier lexeme"

-- Appends an argument to an existing let binding.
letBindingAppendArg :: LetBinding Range -> Lexeme -> LetBinding Range
letBindingAppendArg (LetBinding r1 name names) Lexeme { tok=T.Ident, val=newName, rng=r2 }
    = LetBinding r' name (names ++ [ Name r2 newName ])
    where r' = r1 `mergeRange` r2
letBindingAppendArg _ _ = error "Compiler bug: letBindingAppendArg called with a non-identifier lexeme"

-- Creates a function application with a function name and one argument
mkFunApp :: Exp Range -> Exp Range -> FunApp Range
mkFunApp e1 e2 = FunApp r e1 [e2]
    where r = (getAnn e1) `mergeRange` (getAnn e2)

-- Appends an argument to an existing function application
funAppAppendArg :: FunApp Range -> Exp Range -> FunApp Range
funAppAppendArg (FunApp r1 fun args) e1 = FunApp r' fun (args ++ [e1])
    where r' = r1 `mergeRange` (getAnn e1)

parseError :: Lexeme -> Alex a
parseError _ = do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    alexError $ "Parse error at line " <> show line <> ", column " <> show col

lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}