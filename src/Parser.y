{
module Parser ( parseHiss ) where
import Lexer ( Lexeme(..), Token(..), Range(..), AlexPosn(..), Alex, mergeRange, alexError, alexGetInput, alexMonadScan )
import AST( Exp(..), Name(..), BinOp(..), LetBinding(..), FunApp(..), getAnn )
import Data.Maybe (fromJust)
}

%name parseHiss
%tokentype { Lexeme } 
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Lexeme { tok = EOF } }
%expect 0 -- compile error if shift/reduce or reduce/reduce conflicts exist

-- associativity and operator precedence
%right 'else'
%right 'in' 
%left '+' '-'
%left '*' '/'

%token
    '('                    { Lexeme{ tok = LParen } }
    ')'                    { Lexeme{ tok = RParen } }
    '+'                    { Lexeme{ tok = Plus } }
    '-'                    { Lexeme{ tok = Minus } }
    '*'                    { Lexeme{ tok = Star } }
    '/'                    { Lexeme{ tok = Slash } }
    '='                    { Lexeme{ tok = Equals } }
    'let'                  { Lexeme{ tok = Let } }
    'in'                   { Lexeme{ tok = In } }
    'if'                   { Lexeme{ tok = If } }
    'then'                 { Lexeme{ tok = Then } }
    'else'                 { Lexeme{ tok = Else } }
    int                    { Lexeme{ tok = Int } }
    ident                  { Lexeme{ tok = Ident } }


%%

exp : atom %shift                          { $1 }
    | funApp %shift                        { mkFunAppExp $1 }

-- expression atom, i.e., an expression that is not a function application
-- factoring out atom forces left-associativity of function application
atom : 'let' letBinding '=' exp 'in' exp   { mkLetInExp $2 $4 $6}
     | 'if' exp 'then' exp 'else' exp      { mkIfExp $2 $4 $6 }
     | int                                 { mkIntExp $1 }
     | ident                               { mkVarExp $1 } 
     | atom '*' atom                       { mkBinOpExp $1 Mult $3 }
     | atom '/' atom                       { mkBinOpExp $1 Div $3 }
     | atom '+' atom                       { mkBinOpExp $1 Add $3 }
     | atom '-' atom                       { mkBinOpExp $1 Sub $3 }
     | '(' exp ')'                         { mkParenExp $1 $2 $3 }

letBinding : ident                         { mkLetBinding $1 }
           | letBinding ident              { letBindingAppendArg $1 $2 }

-- using atom here ensures that 'f a b c' parses as 'f applied to a,b,c' and not 'f (a (b c))'
funApp : atom atom %shift                  { mkFunApp $1 $2 }
       | funApp atom %shift                { funAppAppendArg $1 $2 }

{
mkLetInExp :: LetBinding Range -> Exp Range -> Exp Range -> Exp Range
mkLetInExp binding e1 e2 = ELetIn r binding e1 e2
    where r = (getAnn binding) `mergeRange` (getAnn e1) `mergeRange` (getAnn e2) 

mkIfExp :: Exp Range -> Exp Range -> Exp Range -> Exp Range
mkIfExp e1 e2 e3 = EIf r e1 e2 e3
    where r = (getAnn e1) `mergeRange` (getAnn e2) `mergeRange` (getAnn e3)

mkIntExp :: Lexeme -> Exp Range
mkIntExp Lexeme { tok=Int, val=str, rng=r } = EInt r (read str)
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
mkLetBinding Lexeme { tok=Ident, val=name, rng=r } = LetBinding r (Name r name) []
mkLetBinding _ = error "Compiler bug: mkLetBinding called with a non-identifier lexeme"

-- Appends an argument to an existing let binding.
letBindingAppendArg :: LetBinding Range -> Lexeme -> LetBinding Range
letBindingAppendArg (LetBinding r1 name names) Lexeme { tok=Ident, val=newName, rng=r2 }
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