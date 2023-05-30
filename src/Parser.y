{
module Parser ( parseHiss ) where
import Lexer ( Lexeme(..), Token(..), Range(..), AlexPosn(..), Alex, mergeRange, alexError, alexGetInput, alexMonadScan )
import AST( Exp(..), Name(..), BinOp(..), LetBinding(..), getAnn )
import Data.Maybe (fromJust)
}

%name parseHiss
%tokentype { Lexeme } 
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Lexeme { tok = EOF } }

-- associativity
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

exp : 'let' letBinding '=' exp 'in' exp   { mkLetIn $2 $4 $6}
    | 'if' exp 'then' exp 'else' exp      { mkIf $2 $4 $6 }
    | ident                               { mkVar $1 } 
    | exp '*' exp                         { mkBinOp $1 Mult $3 }
    | exp '/' exp                         { mkBinOp $1 Div $3 }
    | exp '+' exp                         { mkBinOp $1 Add $3 }
    | exp '-' exp                         { mkBinOp $1 Sub $3 }
    | '(' exp ')'                         { mkParen $1 $2 $3 }

letBinding : ident                        { mkLetBinding $1 }
           | letBinding ident             { letBindingAppendArg $1 $2 }

{
mkIf :: Exp Range -> Exp Range -> Exp Range -> Exp Range
mkIf e1 e2 e3 = EIf r e1 e2 e3
    where r = (getAnn e1) `mergeRange` (getAnn e2) `mergeRange` (getAnn e3)

mkLetIn :: LetBinding Range -> Exp Range -> Exp Range -> Exp Range
mkLetIn binding e1 e2 = ELetIn r binding e1 e2
    where r = (getAnn binding) `mergeRange` (getAnn e1) `mergeRange` (getAnn e2) 

mkLetBinding :: Lexeme -> LetBinding Range
mkLetBinding Lexeme { tok=Ident, val=name, rng=r } = LetBinding r (Name r name) []
mkLetBinding _ = error "Compiler bug: mkLetBinding called with a non-identifier lexeme"

-- Appends an argument to an existing let binding.
letBindingAppendArg :: LetBinding Range -> Lexeme -> LetBinding Range
letBindingAppendArg (LetBinding r1 name names) Lexeme { tok=Ident, val=newName, rng=r2 }
    = LetBinding r' name (names ++ [ Name r2 newName ])
    where r' = r1 `mergeRange` r2
letBindingAppendArg _ _ = error "Compiler bug: letBindingAppendArg called with a non-identifier lexeme"

mkBinOp :: Exp Range -> BinOp -> Exp Range -> Exp Range
mkBinOp e1 op e2 = EBinOp r e1 op e2
    where r = (getAnn e1) `mergeRange` (getAnn e2)

mkParen :: Lexeme -> Exp Range -> Lexeme -> Exp Range
mkParen Lexeme { rng = lRng } e1 Lexeme { rng = rRng } = EParen r (e1)
    where r = lRng `mergeRange` rRng

mkVar :: Lexeme -> Exp Range
mkVar Lexeme { rng = rng, tok = tok, val = val }
    = EVar rng (Name rng val)

parseError :: Lexeme -> Alex a
parseError _ = do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    alexError $ "Parse error at line " <> show line <> ", column " <> show col

lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)

}