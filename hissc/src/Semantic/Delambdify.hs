{-
    Hoists local function declarations (and lambda expressions if we ever add them) into global functions to simplify code generation.
    Why is it called delambdify if we don't (yet) support lambda expressions? ...because it's fun.
-}
module Semantic.Delambdify (delambdify) where
import Syntax.AST (Program (..), Decl (..), Annotated (getAnn), declGetName, Expr (..), getIdent, progDecls, Binding (..), bindingGetName, Name (Name))
import Control.Monad.Writer (Writer, runWriter, MonadWriter (tell))
import Control.Monad.State (StateT (runStateT), evalStateT, modify, MonadState (get))
import Data.Set ( Set )
import Data.Set qualified as Set

{-
    Delambdify a r represents a computation that can:
    - Read and update a set of identifiers
    - Append new Decl a's
    - Return a value of type r
-}
type Delambdify a r = StateT (Set String) (Writer [Decl a]) r

-- Appends an integer suffix to an identifier if the suffix is nonzero.
suffixIdent :: Int -> String -> String
suffixIdent 0 i = i
suffixIdent c i = i <> show c

-- Finds the smallest integer suffix so that its identifier is unique.
findUniqueIdent :: Int -> String -> Delambdify a String
findUniqueIdent ctr ident = do
    idents <- get
    let ident' = suffixIdent ctr ident
    if ident' `Set.member` idents 
        then findUniqueIdent (ctr+1) ident -- Increment the counter
        else return ident' -- Return this identifier

-- Makes an identifier unique, adds it to the set of identifiers, 
makeUniqueIdent :: String -> Delambdify a String
makeUniqueIdent ident = do
    ident' <- findUniqueIdent 0 ident
    modify (Set.insert ident')
    return ident'

delambdifyExpr :: String -> Expr a -> Delambdify a (Expr a)
delambdifyExpr prefix (ELetIn a (FuncBinding fa (Name na ident) args) bodyExpr inExpr) = do
    {-
        When we encounter a local function declaration:
        1. Mangle its name into something that'll be unique globally.
        2. Write that new declaration out
        3. Replace the declaration site with an alias.
    -}
    -- Mangle identifier and make sure it is unique
    ident' <- makeUniqueIdent $ "_" <> prefix <> "_" <> ident
    -- Emit new global binding
    let name' = Name na ident'
    let binding' = FuncBinding fa name' args
    let decl' = Decl a binding' bodyExpr
    tell [decl']
    -- Replace declaration site with an alias
    ELetIn a (ValBinding fa (Name na ident)) (EVar fa name') <$> delambdifyExpr prefix inExpr
delambdifyExpr _ expr = pure expr

delambdifyDecl :: Decl a -> Delambdify a (Decl a)
delambdifyDecl d@(Decl a b body) = Decl a b <$> delambdifyExpr name body
    where name = getIdent $ declGetName d

delambdify' :: Program a -> Delambdify a (Program a)
delambdify' (Program a decls) = Program a <$> mapM delambdifyDecl decls

delambdify :: Program a -> Program a
delambdify p =
    -- Note: we don't have to first collect existing program identifiers. 
    -- Mangled names have a leading underscore, which is not a valid Hiss identifier.
    -- All we need to do is make sure mangled identifiers do not collide with each other.
    let (p', newDecls) = runWriter $ evalStateT (delambdify' p) Set.empty
    in Program (getAnn p') (newDecls ++ progDecls p')