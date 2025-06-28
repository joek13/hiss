-- Check (syntactic) validiting of program identifiers.
module Syntax.Identifiers (checkIdentifiers) where
import Syntax (showPosn, start)
import Syntax.Lexer (Range)
import Error (HissError (SyntaxError))
import Syntax.AST (Program, Name, Decl (Decl), bindingGetName, Expr (..), progDecls, getIdent, Annotated (getAnn))
import Control.Monad (forM_)

checkIdentifier' :: String -> Either String ()
checkIdentifier' ('_':_) = Left "identifiers may not begin with underscore"
checkIdentifier' _ = Right ()

checkIdentifier :: Name Range -> Either HissError ()
checkIdentifier name =
    let ident = getIdent name in
    case checkIdentifier' ident of
        Left problem -> Left $ SyntaxError $ "Invalid identifier '"<> ident <>"' at " <> (showPosn . start . getAnn) name <> " (" <> problem <>  ")"
        Right () -> Right ()

exprIdentifiers :: Expr Range -> [Name Range]
exprIdentifiers (EVar _ name) = [name]
exprIdentifiers (EFunApp _ funExpr argExprs) = exprIdentifiers funExpr ++ concatMap exprIdentifiers argExprs
exprIdentifiers (EUnaryOp _ _ expr) = exprIdentifiers expr
exprIdentifiers (EBinOp _ expr1 _ expr2) = concatMap exprIdentifiers [expr1, expr2]
exprIdentifiers (ELetIn _ b expr1 expr2) = bindingGetName b : concatMap exprIdentifiers [expr1, expr2]
exprIdentifiers (EIf _ condExpr thenExpr elseExpr) = concatMap exprIdentifiers [condExpr, thenExpr, elseExpr]
exprIdentifiers (EParen _ expr) = exprIdentifiers expr
exprIdentifiers _ = []

declIdentifiers :: Decl Range -> [Name Range]
declIdentifiers (Decl _ b e) = bindingGetName b:exprIdentifiers e

progIdentifiers :: Program Range -> [Name Range]
progIdentifiers = concatMap declIdentifiers . progDecls

checkIdentifiers :: Program Range -> Either HissError (Program Range)
checkIdentifiers prog = do
    forM_ (progIdentifiers prog) checkIdentifier
    return prog