module Semantic.TypecheckingSpec (spec) where

import Error (HissError (..))
import Semantic.Typechecking (typecheck)
import Semantic.Types (Cons (..), Type (..), Var (..), getTy)
import Syntax (parseDeclaration, parseExpression, parseProgram)
import Test.Hspec (Spec, context, describe, it, shouldBe)

infer' :: String -> Either HissError Type
infer' str = do
  ast <- parseExpression str
  ast' <- typecheck ast
  return $ getTy ast'

inferDecl' :: String -> Either HissError Type
inferDecl' str = do
  ast <- parseDeclaration str
  ast' <- typecheck ast
  return $ getTy ast'

inferProg' :: String -> Either HissError Type
inferProg' str = do
  ast <- parseProgram str
  ast' <- typecheck ast
  return $ getTy ast'

intTy :: Type
intTy = TCons CInt

boolTy :: Type
boolTy = TCons CBool

var :: String -> Type
var = TVar . Var

mkCurried :: Type -> [Type] -> Type
mkCurried ret [] = TFunc TUnit ret
mkCurried ret args = foldr TFunc ret args

spec :: Spec
spec = do
  describe "typecheck" $ do
    context "Expr" $ do
      it "typechecks literal expressions" $ do
        infer' "123" `shouldBe` Right intTy
        infer' "false" `shouldBe` Right boolTy
      it "typechecks variables" $ do
        infer' "let x = 5 in x" `shouldBe` Right intTy
      it "typechecks a simple function" $ do
        infer' "let f(x) = x+1 in f(1)" `shouldBe` Right intTy
      it "typechecks zero-arg function" $ do
        infer' "let f() = 0 in f" `shouldBe` Right (TFunc TUnit intTy)
      it "typechecks polymorphic functions" $ do
        infer' "let f(x) = x in f" `shouldBe` Right (TFunc (var "a") (var "a"))
        infer' "let const(c, x) = c in const" `shouldBe` Right (mkCurried (var "a") [var "a", var "b"])
        infer' "let app(f, x) = f(x) in app" `shouldBe` Right (TFunc (TFunc (var "a") (var "b")) (TFunc (var "a") (var "b")))
      it "typechecks unary operators" $ do
        infer' "let f(x) = !x in f" `shouldBe` Right (TFunc boolTy boolTy)
      it "typechecks binary operators" $ do
        infer' "let f(x,y) = x + y in f" `shouldBe` Right (mkCurried intTy [intTy, intTy])
        infer' "let f(x,y) = x - y in f" `shouldBe` Right (mkCurried intTy [intTy, intTy])
        infer' "let f(x,y) = x * y in f" `shouldBe` Right (mkCurried intTy [intTy, intTy])
        infer' "let f(x,y) = x / y in f" `shouldBe` Right (mkCurried intTy [intTy, intTy])
        infer' "let f(x,y) = x == y in f" `shouldBe` Right (mkCurried boolTy [intTy, intTy]) -- TODO: support boolean eq/neq
        infer' "let f(x,y) = x != y in f" `shouldBe` Right (mkCurried boolTy [intTy, intTy]) -- TODO: support boolean eq/neq
        infer' "let f(x,y) = x > y in f" `shouldBe` Right (mkCurried boolTy [intTy, intTy])
        infer' "let f(x,y) = x >= y in f" `shouldBe` Right (mkCurried boolTy [intTy, intTy])
        infer' "let f(x,y) = x < y in f" `shouldBe` Right (mkCurried boolTy [intTy, intTy])
        infer' "let f(x,y) = x <= y in f" `shouldBe` Right (mkCurried boolTy [intTy, intTy])
        infer' "let f(x,y) = x || y in f" `shouldBe` Right (mkCurried boolTy [boolTy, boolTy])
        infer' "let f(x,y) = x && y in f" `shouldBe` Right (mkCurried boolTy [boolTy, boolTy])
      it "rejects bad binary operators" $ do
        infer' "true + 1" `shouldBe` Left (SemanticError "Type error: cannot unify types bool and int")
        infer' "1 || false" `shouldBe` Left (SemanticError "Type error: cannot unify types int and bool")
        infer' "false > 0" `shouldBe` Left (SemanticError "Type error: cannot unify types bool and int")
      it "typechecks partial application" $ do
        infer' "let f(x,y) = x + y in f(1)" `shouldBe` Right (mkCurried intTy [intTy])
        infer' "let f(x) = let g(y) = x + y in g in f(1)" `shouldBe` Right (mkCurried intTy [intTy])
      it "rejects bad application" $ do
        infer' "true()" `shouldBe` Left (SemanticError "Type error: cannot unify types bool and () -> a")
        infer' "let f(x) = x in f(1,2)" `shouldBe` Left (SemanticError "Type error: cannot unify types int -> d and int")
      it "rejects infinite type" $ do
        infer' "let omega(f) = f(f) in omega" `shouldBe` Left (SemanticError "Type error: cannot construct infinite type")
    context "Decl" $ do
      it "typechecks a value declaration" $ do
        inferDecl' "a = false" `shouldBe` Right boolTy
      it "typechecks a function declaration" $ do
        inferDecl' "f(x) = x + 1" `shouldBe` Right (TFunc intTy intTy)
    context "Program" $ do
      it "typechecks a program with mutual recursion" $ do
        inferProg' "even(n) = if n == 0 then true else !(odd(n-1)) \n odd(n) = if n == 1 then true else !(even(n-1))" `shouldBe` Right TUnit
      it "rejects a program that is type incorrect" $ do
        inferProg' "f(x) = x + 1 \n main() = f(3) || false" `shouldBe` Left (SemanticError "Type error: cannot unify types int and bool")