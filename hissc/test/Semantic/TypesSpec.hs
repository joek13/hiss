module Semantic.TypesSpec (spec) where

import Semantic.Types (Cons (..), Type (..), Var (..), relabel)
import Test.Hspec (Spec, describe, it, shouldBe)

intTy :: Type
intTy = TCons CInt

var :: String -> Type
var = TVar . Var

spec :: Spec
spec = do
  describe "show Type" $ do
    it "arrow is right-associative in function types" $ do
      show (TFunc intTy (TFunc intTy intTy)) `shouldBe` "int -> int -> int"
      show (TFunc (TFunc intTy intTy) intTy) `shouldBe` "(int -> int) -> int"
  describe "relabel" $ do
    it "leaves cons and units unchanged" $ do
      relabel intTy `shouldBe` intTy
      relabel TUnit `shouldBe` TUnit
    it "relabels a single variable" $ do
      relabel (var "h") `shouldBe` var "a"
    it "relabels a function type" $ do
      relabel (TFunc (var "e") (TFunc (var "h") (var "g"))) `shouldBe` TFunc (var "a") (TFunc (var "b") (var "c"))