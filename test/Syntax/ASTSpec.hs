module Syntax.ASTSpec (spec) where

import Syntax.AST (BinOp (Sub), Exp (..), FunApp (..), LetBinding (..), Name (..), getAnn, mapAnn)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "getAnn" $ do
    it "gets annotation from each AST node type" $ do
      getAnn (EInt "target" 42) `shouldBe` "target"
      getAnn (EVar "target" undefined) `shouldBe` "target"
      getAnn (EFunApp "target" undefined) `shouldBe` "target"
      getAnn (EBinOp "target" undefined undefined undefined) `shouldBe` "target"
      getAnn (ELetIn "target" undefined undefined undefined) `shouldBe` "target"
      getAnn (EIf "target" undefined undefined undefined) `shouldBe` "target"
      getAnn (EParen "target" undefined) `shouldBe` "target"
      getAnn (Name "target" undefined) `shouldBe` "target"
      getAnn (LetBinding "target" undefined undefined) `shouldBe` "target"
      getAnn (FunApp "target" undefined undefined) `shouldBe` "target"
  describe "mapAnn" $
    it "maps annotations on a sample AST" $ -- uses integer annotations to test mapAnn
      mapAnn
        (* 2)
        ( ELetIn
            1
            (LetBinding 2 (Name 3 "f") [Name 4 "x"])
            (EIf 5 (EVar 6 (Name 7 "x")) (EFunApp 8 (FunApp 9 (EVar 10 (Name 11 "f")) [EParen 12 (EBinOp 13 (EVar 14 (Name 15 "x")) Sub (EInt 16 1))])) (EInt 17 42))
            (EFunApp 18 (FunApp 19 (EVar 20 (Name 21 "f")) [EInt 22 4])) ::
            Exp Int
        )
        `shouldBe` ELetIn
          2
          (LetBinding 4 (Name 6 "f") [Name 8 "x"])
          (EIf 10 (EVar 12 (Name 14 "x")) (EFunApp 16 (FunApp 18 (EVar 20 (Name 22 "f")) [EParen 24 (EBinOp 26 (EVar 28 (Name 30 "x")) Sub (EInt 32 1))])) (EInt 34 42))
          (EFunApp 36 (FunApp 38 (EVar 40 (Name 42 "f")) [EInt 44 4]))
