module Syntax.ParserSpec (spec) where

import Syntax.AST (BinOp (..), Exp (..), FunApp (..), LetBinding (..), Name (..), stripAnns)
import Syntax.Lexer (Range, runAlex)
import Syntax.Parser (parseHiss)
import Test.Hspec (Spec, describe, it, shouldBe)

parseString :: String -> Either String (Exp Range)
parseString inp = runAlex inp parseHiss

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left _) = error "Called fromRight on Left"

spec :: Spec
spec = do
  describe "parseHiss" $ do
    it "parses a simple program" $
      stripAnns (fromRight $ parseString "let x = 4 in x * x")
        `shouldBe` ELetIn
          ()
          (LetBinding () (Name () "x") [])
          (EInt () 4)
          (EBinOp () (EVar () (Name () "x")) Mult (EVar () (Name () "x")))
    it "respects operator precedence for add/sub/mult/div" $
      stripAnns (fromRight $ parseString "a * b / c + d - e")
        `shouldBe` EBinOp -- i.e., (((a*b)/c)+d)-e
          ()
          ( EBinOp
              ()
              ( EBinOp
                  ()
                  (EBinOp () (EVar () (Name () "a")) Mult (EVar () (Name () "b")))
                  Div
                  (EVar () (Name () "c"))
              )
              Add
              (EVar () (Name () "d"))
          )
          Sub
          (EVar () (Name () "e"))
    it "respects operator precedence for let...in and addition" $
      stripAnns (fromRight $ parseString "let a = 0 in a + 3")
        `shouldBe` ELetIn -- i.e., let a = 0 in (a+3)
          ()
          (LetBinding () (Name () "a") [])
          (EInt () 0)
          (EBinOp () (EVar () (Name () "a")) Add (EInt () 3))
    it "correctly parses chained let-in expressions" $
      stripAnns (fromRight $ parseString "let a = 0 in let b = 1 in a+b")
        `shouldBe` ELetIn -- i.e., let a = 0 in (let b = 1 in a+b)
          ()
          (LetBinding () (Name () "a") [])
          (EInt () 0)
          ( ELetIn
              ()
              (LetBinding () (Name () "b") [])
              (EInt () 1)
              (EBinOp () (EVar () (Name () "a")) Add (EVar () (Name () "b")))
          )
    it "respects left associativity of function application" $
      stripAnns (fromRight $ parseString "f 0 1 2") -- i.e., f applied to 0,1,2 and not f (0 (1 2))
        `shouldBe` EFunApp
          ()
          (FunApp () (EVar () (Name () "f")) [EInt () 0, EInt () 1, EInt () 2])
    it "respects precedence of function application over multiplication" $
      stripAnns (fromRight $ parseString "f a * b c") -- i.e., (f a) * (f b)
        `shouldBe` EBinOp
          ()
          (EFunApp () (FunApp () (EVar () (Name () "f")) [EVar () (Name () "a")]))
          Mult
          (EFunApp () (FunApp () (EVar () (Name () "b")) [EVar () (Name () "c")]))
    it "fails to parse a simple invalid program" $
      -- can only have names in a let binding. 'f 0' is the problem.
      parseString "let f 0 = 0 in 123" `shouldBe` Left "Parse error at line 1, column 8"
