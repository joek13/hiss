module Syntax.ParserSpec (spec) where

import Error (HissError (..))
import Syntax (parseExpression)
import Syntax.AST (BinOp (..), Binding (..), Expr (..), Name (..), getAnn, stripAnns)
import Syntax.Lexer (AlexPosn (AlexPn), Range (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Util (fromRight)

spec :: Spec
spec = do
  describe "parseExpression" $ do
    it "parses a simple expression" $
      stripAnns (fromRight $ parseExpression "let x = 4 in x * x")
        `shouldBe` ELetIn
          ()
          (ValBinding () (Name () "x"))
          (EInt () 4)
          (EBinOp () (EVar () (Name () "x")) Mult (EVar () (Name () "x")))
    it "respects operator precedence for comparisons and boolean operators" $
      stripAnns (fromRight $ parseExpression "a < b && c >= d || e <= f && g == h")
        `shouldBe` EBinOp
          ()
          ( EBinOp
              ()
              ( EBinOp
                  ()
                  (EBinOp () (EVar () (Name () "a")) LessThan (EVar () (Name () "b")))
                  And
                  (EBinOp () (EVar () (Name () "c")) GreaterEqual (EVar () (Name () "d")))
              )
              Or
              (EBinOp () (EVar () (Name () "e")) LessEqual (EVar () (Name () "f")))
          )
          And
          ( EBinOp
              ()
              (EVar () (Name () "g"))
              Equals
              (EVar () (Name () "h"))
          )
    it "respects operator precedence for add/sub/mult/div" $
      stripAnns (fromRight $ parseExpression "a * b / c + d - e")
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
      stripAnns (fromRight $ parseExpression "let a = 0 in a + 3")
        `shouldBe` ELetIn -- i.e., let a = 0 in (a+3)
          ()
          (ValBinding () (Name () "a"))
          (EInt () 0)
          (EBinOp () (EVar () (Name () "a")) Add (EInt () 3))
    it "correctly parses chained let-in expressions" $
      stripAnns (fromRight $ parseExpression "let a = 0 in let b = 1 in a+b")
        `shouldBe` ELetIn -- i.e., let a = 0 in (let b = 1 in a+b)
          ()
          (ValBinding () (Name () "a"))
          (EInt () 0)
          ( ELetIn
              ()
              (ValBinding () (Name () "b"))
              (EInt () 1)
              (EBinOp () (EVar () (Name () "a")) Add (EVar () (Name () "b")))
          )
    it "parses a simple function application" $
      -- note: this test was more critical when we used Haskell-style function application syntax (e.g., "f x" instead of "f(x)")
      stripAnns (fromRight $ parseExpression "f(0,1,2)")
        `shouldBe` EFunApp
          ()
          (EVar () (Name () "f"))
          [EInt () 0, EInt () 1, EInt () 2]
    it "respects precedence of function application over multiplication" $
      -- note: this test was more critical when we used Haskell-style function application syntax (e.g., "f x" instead of "f(x)")
      stripAnns (fromRight $ parseExpression "f(a) * f(b)")
        `shouldBe` EBinOp
          ()
          (EFunApp () (EVar () (Name () "f")) [EVar () (Name () "a")])
          Mult
          (EFunApp () (EVar () (Name () "f")) [EVar () (Name () "b")])
    it "correctly tracks range of compound expressions" $
      getAnn (fromRight $ parseExpression "(if a then b else c)")
        `shouldBe` Range (AlexPn 0 1 1) (AlexPn 20 1 21)
    it "fails to parse a simple invalid program" $
      -- can only have names in a let binding. 'f 0' is the problem.
      parseExpression "let f(0) = 0 in 123" `shouldBe` Left (SyntaxError "Parse error at line 1, column 8")
