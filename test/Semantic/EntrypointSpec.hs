module Semantic.EntrypointSpec (spec) where

import Error (HissError (SemanticError))
import Semantic.Entrypoint (checkEntrypoint)
import Syntax (parseProgram)
import Syntax.AST (Program)
import Syntax.Lexer (Range (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Util (fromRight)

prog1 :: Program Range
prog1 = fromRight $ parseProgram "f() = 5"

prog2 :: Program Range
prog2 = fromRight $ parseProgram "main = 1337"

prog3 :: Program Range
prog3 = fromRight $ parseProgram "main(x) = x"

prog4 :: Program Range
prog4 = fromRight $ parseProgram "main() = 42"

spec :: Spec
spec = do
  describe "checkEntrypoint" $ do
    it "errors on missing 'main' function" $
      checkEntrypoint prog1 `shouldBe` Left (SemanticError "Missing function 'main'")
    it "errors on 'main' declared as a value" $
      checkEntrypoint prog2 `shouldBe` Left (SemanticError "'main' must be declared as a function")
    it "errors on 'main' declared with arguments" $
      checkEntrypoint prog3 `shouldBe` Left (SemanticError "Function 'main' must have 0 arguments, not 1")
    it "accepts valid program" $
      checkEntrypoint prog4 `shouldBe` Right prog4