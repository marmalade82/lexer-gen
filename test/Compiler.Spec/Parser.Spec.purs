module Compiler.Parser.Spec where


import Data.Array as Array
import Data.Either (isLeft, Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Compiler (CompileResult, compile, Errors)
import Prelude
import Test.Spec (class Example, Spec, SpecT(..), describe, describeOnly, it, itOnly, pending)
import Test.Spec.Assertions (fail, shouldEqual)


parserSpec :: Spec Unit
parserSpec = describe "Test parsing errors" do 
    it "returns parsing errors" do
        let program = "%%_normal_%%" <> "\n" <> "example" <> "(walla)"
            result = compile program
        case result of 
            Left errors -> do
                Array.length errors `shouldEqual` 1
                _ <- sequence $ (\x -> Str.contains (Pattern "Terminator") x `shouldEqual` true) <$> errors
                pure unit
            _ -> fail "Result didn't have errors"
    pure unit