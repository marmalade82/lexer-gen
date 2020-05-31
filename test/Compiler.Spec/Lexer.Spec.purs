module Compiler.Lexer.Spec where

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

lexerSpec :: Spec Unit
lexerSpec = describe "Test lexing errors" do
    it "unrecognized tokens" do
        let program = "////"
            result = compile program
        case result of 
            Left errors -> do
                case Array.head errors of 
                    Nothing -> fail "Result didn't have errors"
                    Just err -> Str.contains (Pattern "Error near") err `shouldEqual` true
            _ -> fail "Result didn't have errors"
    it "multiple unrecognized tokens - first one ends compilation with an error" do 
        let program = Str.joinWith ""
                ["%%_normal_%%", "%%_error_%%", "%%_FAILURE_%%", "\"error\"", "%%FAT%", "(regex)", "name", ";"]
            result = compile program
        case result of 
            Left errors -> do
                Array.length errors `shouldEqual` 1
                _ <- sequence $ (\x -> Str.contains (Pattern "Error near") x `shouldEqual` true) <$> errors
                _ <- sequence $ (\x -> Str.contains (Pattern "FAILURE") x `shouldEqual` true) <$> errors
                pure unit
            _ -> fail "Result didn't have errors"