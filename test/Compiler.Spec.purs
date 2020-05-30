module Test.CompilerSpec where

import Prelude

import Compiler (CompileResult, compile, Errors)
import Data.Array as Array
import Data.Either (isLeft, Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Test.Spec (class Example, Spec, SpecT(..), describe, describeOnly, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describeOnly "Integration test compiler" do
    lexerSpec
    parserSpec
    typecheckSpec
    pure unit


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

-- Leaving off types here because I don't understand how to write the signature
--testErrors :: forall m t arg g. Monad m => Example t arg g => CompileResult -> (Errors -> t) -> SpecT g arg m Unit
testErrors res x = do 
    case res of 
        Left errors -> x errors
        _ -> fail "Result didn't have errors"

--testNoErrors :: forall m t arg g. Monad m => Example t arg g => CompileResult -> SpecT g arg m Unit
testNoErrors res = do 
    case res of 
        Right _ -> pure unit
        _ -> fail "Result should not have errors"

typecheckSpec :: Spec Unit
typecheckSpec = describe "Test typechecker errors" do
    uniqueNamesSpec
    reservedNamesSpec
    uniqueErrorsSpec
    definedErrorsSpec
    pure unit

    where 
        uniqueNamesSpec :: Spec Unit
        uniqueNamesSpec = describe "Names must be unique" do 
            it "no names" do 
                let program = "%%_normal_%%"
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 0 
                    )
            it "one name" do 
                let program = "%%_normal_%%" <> "\n" <> "hi (regex);"
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 0 
                    )
            it "no duplicates" do
                let program = 
                        "%%_normal_%%" <> "\n" <> 
                        "hi (regex);" <> "\n" <> 
                        "safe (regex);"
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 0 
                    )
            it "one duplicate" do 
                let program = 
                        "%%_normal_%%" <> "\n" <> 
                        "hi (regex);" <> "\n" <> 
                        "hi (regex);"
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 1 
                    )
            it "two matching duplicates" do
                let program = 
                        "%%_normal_%%" <> "\n" <> 
                        "hi (regex);" <> "\n" <> 
                        "hi (regex);" <> "\n" <> 
                        "hi (regex);"
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 2 
                    )
            it "two different duplicates" do
                let program =
                        "%%_normal_%%" <> "\n" <> 
                        "hi (regex);" <> "\n" <> 
                        "bye (regex);" <> "\n" <> 
                        "hi (regex);" <> "\n" <>
                        "bye (regex);"
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 2
                    )
        reservedNamesSpec :: Spec Unit
        reservedNamesSpec = describe "Reserved token names are rejected" do 
            it "reserved default is rejected" do 
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "$Default (regex);"
                        ]
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 1
                    )
        uniqueErrorsSpec :: Spec Unit
        uniqueErrorsSpec = describe "No two error definitions can use the same token definition" do 
            it "no error definitions" do
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        ]
                    result = compile program
                testNoErrors result
            it "two different error definitions" do
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        , "%%_error_%%"
                        , "hi \"WHAT\";"
                        , "bye \"WHAT\";"
                        ]
                    result = compile program
                testNoErrors result
            it "two matching duplicates" do 
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        , "%%_error_%%"
                        , "hi \"WHAT\";"
                        , "hi \"WHAT\";"
                        , "hi \"WHAT\";"
                        ]
                    result = compile program
                testErrors result 
                    ( \errors -> do 
                        Array.length errors `shouldEqual` 2
                    )
            it "two sets of duplicates" do
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        , "%%_error_%%"
                        , "hi \"WHAT\";"
                        , "bye \"WHAT\";"
                        , "hi \"WHAT\";"
                        , "bye \"WHAT\";"
                        ]
                    result = compile program
                testErrors result 
                    ( \errors -> do 
                        Array.length errors `shouldEqual` 2
                    )
        definedErrorsSpec :: Spec Unit
        definedErrorsSpec = describe "Error definitions must use defined tokens" do
            it "sync defined correctly" do 
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        , "%%_error_%%"
                        , "hi \"WHAT\" bye;"
                        ]
                    result = compile program
                testNoErrors result
            it "sync defined incorrectly" do 
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        , "%%_error_%%"
                        , "hi \"WHAT\" sigh;"
                        ]
                    result = compile program
                testErrors result 
                    ( \errors -> do 
                        Array.length errors `shouldEqual` 1
                    )
            it "one error defined incorrectly" do 
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        , "%%_error_%%"
                        , "sigh \"WHAT\";"
                        ]
                    result = compile program
                testErrors result 
                    ( \errors -> do 
                        Array.length errors `shouldEqual` 1
                    )
            it "two errors defined incorrectly" do
                let program = Str.joinWith "\n"
                        [ "%%_normal_%%"
                        , "hi (regex);"
                        , "bye (regex);"
                        , "%%_error_%%"
                        , "sigh \"WHAT\";"
                        , "try \"WHAT\";"
                        ]
                    result = compile program
                testErrors result 
                    ( \errors -> do 
                        Array.length errors `shouldEqual` 1
                    )