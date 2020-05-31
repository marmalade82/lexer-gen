module Compiler.TypeChecker.Spec where


import Prelude

import Compiler (CompileResult, compile, Errors)
import Compiler.TestUtils (testErrors, testNoErrors)
import Data.Array as Array
import Data.Either (isLeft, Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Test.Spec (class Example, Spec, SpecT(..), describe, describeOnly, it, itOnly, pending)
import Test.Spec.Assertions (fail, shouldEqual)

typecheckSpec :: Spec Unit
typecheckSpec = describe "Test typechecker errors" do
    uniqueNamesSpec
    uniqueErrorsSpec
    definedErrorsSpec
    pure unit

    where 
        uniqueNamesSpec :: Spec Unit
        uniqueNamesSpec = describe "Names must be unique" do 
            it "no names" do 
                let program = "%%_normal_%%"
                    result = compile program
                    
                testNoErrors result
            it "one name" do 
                let program = "%%_normal_%%" <> "\n" <> "hi (regex);"
                    result = compile program
                testNoErrors result
            it "no duplicates" do
                let program = 
                        "%%_normal_%%" <> "\n" <> 
                        "hi (regex);" <> "\n" <> 
                        "safe (regex);"
                    result = compile program
                testNoErrors result
            it "one duplicate" do 
                let program = 
                        "%%_normal_%%" <> "\n" <> 
                        "hi (regex);" <> "\n" <> 
                        "hi (regex);"
                    result = compile program
                testErrors result 
                    (\errors -> do 
                        Array.length errors `shouldEqual` 1 
                        _ <- sequence $ (\x -> Str.contains (Pattern "already been used") x `shouldEqual` true) <$> errors
                        pure unit
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
                        _ <- sequence $ (\x -> Str.contains (Pattern "already been used") x `shouldEqual` true) <$> errors
                        pure unit
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
                        _ <- sequence $ (\x -> Str.contains (Pattern "already been used") x `shouldEqual` true) <$> errors
                        pure unit
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
                        _ <- sequence $ (\x -> Str.contains (Pattern "define an error") x `shouldEqual` true) <$> errors
                        pure unit
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
                        _ <- sequence $ (\x -> Str.contains (Pattern "define an error") x `shouldEqual` true) <$> errors
                        pure unit
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
                        _ <- sequence $ (\x -> Str.contains (Pattern "was not defined") x `shouldEqual` true) <$> errors
                        pure unit
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
                        _ <- sequence $ (\x -> Str.contains (Pattern "was not defined") x `shouldEqual` true) <$> errors
                        pure unit
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
                        Array.length errors `shouldEqual` 2
                        _ <- sequence $ (\x -> Str.contains (Pattern "was not defined") x `shouldEqual` true) <$> errors
                        pure unit
                    )