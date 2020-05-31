module Test.CompilerSpec where

import Prelude

import Compiler (CompileResult, compile, Errors)
import Data.Array as Array
import Data.Either (isLeft, Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Globals (__dirname)
import Node.Path (FilePath)
import Node.Path as Path
import Test.Spec (class Example, Spec, SpecT(..), describe, describeOnly, it, itOnly, pending)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "Integration test compiler" do
    lexerSpec
    parserSpec
    typecheckSpec
    genSpec
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
        x -> fail $ "Result didn't have errors: "

-- Leaving off types here because I don't understand how to write the signature
--testNoErrors :: forall m t arg g. Monad m => Example t arg g => CompileResult -> SpecT g arg m Unit
testNoErrors res = do 
    case res of 
        Right _ -> pure unit
        x -> fail $ "Result should not have errors: "

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

genSpec :: Spec Unit
genSpec = describe "Generated code performs as expected" do
    pending "ALL THE CODEGEN SPEC TESTS, AND THEN SOME EXTRA"



generateLexer :: String -> String -> Aff.Aff Unit
generateLexer prog fileName = do
    liftEffect $ do
        let program = compile prog
            generated = case program of 
                Left _ -> ""
                Right lexer -> lexer
        file <- Path.resolve [__dirname] ("../../test/compiler-inputs/" <> fileName)
        exists <- FS.exists file 
        if exists
        then FS.truncate file 0
        else FS.writeTextFile UTF8 file ""
        FS.writeTextFile UTF8 file generated
        pure unit

runTest :: String -> Aff.Aff Boolean
runTest testCommand = Aff.makeAff 
    (\emit -> do
        cwd <- getCwd
        liftEffect $ log cwd
        -- This should cause the tests to run
        -- Since this will run asynchronously, we really want to lift this into Aff
        cp <- CP.spawn "npm.cmd" ["run", testCommand]
            { cwd : Just cwd
            , detached: false
            , env: Nothing
            , gid: Nothing
            , stdio: CP.inherit
            , uid: Nothing
            }
        CP.onExit cp 
                (\exit -> do 
                    log "exiting"
                    case exit of 
                        CP.Normally 0 -> do
                            emit $ Right true
                        CP.Normally x -> do 
                            log $ "normally " <> show x
                            emit $ Right false
                        CP.BySignal sig -> do 
                            log "by signal"
                            emit $ Right false
                )
        CP.onError cp 
            (\error -> do 
                log "erroring"
                log $ show error
                emit (Right false)
            )

        pure Aff.nonCanceler
    )


getCwd :: Effect FilePath
getCwd = Path.resolve [__dirname] "../../test/compiler-gen-inputs"

waitForPrettier :: Aff.Aff Unit
waitForPrettier = Aff.makeAff 
    (\emit -> do
        cwd <- getCwd
        cp <- CP.spawn "npm.cmd" ["run", "pretty"]
            { cwd : Just cwd
            , detached: false
            , env: Nothing
            , gid: Nothing
            , stdio: CP.ignore
            , uid: Nothing
            }
        CP.onExit cp 
                (\exit -> do 
                    log "exiting"
                    emit $ Right unit
                )
        CP.onError cp 
            (\error -> do 
                log "erroring"
                emit $ Right unit
            )
        pure Aff.nonCanceler
    )