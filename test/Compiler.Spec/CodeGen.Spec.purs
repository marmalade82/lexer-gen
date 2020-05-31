module Compiler.CodeGen.Spec where

import Prelude

import Compiler (CompileResult, compile, Errors)
import Data.Array as Array
import Data.Either (isLeft, Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Globals (__dirname)
import Node.Path (FilePath)
import Node.Path as Path
--import SideEffect.Log (sideEffectLog)
import Test.Spec (class Example, Spec, SpecT(..), after_, describe, describeOnly, it, itOnly, pending)
import Test.Spec.Assertions (fail, shouldEqual)



genSpec :: Spec Unit
genSpec = after_ waitForPrettier $ describe "Generated code performs as expected" do
    -- TODO : Remove parentheses and quotes while lexing
    emptySpec
    oneTokenSpec
    twoTokenSpec
    defaultErrorSpec
    customErrorsSpec

emptySpec :: Spec Unit
emptySpec = describe "Without any token definitions" do 
    it "Tests succeed" do 
        let program = "%%_normal_%%"
        _ <- tryCompile program "emptySpec.js"
        result <- runTest "test-empty"
        result `shouldEqual` true

oneTokenSpec :: Spec Unit
oneTokenSpec = describe "With one token defined" do 
    it "Tests succeed" do 
        let program = Str.joinWith "\n" 
                [ "%%_normal_%%"
                , "me (I);"
                ]
        _ <- tryCompile program "oneTokenSpec.js"
        result <- runTest "test-one"
        result `shouldEqual` true

twoTokenSpec :: Spec Unit
twoTokenSpec = describe "With two tokens defined" do 
    it "Tests succeed" do 
        let program = Str.joinWith "\n"
                [ "%%_normal_%%"
                , "me (I);"
                , "verb (am);"
                ]
        _ <- tryCompile program "twoTokenSpec.js"
        result <- runTest "test-two"
        result `shouldEqual` true

defaultErrorSpec :: Spec Unit
defaultErrorSpec = describe "Replace default error" do 
    it "With sync" do 
        let program = Str.joinWith "\n" 
                [ "%%_normal_%%"
                , "space ( );"
                , "%%_default_%%"
                , "$Default \"I am an error\" space;"
                ]
        _ <- tryCompile program "defaultErrorSyncSpec.js"
        result <- runTest "test-default-sync"
        result `shouldEqual` true
    it "Without sync" do 
        let program = Str.joinWith "\n"
                [ "%%_normal_%%"
                , "%%_default_%%"
                , "$Default \"I am an error\";"
                ]
        _ <- tryCompile program "defaultErrorNoSyncSpec.js"
        result <- runTest "test-default-nosync"
        result `shouldEqual` true

customErrorsSpec :: Spec Unit
customErrorsSpec = describe "Custom errors" do
  it "With sync" do
    let program = Str.joinWith "\n"
            [ "%%_normal_%%"
            , "me (I);"
            , "verb (am);"
            , "space ( );"
            , "%%_error_%%"
            , "me \"Error 1\" space;"
            , "verb \"Error 2\" space;"
            ]
    _ <- tryCompile program "customErrorSyncSpec.js"
    result <- runTest "test-custom-sync"
    result `shouldEqual` true
  it "Without sync" do 
    let program = Str.joinWith "\n"
            [ "%%_normal_%%"
            , "me (I);"
            , "verb (am);"
            , "space ( );"
            , "%%_error_%%"
            , "me \"Error 1\";"
            , "verb \"Error 2\";"
            ]
    _ <- tryCompile program "customErrorNoSyncSpec.js"
    result <- runTest "test-custom-nosync"
    result `shouldEqual` true

tryCompile prog file = case compile prog of 
    Left errors -> fail $ "Compilation was unsuccessful: " <> show errors
    Right lexer -> writeLexer lexer file

writeLexer :: String -> String -> Aff.Aff Unit
writeLexer generated fileName = do
    liftEffect $ do
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
getCwd = Path.resolve [__dirname] "../../test/compiler-inputs"

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