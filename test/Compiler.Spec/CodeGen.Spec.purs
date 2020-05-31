module Compiler.CodeGen.Spec where

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



genSpec :: Spec Unit
genSpec = describe "Generated code performs as expected" do
    pending "ALL THE CODEGEN SPEC TESTS, AND THEN SOME EXTRA"



compileLexer :: String -> String -> Aff.Aff Unit
compileLexer prog fileName = do
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