module Test.CodeGenSpec where


import Prelude

import CodeGen (generate, GenAST(..))
import Data.Array.NonEmpty (appendArray, singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (thaw)
import Node.Buffer as Buf
import Node.Buffer.Class (class MutableBuffer)
import Node.Buffer.Immutable as ImmBuf
import Node.ChildProcess (ChildProcess)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Globals (__dirname)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Node.Stream as Stream
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

{- Tests for this module need to generate a test tokenizing module in ES6,
    write it to a file called "lexer.js" in code-gen-inputs/, and then start the jest 
    tests in that folder, and report the results of the jest tests
-}
spec :: Spec Unit
spec = describe "Code generation" do 
    emptySpec
    pure unit

emptySpec :: Spec Unit
emptySpec = describe "Without any token definitions" do 
    it "Correct errors are generated" do 
        _ <- generateProgram
        result <- waitForChildProcess
        result `shouldEqual` true

generateProgram :: Aff.Aff Unit
generateProgram = do
    liftEffect $ do
        let program = Program []
            generated = generate program :: String
        file <- Path.resolve [__dirname] "../../test/code-gen-inputs/emptySpec.js"
        exists <- FS.exists file 
        if exists
        then FS.truncate file 0
        else FS.writeTextFile UTF8 file ""
        FS.writeTextFile UTF8 file generated
        pure unit
    waitForPrettier

getCwd :: Effect FilePath
getCwd = Path.resolve [__dirname] "../../test/code-gen-inputs"

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

waitForChildProcess :: Aff.Aff Boolean
waitForChildProcess = Aff.makeAff 
    (\emit -> do
        cwd <- getCwd
        liftEffect $ log cwd
        -- This should cause the tests to run
        -- Since this will run asynchronously, we really want to lift this into Aff
        cp <- CP.spawn "npm.cmd" ["run", "test"]
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