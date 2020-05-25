module Test.CodeGenSpec where


import Prelude

import CodeGen (GenAST(..), TokenType(..), generate)
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
import Test.Spec (Spec, after_, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

{- Tests for this module need to generate a test tokenizing module in ES6,
    write it to a file called "lexer.js" in code-gen-inputs/, and then start the jest 
    tests in that folder, and report the results of the jest tests
-}
spec :: Spec Unit
spec = after_ waitForPrettier $ describe "Code generation" do 
    emptySpec
    oneTokenSpec
    twoTokenSpec
    defaultErrorSpec
    customErrorsSpec
    pure unit

emptySpec :: Spec Unit
emptySpec = describe "Without any token definitions" do 
    it "Tests succeed" do 
        let program = Program []
        _ <- generateLexer program "emptySpec.js"
        result <- runTest "test-empty"
        result `shouldEqual` true

oneTokenSpec :: Spec Unit
oneTokenSpec = describe "With one token defined" do 
    it "Tests succeed" do 
        let program = Program 
                [ NormalSpecs 
                    [ NormalSpec
                        [ Name { type: N, lexeme: "me", line: 0, column: 0 }
                        , Regex { type: R, lexeme: "I", line: 3, column: 3 }
                        ]
                    ]
                ]
        _ <- generateLexer program "oneTokenSpec.js"
        result <- runTest "test-one"
        result `shouldEqual` true

twoTokenSpec :: Spec Unit
twoTokenSpec = describe "With two tokens defined" do 
    it "Tests succeed" do 
        let program = Program
                [ NormalSpecs 
                    [ NormalSpec
                        [ Name { type: N, lexeme: "me", line: 0, column: 0 }
                        , Regex { type: R, lexeme: "I", line: 3, column: 3 }
                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "verb", line: 0, column: 0 }
                        , Regex { type: R, lexeme: "am", line: 3, column: 3 }
                        ]
                    ]
                ]
        _ <- generateLexer program "twoTokenSpec.js"
        result <- runTest "test-two"
        result `shouldEqual` true

defaultErrorSpec :: Spec Unit
defaultErrorSpec = describe "Replace default error" do 
    it "With sync" do 
        let program = Program
                [ NormalSpecs 
                    [ NormalSpec
                        [ Name { type: N, lexeme: "space",  line: 0, column: 0 }
                        , Regex { type: R, lexeme: " ", line: 3, column: 3 }
                        ]

                    ]
                , DefaultSpecs
                    [ DefaultError
                        [ ErrorMessage { type: EM, lexeme: "I am an error", line: 0, column: 0}
                        , Name { type: N, lexeme: "space", line: 3, column: 3}
                        ]
                    ]
                ]
        _ <- generateLexer program "defaultErrorSyncSpec.js"
        result <- runTest "test-default-sync"
        result `shouldEqual` true
    it "Without sync" do 
        let program = Program
                [ DefaultSpecs
                    [ DefaultError
                        [ ErrorMessage { type: EM, lexeme: "I am an error", line: 0, column: 0}
                        ]
                    ]
                ]
        _ <- generateLexer program "defaultErrorNoSyncSpec.js"
        result <- runTest "test-default-nosync"
        result `shouldEqual` true

customErrorsSpec :: Spec Unit
customErrorsSpec = describe "Custom errors" do
  it "With sync" do
    let program = Program
            [ NormalSpecs
                [ NormalSpec
                    [ Name { type: N, lexeme: "me", line: 0, column: 0 }
                    , Regex { type: R, lexeme: "I", line: 3, column: 3 }
                    ]
                , NormalSpec
                    [ Name { type: N, lexeme: "verb", line: 0, column: 0 }
                    , Regex { type: R, lexeme: "am", line: 3, column: 3 }
                    ]
                , NormalSpec
                    [ Name { type: N, lexeme: "space", line: 0, column: 0 }
                    , Regex { type: R, lexeme: " ", line: 3, column: 3 }
                    ]
                ]
            , ErrorSpecs
                [ ErrorSpec
                    [ Name { type: N, lexeme: "me", line: 0, column: 0}
                    , ErrorMessage { type: EM, lexeme: "Error 1", line: 3, column: 3}
                    , Name { type: N, lexeme: "space", line: 3, column: 3}
                    ]
                , ErrorSpec
                    [ Name { type: N, lexeme: "verb", line: 0, column: 0}
                    , ErrorMessage { type: EM, lexeme: "Error 2", line: 3, column: 3}
                    , Name { type: N, lexeme: "space", line: 3, column: 3}
                    ]
                ]
            ]
    _ <- generateLexer program "customErrorSyncSpec.js"
    result <- runTest "test-custom-sync"
    result `shouldEqual` true
  it "Without sync" do 
    let program = Program
            [ NormalSpecs
                [ NormalSpec
                    [ Name { type: N, lexeme: "me", line: 0, column: 0 }
                    , Regex { type: R, lexeme: "I", line: 3, column: 3 }
                    ]
                , NormalSpec
                    [ Name { type: N, lexeme: "verb", line: 0, column: 0 }
                    , Regex { type: R, lexeme: "am", line: 3, column: 3 }
                    ]
                , NormalSpec
                    [ Name { type: N, lexeme: "space", line: 0, column: 0 }
                    , Regex { type: R, lexeme: " ", line: 3, column: 3 }
                    ]
                ]
            , ErrorSpecs
                [ ErrorSpec
                    [ Name { type: N, lexeme: "me", line: 0, column: 0}
                    , ErrorMessage { type: EM, lexeme: "Error 1", line: 3, column: 3}
                    ]
                , ErrorSpec
                    [ Name { type: N, lexeme: "verb", line: 0, column: 0}
                    , ErrorMessage { type: EM, lexeme: "Error 2", line: 3, column: 3}
                    ]
                ]
            ]
    _ <- generateLexer program "customErrorNoSyncSpec.js"
    result <- runTest "test-custom-nosync"
    result `shouldEqual` true


generateLexer :: GenAST -> String -> Aff.Aff Unit
generateLexer ast fileName = do
    liftEffect $ do
        let program = ast
            generated = generate program :: String
        file <- Path.resolve [__dirname] ("../../test/code-gen-inputs/" <> fileName)
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