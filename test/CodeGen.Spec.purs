module Test.CodeGenSpec where


import Prelude

import CodeGen (generate, GenAST(..))
import Data.Array.NonEmpty (appendArray, singleton)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (thaw)
import Node.Buffer as Buf
import Node.Buffer.Class (class MutableBuffer)
import Node.Buffer.Immutable as ImmBuf
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Globals (__dirname)
import Node.Path as Path
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
        let program = Program []
            generated = generate program :: String
            _ = unsafePerformEffect $ do
                    file <- Path.resolve [__dirname] "../../test/code-gen-inputs/emptySpec.js"
                    exists <- FS.exists file 
                    if exists
                    then FS.truncate file 0
                    else FS.writeTextFile UTF8 file ""
                    FS.writeTextFile UTF8 file generated
                    pure unit
            result = unsafePerformEffect $ do 
                    cwd <- Path.resove [__dirname] "../../test/code-gen-inputs"
                    -- This should cause the tests to run
                    cp <- CP.spawn "npm run test" []
                        { cwd : cwd
                        , detached: false
                        , env: Nothing
                        , gid: Nothing
                        , stdio: CP.inherit
                        , uid: Nothing
                        }
                    CP.onExit cp 
                        (\exit -> do 
                            case exit of 
                                CP.Normally _ -> pure unit
                                CP.BySignal sig -> 
                        )
            -- Is there a CSP channel implementation? I'd like to block here until the child process is done running.
            -- probably have to ask freenode. Or is the Aff monad what I need?
        result `shouldEqual` Right unit
        pure unit
