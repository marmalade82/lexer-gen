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
        pure unit
