module Test.CompilerSpec where

import Prelude

import Compiler (CompileResult, compile, Errors)
import Compiler.CodeGen.Spec (genSpec)
import Compiler.Lexer.Spec (lexerSpec)
import Compiler.Parser.Spec (parserSpec)
import Compiler.TypeChecker.Spec (typecheckSpec)
import Test.Spec (class Example, Spec, SpecT(..), describe, describeOnly, it, itOnly, pending)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describeOnly "Integration test compiler" do
    lexerSpec
    parserSpec
    typecheckSpec
    genSpec
    pure unit

