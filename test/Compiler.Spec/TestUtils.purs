module Compiler.TestUtils where

import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as Str
import Data.Traversable (sequence)
import Compiler (CompileResult, compile, Errors)
import Prelude
import Data.Either (isLeft, Either(..))
import Test.Spec (class Example, Spec, SpecT(..), describe, describeOnly, it, itOnly, pending)
import Test.Spec.Assertions (fail, shouldEqual)

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