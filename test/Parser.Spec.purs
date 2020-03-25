
module Test.ParserSpec where 

import Prelude

import Data.String as Str
import Lexer (Token, TokenType(..), lex)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec = pending "Test tokenization of string"