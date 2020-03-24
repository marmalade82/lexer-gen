module Test.TokenSpec where 

import Prelude

import Lexer (TokenType(..), lex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Test tokenization of file" do
    it "normal header" do 
        let result = (\x -> x.type) <$> lex "%%_normal_%%"  
        result `shouldEqual` [ NormalHeader ]
    it "error header" do 
        let result = (\x -> x.type) <$> lex "%%_error_%%"
        result `shouldEqual` [ ErrorHeader ]