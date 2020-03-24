module Test.TokenSpec where 

import Prelude

import Lexer (Token, TokenType(..), lex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Test tokenization of file" do
            individualTokenSpec
            individualLexemeSpec

individualTokenSpec :: Spec Unit
individualTokenSpec = describe "individual tokens" do 
    it "normal header" do 
        let result = extractType <$> lex "%%_normal_%%"  
        result `shouldEqual` [ NormalHeader ]
    it "error header" do 
        let result = extractType <$> lex "%%_error_%%"
        result `shouldEqual` [ ErrorHeader ]
    it "default header" do 
        let result = extractType <$> lex "%%_default_%%"
        result `shouldEqual` [ DefaultHeader ]
    it "error message" do 
        let result = extractType <$> lex "\"i am an error\""
        result `shouldEqual` [ ErrorMessage ]
    it "regex" do 
        let result = extractType <$> lex "(i am a regex)"
        result `shouldEqual` [ Regex ]
    it "terminator" do 
        let result = extractType <$> lex ";"
        result `shouldEqual` [ Terminator ]
    it "name" do 
        let result = extractType <$> lex "-_0123hello"
        result `shouldEqual` [ Name ]
    it "failure" do 
        let result = extractType <$> lex "////"
        result `shouldEqual` [ FAIL ]

individualLexemeSpec :: Spec Unit 
individualLexemeSpec = describe "individual lexemes" do 
    it "normal header" do 
        let result = extractLexeme <$> lex "%%_normal_%%"  
        result `shouldEqual` ["%%_normal_%%"]
    it "error header" do 
        let result = extractLexeme <$> lex "%%_error_%%"
        result `shouldEqual` ["%%_error_%%"]
    it "default header" do 
        let result = extractLexeme <$> lex "%%_default_%%"
        result `shouldEqual` ["%%_default_%%"]
    it "error message" do 
        let result = extractLexeme <$> lex "\"i am an error\""
        result `shouldEqual` ["\"i am an error\""]
    it "regex" do 
        let result = extractLexeme <$> lex "(i am a regex)"
        result `shouldEqual` ["(i am a regex)"]
    it "terminator" do 
        let result = extractLexeme <$> lex ";"
        result `shouldEqual` [ ";" ]
    it "name" do 
        let result = extractLexeme <$> lex "-_0123hello"
        result `shouldEqual` [ "-_0123hello" ]
    it "failure" do 
        let result = extractLexeme <$> lex "////"
        result `shouldEqual` [ "////" ]

extractType :: Token -> TokenType
extractType t = t.type

extractLexeme :: Token -> String
extractLexeme t = t.lexeme