module Test.TokenSpec where 

import Prelude

import Lexer (Token, TokenType(..), lex)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Test tokenization of file" do
            individualTokenSpec
            individualLexemeSpec
            lineCaptureSpec
            columnCaptureSpec
            multipleTokenSpec

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
    it "whitespace is NOT captured" do 
        let result = extractLexeme <$> lex "  \n"
        result `shouldEqual` []

lineCaptureSpec :: Spec Unit
lineCaptureSpec = describe "line capture" do 
    describe "one token per line" do 
        it "one line" do 
            let result = extractLine <$> lex "%%_normal_%%"
            result `shouldEqual` [ 0 ]
        it "two lines" do 
            let result = extractLine <$> lex "%%_normal%% \n %%_error_%%"
            result `shouldEqual` [ 0, 1]
    describe "two tokens per line" do 
        it "one line" do 
            let result = extractLine <$> lex "%%_normal_%% %%_normal_%%"
            result `shouldEqual` [ 0, 0 ]
        it "two lines" do 
            let result = extractLine <$> lex "%%_normal_%% %%_normal_%% \n %%_error_%% %%_error_%% "
            result `shouldEqual` [ 0, 0, 1, 1 ]

columnCaptureSpec :: Spec Unit
columnCaptureSpec = describe "column capture" do 
    describe "one token per line" do 
        it "one line" do 
            let result = extractColumn <$> lex "%%_normal_%%"
            result `shouldEqual` [ 0 ]
        it "two lines" do 
            let result = extractColumn <$> lex "%%_normal%% \n %%_error_%%"
            result `shouldEqual` [ 0, 1]
    describe "two tokens per line" do 
        it "one line" do 
            let result = extractColumn <$> lex "%%_normal_%% %%_normal_%%"
            result `shouldEqual` [ 0, 13 ]
        it "two lines" do 
            let result = extractColumn <$> lex "%%_normal_%% %%_normal_%% \n %%_error_%% %%_error_%% "
            result `shouldEqual` [ 0, 13, 1, 13 ]

multipleTokenSpec :: Spec Unit
multipleTokenSpec = describe "multiple tokens" do 
    pending "all tokens but failure"
    pending "failure within tokens"


extractType :: Token -> TokenType
extractType t = t.type

extractLexeme :: Token -> String
extractLexeme t = t.lexeme

extractLine :: Token -> Int
extractLine t = t.line

extractColumn :: Token -> Int
extractColumn t = t.column