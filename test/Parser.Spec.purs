
module Test.ParserSpec where 

import Prelude

import Data.Array.NonEmpty (appendArray, singleton)
import Parser (Token, TokenType(..), parse, AST)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec = describe "Parsing" do 
    headerSpec
    normalSectionSpec
    errorSectionSpec
    defaultSectionSpec
    errorSpec

headerSpec :: Spec Unit
headerSpec = describe "Headers" do 
    it "Normal header" do 
        let tokens = makeBasicToken <$>  
                singleton NormalHeader
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh"
    it "Error header" do 
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ ErrorHeader]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh-eh"
    it "Default header" do
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ DefaultHeader ]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh-eh"


normalSectionSpec :: Spec Unit
normalSectionSpec = describe "Normal section" do 
    it "One spec" do
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ Name, Regex, Terminator ]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh,n-r"
    it "Two specs" do
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ Name, Regex, Terminator, Name, Regex, Terminator ]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh,n-r!n-r"

errorSectionSpec :: Spec Unit
errorSectionSpec = describe "Error section" do 
    it "One spec" do 
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ ErrorHeader, Name, ErrorMessage, Name, Terminator]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh-eh,n-em-n"
    it "Two specs" do 
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ ErrorHeader 
                    , Name, ErrorMessage, Name, Terminator
                    , Name, ErrorMessage, Terminator
                    ]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh-eh,n-em-n!n-em"

defaultSectionSpec :: Spec Unit
defaultSectionSpec = describe "Default section" do 
    it "One spec without sync" do
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ ErrorHeader, DefaultHeader
                    , Default, ErrorMessage, Terminator
                    ]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh-eh-dh,d-em"
    it "One spec with sync" do
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray`
                    [ ErrorHeader, DefaultHeader
                    , Default, ErrorMessage, Name, Terminator
                    ]
        let result = asTestString $ parse tokens
        result `shouldEqual` "p,nh-eh-dh,d-em-n"

errorSpec :: Spec Unit
errorSpec = describe "Error messages are reported" do
    pending "Error messages must also be returned from the parse"
    pending "Parse attempts to recover by discarding tokens until it can sync"
    pending "Parse parses with warnings when possible"

makeBasicToken :: TokenType -> Token
makeBasicToken t = 
    { type: t
    , lexeme: ""
    , line: 0
    , column: 0
    }

asTestString :: AST -> String
asTestString ast = ""