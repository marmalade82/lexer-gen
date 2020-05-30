
module Test.ParserSpec where 

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (appendArray, singleton)
import Data.Maybe (Maybe(..))
import Parser (AST(..), ParseResult, Token, TokenType(..), parse)
import Test.Spec (Spec, describe, describeOnly, it, pending)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec = describeOnly "Parsing" do 
--spec = describe "Parsing" do 
    headerSpec 
    normalSectionSpec
    errorSectionSpec
    combinedSectionSpec
    defaultSectionSpec
    errorSpec
    stringSpec

headerSpec :: Spec Unit
headerSpec = describe "Headers" do 
    parseSpec
    astSpec

    where 
        astSpec :: Spec Unit
        astSpec = describe "ast" do
            it "Normal header" do 
                let tokens = makeBasicToken <$>  
                        singleton NormalHeader `appendArray` [ EOF ]
                let result = asTestString $ parse tokens 
                result `shouldEqual` "p"
            it "Error header" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, EOF]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p"
            it "Default header" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ DefaultHeader, EOF ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p"
            it "All headers" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader, EOF ] 
                let result = asTestString $ parse tokens
                result `shouldEqual` "p"
        parseSpec :: Spec Unit
        parseSpec = describe "parses" do
            it "Normal header" do 
                let tokens = makeBasicToken <$>  
                        singleton NormalHeader `appendArray` [ EOF ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "Error header" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, EOF ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "Default header" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ DefaultHeader, EOF ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "All headers" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader, EOF ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true



normalSectionSpec :: Spec Unit
normalSectionSpec = describe "Normal section" do 
    parseSpec
    astSpec

    where 
        parseSpec :: Spec Unit
        parseSpec = describe "parses" do
            it "One spec" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ Name, Regex, Terminator, EOF ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "Two specs" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ Name, Regex, Terminator, Name, Regex, Terminator, EOF ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true

        astSpec :: Spec Unit
        astSpec = describe "ast" do
            it "One spec" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ Name, Regex, Terminator, EOF ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh,ns,n-r"
            it "Two specs" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ Name, Regex, Terminator, Name, Regex, Terminator, EOF ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh,ns,n-r!ns,n-r"

errorSectionSpec :: Spec Unit
errorSectionSpec = describe "Error section" do 
    parseSpec
    astSpec

    where 
        parseSpec :: Spec Unit
        parseSpec = describe "parses" do
            it "One spec" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, Name, ErrorMessage, Name, Terminator, EOF]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "Two specs" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader 
                            , Name, ErrorMessage, Name, Terminator
                            , Name, ErrorMessage, Terminator, EOF
                            ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true

        astSpec :: Spec Unit
        astSpec = describe "ast" do
            it "One spec" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, Name, ErrorMessage, Name, Terminator, EOF]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,eh,es,n-em-n"
            it "Two specs" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader 
                            , Name, ErrorMessage, Name, Terminator
                            , Name, ErrorMessage, Terminator, EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,eh,es,n-em-n!es,n-em"

defaultSectionSpec :: Spec Unit
defaultSectionSpec = describe "Default section" do 
    parseSpec
    astSpec

    where 
        parseSpec :: Spec Unit
        parseSpec = describe "parses" do
            it "One spec without sync" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader
                            , Default, ErrorMessage, Terminator, EOF
                            ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "One spec with sync" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader
                            , Default, ErrorMessage, Name, Terminator, EOF
                            ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true

        astSpec :: Spec Unit
        astSpec = describe "ast" do
            it "One spec without sync" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader
                            , Default, ErrorMessage, Terminator, EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,dh,de,em"
            it "One spec with sync" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader
                            , Default, ErrorMessage, Name, Terminator, EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,dh,de,em-n"

combinedSectionSpec :: Spec Unit
combinedSectionSpec = describe "Combined sections are parsed correctly" do 
    parseSpec
    astSpec

    where
        parseSpec :: Spec Unit
        parseSpec = describe "parse" do
            it "Normal and error" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [   Name, Regex, Terminator
                            , ErrorHeader
                            ,   Name, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "Normal and default" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [   Name, Regex, Terminator
                            , DefaultHeader
                            ,   Default, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "Error and default" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [ ErrorHeader
                            ,   Name, ErrorMessage, Terminator
                            , DefaultHeader
                            ,   Default, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true
            it "Normal, error default" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [   Name, Regex, Terminator
                            , ErrorHeader
                            ,   Name, ErrorMessage, Terminator
                            , DefaultHeader
                            ,   Default, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = parse tokens
                asErrors result `shouldEqual` []
                asSuccess result `shouldEqual` true

        astSpec :: Spec Unit
        astSpec = describe "ast" do 
            it "Normal and error" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [   Name, Regex, Terminator
                            , ErrorHeader
                            ,   Name, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh,ns,n-r!!eh,es,n-em"
            it "Normal and default" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [   Name, Regex, Terminator
                            , DefaultHeader
                            ,   Default, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh,ns,n-r!!dh,de,em"
            it "Error and default" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [ ErrorHeader
                            ,   Name, ErrorMessage, Terminator
                            , DefaultHeader
                            ,   Default, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,eh,es,n-em!!dh,de,em"
            it "Normal, error default" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray` 
                            [   Name, Regex, Terminator
                            , ErrorHeader
                            ,   Name, ErrorMessage, Terminator
                            , DefaultHeader
                            ,   Default, ErrorMessage, Terminator
                            , EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh,ns,n-r!!eh,es,n-em!!dh,de,em"

errorSpec :: Spec Unit
errorSpec = describe "Error messages are reported" do
    it "Error messages must also be returned from the parse" do
        let tokens = makeBasicToken <$>
                singleton NormalHeader `appendArray` 
                    [ Name, Regex
                    , EOF
                    ]
            result = parse tokens
        result.success `shouldEqual` false
        Array.length result.errors `shouldEqual` 1
    pending "Parse attempts to recover by discarding tokens until it can sync"
    pending "Parse parses with warnings when possible"

makeBasicToken :: TokenType -> Token
makeBasicToken t = 
    { type: t
    , lexeme: ""
    , line: 0
    , column: 0
    }

type TestStringResult = 
    { str :: String
    , depth :: Int
    }

asTestString :: ParseResult -> String
asTestString result = case result.tree of 
    Nothing -> "NO TREE FOUND: " <> show result.errors
    Just ast -> show ast


asSuccess :: ParseResult -> Boolean
asSuccess res = res.success

asErrors :: ParseResult -> Array String
asErrors res = map errors res.errors
    where errors = \x -> x.message


stringSpec :: Spec Unit
stringSpec = do 
    describe "Testing stringable" do 
        it "Strings headers" do 
            let ast = NProgram (Just $ NNormalSpecs []) (Just $ NErrorSpecs []) (Just $ NDefaultSpecs [])
            show ast `shouldEqual` ("p,nh-eh-dh")