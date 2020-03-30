
module Test.ParserSpec where 

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (appendArray, singleton)
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Traversable (foldr, foldl)
import Math (max)
import Parser (AST(..), ParseResult, Token, TokenType(..), parse)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)


spec :: Spec Unit
spec = describe "Parsing" do 
    headerSpec 
    normalSectionSpec
    errorSectionSpec
    defaultSectionSpec
    --errorSpec

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
                result `shouldEqual` "p,nh"
            it "Error header" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, EOF]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh-eh"
            it "Default header" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ DefaultHeader, EOF ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh-eh"
            it "All headers" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader, EOF ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh-eh-dh"
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
    --astSpec

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
                result `shouldEqual` "p,nh,n-r"
            it "Two specs" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ Name, Regex, Terminator, Name, Regex, Terminator, EOF ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh,n-r!n-r"

errorSectionSpec :: Spec Unit
errorSectionSpec = describe "Error section" do 
    parseSpec
    --astSpec

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
                result `shouldEqual` "p,nh-eh,n-em-n"
            it "Two specs" do 
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader 
                            , Name, ErrorMessage, Name, Terminator
                            , Name, ErrorMessage, Terminator, EOF
                            ]
                let result = asTestString $ parse tokens
                result `shouldEqual` "p,nh-eh,es,n-em-n!es,n-em"

defaultSectionSpec :: Spec Unit
defaultSectionSpec = describe "Default section" do 
    parseSpec
    --astSpec

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
                result `shouldEqual` "p,nh-eh-dh,d-em"
            it "One spec with sync" do
                let tokens = makeBasicToken <$>
                        singleton NormalHeader `appendArray`
                            [ ErrorHeader, DefaultHeader
                            , Default, ErrorMessage, Name, Terminator, EOF
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

type TestStringResult = 
    { str :: String
    , depth :: Int
    }

asTestString :: ParseResult -> String
asTestString result = case result.tree of 
    Nothing -> ""
    Just ast -> (doString ast 0).str
    where doString :: AST -> Int -> TestStringResult
          doString ast depth = case ast of 
                NProgram n e d -> 
                    let nString :: TestStringResult
                        nString = doString n depth

                        eString :: TestStringResult
                        eString = maybeDoString e depth

                        dString :: TestStringResult
                        dString = maybeDoString d depth

                        children :: Array TestStringResult
                        children = [nString, eString, dString]

                        str = "p," <> (joinNonEmpty children)
                        de = (resultDepth children)
                    in  makeResult str de
                    
                NNormalSpecs arr -> 
                    let children :: Array TestStringResult
                        children = (flip doString $ depth) <$> arr

                        str = "nh," <> (joinNonEmpty children)
                        de = (resultDepth children)
                    in  makeResult str de
                NErrorSpecs arr ->
                    let children :: Array TestStringResult
                        children = (flip doString $ depth) <$> arr
                        
                        str = "eh," <> (joinNonEmpty children)
                        de = (resultDepth children)
                    in makeResult str de
                NDefaultSpecs arr ->
                    let children :: Array TestStringResult
                        children = (flip doString $ depth) <$> arr

                        str = "dh," <> (joinNonEmpty children)
                        de = (resultDepth children)
                    in makeResult str de
                _ -> makeResult "" 0
                where 
                    resultDepth :: Array TestStringResult -> Int
                    resultDepth children = -- we add 1, for the current level, to the depth of the child levels
                        maxDepth children + 1

                    makeResult :: String -> Int -> TestStringResult
                    makeResult s dep = 
                                { str : s
                                , depth: chooseLarger 0 dep
                                }

          maybeDoString :: Maybe AST -> Int -> TestStringResult
          maybeDoString m depth = case m of 
            Nothing ->  { str: ""
                        , depth: 0
                        }
            Just ast -> doString ast depth

          joinNonEmpty :: Array TestStringResult -> String
          joinNonEmpty arr = 
                    let withoutEmpty :: Array TestStringResult
                        withoutEmpty = Array.filter nonEmpty arr
                        
                        joined :: String
                        joined = foldl join "" withoutEmpty
                    in  joined
                    where 
                        nonEmpty :: TestStringResult -> Boolean
                        nonEmpty r = (Str.length r.str) > 0

                        join :: String -> TestStringResult -> String
                        join acc res = 
                            if Str.null acc 
                            then res.str
                            else 
                                let sep =   if res.depth < 2
                                            then "-"
                                            else "!"
                                in  acc <> sep <> res.str
          maxDepth :: Array TestStringResult -> Int
          maxDepth arr = 
                    let depths :: Array Int
                        depths = (\x -> x.depth) <$> arr

                        maximum :: Int
                        maximum = foldr chooseLarger 0 depths
                    in maximum
          chooseLarger :: Int -> Int -> Int
          chooseLarger l r = round $ max (toNumber l) (toNumber r)


asSuccess :: ParseResult -> Boolean
asSuccess res = res.success

asErrors :: ParseResult -> Array String
asErrors res = map errors res.errors
    where errors = \x -> x.message