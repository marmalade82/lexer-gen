module Compiler 
    ( compile
    , CompileResult
    , Errors
    , Lexer
    , exec
    ) where 

import Prelude

import CodeGen (GenAST, generate)
import CodeGen as CodeGen
import Data.Array (foldr)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), isLeft, isRight)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Lexer (Token, TokenType(..), lex)
import Lexer as Lexer
import Parser (AST(..), parse)
import Parser as Parser
import TypeChecker (noErrors, typecheck)


{- This module
    unifies the lexer, parser, type checker, and code generator into one whole
    program, returning either the compiled program or an array of the errors 
    for display to the user in whatever format the calling program desires
-}

type Output = 
    { errors :: Errors
    , lexer :: String
    }

-- This function takes the input lexing program and returns an object with any errors,
-- and the lexer if the compilation succeeded. Needed for an easy bridge to JavaScript
exec :: String -> Output
exec prog = do 
    case compile prog of 
        Left errors -> 
            { errors: errors
            , lexer: ""
            }
        Right lexer -> 
            { errors: []
            , lexer: lexer
            }

type Errors = Array String
type Lexer = String
type CompileResult = Either Errors Lexer

-- Performs compilation and generates either errors or the lexing program
compile :: String -> Either Errors Lexer
compile prog = do   
    parserTokens <- doLex prog
    tree <- doParse parserTokens
    checkedTree <- doTypeCheck tree
    lexer <- doGenerate checkedTree
    pure lexer
    where
        doLex :: String -> Either Errors (NonEmptyArray Parser.Token)
        doLex p = do
            let tokens = lex p
            findErrors tokens
            let parserTokens_ = NonEmptyArray.fromArray $ Array.snoc (convertForParser <$> tokens) Parser.eof
            case parserTokens_ of 
                Nothing -> Left ["Program is empty. Could not compile"]
                Just parserTokens -> do
                    pure parserTokens
        doParse :: (NonEmptyArray Parser.Token) -> Either Errors GenAST
        doParse tokens = do
            let results = parse tokens
            if results.success
            then 
                case results.tree of 
                    Nothing -> Left $ ["Error during parsing. No AST could be generated"] <> (Parser.toString <$> results.errors)
                    Just tree -> convertForCodeGen tree
            else do
                Left $ Parser.toString <$> results.errors
        doTypeCheck :: GenAST -> Either Errors GenAST
        doTypeCheck ast = do 
            let results = typecheck ast
            if noErrors results
            then pure $ ast
            else Left $ results.errors
        doGenerate :: GenAST -> Either Errors Lexer
        doGenerate ast = Right $ generate ast

findErrors :: Array Token -> Either Errors Unit
findErrors arr = do
    let failures = Array.filter (\tok -> tok.type == FAIL) arr
        errors = (errorMessage <$> failures) :: Array String
    if Array.null errors
    then pure unit
    else Left errors
    where 
        errorMessage :: Token -> String
        errorMessage tok = case tok.type of 
            FAIL -> "Error near " <> tok.lexeme <> " at " <> show tok.line <> ":" <> show tok.column
            _ -> ""

convertForParser :: Lexer.Token -> Parser.Token
convertForParser tok = 
    { lexeme: tok.lexeme
    , line: tok.line
    , column: tok.column
    , type: convertType tok.type
    }
    where 
        convertType :: Lexer.TokenType -> Parser.TokenType
        convertType t = case t of 
           Lexer.NormalHeader -> Parser.NormalHeader 
           Lexer.ErrorHeader -> Parser.ErrorHeader
           Lexer.DefaultHeader -> Parser.DefaultHeader
           Lexer.Regex -> Parser.Regex
           Lexer.ErrorMessage -> Parser.ErrorMessage
           Lexer.Name -> Parser.Name
           Lexer.Default -> Parser.Default
           Lexer.Terminator -> Parser.Terminator
           Lexer.WhiteSpace -> Parser.FAIL
           Lexer.FAIL -> Parser.FAIL

convertForCodeGen :: Parser.AST -> Either Errors GenAST
convertForCodeGen ast = case ast of 
    NProgram ast1 ast2 ast3 -> do 
        arr <- convertArrayForCodeGen [ast1, ast2, ast3]
        pure $ CodeGen.Program arr
    NNormalSpecs arr_ -> do 
        arr <- convertArrayForCodeGen (Just <$> arr_)  
        pure $ CodeGen.NormalSpecs arr
    NErrorSpecs arr_ -> do
        arr <- convertArrayForCodeGen (Just <$> arr_)
        pure $ CodeGen.ErrorSpecs arr
    NDefaultSpecs arr_ -> do 
        arr <- convertArrayForCodeGen (Just <$> arr_)
        pure $ CodeGen.DefaultSpecs arr
    NNormalSpec ast1 ast2 -> do 
        arr <- convertArrayForCodeGen [Just ast1, Just ast2] 
        pure $ CodeGen.NormalSpec arr
    NErrorSpec ast1 ast2 mast3 -> do 
        arr <- convertArrayForCodeGen [Just ast1, Just ast2, mast3] 
        pure $ CodeGen.ErrorSpec arr
    NDefaultError ast1 mast2 -> do 
        arr <- convertArrayForCodeGen  [Just ast1, mast2]
        pure $ CodeGen.DefaultError arr
    NRegex tok -> do 
        t <- convertToken tok
        pure $ CodeGen.Regex t
    NName tok -> do 
        t <- convertToken tok
        pure $ CodeGen.Name t
    NErrorMessage tok -> do 
        t <- convertToken tok
        pure $ CodeGen.ErrorMessage t
    where
        convertArrayForCodeGen :: Array (Maybe Parser.AST) -> Either Errors (Array GenAST)
        convertArrayForCodeGen asts =  do
            sequence $ foldr acc [] asts :: Either Errors (Array GenAST)
        acc :: Maybe Parser.AST -> Array (Either (Array String) GenAST) -> Array (Either (Array String) GenAST)
        acc maybe arr = case maybe of 
            Nothing -> arr
            Just tree -> case convertForCodeGen tree of 
                Left errors -> Array.cons (Left errors) arr
                Right ast_ -> Array.cons (Right ast_) arr
        convertToken :: Parser.Token -> Either Errors CodeGen.Token
        convertToken tok = case convertType tok.type of 
            Right t -> Right
                { lexeme: tok.lexeme
                , column: tok.column
                , line: tok.line
                , type: t
                }
            Left s -> Left s
        convertType :: Parser.TokenType -> Either Errors CodeGen.TokenType
        convertType t = case t of 
            Parser.Name -> Right CodeGen.N 
            Parser.Regex -> Right CodeGen.R
            Parser.ErrorMessage -> Right CodeGen.EM
            _ -> Left $ ["Encountered invalid token type during code generation: " <> show t]
        lefts :: forall a b. Array (Either a b) -> Array (Either a b)
        lefts = Array.filter isLeft

        rights :: forall a b. Array (Either a b) -> Array (Either a b)
        rights = Array.filter isRight

