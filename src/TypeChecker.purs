module TypeChecker where

import Prelude

import Control.Monad.State (evalState, get)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (foldr, sequence)
import FirstPass (firstPass, nameExists)
import TypeChecker.Internals (Context, TypeState)
import Types (GenAST(..))

{- This module scans the generated AST and ensures that everything is correctly typed.
    The typing rules are simple:
        - Each NormalSpec defines a type of token
        - Two NormalSpecs cannot define a token with the same name
        - A NormalSpec cannot use a reserved token type, like the default token
        - Because ErrorSpec references one or more token types, each token type must have been
            defined in the NormalSpecs
        - Two ErrorSpecs cannot utilize tokens with the same name
-}

type Errors = Array String
type CheckResults = 
    { errors :: Errors

    }

initialContext :: Context
initialContext = 
    { 
      tokenTypes: []
    , errorTypes: []
    }


typecheck :: GenAST -> CheckResults
typecheck ast = evalState (doTypeCheck ast) initialContext

doTypeCheck :: GenAST -> TypeState CheckResults
doTypeCheck ast = do 
    -- we type check in two passes. 
    -- The first pass records known first instances of token declarations and error declarations
    -- The second pass checks that each actual token declaration and error declaration is valid.
    -- Another way to do this would be in one pass, recording known declarations along the way (in order)
    firstPass ast
    checkRules emptyResults ast

checkRules :: CheckResults -> GenAST -> TypeState CheckResults
checkRules results ast = case ast of
    Program arr -> do 
        res <- sequence (checkRules results <$> arr) :: TypeState (Array CheckResults)
        pure $ joinResults res
    NormalSpecs arr -> do 
        res <- sequence (checkRules results <$> arr) :: TypeState (Array CheckResults)
        pure $ joinResults res
    ErrorSpecs arr -> do 
        res <- sequence (checkRules results <$> arr) :: TypeState (Array CheckResults)
        pure $ joinResults res
    DefaultSpecs arr -> do -- default specs treats its children as a sequence, and that's all.
        res <- sequence (checkRules results <$> arr) :: TypeState (Array CheckResults)
        pure $ joinResults res
    NormalSpec arr -> do
        unique <- sequence (checkUniqueToken <$> arr) :: TypeState (Array CheckResults)
        reserved <- sequence (checkReserved <$> arr) :: TypeState (Array CheckResults)
        pure $ (joinResults $ unique <> reserved)
    ErrorSpec arr -> do 
        usage <- sequence (checkErrorTokenUsage <$> arr) :: TypeState (Array CheckResults)
        unique <- case Array.head arr of 
            Nothing -> pure emptyResults
            Just ast_ -> checkUniqueErrors ast_
        pure $ (joinResults $ [unique] <> usage)
    _ -> pure results
    where
        joinResults :: Array CheckResults -> CheckResults
        joinResults arr = foldr join { errors : [] } arr

        join :: CheckResults -> CheckResults -> CheckResults
        join r1 r2 =
            { errors: r1.errors <> r2.errors

            }

emptyResults :: CheckResults
emptyResults = { errors: [] }

checkUniqueToken :: GenAST -> TypeState CheckResults
checkUniqueToken (Name tok) = do
    ctx <- get
    let tokens = ctx.tokenTypes
    if nameExists tok tokens
    then
        pure emptyResults
    else do 
        let error = tok.lexeme <> "has already been used"
        pure { errors: [error] }
checkUniqueToken _ = pure emptyResults

checkReserved :: GenAST -> TypeState CheckResults
checkReserved (Name tok) = do
    if tok.lexeme == "$Default"
    then do 
        let error = "Cannot use reserved token " <> tok.lexeme
        pure { errors: [error]}
    else 
        pure emptyResults
checkReserved _ = pure emptyResults
    
-- Checks that an errormessage's use of token names have been defined before use
checkErrorTokenUsage :: GenAST -> TypeState CheckResults
checkErrorTokenUsage (Name tok) = do
    ctx <- get
    let tokens = ctx.tokenTypes
    if nameExists tok tokens
    then pure emptyResults
    else do 
        let error = tok.lexeme <> " was not defined as a token"
        pure { errors: [error]}
checkErrorTokenUsage _ = pure emptyResults

checkUniqueErrors :: GenAST -> TypeState CheckResults
checkUniqueErrors (Name tok) = do
    ctx <- get
    let errors = ctx.errorTypes
    if nameExists tok errors
    then 
        pure emptyResults -- this error was the defining error
    else do
        let error = tok.lexeme <> " has already been used to define an error"
        pure { errors: [error] }
checkUniqueErrors _ = pure emptyResults

