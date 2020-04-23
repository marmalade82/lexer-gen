module CodeGen 



where

import Control.Monad.Maybe.Trans
import Control.Monad.State
import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (head, last, length, take, index)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Traversable (sequence)

{-  module for generating code from the AST
    What I've realized is that besides the actual parsing algorithm, very, very few of 
    the types should know anything about how the AST should be structured. This decouples
    the tree and code generation from the parsing algorithm itself.

    The code generator will *try* to turn a node and its children into code, but if it cannot,
    it will simply generate no code at all. This is different from a type checker, which analyzes the 
    AST and must report any errors.

    For this same reason, the type checker should not duplicate any effort of the parser -- its goal is 
    only to unify types.
-}

data GenAST 
    = Program (Array GenAST)
    | NormalSpecs (Array GenAST)
    | ErrorSpecs (Array GenAST)
    | DefaultSpecs (Array GenAST)
    | NormalSpec (Array GenAST)
    | ErrorSpec (Array GenAST)
    | DefaultError (Array GenAST)
    | Name Token
    | Regex Token
    | ErrorMessage Token

type Token = 
    { type :: TokenType
    , lexeme :: String
    , line :: Int
    , column :: Int
    }

data TokenType
    = N
    | R
    | EM
derive instance genericTokenType :: Generic TokenType _
instance showTokenType :: Show TokenType where show = genericShow
instance eqTokenType :: Eq TokenType where eq = genericEq


generate :: GenAST -> String
generate ast = evalState (doGenerate $ Just ast) ""

type CodeState a = State String a


-- generation should really lead to the emmision of strings to a file, line by line. Running
-- the file for its effects should determine whether the test passes or fails.
doGenerate :: Maybe GenAST -> CodeState String
doGenerate Nothing = do 
    pure ""
doGenerate (Just ast) = do 
    case ast of 
        Program arr ->
            let generated :: CodeState String
                generated = do 
                    matchers <- doGenerate $ head arr
                    errors <- doGenerate $ (eHead 2) arr
                    defaults <- doGenerate $ (eHead 3) arr
                    pure $ makeProgram matchers errors defaults
            in  generated
            where
                -- We also need to make sure that the function definitions make it into the final program.
                -- This might be a good place to use the State monad, since the state of the program so far is 
                -- a huge background part of the program
                makeProgram :: String -> String -> String -> String
                makeProgram matchers errors defaults = 
                    let prog = 
                            [ "const matchers = {};" 
                            , "const errors = {};"
                            , "\n"
                            , "export function lex(str) {"

                            , "};"
                            , "export default lex;"
                            ]
                    in  Str.joinWith "\n" prog 
        NormalSpecs arr ->
            let mapped :: (CodeState (Array String))
                mapped = sequence (doGenerate <$> (Just <$> arr))

                generated :: CodeState String
                generated = do 
                    arr_2 <- mapped
                    pure $ Str.joinWith "\n" arr_2
            in  generated
        ErrorSpecs arr ->
            let mapped :: (CodeState (Array String))
                mapped = sequence (doGenerate <$> (Just <$> arr))
            
                generated :: CodeState String
                generated = do 
                    arr_2 <- mapped
                    pure $ Str.joinWith "\n" arr_2
            in  generated
        DefaultSpecs arr -> -- default specs treats its children as a sequence, and that's all.
            let mapped :: (CodeState (Array String))
                mapped = sequence (doGenerate <$> (Just <$> arr))

                generated :: CodeState String
                generated = do 
                    arr_2 <- mapped
                    pure $ Str.joinWith "\n" arr_2
            in  generated
        NormalSpec arr -> 
            let generated :: CodeState String
                generated = do 
                    name <- generateName $ head arr
                    regex <- generateRegex $ (eHead 2) arr
                    makeMatcher name regex
            in  generated
        ErrorSpec arr -> 
            let generated :: CodeState String
                generated = do 
                    name <- generateName $ head arr
                    message <- generateMessage $ (eHead 2) arr
                    sync <- generateName $ (eHead 3) arr
                    makeError name message sync
            in  generated
        DefaultError arr -> 
            let generated :: CodeState String
                generated = do
                    message <- generateMessage $ head arr
                    sync <- generateName $ (eHead 2) arr
                    makeError "$default" message sync
            in  generated

        -- Leaf nodes lack context. Without context, there's no point to generating code.
        Name tok -> pure "" 
        Regex tok -> pure ""
        ErrorMessage tok -> pure ""

    where 
        generateName :: Maybe GenAST -> CodeState String
        generateName (Just (Name tok)) = pure ""
        generateName _ = pure ""

        generateRegex :: Maybe GenAST -> CodeState String
        generateRegex (Just (Regex tok)) = pure ""
        generateRegex _ = pure ""

        makeMatcher :: String -> String -> CodeState String
        makeMatcher name regex = pure ""

        generateMessage :: Maybe GenAST -> CodeState String
        generateMessage (Just (ErrorMessage tok)) = pure ""
        generateMessage _ = pure ""

        makeError :: String -> String -> String -> CodeState String
        makeError name message sync = pure ""

eHead :: forall a. Int -> Array a -> Maybe a
eHead = flip index