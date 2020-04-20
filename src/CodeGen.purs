module CodeGen 



where

import Prelude
import Data.Array (head)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.String as Str

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


-- generation should really lead to the emision of strings to a file, line by line. Running
-- the file for its effects should determine whether the test passes or fails.
generate :: GenAST -> String
generate ast = case ast of 
    Program arr ->
        let generated :: Either String String
            generated = do 
                matchers <- generate $ head arr
                errors <- generate $ (head <<< head) arr
                defaults <- generate $ (head <<< head <<< head) arr
                pure makeProgram matchers errors defaults
        in  case generated of 
                Left x -> ""
                Right x -> x
        where
            -- We also need to make sure that the function definitions make it into the final program.
            -- This might be a good place to use the State monad, since the state of the program so far is 
            -- a huge background part of the program
            makeProgram :: Array String -> Array String -> Array String -> String
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
        let generated :: Array String
            generated = generate <$> arr
        in  Str.joinWith "\n" generated
    ErrorSpecs arr ->
        let generated :: Array String
            generated = generate <$> arr
        in  Str.joinWith "\n" generated
    DefaultSpecs arr -> -- default specs treats its children as a sequence, and that's all.
        let generated :: Array String
            generated = generate <$> arr
        in  Str.joinWith "\n" generated
    NormalSpec arr -> 
        let generated :: Either String String
            generated = do 
                name <- generateName $ head arr
                regex <- generateRegex $ (head <<< head) arr
                pure $ makeMatcher name regex
        in  case generated of 
                Left x -> ""
                Right x -> x
    ErrorSpec arr -> 
        let generated :: Either String String 
            generated = do 
                name <- generateName $ head arr
                message <- generateMessage $ (head <<< head) arr
                sync <- generateName $ (head <<< head <<< head) arr
                pure $ makeError name message sync
        in  case generated of 
                Left x -> ""
                Right x -> x
    DefaultError arr -> 
        let generated :: Either String String
            generated = do
                message <- generateMessage $ head arr
                sync <- generateName $ (head <<< head) arr
                pure $ makeError "$default" message sync
        in  case generated of 
                Left x -> ""
                Right x -> x

    -- Leaf nodes lack context. Without context, there's no point to generating code.
    Name tok -> "" 
    Regex tok -> ""
    ErrorMessage tok -> ""