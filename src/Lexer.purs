module Lexer where 

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, head)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)

data TokenType 
    = NormalHeader
    | ErrorHeader
    | DefaultHeader
    | Regex
    | ErrorMessage
    | Terminator
    | Name
    | FAIL
derive instance genericTokenType :: Generic TokenType _
instance showTokenType :: Show TokenType where show = genericShow
instance eqTokenType :: Eq TokenType where eq = genericEq

type Token = 
    { type :: TokenType
    , lexeme :: String
    , line :: Int
    , column :: Int
    }


lex :: String -> Array Token
lex str = 
    let 
        matchers = (map <<< map) match
            [ normalHeader
            , errorHeader
            ]
        resultAll = (\x -> x <*> pure str) <$> matchers
        possible = possibleTokens resultAll
        bestResult = chooseBest possible
    in  case bestResult of 
            Nothing -> []
            Just best -> [best]
        --matchNormal = match <$> normalHeader 
        --result = matchNormal <*> pure str
    --in  case result of 
    --        Left _ -> []
    --        Right x -> generateToken x

type RegexResult = Maybe(NonEmptyArray(Maybe String))

possibleTokens :: Array (Either String RegexResult) -> Array Token
possibleTokens xs = []

chooseBest :: Array Token -> Maybe Token
chooseBest possible = Array.head possible

generateToken :: RegexResult -> Array Token
generateToken x = 
    case x of 
        Nothing -> []
        Just xs -> case head xs of 
            Nothing -> []
            Just lexeme -> 
                let token = 
                        { type: NormalHeader
                        , lexeme: lexeme
                        , line: 0
                        , column: 0
                        }
                in [token]


normalHeader :: Either String Regex
normalHeader = regex "^%%_normal_%%" noFlags

errorHeader :: Either String Regex
errorHeader = regex "^%%_error_%%" noFlags