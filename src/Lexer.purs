module Lexer where 

import Prelude

import Data.Foldable (foldr)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, appendArray, singleton, head)
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

type MatchResult = 
    { type :: TokenType
    , result :: Either String RegexResult
    }

lex :: String -> Array Token
lex str = 
    let 
        doMatch :: RegexPair -> String -> MatchResult
        doMatch pair s = 
            let m :: String -> Regex -> RegexResult 
                m = flip match

                regex :: Either String Regex
                regex = pair.regex
            in 
                { type: pair.type
                , result: (m s) <$> (regex)
                }

        matchers :: NonEmptyArray (String -> MatchResult)
        matchers = (map) doMatch allRegex

        resultAll :: NonEmptyArray (MatchResult)
        resultAll = matchers <*> pure str

        possible :: Array Token
        possible = possibleTokens resultAll

        bestResult :: Maybe Token
        bestResult = chooseBest possible

    in  case bestResult of 
            Nothing -> []
            Just best -> [best]

type RegexResult = Maybe(NonEmptyArray(Maybe String))

possibleTokens :: NonEmptyArray MatchResult -> Array Token
possibleTokens xs =
    let acc :: MatchResult -> Array Token -> Array Token
        acc = \match -> \arr -> 
                case matchToToken match of 
                    Nothing -> arr
                    Just tok -> Array.cons tok arr
    
        possible :: Array ( Token )        
        possible = foldr acc [] xs
    in  if Array.null possible
        then let fail =
                    { type: FAIL
                    , lexeme: ""
                    , line: 0
                    , column: 0
                    } 
             in  [ fail ]
        else possible

    where 
        matchToToken :: MatchResult -> Maybe Token
        matchToToken { type: t, result: r } = case r of 
            Left _ -> Nothing
            Right result -> resultToToken result t
        resultToToken :: RegexResult -> TokenType -> Maybe Token
        resultToToken r t = 
            case r of 
                Nothing -> Nothing
                Just rs -> case head rs of 
                    Nothing -> Nothing
                    Just lexeme ->
                        let token = 
                                { type: t
                                , lexeme: lexeme
                                , line: 0
                                , column: 0
                                }
                        in Just token

-- TODO need to write algorithm for choosing best token possible
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


type RegexPair = 
    { type :: TokenType
    , regex :: Either String Regex
    }

allRegex :: NonEmptyArray RegexPair
allRegex =
    let normal = { type: NormalHeader, regex: normalHeader }
        error = { type: ErrorHeader, regex: errorHeader }
        default = { type: DefaultHeader, regex: defaultHeader }
        errorM = { type: ErrorMessage, regex: errorMessage }
        regexTok = { type: Regex, regex: regexT }
        terminatorTok = { type : Terminator, regex: terminator }
        nameTok = { type: Name, regex: name }
    in 
        singleton normal `appendArray` 
                [ error
                , default
                , errorM
                , regexTok
                , terminatorTok
                , nameTok
                ]

    where 
        normalHeader :: Either String Regex
        normalHeader = regex "^%%_normal_%%" noFlags

        errorHeader :: Either String Regex
        errorHeader = regex "^%%_error_%%" noFlags

        defaultHeader :: Either String Regex
        defaultHeader = regex "^%%_default_%%" noFlags

        errorMessage :: Either String Regex
        errorMessage = regex "^\".*\"" noFlags

        regexT :: Either String Regex
        regexT = regex "^\\(.+\\)" noFlags

        terminator :: Either String Regex
        terminator = regex "^\\;" noFlags 

        name :: Either String Regex
        name = regex "^[\\-\\_\\w]+" noFlags