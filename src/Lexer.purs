module Lexer where 

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, appendArray, fromArray, head, singleton)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, Pattern(..), codePointFromChar, drop, length)
import Data.String as Str
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
    | WhiteSpace
    | Default
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

type RegexResult = Maybe(NonEmptyArray(Maybe String))

type RegexPair = 
    { type :: TokenType
    , regex :: Either String Regex
    }

type ColumnIndex = Int
type LineIndex = Int

lex :: String -> Array Token
lex str = 
    let tokens :: Array Token
        tokens = doLex str 0 0

        removeWhiteSpace :: Array Token -> Array Token
        removeWhiteSpace toks = Array.filter isNotWhitespace toks
            where isNotWhitespace = \x -> x.type /= WhiteSpace

    in  removeWhiteSpace tokens

doLex :: String -> ColumnIndex -> LineIndex -> Array Token
doLex str c l =
    let 
        matchers :: NonEmptyArray (String -> MatchResult)
        matchers = doMatch <$> all
            where   all :: NonEmptyArray RegexPair
                    all = allRegex
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

        matchResults :: NonEmptyArray (MatchResult)
        matchResults = matchers <*> pure s
            where s :: String
                  s = str

        possibleToks :: NonEmptyArray Token
        possibleToks = 
            let tokens = possibleTokens matchResults c l
            in  case fromArray tokens of 
                    Nothing -> 
                        let fail = 
                                { type: FAIL
                                , lexeme: str
                                , line: l
                                , column: c
                                }
                        in singleton fail
                    Just arr -> arr

        bestToken :: Token
        bestToken = chooseBest possibleToks

    in  if length str == 0 
        then []
        else 
            case bestToken.type of 
                FAIL -> [ bestToken ]
                x -> 
                    let resultLength :: Int
                        resultLength = length bestToken.lexeme
                
                        remaining :: String 
                        remaining = drop resultLength str

                        nextColumn :: ColumnIndex
                        nextColumn = 
                            if x /= WhiteSpace 
                            then c + resultLength
                            else newColumnCount bestToken.lexeme c

                        nextLine :: LineIndex
                        nextLine = 
                            if x /= WhiteSpace 
                            then l
                            else l + (newlineCount bestToken.lexeme)
                    in  Array.cons bestToken $ doLex remaining nextColumn nextLine
    where 
        newlineCount :: String -> Int
        newlineCount s = 
            let 
                newlines :: Array CodePoint
                newlines = Array.filter isNewline $ Str.toCodePointArray s
            in Array.length newlines
            where isNewline :: CodePoint -> Boolean
                  isNewline x = x == (codePointFromChar '\n')
        newColumnCount :: String -> ColumnIndex -> Int
        newColumnCount s original = 
            if not Str.contains (Pattern "\n") s
            then original + (length s)
            else 
                let beforeLastNewline :: Array CodePoint
                    beforeLastNewline = Array.takeWhile isNotNewline $ Array.reverse (Str.toCodePointArray s)

                    new :: ColumnIndex
                    new = Array.length beforeLastNewline
                in  new
            where isNotNewline :: CodePoint -> Boolean
                  isNotNewline x = x /= (codePointFromChar '\n')


possibleTokens :: NonEmptyArray MatchResult -> ColumnIndex -> LineIndex -> Array Token
possibleTokens xs c l =
    let 
        possible :: Array ( Token )        
        possible = foldr acc [] xs
            where
                acc :: MatchResult -> Array Token -> Array Token
                acc = \match -> \arr -> 
                        case matchToToken match of 
                            Nothing -> arr
                            Just tok -> Array.cons tok arr
    in  
        possible
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
                                , line: l
                                , column: c
                                }
                        in Just token

-- TODO need to write algorithm for choosing best token possible
-- This may not actually be useful here, since my token classes seem to be exclusive of each other.
chooseBest :: NonEmptyArray Token -> Token
chooseBest possible = head possible

allRegex :: NonEmptyArray RegexPair
allRegex =
    let normal = { type: NormalHeader, regex: normalHeader }
        error = { type: ErrorHeader, regex: errorHeader }
        defaultH = { type: DefaultHeader, regex: defaultHeader }
        errorM = { type: ErrorMessage, regex: errorMessage }
        regexTok = { type: Regex, regex: regexT }
        terminatorTok = { type : Terminator, regex: terminator }
        nameTok = { type: Name, regex: name }
        spaceTok = { type: WhiteSpace, regex: spaces}
        defaultTok = { type: Default, regex: default }
    in 
        singleton normal `appendArray` 
                [ error
                , defaultH
                , errorM
                , regexTok
                , terminatorTok
                , nameTok
                , spaceTok
                , defaultTok
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

        spaces :: Either String Regex
        spaces = regex "^\\s+" noFlags

        default :: Either String Regex
        default = regex "^\\$Default" noFlags