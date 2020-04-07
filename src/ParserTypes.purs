
module ParserTypes
    ( TokenType(..)
    , Token
    , AST(..)
    , DerivationType(..)
    , equals
    ) where
import Prelude
import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Traversable (foldr, foldl)
import Data.Int (round, toNumber)
import Math (max)

data TokenType
    = NormalHeader
    | ErrorHeader
    | DefaultHeader
    | Regex
    | ErrorMessage
    | Terminator
    | Name
    | FAIL
    | Default
    | EOF
derive instance genericTokenType :: Generic TokenType _
instance showTokenType :: Show TokenType where show = genericShow
instance eqTokenType :: Eq TokenType where eq = genericEq

type Token = 
    { type :: TokenType
    , lexeme :: String
    , line :: Int
    , column :: Int
    }

type TestStringResult = 
    { str :: String
    , depth :: Int
    }

data AST
    = NProgram AST (Maybe AST) (Maybe AST)
    | NNormalSpecs (Array (AST))
    | NErrorSpecs (Array (AST))
    | NDefaultSpecs (Array (AST))
    | NNormalSpec AST AST
    | NErrorSpec AST AST (Maybe AST)
    | NRegex Token
    | NName Token
    | NErrorMessage Token
    | NDefaultError AST (Maybe AST)
derive instance genericAST :: Generic AST _
instance showAST :: Show AST where 
    show tree = 
        let result :: TestStringResult
            result = doString tree 0
        in  result.str
        where 
            doString :: AST -> Int -> TestStringResult
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

data DerivationType
    = --nonterminals
      DProgram
    | DNormalTokens
    | DErrorTokens
    | DDefaultTokens
    | DNormalSpecs
    | DNormalSpec 
    | DErrorSpecs
    | DErrorSpec
    | DDefaultSpecs
    | DDefaultError
    | DOptionalSync
        -- terminals 
    | DNormalHeader
    | DErrorHeader
    | DDefaultHeader
    | DRegex
    | DErrorMessage
    | DTerminator
    | DName
    | DFAIL
    | DDefault
    | DEof
        -- utility for AST building
    | DoneWithNode
derive instance genericDerivType :: Generic DerivationType _
instance showDerivationType :: Show DerivationType where show = genericShow
instance eqDerivationType :: Eq DerivationType where eq = genericEq

equals :: TokenType -> DerivationType -> Boolean
equals NormalHeader DNormalHeader = true
equals NormalHeader _ = false
equals ErrorHeader DErrorHeader = true
equals ErrorHeader _ = false
equals DefaultHeader DDefaultHeader = true
equals DefaultHeader _ = false
equals Regex DRegex = true
equals Regex _ = false
equals ErrorMessage DErrorMessage = true
equals ErrorMessage _ = false
equals Terminator DTerminator = true
equals Terminator _ = false
equals Name DName = true
equals Name _ = false
equals Default DDefault = true
equals Default _ = false
equals EOF DEof = true
equals EOF _ = false
equals FAIL _ = false