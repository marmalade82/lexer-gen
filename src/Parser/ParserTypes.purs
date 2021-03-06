
module ParserTypes
    ( TokenType(..)
    , Token
    , AST(..)
    , DerivationType(..)
    , equals
    , class ValidForAst
    , validForAst
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

class ValidForAst a where
    validForAst :: a -> Boolean

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
instance validAstTokenType :: ValidForAst TokenType where 
    validForAst t = case t of 
        NormalHeader -> false
        ErrorHeader -> false
        DefaultHeader -> false
        Regex -> true
        ErrorMessage -> true
        Terminator -> false
        Name -> true
        FAIL -> false
        Default -> false
        EOF -> false


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
    = NProgram (Maybe AST) (Maybe AST) (Maybe AST)
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
                            nString = maybeDoString n depth

                            eString :: TestStringResult
                            eString = maybeDoString e depth

                            dString :: TestStringResult
                            dString = maybeDoString d depth

                            children :: Array TestStringResult
                            children = [nString, eString, dString]

                            de = (resultDepth children)
                            str = "p" <> (if de > 1 then "," else "") <> (joinNonEmpty children)
                        in  makeResult str de
                        
                    NNormalSpecs arr -> 
                        let children :: Array TestStringResult
                            children = (flip doString $ depth) <$> arr

                            de = (resultDepth children)
                            str = "nh" <> (if de > 1 then "," else "") <> (joinNonEmpty children)
                        in  makeResult str de
                    NErrorSpecs arr ->
                        let children :: Array TestStringResult
                            children = (flip doString $ depth) <$> arr
                            
                            de = (resultDepth children)
                            str = "eh" <> (if de > 1 then "," else "") <> (joinNonEmpty children)
                        in  makeResult str de
                    NDefaultSpecs arr ->
                        let children :: Array TestStringResult
                            children = (flip doString $ depth) <$> arr

                            de = (resultDepth children)
                            str = "dh" <> (if de > 1 then "," else "") <> (joinNonEmpty children)
                        in  makeResult str de
                    NNormalSpec name regex -> 
                        let nString :: TestStringResult
                            nString = doString name depth

                            rString :: TestStringResult
                            rString = doString regex depth

                            children :: Array TestStringResult 
                            children = [nString, rString]

                            de = resultDepth children
                            str = "ns" <>  (if de > 1 then "," else "") <> (joinNonEmpty children)
                        in  makeResult str de
                    NErrorSpec name error msync -> 
                        let nString :: TestStringResult
                            nString = doString name depth

                            eString :: TestStringResult
                            eString = doString error depth

                            sString :: TestStringResult
                            sString = maybeDoString msync depth

                            children :: Array TestStringResult
                            children = [nString, eString, sString]

                            de = resultDepth children
                            str = "es" <> (if de > 1 then "," else "") <> (joinNonEmpty children)
                        in  makeResult str de
                    NDefaultError error msync ->
                        let eString :: TestStringResult
                            eString = doString error depth

                            sString :: TestStringResult
                            sString = maybeDoString msync depth

                            children :: Array TestStringResult
                            children = [eString, sString]

                            de = resultDepth children
                            str = "de" <> (if de > 1 then "," else "") <> (joinNonEmpty children)
                        in  makeResult str de
                    NName tok -> makeResult "n" 1
                    NRegex tok -> makeResult "r" 1
                    NErrorMessage tok -> makeResult "em" 1
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
                                                else Str.joinWith "" $ Array.replicate (res.depth - 1) "!"
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
instance validAstDerivType :: ValidForAst DerivationType where 
    validForAst d = case d of 
        DProgram -> true
        DNormalSpecs -> true
        DNormalSpec -> true
        DErrorSpecs -> true
        DErrorSpec -> true
        DDefaultSpecs -> true
        DDefaultError -> true
        DName -> true
        DRegex -> true
        DErrorMessage -> true

        -- The following are not part of the AST at this time, so we throw them away
        DNormalTokens -> false
        DErrorTokens -> false
        DDefaultTokens -> false
        DNormalHeader -> false
        DErrorHeader -> false
        DDefaultHeader -> false
        DOptionalSync -> false
        DDefault -> false
        DTerminator -> false
        DEof -> false
        DFAIL -> false
        DoneWithNode -> false

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