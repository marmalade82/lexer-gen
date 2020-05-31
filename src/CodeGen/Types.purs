module Types
    ( GenAST(..)
    , Token
    , TokenType(..)
    ) where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.String as Str

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
instance showGenAST :: Show GenAST where 
    show tree = doShow tree 0
        where 
            doShow :: GenAST -> Int -> String
            doShow ast x = case ast of
                Program arr -> "Program" <> spacer x <> show arr
                NormalSpecs arr -> "NormalSpecs" <> spacer x <> show arr
                ErrorSpecs arr -> "ErrorSpecs" <> spacer x <> show arr
                DefaultSpecs arr -> "DefaultSpecs" <> spacer x <> show arr
                NormalSpec arr -> "NormalSpec" <> spacer x <> show arr
                ErrorSpec arr -> "ErrorSpec" <> spacer x <> show arr
                DefaultError arr -> "DefaultError" <> spacer x <> show arr
                Name tok -> "Name" <> spacer x <> show tok
                Regex tok -> "Regex" <> spacer x <> show tok
                ErrorMessage tok -> "ErrorMessage" <> spacer x <> show tok
            spacer :: Int -> String
            spacer i = "\n\r" <> (Str.joinWith "" (Array.replicate i "  "))

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