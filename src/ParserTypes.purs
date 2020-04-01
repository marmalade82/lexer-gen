
module ParserTypes
    ( TokenType(..)
    , Token
    , AST(..)
    , DerivationType(..)
    , equals
    ) where
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

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