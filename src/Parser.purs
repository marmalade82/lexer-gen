module Parser where 

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, appendArray, singleton )
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy as LL
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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

data TableEntry
    = STUCK
    | Replace (NonEmptyArray DerivationType)
    | Discard

type DeriveIndex = Int
type TokenIndex = Int

type Table = Map DeriveIndex (Map TokenIndex TableEntry)

type ParseResult = 
    { tree :: Maybe AST
    , success :: Boolean
    , errors :: Array ParseError
    }

type ParseError = 
    { message :: String
    , line :: Int
    , column :: Int
    }

parse :: NonEmptyArray Token -> ParseResult
parse arr = 
    let initialStack :: Stack
        initialStack = pushStack DProgram $ pushStack DEof emptyStack
        result = 
            { tree: Nothing
            , success: false
            , errors: []
            }
    in result

type ParseState = 
    { stack :: Stack
    , result :: ParseResult
    }

doParse :: NonEmptyArray Token -> Stack -> ParseResult
doParse ts s = 
    let 
        initialParseState :: ParseState
        initialParseState = 
            { stack: pushStack DProgram $ pushStack DEof emptyStack
            , result:
                { tree: Nothing
                , success: false
                , errors: []
                }
            }

        state :: ParseState
        state = foldl acc initialParseState ts
    in
        { tree: Nothing
        , success: false
        , errors: []
        } 
    where
        acc :: ParseState -> Token -> ParseState
        acc state t = 
            let next :: TokenType
                next = t.type

                nextStack :: Maybe Stack
                nextStack = do 
                    leftmost <- topStack state.stack :: Maybe DerivationType
                    let stack = getNextStack leftmost next state.stack :: Either String Stack
                    case stack of 
                        Left _ -> Nothing
                        Right x -> pure x
            in  case nextStack of 
                    Nothing -> state
                    Just stack ->
                        { stack: stack
                        , result: state.result
                        }

        getNextStack :: DerivationType -> TokenType -> Stack -> Either String Stack
        getNextStack leftmost next stack = 
            if isTerminal leftmost
            then if next `equals` leftmost
                then Right $ popStack stack -- discard the leftmost symbol at the top of the stack
                else Left $ "Terminal did not match"
            else 
                let entry :: TableEntry
                    entry = getEntry leftmost next
                in  case entry of 
                        STUCK -> Left "Parser got suck here"
                        Replace arr ->  -- We need to replace the top of the stack with what's in @arr
                            Right $ foldr push (popStack stack) arr
                        Discard -> Right $ popStack stack -- Discard tells us the leftmost went to empty string
                where 
                    push :: DerivationType -> Stack -> Stack
                    push de st = pushStack de st

tokenTypeToIndex :: TokenType -> Int
tokenTypeToIndex t = 
    case t of 
        NormalHeader -> 0
        ErrorHeader -> 1
        DefaultHeader -> 2
        Regex -> 3
        ErrorMessage -> 4
        Terminator -> 5
        Name -> 6
        FAIL -> 7
        Default -> 8
        EOF -> 9

derivTypeToIndex :: DerivationType -> Int
derivTypeToIndex d =
    case d of
        DProgram            -> 0
        DNormalTokens       -> 1
        DErrorTokens        -> 2
        DDefaultTokens      -> 3
        DNormalSpecs        -> 4
        DNormalSpec         -> 5
        DErrorSpecs         -> 6
        DErrorSpec          -> 7
        DDefaultSpecs       -> 8
        DDefaultError       -> 9
        DOptionalSync       -> 10
        DNormalHeader       -> 11
        DErrorHeader        -> 12
        DDefaultHeader      -> 13
        DRegex              -> 14
        DErrorMessage       -> 15
        DTerminator         -> 16
        DName               -> 17
        DFAIL               -> 18
        DDefault            -> 19
        DEof                -> 20

isTerminal :: DerivationType -> Boolean
isTerminal d = case d of 
    DNormalHeader -> true
    DErrorHeader -> true
    DDefaultHeader -> true
    DRegex -> true
    DErrorMessage -> true
    DTerminator -> true
    DName -> true
    DFAIL -> true
    DDefault -> true
    DEof -> true
    _ -> false

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

getEntry :: DerivationType -> TokenType -> TableEntry
getEntry dType tType = 
    let entry :: Maybe TableEntry
        entry = do
            dRow <- lookup (di dType) table
            e <- lookup (ti tType) dRow :: Maybe TableEntry
            pure e
    in  case entry of 
            Nothing -> STUCK
            Just e -> e
    where 
        di = derivTypeToIndex
        ti = tokenTypeToIndex

        table :: Table
        table = 
            let 
                deriveEntries :: Array (Tuple DeriveIndex (Map TokenIndex TableEntry))
                deriveEntries = 
                    -- Only the nonterminals get entries
                    [ Tuple (di DProgram) program
                    , Tuple (di DNormalTokens) normalTokens
                    , Tuple (di DErrorTokens) errorTokens
                    , Tuple (di DDefaultTokens) defaultTokens
                    , Tuple (di DNormalSpecs) normalSpecs
                    , Tuple (di DNormalSpec) normalSpec
                    , Tuple (di DErrorSpecs) errorSpecs
                    , Tuple (di DErrorSpec) errorSpec
                    , Tuple (di DDefaultSpecs) defaultSpecs
                    , Tuple (di DDefaultError) defaultError
                    , Tuple (di DOptionalSync) optionalSync
                    ]
            in fromFoldable deriveEntries
            where 
                program = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries = [ Tuple (ti NormalHeader) entry]
                    in fromFoldable tokenEntries
                    where entry = Replace $ singleton DNormalTokens `appendArray` [ DErrorTokens, DDefaultTokens ]
                normalTokens =
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries = [ Tuple (ti NormalHeader ) entry ]
                    in fromFoldable tokenEntries
                    where entry = Replace $ singleton DNormalHeader `appendArray` [DNormalSpecs]
                errorTokens = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries = [ Tuple (ti EOF ) eof
                                    , Tuple (ti DefaultHeader) dh
                                    , Tuple (ti ErrorHeader) eh
                                    ]
                    in fromFoldable tokenEntries
                    where 
                        eof = Discard
                        dh = Discard
                        eh = Replace $ singleton DErrorHeader `appendArray` [DErrorSpecs]
                defaultTokens =
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries = [ Tuple (ti EOF) eof
                                    , Tuple (ti DefaultHeader ) dh
                                    ]
                    in fromFoldable tokenEntries
                    where 
                        eof = Discard
                        dh = Replace $ singleton DDefaultHeader `appendArray` [DDefaultSpecs]
                normalSpecs = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries =  [ Tuple (ti EOF ) eof
                                        , Tuple (ti ErrorHeader) eh
                                        , Tuple (ti Name) n
                                        ]
                    in fromFoldable tokenEntries
                    where 
                        eof = Discard
                        eh = Discard
                        n = Replace $ singleton DNormalSpec `appendArray` [DNormalSpecs]
                normalSpec = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries =  [ Tuple (ti Name ) n
                                        ]
                    in fromFoldable tokenEntries
                    where 
                        n = Replace $ singleton DName `appendArray` [ DRegex, DTerminator ]
                errorSpecs = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries =  [ Tuple (ti EOF ) eof
                                        , Tuple (ti DefaultHeader) dh
                                        , Tuple (ti Name) n
                                        ]
                    in fromFoldable tokenEntries
                    where 
                        eof = Discard
                        dh = Discard
                        n = Replace $ singleton DErrorSpec `appendArray` [DErrorSpecs]
                errorSpec = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries =  [ Tuple (ti Name ) n
                                        ]
                    in fromFoldable tokenEntries
                    where 
                        n = Replace $ singleton DName `appendArray` [ DErrorMessage, DOptionalSync, DTerminator ]
                defaultSpecs = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries =  [ Tuple (ti EOF ) eof
                                        , Tuple (ti Default ) de
                                        ]
                    in fromFoldable tokenEntries
                    where 
                        eof = Discard
                        de = Replace $ singleton DDefaultError
                defaultError = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries =  [ Tuple (ti Default ) d
                                        ]
                    in fromFoldable tokenEntries
                    where d = Replace $ singleton DDefault `appendArray` [ DErrorMessage, DOptionalSync, DTerminator ]
                optionalSync = 
                    let tokenEntries :: Array (Tuple TokenIndex TableEntry)
                        tokenEntries =  [ Tuple (ti Terminator ) t
                                        , Tuple (ti Name ) n
                                        ]
                    in fromFoldable tokenEntries
                    where 
                        t = Discard
                        n = Replace $ singleton DName

type Stack = LL.List (DerivationType)

emptyStack :: Stack
emptyStack = LL.nil

pushStack :: DerivationType -> Stack -> Stack
pushStack = LL.cons

topStack :: Stack -> Maybe DerivationType
topStack = LL.head

popStack :: Stack -> Stack
popStack s = case LL.tail s of
    Nothing -> s
    Just rest -> rest

sizeStack :: Stack -> Int
sizeStack = LL.length
