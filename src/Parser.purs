module Parser 
    ( module ParserTypes
    , ParseResult
    , ParseError
    , ParseState
    , parse
    , Stack
    , toString
    , eof
    )


where 

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, appendArray, singleton)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.List.Lazy as LL
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import ParserAST (BuildCommand(..), TreeBuildState, buildTree, emptyBuildState, extract)
import ParserTypes (AST(..), DerivationType(..), TokenType(..), Token, equals)


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
    , diagnosticError :: String
    }

type ParseError = 
    { message :: String
    , line :: Int
    , column :: Int
    }

eof :: Token
eof = 
    { type: EOF
    , lexeme: ""
    , line: -1
    , column: -1
    }

toString :: ParseError -> String
toString err = 
        show err.line <> ":" <> show err.column <> ": " <> err.message

parse :: NonEmptyArray Token -> ParseResult
parse arr = 
    let result :: ParseResult
        result = doParse arr
    in result

type ParseState = 
    { continue :: Boolean
    , stack :: Stack
    , astBuildState :: TreeBuildState
    , result :: ParseResult
    }

doParse :: NonEmptyArray Token -> ParseResult
doParse ts = 
    let 
        initialParseState :: ParseState
        initialParseState = 
            { continue: true
            , stack: pushStack DProgram $ pushStack DEof emptyStack
            , astBuildState: emptyBuildState
            , result:
                { tree: Nothing
                , success: false
                , errors: []
                , diagnosticError: ""
                }
            }

        state :: ParseState
        state = foldl acc initialParseState ts

        buildResults :: Either String AST
        buildResults = extract state.astBuildState
    in
        if sizeStack state.stack == 0
        then case buildResults of 
                Left err -> state.result
                    { success = true
                    , diagnosticError = "Stack was " <> show state.stack <> " when build failed: " <> err
                    }
                Right ast -> state.result 
                    { success = true
                    , tree = Just $ ast
                    }
        else -- we error because we finished parsing without consuming the stack
            state.result
                { success = false
                , diagnosticError = "Stack was " <> show state.stack 
                }
    where
        acc :: ParseState -> Token -> ParseState
        acc state t = 
            let next :: Token
                next = t

                nextState :: Either String ParseState
                nextState = processToken next state
            in  if state.continue  
                then 
                    case nextState of 
                            Left err -> state
                                { continue = false
                                , result { errors = Array.cons (makeError t err) (state.result.errors) }
                                }
                            Right _state -> _state
                                { continue = true
                                }
                else state

        processToken :: Token -> ParseState -> Either String ParseState
        processToken token state = 
            -- To build the tree, we need a Builder module with AST awareness that receives
            -- tokens and non-terminals from the LL1 parsing process and throws them on the stack.
            -- Since LL1 effectively amounts to a depth-first, left-first building of the derivation tree,
            -- using a separate stack here will allow us to combine the top of the stack with what's right underneath it
            -- with some context-aware code that will put the tree together in the right order.
            let stack :: Stack
                stack = state.stack

                current :: TokenType
                current = token.type

                result :: Either String ParseState 
                result = do
                    let l = topStack stack
                        popped = popStack stack
                    case l of 
                        Nothing -> Left $ "No leftmost found on stack on terminal " <> show current
                        Just leftmost -> case leftmost of 
                            DoneWithNode -> do -- special derivation type that marks the end of a subderivation
                                newBuildState <- buildTree state.astBuildState $ Up
                                let newState = state { stack = popped, astBuildState = newBuildState } :: ParseState
                                processToken token newState
                            _ -> do
                                if isTerminal leftmost
                                then if current `equals` leftmost
                                    then do -- We're done with the current token and leftmost symbol. It's time to use it to build
                                        withDeriv <- buildTree state.astBuildState $ AddDerivation leftmost
                                        withMatch <- buildTree withDeriv $ Match token
                                        newBuildState <- buildTree withMatch $ Up -- matching a token always results in going up
                                        pure $ state { stack = popped, astBuildState = newBuildState }
                                    else 
                                        Left $ "Terminal " <> show current <> " did not match " <> show leftmost
                                else -- we need to recursively find the next stack until we are finished with this terminal.
                                    let entry :: TableEntry
                                        entry = getEntry leftmost current
                                    in  case entry of 
                                            STUCK -> -- we're done, since we got stuck
                                                Left $ "Parser got stuck here looking at stack: " <> show stack <> " and token " <> show current
                                            Replace arr -> do   -- We've verified the leftmost derivation has a match
                                                newBuildState <- buildTree state.astBuildState $ AddDerivation leftmost
                                                let replaced = foldr push (pushStack DoneWithNode popped) arr :: Stack
                                                    newState = state { stack = replaced, astBuildState = newBuildState } :: ParseState
                                                processToken token newState
                                            Discard -> do   -- Discard tells us that the nonterminal went to empty string,
                                                            -- so the AST builder should go to the next open slot.
                                                newBuildState <- buildTree state.astBuildState $ Next
                                                let newState = state { stack = popped, astBuildState = newBuildState } :: ParseState
                                                processToken token newState
                                    where 
                                        push :: DerivationType -> Stack -> Stack
                                        push de st = pushStack de st
            in  result
            

        makeError :: Token -> String -> ParseError
        makeError t s = 
            { message: s
            , line: t.line
            , column: t.column
            }

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
            -- Nonvalid deriv types here
        DoneWithNode        -> -1

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
                                        , Tuple (ti DefaultHeader ) dh
                                        , Tuple (ti Name) n
                                        ]
                    in fromFoldable tokenEntries
                    where 
                        eof = Discard
                        eh = Discard
                        dh = Discard
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
