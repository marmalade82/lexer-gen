module Definitions 
    ( defineHelpers
    , defineTokens
    , defineMatchers
    , defineErrors
    , fn
    ) where


import Control.Monad.State
import Prelude

import Data.Array as Array
import Data.Array (concatMap)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Tuple (Tuple(..), fst, snd)


import JavaScript as JS
import StoreInfo (CodeState, TokenNamesStore)

-- Export global function names for others to call
type FunctionDefinitions =
    { newLine :: String
    , newColumn :: String
    , inputRemains :: String
    , discardUntil :: String
    , doMaxMunch :: String
    , isError :: String
    , publish :: String
    , makeMatcher :: String
    , makeError :: String
    , lookupError :: String
    }

fn :: FunctionDefinitions
fn = 
    { newLine: "newLine"
    , newColumn: "newColumn"
    , inputRemains : "inputRemains"
    , discardUntil: "discardUntil"
    , doMaxMunch : "doMaxMunch"
    , isError : "isError"
    , publish : "publish"
    , makeMatcher : "makeMatcher"
    , makeError : "makeError"
    , lookupError : "lookupError"
    }



defineHelpers :: CodeState Unit
defineHelpers = updateProgram helpers

helpers :: String
helpers = Str.joinWith "\n"
    [ JS.comment
        [ "This function calculates the new line position by looking at the lexeme"
        , "and counting the number of newlines in it"
        ]
    , JS.function fn.newLine ["munch", "oldLine"] 
        [ JS.declareLet "count" "0"
        , "for(let i = 0; i < munch.lexeme.length; i++){"
        , JS.ifExpr "munch.lexeme[i] === '\\n'"
        , JS.thenExpr ["count++"]
        , JS.return $ "oldLine + count"
        , "}"
        ]
    , JS.comment 
        [ "This function calculates the new column position by looking at the lexeme"
        , "and finding the number of characters AFTER the last newline"
        ]
    , JS.function fn.newColumn ["munch", "oldCol"] 
        [ JS.declareLet "index" "munch.lexeme.length"
        , JS.declareLet "foundNewline" "false"
        , JS.while "index > 0 && !foundNewline"
            [ "index--"
            , JS.ifExpr "munch.lexeme[index] === '\\n'"
            , JS.thenExpr 
                [ JS.assign "foundNewline" "true"
                , "break;"
                ]
            ]
        , JS.ifExpr "foundNewline"
        , JS.thenExpr [ JS.return $ "munch.lexeme.length - index - 1"]
        , JS.elseExpr [ JS.return $ "oldCol + munch.lexeme.length"]
        ]
    , JS.function fn.inputRemains ["str"]
        [ JS.return "str.length > 0"
        ]
    , JS.comment 
        [ "This function discards characters from the input string until"
        , "the regex matches the syncing regex. Lexing should restart from"
        , "there"
        ]
    , JS.function fn.discardUntil ["str", "sync"]
        [ JS.declareLet "search" "str"
        , JS.declareConst "discarded" "[]"
        , JS.while ("!str.test(" <> "sync" <> ") && str.length > 0")
            [ (<>) "discarded." $ JS.call "push" ["search[0]"]
            , "search = search.slice(1);"
            ]
        , JS.return $ JS.obj 
                [ "discarded", "discarded.join('')"
                , "synced", "search"
                ]
        ]
    , "\n"
    , JS.comment 
        [ "This function runs through all the declared tokens and tries all of them to"
        , "find the one with maximum munch. If two or more have the same length, the one"
        , "that was declared latest in the lexer-gen file takes priority. Once the maximum"
        , "munch is identified, it returns an object containing the token type, the lexeme,"
        , "the column number, and the line number"
        ]
    , JS.function fn.doMaxMunch ["str", "line", "column"]
        [ JS.declareLet "munch" $ 
            "Object.values(matchers)." <> (JS.call "reduce" 
                [ JS.function "match" ["acc", "matcher"]
                    [ JS.declareConst "result" (JS.call "matcher" ["str"])
                    , JS.ifExpr "result !== null"
                    , JS.thenExpr 
                        [ JS.assign "acc.type" "result.type"
                        , JS.assign "acc.lexeme" "result.lexeme"
                        ]
                    , JS.return "acc"
                    ]
                , JS.obj   [ "line",      "line" 
                        , "column",    "column"
                        , "type",      "undefined"
                        , "lexeme",    " '' "
                        ]
                ])

        , JS.ifExpr $ "munch.type === undefined"
        , JS.thenExpr 
            [ JS.assign "munch" $
                "Object.values(errors)." <> (JS.call "reduce"
                    [ JS.function "match" ["acc", "error"]
                        [ JS.declareConst "result" (JS.call "error" ["str"])
                        , JS.ifExpr "result !== null"
                        , JS.thenExpr 
                            [ JS.assign "acc.type" "result.type"
                            , JS.assign "acc.lexeme" "result.lexeme"
                            ]
                        , JS.return "acc"
                        ]
                    , JS.obj   [ "line",      "line" 
                            , "column",    "column"
                            , "type",      "undefined"
                            , "lexeme",    " '' "
                            ]
                    ])
            , JS.ifExpr $ "munch.type === undefined"
            , JS.thenExpr
                [ "throw new Error('Lexing got stuck! No matchers or errors succeeded! Unexpected'); " ]
            ]
        , "\n"
        , JS.return "munch"
        ]
    , JS.comment 
        [ "This function determines whether a given munch is an error munch or not"
        ]
    , JS.function fn.isError ["munch"]
        [ JS.return $ fn.lookupError <> "(munch.type.toString()) !== null"
        ]
    
    , JS.function fn.publish ["munch", "tokens", "errors"]
        [ JS.ifExpr $ JS.call fn.isError ["munch"] 
        , JS.thenExpr
            [ JS.call "publishError" ["munch", "errors"]
            ]
        , JS.elseExpr 
            [ JS.call "publishToken" ["munch", "tokens"]
            ]
        , JS.function "publishError" ["munch", "errors"]
            [ (<>) "errors." $ JS.call "push" 
                [ "`line ${munch.line}, column ${munch.column}: ${" <> fn.lookupError <> "(munch.type)}`"
                ]
            ]
        , JS.function "publishToken" ["munch", "tokens"]
            [ (<>) "tokens." $ JS.call "push" [ "munch" ]
            ]
        ]
    ]

defineTokens :: CodeState Unit
defineTokens = do 
    tokenDefinitions <- exportTokenTypes
    updateProgram tokenDefinitions

exportTokenTypes :: CodeState String
exportTokenTypes = do
    ctx <- get 
    let tokens = ctx.names
        exports = doExport tokens
    pure $ Str.joinWith "\n" exports

    where 
        doExport :: TokenNamesStore -> Array String
        doExport store = 
            let keys :: Array String
                keys = Array.fromFoldable $ Map.keys store
            in  (flip map) keys (\key -> "export " <> (JS.declareConst (asToken key) ("\"" <> key <> "\"")))

defineMatchers :: CodeState Unit
defineMatchers = do
    defineMakeMatcher
    definitions <- matchers
    updateProgram definitions
    where
        matchers :: CodeState String
        matchers = do
            nonErrorTokenStore_ <- nonErrorTokenStore
            let
                matchers_ = (flip map) nonErrorTokenStore_ (\tup -> (fst tup) <> ": " <> (JS.call fn.makeMatcher [asToken $ fst tup, snd tup]) ) :: Array String
                kv = Str.joinWith ",\n" matchers_
            pure $ JS.declareConst "matchers" ("{\n" <> kv <> "\n}")
            where 
                -- 
                nonErrorTokenStore :: CodeState (Array (Tuple String String))
                nonErrorTokenStore = do 
                    ctx <- get
                    let allTokenStore = Map.toUnfoldable ctx.names :: Array (Tuple String String)
                        errorStore = ctx.errors
                        nonErrorStore = (flip Array.filter) allTokenStore 
                                            (\x@(Tuple name _) -> 
                                                (Map.lookup name errorStore) == Nothing
                                            )
                    pure nonErrorStore

defineMakeMatcher :: CodeState Unit
defineMakeMatcher = do
    let -- given name and regex, defines a function that, given the input string,
        -- returns object containing token type and lexeme, or null if no match
        makeMatcher :: String
        makeMatcher = 
            JS.function fn.makeMatcher ["tokenName", "regex"]
                [JS.return $
                    JS.function "matcher" ["input"] 
                        [ JS.declareConst "result" ((<>) "input." $ JS.call "match" ["regex"])
                        , JS.ifExpr "result && result.length > 0"
                        , JS.thenExpr 
                            [ JS.return $ JS.obj 
                                [ "type", "tokenName"
                                , "lexeme", "result[0]" ]
                            ]
                        , JS.elseExpr [ JS.return "null" ]
                        ]
                ]
    updateProgram makeMatcher



defineErrors :: CodeState Unit
defineErrors = do 
    defineMakeError
    defineLookupError
    definitions <- errors
    updateProgram definitions
    where
        errors :: CodeState String
        errors = do 
            ctx <- get
            let regexStore = ctx.names
                errorStore = Map.toUnfoldable ctx.errors  :: Array (Tuple String (Tuple String (Maybe String)))
            let errorMatchers = (flip map) errorStore 
                    (\tup -> 
                        let 
                            name = fst tup
                            regex = case Map.lookup name regexStore of 
                                Nothing -> "undefined"
                                Just reg -> reg
                            message = fst $ snd tup
                            sync = case snd $ snd tup of 
                                Nothing -> "undefined"
                                Just reg -> reg
                        in
                            (name) <> ": " <> (JS.call fn.makeError [asToken $ name, regex, sync ]) 
                    )
                kv = Str.joinWith ",\n" errorMatchers
            pure $ JS.declareConst "errors" ("{\n" <> kv <> "\n}")

defineMakeError :: CodeState Unit
defineMakeError = do 
    let makeError :: String
        makeError = 
            JS.function fn.makeError ["name", "regex", "sync"]
                [ JS.declareConst "initialMatcher" $ JS.call fn.makeMatcher ["name", "regex"]
                , JS.return $ JS.function "matcher" ["input"]
                    [ JS.declareConst "initialResult" $ JS.call "initialMatcher" ["input"]
                    , JS.ifExpr "!sync || initialResult === null"
                    , JS.thenExpr [JS.return "initialResult"]
                    , JS.elseExpr 
                        [ JS.declareConst "afterMatchInput" "input.slice(initialResult.lexeme.length)"
                        , JS.declareConst "{discarded, synced}" $ JS.call "discardUntil" ["afterMatchInput", "sync"]
                        , JS.assign "initialResult.originalLexeme" "initialResult.lexeme"
                        , JS.assign "initialResult.lexeme" "initialResult.lexeme + discarded"
                        , JS.return "initialResult"
                        ]
                    ]
                ]
    updateProgram makeError

defineLookupError :: CodeState Unit
defineLookupError = do
    ctx <- get
    let errorStore = Map.toUnfoldable ctx.errors  :: Array (Tuple String (Tuple String (Maybe String)))
        errorLookup = 
            let kv = (flip concatMap) errorStore
                    (\tup -> 
                        let name = fst tup
                            message = fst $ snd tup
                        in  [name, "'" <> message <> "'"] 
                    )
            in  JS.function fn.lookupError ["type"]
                    [ JS.declareConst "lookup" $ JS.obj kv
                    , JS.ifExpr "lookup[type] !== undefined"
                    , JS.thenExpr [ JS.return "lookup[type]" ]
                    , JS.elseExpr [ JS.return "null" ]
                    ]
    updateProgram errorLookup


updateProgram :: String -> CodeState Unit
updateProgram next = do 
    ctx <- get
    put $ ctx { program = ctx.program <> "\n" <> next }

asToken :: String -> String
asToken name = name