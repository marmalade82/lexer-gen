module CodeGen 
    ( generate
    , GenAST(..)
    , Token(..)
    , TokenType(..)
    )

where

import Control.Monad.Maybe.Trans
import Control.Monad.State
import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (concatMap)
import Data.Array (head, last, length, take, index)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)
import JavaScript as JS


{-  module for generating code from the AST
    What I've realized is that besides the actual parsing algorithm, very, very few of 
    the types should know anything about how the AST should be structured. This decouples
    the tree and code generation from the parsing algorithm itself.

    The code generator will *try* to turn a node and its children into code, but if it cannot,
    it will simply generate no code at all. This is different from a type checker, which analyzes the 
    AST and must report any errors.

    For this same reason, the type checker should not duplicate any effort of the parser -- its goal is 
    only to unify types.
-}

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


generate :: GenAST -> String
generate ast = evalState (doGenerate $ Just ast) initialContext

type TokenNamesStore = Map.Map String String -- from token name to regex for it.
type ErrorStore = Map.Map String (Tuple String (Maybe String)) -- from token name to error message and optional sync

type Context = 
    { program :: String
    , names :: TokenNamesStore
    , errors :: ErrorStore
    }

initialContext :: Context 
initialContext =
    { program: ""
    , names: Map.fromFoldable
        [ Tuple "_default" "new RegExp('.*')"
        ]
    , errors: Map.fromFoldable
        [ Tuple "_default" (Tuple "No match for any token" Nothing)
        ]
    }

type CodeState a = State Context a

updateNames :: String -> String -> CodeState Unit
updateNames token regex = do 
    ctx <- get 
    put $ ctx { names = Map.insert token regex ctx.names }

updateErrors :: String -> String -> Maybe String -> CodeState Unit
updateErrors name message sync = do 
    ctx <- get
    put $ ctx { errors = Map.insert name (Tuple message sync) ctx.errors }

updateProgram :: String -> CodeState Unit
updateProgram next = do 
    ctx <- get
    put $ ctx { program = ctx.program <> "\n" <> next }

-- generation should really lead to the emmision of strings to a file, line by line. Running
-- the file for its effects should determine whether the test passes or fails.
doGenerate :: Maybe GenAST -> CodeState String
doGenerate Nothing = do 
    pure ""
doGenerate (Just ast) = do 
    case ast of 
        Program arr ->
            let generated :: CodeState String
                generated = do 
                    updateProgram helpers
                    -- we assume that code generation for the first 3 children will generate all the known
                    -- token-regex-error pairings, that the program can then use to set up the algorithm.
                    _ <- doGenerate $ head arr
                    _ <- doGenerate $ (eHead 2) arr
                    _ <- doGenerate $ (eHead 3) arr
                    exportTokenTypes_ <- exportTokenTypes -- for use by whatever parser
                    updateProgram exportTokenTypes_
                    matchers <- defineMatchers -- array of matcheres
                    updateProgram matchers
                    errors <- defineErrors
                    updateProgram errors
                    ctx <- get
                    pure $ 
                        makeProgram "" "" "" <> "\n\n" <> ctx.program
            in  generated
            where
                defineMatchers :: CodeState String
                defineMatchers = do
                    updateProgram makeMatcher
                    nonErrorTokenStore_ <- nonErrorTokenStore
                    let
                        matchers = (flip map) nonErrorTokenStore_ (\tup -> (fst tup) <> ": " <> (JS.call "makeMatcher" [asToken $ fst tup, snd tup]) ) :: Array String
                        kv = Str.joinWith ",\n" matchers
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
                        -- given name and regex, defines a function that, given the input string,
                        -- returns object containing token type and lexeme, or null if no match
                        makeMatcher :: String
                        makeMatcher = 
                            JS.function "makeMatcher" ["tokenName", "regex"]
                                [JS.return $
                                    JS.function "matcher" ["input"] 
                                        [ JS.declareConst "result" ((<>) "input." $ JS.call "match" ["regex"])
                                        , JS.ifExpr "result.length > 0"
                                        , JS.thenExpr 
                                            [ JS.return $ JS.obj 
                                                [ "type", "tokenName"
                                                , "lexeme", "result[0]" ]
                                            ]
                                        , JS.elseExpr [ JS.return "null" ]
                                        ]
                                ]
                
                defineErrors :: CodeState String
                defineErrors = do 
                    updateProgram makeError
                    ctx <- get
                    let regexStore = ctx.names
                        errorStore = Map.toUnfoldable ctx.errors  :: Array (Tuple String (Tuple String (Maybe String)))
                        errorLookup = 
                            let kv = (flip concatMap) errorStore
                                    (\tup -> 
                                        let name = fst tup
                                            message = fst $ snd tup
                                        in  [name, "'" <> message <> "'"] 
                                    )
                            in  JS.function "lookupError" ["type"]
                                    [ JS.declareConst "lookup" $ JS.obj kv
                                    , JS.ifExpr "lookup[type] !== undefined"
                                    , JS.thenExpr [ JS.return "lookup[type]" ]
                                    , JS.elseExpr [ JS.return "null" ]
                                    ]
                    updateProgram errorLookup
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
                                    (name) <> ": " <> (JS.call "makeError" [asToken $ name, regex, sync ]) 
                            )
                        kv = Str.joinWith ",\n" errorMatchers
                    pure $ JS.declareConst "errors" ("{\n" <> kv <> "\n}")
                    where
                        -- given name, regex, and sync regex
                        -- returns object containing error token type and lexeme (possibly including the discard to sync)
                        makeError :: String
                        makeError = 
                            JS.function "makeError" ["name", "regex", "sync"]
                                [ JS.declareConst "initialMatcher" $ JS.call "makeMatcher" ["name", "regex"]
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

                -- while the string still has remaining input, we fetch all the 
                -- possible matchers and try them all, taking the one with maximum munch.
                -- If the maximum munch is an error, we fetch the corresponding error implementation
                -- and resync (if specified). We then put the error message in the global
                -- error message store along with the line and column number
                -- If the maximum munch is not an error, we build the corresponding token and put 
                -- it into the the global token store
                -- Once the string has no more input, we return the errors and tokens
                makeProgram :: String -> String -> String -> String
                makeProgram matchers errors defaults = 
                    let prog = 
                            [ 
                              "export " <> JS.function "lex" ["input"]
                                [ JS.declareLet "str" "input"
                                , JS.declareConst "tokens" "[]"
                                , JS.declareConst "errors" "[]"
                                , JS.declareLet "line" "0"
                                , JS.declareLet "column" "0"
                                , JS.while (JS.call "inputRemains" ["str"])
                                    [ JS.declareLet "maxMunch" "doMaxMunch(str, line, column)"
                                    , JS.assign "str" "str.slice(maxMunch.lexeme.length)"
                                    , JS.call "publish" ["maxMunch", "tokens", "errors"]
                                    , JS.assign "line" $ JS.call "newLine" ["maxMunch", "line"]
                                    , JS.assign "column" $ JS.call "newColumn" ["maxMunch", "column"]
                                    ]
                                , JS.return $ JS.obj
                                    [ "tokens", "tokens"
                                    , "errors", "errors"
                                    ]
                                ]
                            , "export default lex;"
                            ]
                    in  Str.joinWith "\n" prog 
                helpers :: String
                helpers = Str.joinWith "\n"
                    [ JS.comment
                        [ "This function calculates the new line position by looking at the lexeme"
                        , "and counting the number of newlines in it"
                        ]
                    , JS.function "newLine" ["munch", "oldLine"] 
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
                    , JS.function "newColumn" ["munch", "oldCol"] 
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
                    , JS.function "inputRemains" ["str"]
                        [ JS.return "str.length > 0"
                        ]
                    , JS.comment 
                        [ "This function discards characters from the input string until"
                        , "the regex matches the syncing regex. Lexing should restart from"
                        , "there"
                        ]
                    , JS.function "discardUntil" ["str", "sync"]
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
                    , JS.function "doMaxMunch" ["str", "line", "column"]
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
                    , JS.function "isError" ["munch"]
                        [ JS.return "lookupError(munch.type.toString()) !== null"
                        ]
                    
                    , JS.function "publish" ["munch", "tokens", "errors"]
                        [ JS.ifExpr $ JS.call "isError" ["munch"] 
                        , JS.thenExpr
                            [ JS.call "publishError" ["munch", "errors"]
                            ]
                        , JS.elseExpr 
                            [ JS.call "publishToken" ["munch", "tokens"]
                            ]
                        , JS.function "publishError" ["munch", "errors"]
                            [ (<>) "errors." $ JS.call "push" 
                                [ "`line ${munch.line}, column ${munch.column}: ${lookupError(munch.type)}`"
                                ]
                            ]
                        , JS.function "publishToken" ["munch", "tokens"]
                            [ (<>) "tokens." $ JS.call "push" [ "munch" ]
                            ]
                        ]
                    ]
                -- generates code for exporting token types that are 
                -- currently in the state
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

        NormalSpecs arr ->
            let mapped :: (CodeState (Array String))
                mapped = sequence (doGenerate <$> (Just <$> arr))

                generated :: CodeState String
                generated = do 
                    arr_2 <- mapped
                    pure $ Str.joinWith "\n" arr_2
            in  generated
        ErrorSpecs arr ->
            let mapped :: (CodeState (Array String))
                mapped = sequence (doGenerate <$> (Just <$> arr))
            
                generated :: CodeState String
                generated = do 
                    arr_2 <- mapped
                    pure $ Str.joinWith "\n" arr_2
            in  generated
        DefaultSpecs arr -> -- default specs treats its children as a sequence, and that's all.
            let mapped :: (CodeState (Array String))
                mapped = sequence (doGenerate <$> (Just <$> arr))

                generated :: CodeState String
                generated = do 
                    arr_2 <- mapped
                    pure $ Str.joinWith "\n" arr_2
            in  generated
        NormalSpec arr -> 
            let generated :: CodeState String
                generated = do 
                    name <- generateName $ head arr
                    regex <- generateRegex $ (eHead 2) arr
                    registerTokenRegex name regex
                    pure ""
            in  generated
        ErrorSpec arr -> 
            let generated :: CodeState String
                generated = do 
                    name <- generateName $ head arr
                    message <- generateMessage $ (eHead 2) arr
                    sync <- generateName $ (eHead 3) arr
                    registerTokenError name message sync
                    pure ""
            in  generated
        DefaultError arr -> 
            let generated :: CodeState String
                generated = do
                    message <- generateMessage $ head arr
                    sync <- generateName $ (eHead 2) arr
                    registerTokenError "_default" message sync
                    pure ""
            in  generated

        
        Name tok -> do 
            pure "" 
        Regex tok -> pure ""
        ErrorMessage tok -> pure ""

    where 
        generateName :: Maybe GenAST -> CodeState String
        generateName (Just (Name tok)) = pure tok.lexeme
        generateName _ = pure ""

        generateRegex :: Maybe GenAST -> CodeState String
        generateRegex (Just (Regex tok)) = pure $ "new RegExp(" <> tok.lexeme <> ")"
        generateRegex _ = pure ""

        registerTokenRegex :: String -> String -> CodeState Unit
        registerTokenRegex name regex = do 
            updateNames name regex

        generateMessage :: Maybe GenAST -> CodeState String
        generateMessage (Just (ErrorMessage tok)) = pure tok.lexeme
        generateMessage _ = pure ""

        -- Writes the error into global state so we can define error functions later
        registerTokenError :: String -> String -> String -> CodeState Unit
        registerTokenError name message sync = do 
            updateErrors name message $ case sync of 
                "" -> Nothing
                x -> Just x

eHead :: forall a. Int -> Array a -> Maybe a
eHead = flip index

asToken :: String -> String
asToken name = name