module CodeGen 



where

import Control.Monad.Maybe.Trans
import Control.Monad.State
import Prelude

import Control.Monad.Trans.Class (lift)
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
generate ast = evalState (doGenerate $ Just ast) { program: "", names: Map.empty, errors: Map.empty }

type TokenNamesStore = Map.Map String Unit
type ErrorStore = Map.Map String String

type Context = 
    { program :: String
    , names :: TokenNamesStore
    , errors :: ErrorStore
    }

type CodeState a = State Context a

updateNames :: Token -> CodeState Unit
updateNames tok = do 
    ctx <- get 
    put $ ctx { names = Map.insert tok.lexeme unit ctx.names }

updateErrors :: String -> String -> CodeState Unit
updateErrors name message = do 
    ctx <- get
    put $ ctx { errors = Map.insert name message ctx.errors }

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
                    pure $ makeProgram matchers errors defaults
            in  generated
            where
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
                            [ "const matchers = {};" 
                            , "const errors = {};"
                            , "export " <> function "lex" ["str"]
                                [ declareConst "tokens" "[]"
                                , declareConst "errors" "[]"
                                , declareConst "line" "0"
                                , declareConst "column" "0"
                                , while $ call "inputRemains" ["str"]
                                    [ declareLet "maxMunch" "doMaxMunch(str, line, column)"
                                    , ifExpr $ call "isError" ["maxMunch"] 
                                    , thenExpr
                                        [ assign "maxMunch" $ call "trySync" ["str" "maxMunch"]
                                        , call "publishError" ["maxMunch", "errors"]
                                        ]
                                    , elseExpr 
                                        [ call "publishToken" ["maxMunch", "tokens"]
                                        ]
                                    , assign "line" $ call "newLine" ["maxMunch" "line"]
                                    , assign "column" $ call "newColumn" ["maxMunch" "column"]
                                    ]
                                , return $ obj
                                    [ "tokens", "tokens"
                                    , "errors", "errors"
                                    ]
                                ]
                            , "export default lex;"
                            ]
                    in  Str.joinWith "\n" prog 
                helpers :: String
                helpers = Str.joinWith "\n"
                    [ comment 
                        [ "This function discards characters from the input string until"
                        , "the regex matches the syncing regex. Lexing should restart from"
                        , "there"
                        ]
                    , function "discardUntil" ["str", "sync"]
                        [ declareLet "search" "str"
                        , while ("!str.test(" <> "sync" <> ") && str.length > 0")
                            [ "search = str.slice(1);"
                            ]
                        , return "search"
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
                            let keys = Map.keys store
                            in  (flip map) keys (\key -> "export " <> declareConst key key <> ";")

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
                    matcher <- makeMatcher name regex
                    updateProgram matcher
                    pure ""
            in  generated
        ErrorSpec arr -> 
            let generated :: CodeState String
                generated = do 
                    name <- generateName $ head arr
                    message <- generateMessage $ (eHead 2) arr
                    sync <- generateName $ (eHead 3) arr
                    fullError <- makeError name message sync
                    updateProgram fullError
                    pure ""
            in  generated
        DefaultError arr -> 
            let generated :: CodeState String
                generated = do
                    message <- generateMessage $ head arr
                    sync <- generateName $ (eHead 2) arr
                    fullError <- makeError "_default" message sync
                    updateProgram fullError
                    pure ""
            in  generated

        
        Name tok -> do 
            updateNames tok
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

        makeMatcher :: String -> String -> CodeState String
        makeMatcher name regex = pure $ Str.joinWith "\n"
            [ declareConst name regex
            , function ("match" <> name) ["str"] 
                [ declareConst "matches" ("str.match(" <> name <> ");")
                , return $ ternary "matches.length > 0" "matches[0]" "null"
                ]
            ]

        generateMessage :: Maybe GenAST -> CodeState String
        generateMessage (Just (ErrorMessage tok)) = pure tok.lexeme
        generateMessage _ = pure ""

        -- describes what happens when a particular token matches an error
        makeError :: String -> String -> String -> CodeState String
        makeError name message sync = pure $ Str.joinWith "\n"
            -- to recover, push the error message into the global list of errors, and then 
            -- discard the input string until we match the sync token, if it exists
            [ function ("error" <> name) ["str"]
                [ "errors.push(" <> message <> ");"
                , declareLet "rem" "''"
                , declareConst "sync" ("matchers['" <> name <> "']")
                , ifExpr "sync" <> thenExpr ["rem = discardUntil(" <> "str, sync" <> ");"]
                , return "rem"
                ]
            ]

eHead :: forall a. Int -> Array a -> Maybe a
eHead = flip index

declareConst :: String -> String -> String
declareConst name expr = 
    "const " <> name <> " = " <> expr <> ";"

declareLet :: String -> String -> String
declareLet name expr =
    "let " <> name <> " = " <> expr <> ";"

ifExpr :: String -> String
ifExpr expr = 
    "if(" <> expr <> ")"

thenExpr :: Array String -> String
thenExpr body = Str.joinWith "\n"
    let body_ = Str.joinWith "\n" body
    in  [ "{"
        , body_
        , "}"
        ]

elseExpr :: Array String -> String
elseExpr body = Str.joinWith "\n"
    let body_ = Str.joinWith "\n" body
    in  [ "else {"
        , body_
        , "}"
        ]

function :: String -> Array String -> Array String -> String
function name args body = 
    let args_ = Str.joinWith ", " args
        body_ = Str.joinWith "\n" body
    in  Str.joinWith "\n" 
            [ "function " <> name <> "(" <> args_ <> ") {"
            , body_
            , "}"
            ]

return :: String -> String
return expr = 
    "return " <> expr <> ";"

ternary :: String -> String -> String -> String
ternary test t f = 
    test <> " ? " <> t <> " : " <> f <> ";"

comment :: Array String -> String
comment body = 
    let body_ = Str.joinWith "\n * " body
    in  Str.joinWith "\n * "
            [ "/*"
            , body_
            , "*/"
            ]

while :: String -> Array String -> String
while cond body = 
    let body_ = Str.joinWith "\n" body
    in  Str.joinWith "\n"
            [ "while(" <> cond <> "){"
            , body_
            , "}"
            ]

assign :: String -> String -> String
assign var val = 
    var <> " = " <> val <> ";"

call :: String -> Array String -> String
call fn args = 
    fn <> "(" <> (Str.joinWith ", " args) <> ")"

obj :: Array String -> String
obj key_values = 
    let _key_values = Str.joinWith ",\n" $ (flip map) (group key_values 2) (\kv -> Str.joinWith ": " kv)
    in  Str.joinWith "\n"
            [ "{"
            , _key_values
            , "}"
            ]
    where 
        group :: Array String -> Int -> Array String
        group arr count = doGroup arr count []
            where 
                doGroup :: Array String -> Int -> Array String -> Array String
                doGroup arr count acc =
                    if  Array.length arr < count then 
                        acc
                    else 
                        let dropped = Array.drop count arr
                            newGroup = Array.take count arr
                        in  doGroup dropped count (Array.cons newGroup acc)

