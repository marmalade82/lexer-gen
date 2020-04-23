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

type Context = 
    { program :: String
    , names :: Map.Map String Unit
    , errors :: Map.Map String String
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
                    matchers <- doGenerate $ head arr
                    errors <- doGenerate $ (eHead 2) arr
                    defaults <- doGenerate $ (eHead 3) arr
                    pure $ makeProgram matchers errors defaults
            in  generated
            where
                -- We also need to make sure that the function definitions make it into the final program.
                -- This might be a good place to use the State monad, since the state of the program so far is 
                -- a huge background part of the program
                makeProgram :: String -> String -> String -> String
                makeProgram matchers errors defaults = 
                    let prog = 
                            [ "const matchers = {};" 
                            , "const errors = {};"
                            , "\n"
                            , "export function lex(str) {"

                            , "};"
                            , "export default lex;"
                            ]
                    in  Str.joinWith "\n" prog 
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
                    makeError name message sync
                    updateProgram makeError
                    pure ""
            in  generated
        DefaultError arr -> 
            let generated :: CodeState String
                generated = do
                    message <- generateMessage $ head arr
                    sync <- generateName $ (eHead 2) arr
                    makeError "_default" message sync
                    updateProgram makeError
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
                , ifExpr "sync" <> thenExpr "rem = discardUntil(" <> "str, sync" <> ");"
                , return "rem"
                ]
            ]

eHead :: forall a. Int -> Array a -> Maybe a
eHead = flip index

declareConst :: String -> String -> String
declareConst name expr = 
    "const " <> name <> " = " <> expr <> ";"

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
