module CodeGen 
    ( generate
    , module Types
    )

where

import Control.Monad.Maybe.Trans
import Control.Monad.State
import Prelude

import BuildProgram (buildProgram)
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
import Definitions (defineHelpers, defineTokens)
import JavaScript as JS
import StoreInfo (storeInfo, Context, TokenNamesStore, ErrorStore, CodeState)
import Types (GenAST(..), Token, TokenType(..))


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

generate :: GenAST -> String
generate ast = evalState (doGenerate $ Just ast) initialContext

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

updateProgram :: String -> CodeState Unit
updateProgram next = do 
    ctx <- get
    put $ ctx { program = ctx.program <> "\n" <> next }

-- generation should really lead to the emission of strings to a file, line by line. Running
-- the file for its effects should determine whether the test passes or fails.
doGenerate :: Maybe GenAST -> CodeState String
doGenerate Nothing = do 
    pure ""
doGenerate (Just ast) = do 
    case ast of 
        Program arr -> do 
            -- Gather all needed info by scanning the AST, and then use all the info
            -- to write the lexing program
            storeInfo (Program arr)
            buildProgram
        _ -> do 
            -- If there's no program, then we generate nothing
            pure ""

asToken :: String -> String
asToken name = name