module StoreInfo 
    ( storeInfo
    , Context
    , TokenNamesStore
    , ErrorStore
    , CodeState
    )
where

{- This module provides the service of updating the global state context with information gathered
    from the AST.
-}

import Prelude
import Control.Monad.State
import Data.Array (head, last, length, take, index)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd)

import Types (GenAST(..))

type Context = 
    { program :: String
    , names :: TokenNamesStore
    , errors :: ErrorStore
    }

type TokenNamesStore = Map.Map String String -- from token name to regex for it.
type ErrorStore = Map.Map String (Tuple String (Maybe String)) -- from token name to error message and optional sync

type CodeState a = State Context a

storeInfo :: GenAST -> CodeState Unit
storeInfo ast = case ast of 
        Program arr -> do 
            _ <- sequence (storeInfo <$> arr) :: CodeState (Array Unit)
            pure unit
        NormalSpecs arr -> do 
            _ <- sequence (storeInfo <$> arr) :: CodeState (Array Unit)
            pure unit
        ErrorSpecs arr -> do 
            _ <- sequence (storeInfo <$> arr) :: CodeState (Array Unit)
            pure unit
        DefaultSpecs arr -> do -- default specs treats its children as a sequence, and that's all.
            _ <- sequence (storeInfo <$> arr) :: CodeState (Array Unit)
            pure unit
        NormalSpec arr -> do
            name <- generateName $ head arr
            regex <- generateRegex $ (eHead 2) arr
            registerTokenRegex name regex
        ErrorSpec arr -> do 
            name <- generateName $ head arr
            message <- generateMessage $ (eHead 2) arr
            sync <- generateName $ (eHead 3) arr
            registerTokenError name message sync
        DefaultError arr -> do 
            message <- generateMessage $ head arr
            sync <- generateName $ (eHead 2) arr
            registerTokenError "_default" message sync
        _ -> pure unit
    where 
        eHead :: forall a. Int -> Array a -> Maybe a
        eHead = flip index

generateName :: Maybe GenAST -> CodeState String
generateName (Just (Name tok)) = pure tok.lexeme
generateName _ = pure ""

generateRegex :: Maybe GenAST -> CodeState String
generateRegex (Just (Regex tok)) = pure $ "new RegExp(" <> tok.lexeme <> ")"
generateRegex _ = pure ""

generateMessage :: Maybe GenAST -> CodeState String
generateMessage (Just (ErrorMessage tok)) = pure tok.lexeme
generateMessage _ = pure ""

registerTokenRegex :: String -> String -> CodeState Unit
registerTokenRegex name regex = do 
    updateNames name regex

-- Writes the error into global state so we can define error functions later
registerTokenError :: String -> String -> String -> CodeState Unit
registerTokenError name message sync = do 
    updateErrors name message $ case sync of 
        "" -> Nothing
        x -> Just x

updateNames :: String -> String -> CodeState Unit
updateNames token regex = do 
    ctx <- get 
    put $ ctx { names = Map.insert token regex ctx.names }

updateErrors :: String -> String -> Maybe String -> CodeState Unit
updateErrors name message sync = do 
    ctx <- get
    put $ ctx { errors = Map.insert name (Tuple message sync) ctx.errors }
