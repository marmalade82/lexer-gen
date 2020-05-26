module FirstPass 
    ( firstPass
    , nameExists
    ) where

import Prelude

import Control.Monad.State (get, put)
import Data.Array (head, index)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import TypeChecker.Internals (TypeState)
import Types (GenAST(..), Token)


firstPass :: GenAST -> TypeState Unit
firstPass ast = case ast of 
        Program arr -> do 
            _ <- sequence (firstPass <$> arr) :: TypeState (Array Unit)
            pure unit
        NormalSpecs arr -> do 
            _ <- sequence (firstPass <$> arr) :: TypeState (Array Unit)
            pure unit
        ErrorSpecs arr -> do 
            _ <- sequence (firstPass <$> arr) :: TypeState (Array Unit)
            pure unit
        DefaultSpecs arr -> do -- default specs treats its children as a sequence, and that's all.
            _ <- sequence (firstPass <$> arr) :: TypeState (Array Unit)
            pure unit
        NormalSpec arr -> do
            ctx <- get
            let tokens = ctx.tokenTypes
            case generateName $ head arr of 
                Nothing -> pure unit
                Just name -> do 
                    if not $ nameExists name tokens
                    then put ctx { tokenTypes = (Array.cons name tokens) }
                    else pure unit
        ErrorSpec arr -> do 
            ctx <- get
            let errors = ctx.errorTypes
            case generateName $ head arr of 
                Nothing -> pure unit
                Just name -> do 
                    if nameExists name errors
                    then put ctx { errorTypes = Array.cons name errors }
                    else pure unit
        _ -> pure unit
    where 
        eHead :: forall a. Int -> Array a -> Maybe a
        eHead = flip index

generateName :: Maybe GenAST -> Maybe Token
generateName (Just (Name tok)) = Just tok
generateName _ = Nothing

nameExists :: Token -> Array Token -> Boolean
nameExists tok arr = 
    case Array.find (sameTok tok) arr of 
        Nothing -> false
        _ -> true
    where 
        sameTok :: Token -> Token -> Boolean
        sameTok t1 t2 = 
            t1.type == t2.type && t1.lexeme == t2.lexeme && t1.column == t2.column && t1.line == t2.line