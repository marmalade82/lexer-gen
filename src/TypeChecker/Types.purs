module TypeChecker.Internals 
    ( Context
    , TypeState
    ) where

import Control.Monad.State (State)
import Types (Token)

type Context = 
    { tokenTypes :: Array Token
    , errorTypes :: Array Token
    }

type TypeState a = State Context a
