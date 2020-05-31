module SideEffect.Log 
     
    
     where

import Prelude



foreign import log :: String -> Unit


sideEffectLog :: String -> Unit
sideEffectLog str = log str