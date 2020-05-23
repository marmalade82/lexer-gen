module BuildProgram 
    ( buildProgram

    ) where

import Control.Monad.State (get)
import Prelude
import Data.String as Str

import Definitions (defineErrors, defineHelpers, defineMatchers, defineTokens, fn)
import JavaScript as JS
import StoreInfo (CodeState)

buildProgram :: CodeState String
buildProgram = do 
    defineHelpers
    defineTokens
    defineMatchers
    defineErrors
    writeLexer


writeLexer :: CodeState String
writeLexer = do
    let prog = Str.joinWith "\n"
            [ 
                "export " <> JS.function "lex" ["input"]
                [ JS.declareLet "str" "input"
                , JS.declareConst "tokens" "[]"
                , JS.declareConst "errors" "[]"
                , JS.declareLet "line" "0"
                , JS.declareLet "column" "0"
                , JS.while (JS.call fn.inputRemains ["str"])
                    [ JS.declareLet "maxMunch" $ JS.call fn.doMaxMunch ["str", "line", "column"]
                    , JS.assign "str" "str.slice(maxMunch.lexeme.length)"
                    , JS.call fn.publish ["maxMunch", "tokens", "errors"]
                    , JS.assign "line" $ JS.call fn.newLine ["maxMunch", "line"]
                    , JS.assign "column" $ JS.call fn.newColumn ["maxMunch", "column"]
                    ]
                , JS.return $ JS.obj
                    [ "tokens", "tokens"
                    , "errors", "errors"
                    ]
                ]
            , "export default lex;"
            ]
    ctx <- get
    pure $ prog <> "\n\n" <> ctx.program


