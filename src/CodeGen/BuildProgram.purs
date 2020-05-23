module BuildProgram 
    ( buildProgram

    ) where

import Control.Monad.State
import Prelude
import Data.String as Str

import Definitions (defineErrors, defineHelpers, defineMatchers, defineTokens)
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
    ctx <- get
    pure $ prog <> "\n\n" <> ctx.program


