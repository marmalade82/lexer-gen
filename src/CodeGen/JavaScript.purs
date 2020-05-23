module JavaScript

where

import Prelude
import Data.Array as Array
import Data.String as Str

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

elseIfExpr :: String -> String
elseIfExpr cond = "else if(" <> cond <> ")"

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
        body_ = "\t" <> Str.joinWith "\n\t" body
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

tab :: String
tab = "    "

while :: String -> Array String -> String
while cond body = 
    let body_ = tab <> Str.joinWith ("\n" <> tab) body
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
        group :: Array String -> Int -> Array (Array String)
        group arr_ count_ = doGroup arr_ count_ []
            where 
                doGroup :: Array String -> Int -> Array (Array String) -> Array (Array String)
                doGroup arr count acc =
                    if  Array.length arr < count then 
                        acc
                    else 
                        let dropped = Array.drop count arr
                            newGroup = Array.take count arr
                        in  doGroup dropped count (Array.cons newGroup acc)