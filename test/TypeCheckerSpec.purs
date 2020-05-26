
module Test.TypeCheckerSpec where 

import Prelude

import Data.Array (length)
import Test.Spec (Spec, describe, describeOnly, it)
import Test.Spec.Assertions (shouldEqual)
import TypeChecker (typecheck)
import Types (GenAST(..), TokenType(..))

spec :: Spec Unit
spec = describeOnly "Test type checking of parsed AST" do
    uniqueNamesSpec
    reservedNamesSpec
    uniqueErrorsSpec
    definedErrorsSpec

uniqueNamesSpec :: Spec Unit
uniqueNamesSpec = describe "Names must be unique" do 
    it "no names" do 
        let program = Program
                [ NormalSpecs
                    [

                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 0
    it "one name" do 
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 0
    it "no duplicates" do
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "safe", column: 5, line: 5 }

                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 0
    it "one duplicate" do 
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "hi", column: 5, line: 5 }

                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 1
    it "two matching duplicates" do
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "hi", column: 5, line: 5 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "hi", column: 10, line: 10 }

                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 2
    it "two different duplicates" do
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "hi", column: 5, line: 5 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 10, line: 10 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 15, line: 15 }

                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 2

            
reservedNamesSpec :: Spec Unit
reservedNamesSpec = describe "Reserved token names are rejected" do 
    it "reserved default is rejected" do 
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "$Default", column: 0, line: 0 }
                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 1
    
uniqueErrorsSpec :: Spec Unit
uniqueErrorsSpec = describe "No two error definitions can use the same token definition" do 
    it "no error definitions" do
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 5, line: 5 }

                        ]
                    ]
                , ErrorSpecs
                    [

                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 0
    it "two different error definitions" do
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 5, line: 5 }

                        ]
                    ]
                , ErrorSpecs
                    [ ErrorSpec
                        [ Name { type: N, lexeme: "hi", column: 10, line: 10 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 15, line: 15 }
                        ]
                    , ErrorSpec
                        [ Name { type: N, lexeme: "bye", column: 20, line: 20 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 25, line: 25 }
                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 0
    it "two matching duplicates" do 
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 5, line: 5 }

                        ]
                    ]
                , ErrorSpecs
                    [ ErrorSpec
                        [ Name { type: N, lexeme: "hi", column: 10, line: 10 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 15, line: 15 }
                        ]
                    , ErrorSpec
                        [ Name { type: N, lexeme: "hi", column: 20, line: 20 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 25, line: 25 }
                        ]
                    , ErrorSpec
                        [ Name { type: N, lexeme: "hi", column: 30, line: 30 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 35, line: 35 }
                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 2
    it "two sets of duplicates" do
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 5, line: 5 }

                        ]
                    ]
                , ErrorSpecs
                    [ ErrorSpec
                        [ Name { type: N, lexeme: "hi", column: 10, line: 10 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 15, line: 15 }
                        ]
                    , ErrorSpec
                        [ Name { type: N, lexeme: "hi", column: 20, line: 20 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 25, line: 25 }
                        ]
                    , ErrorSpec
                        [ Name { type: N, lexeme: "bye", column: 30, line: 30 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 35, line: 35 }
                        ]
                    , ErrorSpec
                        [ Name { type: N, lexeme: "bye", column: 40, line: 40 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 45, line: 45 }
                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 2

definedErrorsSpec :: Spec Unit
definedErrorsSpec = describe "Error definitions must use defined tokens" do
    it "one error defined incorrectly" do 
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 5, line: 5 }

                        ]
                    ]
                , ErrorSpecs
                    [ ErrorSpec
                        [ Name { type: N, lexeme: "sigh", column: 10, line: 10 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 15, line: 15 }
                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 1
    it "two errors defined incorrectly" do
        let program = Program
                [ NormalSpecs
                    [ NormalSpec 
                        [ Name { type: N, lexeme: "hi", column: 0, line: 0 }

                        ]
                    , NormalSpec
                        [ Name { type: N, lexeme: "bye", column: 5, line: 5 }

                        ]
                    ]
                , ErrorSpecs
                    [ ErrorSpec
                        [ Name { type: N, lexeme: "sigh", column: 10, line: 10 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 15, line: 15 }
                        ]
                    , ErrorSpec
                        [ Name { type: N, lexeme: "flight", column: 20, line: 20 }
                        , ErrorMessage { type: EM, lexeme: "error", column: 25, line: 25 }
                        ]
                    ]
                ]
            result = typecheck program
        length result.errors `shouldEqual` 2