module Tests.Parser where

import Glow.Ast
import Glow.Parser
import Glow.Prelude

import qualified Data.Text.Lazy  as LT
import           Text.Megaparsec (errorBundlePretty, runParser)

import Test.Hspec

tests = describe "Glow.Parser" $ do
    describe "function parameters" $ do
        describe "individual parameters" $ do
            describe "Invalid" $ checkExamples
                [ mkParseTest
                    fnParam
                    [")"]
                    Nothing
                , mkParseTest
                    fnParam
                    ["9"]
                    Nothing
                ]
            describe "Identifier" $ checkExamples
                [ mkParseTest
                    fnParam
                    ["seller"]
                    (Just Param
                        { pName = "seller"
                        , pType = Nothing
                        }
                    )
                ]
            describe "Identifier:Type" $ checkExamples
                [ mkParseTest
                    fnParam
                    ["seller:Address"]
                    (Just Param
                        { pName = "seller"
                        , pType = Just (TyIdent "Address")
                        }
                    )
                ]
        describe "parameter lists" $ do
            describe "no parameters" $ checkExamples
                [ mkParseTest
                    paramList
                    ["()"]
                    (Just [])
                ]
            describe "parameters" $ checkExamples
                [ mkParseTest
                    paramList
                    ["(seller, seller2)"]
                    (Just
                        [ Param { pName = "seller", pType = Nothing }
                        , Param { pName = "seller2", pType = Nothing }
                        ]
                    )
                ]
            describe "parameters with types" $ checkExamples
                [ mkParseTest
                    paramList
                    ["(seller:Address, seller2:Address)"]
                    (Just
                        [ Param { pName = "seller", pType = Just $ TyIdent "Address" }
                        , Param { pName = "seller2", pType = Just $ TyIdent "Address" }
                        ]
                    )
                ]
            describe "parameters with one type" $ checkExamples
                [ mkParseTest
                    paramList
                    ["(seller:Address, seller2)"]
                    (Just
                        [ Param { pName = "seller", pType = Just $ TyIdent "Address" }
                        , Param { pName = "seller2", pType = Nothing }
                        ]
                    )
                ]
    describe "Expressions" $ do
        checkExamples
            [ mkParseTest
                expr
                ["2"]
                (Just $ ExLiteral $ LitNat 2)
            , mkParseTest
                expr
                ["\"abc\""]
                (Just $ ExLiteral $ LitStr "abc")
            , mkParseTest
                expr
                ["0xABC123"]
                (Just $ ExLiteral $ LitNat 11256099)
            , mkParseTest
                expr
                ["\"\\x63\\X4A\""]
                (Just $ ExLiteral $ LitStr "cJ")
            ]
        describe "With curly braces" $ checkExamples
            [ mkParseTest
                expr
                ["{}"]
                (Just $ ExRecord [])
            , mkParseTest
                expr
                ["{ x }"]
                (Just $ ExIdent "x")
            , mkParseTest
                expr
                ["{ x: 1 }"]
                (Just $ ExRecord [("x", ExLiteral $ LitNat 1)])
            , mkParseTest
                expr
                ["{ x; 1 }"]
                (Just $ ExBody
                    [ StExpr $ ExIdent "x" ]
                    (ExLiteral $ LitNat 1)
                )
            -- TODO: if/when we add record-field shorthand, it would look like this:
            --
            -- mkParseTest
            --  expr
            --  ["{ x, y }"]
            --  (Just $ ExRecord
            --      [ ("x", ExIdent "x")
            --      , ("y", ExIdent "y")
            --      ]
            --  )
            , mkParseTest
                expr
                ["{ x: 1, y: 2 }"]
                (Just $ ExRecord
                    [ ("x", ExLiteral $ LitNat 1)
                    , ("y", ExLiteral $ LitNat 2)
                    ]
                )
            ]
    describe "Statements" $ checkExamples
        [ mkParseTest
            stmt
            ["let a = 1"]
            (Just $ StLet "a" (ExLiteral $ LitNat 1))
        , mkParseTest
            stmt
            ["("]
            Nothing
        ]
    describe "Whole programs" $ do
        describe "valid examples" $ checkExamples
            [ mkParseTest
                program
                [ "let publishHello = () => {\n"
                , "};"
                ]
                (Just
                    [ StLet "publishHello" $ ExLambda Function
                        { fParams = []
                        , fBody = ExRecord []
                        }
                    ]
                )
            , mkParseTest
                program
                [ "let publishHello = (seller:Address,price) => {\n"
                , "};"
                ]
                (Just
                    [ StLet "publishHello" $ ExLambda Function
                        { fParams =
                            [ Param { pName = "seller", pType = Just (TyIdent "Address") }
                            , Param { pName = "price", pType = Nothing }
                            ]
                        , fBody = ExRecord []
                        }
                    ]
                )
            , mkParseTest
                program
                [ "let a = 1;\n"
                , "// some comments followed by newline\n"
                , "/* block comment\n"
                , "*/\n"
                ]
                (Just
                    [ StLet "a" (ExLiteral (LitNat 1))
                    ]
                )
            , mkParseTest
                program
                [ "let a = 1;\n"
                , "let b = 2;"
                ]
                (Just
                    [ StLet "a" (ExLiteral (LitNat 1))
                    , StLet "b" (ExLiteral (LitNat 2))
                    ]
                )
            ]
        describe "Invalid examples" $ checkExamples
            [ mkParseTest
                program
                [ "let a = 1;\n"
                , "("
                ]
                Nothing
            , mkParseTest
                program
                [ "let a = 1;\n"
                , "// invalid statement:\n"
                , "("
                ]
                Nothing
            ]

mkParseTest :: (Show a, Eq a) => Parser a -> [LT.Text] -> Maybe a -> Expectation
mkParseTest p input result =
    case (runParser p "example" (mconcat input), result) of
        (Left _, Nothing) -> pure () -- failed, as expected.
        (Left e, Just _) -> error $ "parse error: " <> errorBundlePretty e
        (Right actual, Just expected) -> actual `shouldBe` expected
        (Right v, Nothing) ->
            error $ "Unxpected successful parse: " <> show v

checkExamples :: [Expectation] -> Spec
checkExamples es =
    for_ (zip [1::Int ..] es) $ \(i, example) ->
        it ("should handle example #" <> show i <> " correctly.") example
