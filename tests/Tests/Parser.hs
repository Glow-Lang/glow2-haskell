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
