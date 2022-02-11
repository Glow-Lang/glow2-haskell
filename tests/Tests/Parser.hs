module Tests.Parser where

import Glow.Ast
import Glow.Parser
import Glow.Prelude

import qualified Data.Text.Lazy  as LT
import           Text.Megaparsec (errorBundlePretty, runParser)

import Test.Hspec

tests = describe "Glow.Parser" $ do
    describe "function parameters" $ do
        describe "individual parameters" $ checkExamples
            [
            -- Invalid
              mkParseTest
                fnParam
                [")"]
                Nothing
            , mkParseTest
                fnParam
                ["9"]
                Nothing
            -- Identifer
            , mkParseTest
                fnParam
                ["seller"]
                (Just Param
                    { pName = "seller"
                    , pType = Nothing
                    }
                )
            -- Identifier:Type
            , mkParseTest
                fnParam
                ["seller:Address"]
                (Just Param
                    { pName = "seller"
                    , pType = Just (TyIdent "Address")
                    }
                )
            ]
        describe "parameter lists" $ checkExamples
            [
            -- No Parameters
              mkParseTest
                paramList
                ["()"]
                (Just [])
            -- parameters
            , mkParseTest
                paramList
                ["(seller, seller2)"]
                (Just
                    [ Param { pName = "seller", pType = Nothing }
                    , Param { pName = "seller2", pType = Nothing }
                    ]
                )
            -- parameters with types
            , mkParseTest
                paramList
                ["(seller:Address, seller2:Address)"]
                (Just
                    [ Param { pName = "seller", pType = Just $ TyIdent "Address" }
                    , Param { pName = "seller2", pType = Just $ TyIdent "Address" }
                    ]
                )
            -- parameters with one type
            , mkParseTest
                paramList
                ["(seller:Address, seller2)"]
                (Just
                    [ Param { pName = "seller", pType = Just $ TyIdent "Address" }
                    , Param { pName = "seller2", pType = Nothing }
                    ]
                )
            ]
    describe "Whole programs" $ checkExamples
        [ mkParseTest
            program
            [ "let publishHello = () => {\n"
            , "};"
            ]
            (Just
                [ StLet "publishHello" $ ExLambda Function
                    { fParams = []
                    , fBodyStmts = []
                    , fBodyExpr = ExRecord []
                    }
                ]
            )
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
