module Tests.Parser where

import Glow.Ast
import Glow.Parser
import Glow.Prelude

import Text.Megaparsec (errorBundlePretty, runParser)

import Test.Hspec

tests = describe "Glow.Parser" $ do
    for_ (zip [1::Int ..] programExamples) $ \(i, (input, expected)) ->
        it ("should parse example program #" <> show i <> " correctly.") $ do
            case runParser program "example" input of
                Left e -> error $ "parse error: " <> errorBundlePretty e
                Right actual ->
                    actual `shouldBe` expected

programExamples =
    [ ( mconcat
            [ "let publishHello = () => {\n"
            , "};"
            ]
      , [ StLet "publishHello" $ ExLambda Function
            { fParams = []
            , fBodyStmts = []
            , fBodyExpr = ExRecord []
            }
        ]
      )
    ]
