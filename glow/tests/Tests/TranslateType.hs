module Tests.TranslateType where

import qualified Glow.Ast.LastLeg as GAL
import Glow.Gerbil.Types as GGT
import Glow.Prelude
import Glow.Translate.GerbilTypeToLastLeg
import Test.Hspec

tests = describe "Glow.Translate.GerbilTypeToLastLeg.translateType" $ do
  it ("Should handle nested-tuple trees for N = [0..(N-1)] representations") $ do
    translateType (TyTuple []) `shouldBe` GAL.TTuple []
    translateType (TyTuple [TyTuple []]) `shouldBe` GAL.TTuple [GAL.TTuple []]
    translateType (TyTuple [TyTuple [], TyTuple [TyTuple []]])
      `shouldBe` GAL.TTuple [GAL.TTuple [], GAL.TTuple [GAL.TTuple []]]
    translateType (TyTuple [TyTuple [], TyTuple [TyTuple []], TyTuple [TyTuple [], TyTuple [TyTuple []]]])
      `shouldBe` GAL.TTuple [GAL.TTuple [], GAL.TTuple [GAL.TTuple []], GAL.TTuple [GAL.TTuple [], GAL.TTuple [GAL.TTuple []]]]
  it ("Should handle function types") $ do
    translateType (TyArrow [] (TyTuple [])) `shouldBe` GAL.TFunc (GAL.FuncType [] (GAL.TTuple []) False)
    translateType (TyArrow [TyTuple []] (TyTuple [])) `shouldBe` GAL.TFunc (GAL.FuncType [GAL.TTuple []] (GAL.TTuple []) False)
    translateType (TyArrow [TyTuple [], TyArrow [TyTuple []] (TyTuple [])] (TyTuple [])) `shouldBe` GAL.TFunc (GAL.FuncType [GAL.TTuple [], GAL.TFunc (GAL.FuncType [GAL.TTuple []] (GAL.TTuple []) False)] (GAL.TTuple []) False)
