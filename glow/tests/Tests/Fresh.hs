module Tests.Fresh where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Glow.Gerbil.Fresh
import Glow.Prelude
import Text.SExpression
import Test.Hspec

tests = describe "Glow.Gerbil.Fresh" $ do
  it ("Should generate fresh names when given duplicate names") $ do
    runState (fresh "a") Map.empty `shouldBe` ("a", Map.fromList [("a",[0])])
    runState (mapM fresh ["a", "a", "a", "a"]) Map.empty `shouldBe` (["a","a0","a1","a2"], Map.fromList [("a",[3])])
  it ("Should not break on names with 0 at the end") $ do
    runState (fresh "a0") Map.empty `shouldBe` ("a0", Map.fromList [("a",[1])])
    runState (mapM fresh ["a", "a0", "a", "a0"]) Map.empty `shouldBe` (["a","a0","a1","a2"], Map.fromList [("a",[3])])
    runState (mapM fresh ["a0", "a", "a0", "a"]) Map.empty `shouldBe` (["a0","a1","a2","a3"], Map.fromList [("a",[4])])
  it ("Should prioritize given numbered names when not taken") $ do
    runState (fresh "a5") Map.empty `shouldBe` ("a5", Map.fromList [("a",[0, 1, 2, 3, 4, 6])])
    runState (mapM fresh ["a5", "a", "a", "a"]) Map.empty `shouldBe` (["a5","a0","a1","a2"], Map.fromList [("a",[3,4,6])])
  it ("Should mark all names as used within an SExpr") $ do
    execState (markAtomsUsed (List [Atom "a", List [Atom "b", Atom "c"], Atom "d"])) Map.empty `shouldBe` Map.fromList [("a",[0]), ("b",[0]), ("c",[0]), ("d",[0])]
