module Tests.Ast.Common where

import qualified Glow.Ast.Common
import Glow.Prelude
import Test.Hspec

tests = describe "Glow.Ast.Common.bitLength" $ do
  let bitLength = (Glow.Ast.Common.bitLength :: Integer -> Integer)
  it ("Should generate two’s complement bit lengths without leading 0s/1s") $ do
    bitLength 0 `shouldBe` 0
    bitLength 1 `shouldBe` 1
    bitLength 2 `shouldBe` 2
    bitLength 3 `shouldBe` 2
    bitLength 4 `shouldBe` 3
    bitLength 7 `shouldBe` 3
    bitLength 8 `shouldBe` 4
    bitLength 15 `shouldBe` 4
    bitLength 16 `shouldBe` 5
    bitLength 31 `shouldBe` 5
    bitLength 32 `shouldBe` 6
    bitLength 63 `shouldBe` 6
    bitLength 64 `shouldBe` 7
    bitLength 127 `shouldBe` 7
    bitLength 128 `shouldBe` 8
    bitLength 255 `shouldBe` 8
    bitLength 256 `shouldBe` 9
    bitLength (-1) `shouldBe` 0
    bitLength (-2) `shouldBe` 1
    bitLength (-3) `shouldBe` 2
    bitLength (-4) `shouldBe` 2
    bitLength (-5) `shouldBe` 3
    bitLength (-8) `shouldBe` 3
    bitLength (-9) `shouldBe` 4
    bitLength (-16) `shouldBe` 4
    bitLength (-17) `shouldBe` 5
    bitLength (-32) `shouldBe` 5
    bitLength (-33) `shouldBe` 6
    bitLength (-64) `shouldBe` 6
    bitLength (-65) `shouldBe` 7
    bitLength (-128) `shouldBe` 7
    bitLength (-129) `shouldBe` 8
    bitLength (-256) `shouldBe` 8
    bitLength (-257) `shouldBe` 9
