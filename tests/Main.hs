module Main (main) where

import Glow.Prelude

import qualified Tests.Parser
import qualified Tests.Runtime.Interaction

import Test.Hspec

main :: IO ()
main = hspec $ do
    Tests.Parser.tests
    Tests.Runtime.Interaction.tests
