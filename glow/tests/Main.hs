module Main (main) where

import Glow.Prelude
import Test.Hspec
import qualified Tests.AstCommon
import qualified Tests.Lurk
import qualified Tests.Parser
import qualified Tests.Runtime.Interaction

main :: IO ()
main = hspec $ do
  Tests.AstCommon.tests
  Tests.Parser.tests
  Tests.Runtime.Interaction.tests
  Tests.Lurk.tests
