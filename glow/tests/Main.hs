module Main (main) where

import Glow.Prelude
import Test.Hspec
import qualified Tests.Ast.Common
import qualified Tests.Fresh
import qualified Tests.FunctionLift
import qualified Tests.Lurk
import qualified Tests.Parser
import qualified Tests.Runtime.Interaction

main :: IO ()
main = hspec $ do
  Tests.Ast.Common.tests
  Tests.Parser.tests
  Tests.Runtime.Interaction.tests
  Tests.Fresh.tests
  Tests.FunctionLift.tests
  Tests.Lurk.tests
