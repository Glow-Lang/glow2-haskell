module Main (main) where

import Glow.Prelude

import qualified Tests.Parser

import Test.Hspec

main :: IO ()
main = hspec $ do
    Tests.Parser.tests
