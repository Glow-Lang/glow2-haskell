module Glow.Main (main) where

import Glow.Prelude

import Glow.Parser     (program)
import Text.Megaparsec (parseTest)

import qualified Data.Text.Lazy as LT

main :: IO ()
main = do
    input <- getContents
    parseTest program (LT.pack input)
