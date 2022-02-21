module Glow.Main (main) where

import qualified Data.Text.Lazy as LT
import Glow.Parser (program)
import Glow.Prelude
import Text.Megaparsec (parseTest)

main :: IO ()
main = do
  input <- getContents
  parseTest program (LT.pack input)
