module Glow.Main (main) where

-- import qualified Data.Text.Lazy as LT
-- import Glow.Parser (program)

import Glow.Gerbil.Parser (parseModule)
import Glow.Prelude
import Text.Megaparsec (parseTest)
import Text.SExpression (def, parseSExpr)

main :: IO ()
main = do
  input <- getContents
  parseTest (parseModule <$> parseSExpr def) input
