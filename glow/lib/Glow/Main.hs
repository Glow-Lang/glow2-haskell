module Glow.Main (main) where

-- import qualified Data.Text.Lazy as LT
-- import Glow.Parser (program)

import Glow.Gerbil.Parser (parseModule)
import Glow.Prelude
import Text.Megaparsec (errorBundlePretty, runParser)
import qualified Text.SExpression as SExpr
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  input <- getContents

  -- `glow pass` puts the name of the file at the top of the output; strip
  -- it off:
  let trimmedInput = case lines input of
        [] -> error "empty input"
        (_ : xs) -> unlines xs

  case runParser (SExpr.parseSExpr SExpr.def) "glow pass" trimmedInput of
    Right sexpr ->
      pPrint (parseModule sexpr)
    Left e -> do
      putStrLn (errorBundlePretty e)
      exitFailure
