module Glow.Gerbil.ImportSExpr where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Void (Void)
import Glow.Prelude hiding (many)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.SExpression as SExpr

data Output = Output
  { oFile :: FilePath,
    oSExpr :: SExpr.SExpr
  }
  deriving (Show, Eq)

type Parser = Parsec Void String

parseOutputs :: Parser [Output]
parseOutputs = many parseOutput

parseOutput :: Parser Output
parseOutput =
  Output
    <$> (takeWhileP Nothing (/= '\n') <* char '\n')
    <*> SExpr.parseSExpr SExpr.def
