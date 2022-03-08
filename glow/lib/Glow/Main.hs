module Glow.Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Void (Void)
import qualified Glow.Gerbil.ImportSExpr as ISExp
import Glow.Gerbil.Parser (parseModule)
import Glow.Prelude
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)
import qualified Text.SExpression as SExpr
import Text.Show.Pretty (pPrint)

main :: IO ()
main = do
  [exe, file] <- getArgs
  fed <-
    ISExp.frontEndData
      ISExp.FrontEndParams
        { ISExp.fepExePath = exe,
          ISExp.fepFile = file
        }
  case fed of
    Left e -> putStrLn (ISExp.formatError e)
    Right v -> pPrint v
