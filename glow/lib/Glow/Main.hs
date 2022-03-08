module Glow.Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import qualified Glow.Gerbil.ImportSExpr as ImportSExpr
import Glow.Gerbil.Parser (parseModule)
import qualified Glow.Gerbil.Types as GT
import Glow.Prelude
import qualified System.Process.Typed as P
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)
import qualified Text.SExpression as SExpr
import Text.Show.Pretty (pPrint)

data FrontEndParams = FrontEndParams
  { fepExePath :: FilePath,
    fepFile :: FilePath
  }
  deriving (Show, Eq)

data FrontEndData = FrontEndData
  { fedProject :: [GT.Statement],
    fedTypeTable :: ImportSExpr.Output
  }
  deriving (Show, Eq)

data FrontEndError
  = SexpParseError (ParseErrorBundle String Void)
  deriving (Show, Eq)

formatError :: FrontEndError -> String
formatError (SexpParseError e) = errorBundlePretty e

frontEndData :: FrontEndParams -> IO (Either FrontEndError FrontEndData)
frontEndData params = do
  let pass passName = do
        bytes <-
          P.proc (fepExePath params) ["pass", passName, fepFile params]
            & P.readProcessStdout_
        let input =
              T.unpack $ decodeUtf8 (LBS.toStrict bytes)
            parseResult =
              runParser
                ImportSExpr.parseOutputs
                ("glow pass " <> passName)
                input
        pure $ case parseResult of
          Left e -> Left (SexpParseError e)
          Right v -> Right v
  project <- pass "project"
  methodResolve <- pass "method-resolve"
  pure $ do
    stmtss <- map (parseModule . ImportSExpr.oSExpr) <$> project
    resolve <- methodResolve
    case (stmtss, resolve) of
      ([stmts], (_ : types : _)) ->
        Right
          FrontEndData
            { fedProject = stmts,
              fedTypeTable = types
            }
      _ -> error ("wrong number of outputs: " <> show (length stmtss, length resolve))

main :: IO ()
main = do
  [exe, file] <- getArgs
  fed <-
    frontEndData
      FrontEndParams
        { fepExePath = exe,
          fepFile = file
        }
  case fed of
    Left e -> putStrLn (formatError e)
    Right v -> pPrint v
