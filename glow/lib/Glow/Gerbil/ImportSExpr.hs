-- | This module handles importing data from the gerbil scheme frontend,
-- via the s-expression format emitted by @glow pass@.
module Glow.Gerbil.ImportSExpr
  ( FrontEndError (..),
    FrontEndParams (..),
    FrontEndData (..),
    Output (..),
    frontEndData,
    formatError,
  )
where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Glow.Gerbil.ParseAnf as ParseAnf
import Glow.Gerbil.ParseProject as ParseProject
import qualified Glow.Gerbil.Types as GT
import Glow.Prelude hiding (many)
import qualified System.Process.Typed as P
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.SExpression as SExpr

-- | An entry in the output of @glow pass@. This consists of a file name (on the first
-- line) and an s-expression.
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

-- | Parameters for invoking the glow frontend.
data FrontEndParams = FrontEndParams
  { -- | Path to the glow frontend executable.
    fepExePath :: FilePath,
    -- | Glow source file to process.
    fepFile :: FilePath
  }
  deriving (Show, Eq)

-- | Data extracted from the frontend.
data FrontEndData = FrontEndData
  { -- | Output of @glow pass project@
    fedProject :: [GT.ProjectStatement],
    -- | Output of @glow pass anf@
    fedAnf :: [GT.AnfStatement],
    -- | Type table, extracted from the output of @glow pass method-resolve@.
    -- TODO: parse this into a more strongly typed form, rather than just
    -- keeping it as an s-expression.
    fedTypeTable :: Output
  }
  deriving (Show, Eq)

data FrontEndError
  = SexpParseError (ParseErrorBundle String Void)
  deriving (Show, Eq)

formatError :: FrontEndError -> String
formatError (SexpParseError e) = errorBundlePretty e

-- | Import data from the frontend.
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
                parseOutputs
                ("glow pass " <> passName)
                input
        pure $ case parseResult of
          Left e -> Left (SexpParseError e)
          Right v -> Right v
  project <- pass "project"
  anf <- pass "anf"
  methodResolve <- pass "method-resolve"
  pure $ do
    projectStmtss <- map (ParseProject.parseModule . oSExpr) <$> project
    anfStmtss <- map (ParseAnf.parseModule . oSExpr) <$> anf
    resolve <- methodResolve
    case (projectStmtss, anfStmtss, resolve) of
      ([projectStmts], [anfStmts], (_ : types : _)) ->
        Right
          FrontEndData
            { fedProject = projectStmts,
              fedAnf = anfStmts,
              fedTypeTable = types
            }
      _ -> error ("wrong number of outputs: " <> show (length projectStmtss, length anfStmtss, length resolve))
