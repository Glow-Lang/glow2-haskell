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

import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Map.Strict as Map (Map, empty)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Glow.Gerbil.Fresh as Fresh
import Glow.Gerbil.ParseAnf as ParseAnf
import Glow.Gerbil.ParseCommon (parseTypeTable)
import Glow.Gerbil.ParseProject as ParseProject
import Glow.Gerbil.Types as GT
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
    <$> parseLine
    <*> SExpr.parseSExpr SExpr.def

parseLine :: Parser String
parseLine = takeWhileP Nothing (/= '\n') <* char '\n'

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
    -- | Type table, extracted from the finaltypetable output of @glow pass project@.
    fedTypeTable :: Map BS.ByteString Type,
    -- | UnusedTable, marking which variables are used/unused in the input s-expressions
    fedUnusedTable :: Fresh.UnusedTable
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
              T.unpack $ decodeUtf8 (BS.toStrict bytes)
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
  pure $ do
    projectoSEs <- map oSExpr <$> project
    anfoSEs <- map oSExpr <$> anf
    case (projectoSEs, anfoSEs) of
      ([projectStmts, types], [anfStmts]) ->
        Right
          FrontEndData
            { fedProject = ParseProject.parseModule projectStmts,
              fedAnf = ParseAnf.parseModule anfStmts,
              fedTypeTable = parseTypeTable types,
              fedUnusedTable = execState (mapM_ Fresh.markAtomsUsed (anfoSEs <> projectoSEs)) Map.empty
            }
      _ -> error ("wrong number of outputs: " <> show (length projectoSEs, length anfoSEs))
