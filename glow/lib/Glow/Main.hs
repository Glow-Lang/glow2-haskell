module Glow.Main (main) where

import qualified Glow.Gerbil.ImportSExpr as ISExp
import Glow.Gerbil.ParseCommon (parseTypeTable)
import Glow.Gerbil.ParseProject (extractPrograms)
import Glow.Prelude
import Text.Show.Pretty (pPrint)

-- Example:
-- cabal run glow ../../glow/glow ../../glow/dapps/buy_sig.glow

main :: IO ()
main = do
  args <- getArgs
  case args of
    [exe, file] -> do
      fed <-
        ISExp.frontEndData
          ISExp.FrontEndParams
            { ISExp.fepExePath = exe,
              ISExp.fepFile = file
            }
      case fed of
        Left e -> putStrLn (ISExp.formatError e)
        Right v -> do
          pPrint v
          pPrint (extractPrograms (ISExp.fedProject v))
          pPrint (ISExp.fedAnf v)
          pPrint (parseTypeTable (ISExp.oSExpr (ISExp.fedTypeTable v)))
    _ -> do
      putStrLn "Usage: glow <path/to/glow/frontend> <source-file.glow>"
      exitFailure
