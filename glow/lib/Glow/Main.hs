module Glow.Main (main) where

import qualified Glow.Gerbil.ImportSExpr as ISExp
import Glow.Gerbil.Parser (extractPrograms)
import Glow.Prelude
import Text.Show.Pretty (pPrint)

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
    _ -> do
      putStrLn "Usage: glow <path/to/glow/frontend> <source-file.glow>"
      exitFailure
