module Glow.Main (main) where

import qualified Glow.Gerbil.ImportSExpr as ISExp
import Glow.Gerbil.ParseProject (extractPrograms)
import Glow.Ast.Common
import Glow.Prelude
import Text.Show.Pretty (pPrint)
import Glow.Precompiled.Base

import Glow.Runtime.Interaction.Sim

import Glow.Consensus.Lurk

import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey))

import Glow.Runtime.Lurk.Commands
import Glow.Runtime.Interaction.InteractWithServer
import Glow.Runtime.Interaction.Interact
import Data.UUID as UUID
import Data.Maybe (fromJust)

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
          pPrint (ISExp.fedTypeTable v)
          pPrint (precompile $ v)
    _ -> do
      putStrLn "Usage: glow <path/to/glow/frontend> <source-file.glow>"
      exitFailure


mainCF :: IO ()
mainCF = do
  let args = ["glow" , "/Users/Marcin/glow/dapps/coin_flip.glow"]
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
          case precompile $ v of
             Left err -> error err
             Right pcc -> runScriptedSim pcc
               [GLNat 10, GLNat 2]
               [(Id "A" , (LedgerPubKey "Aaa" , []))
               ,(Id "B" , (LedgerPubKey "Bbb" , []))] >>= traverse_ putStrLn
          -- pPrint v
          -- pPrint (extractPrograms (ISExp.fedProject v))
          -- pPrint (ISExp.fedAnf v)
          -- pPrint (ISExp.fedTypeTable v)
          -- pPrint (precompile $ v)
    _ -> do
      putStrLn "Usage: glow <path/to/glow/frontend> <source-file.glow>"
      exitFailure

-- deployTest :: IO ()
-- deployTest = do
--   let args = ["glow" , "/Users/Marcin/glow/dapps/coin_flip.glow"]
--   case args of
--     [exe, file] -> do
--       fed <-
--         ISExp.frontEndData
--           ISExp.FrontEndParams
--             { ISExp.fepExePath = exe,
--               ISExp.fepFile = file
--             }
--       case fed of
--         Left e -> putStrLn (ISExp.formatError e)
--         Right v -> do
--           case precompile $ v of
--              Left err -> error err
--              Right pcc -> deployContract pcc
--                [GLNat 10, GLNat 2]
--                [(Id "A" , (LedgerPubKey "A" ))
--                ,(Id "B" , (LedgerPubKey "B" ))] >>= (putStrLn . show)
--           -- pPrint v
--           -- pPrint (extractPrograms (ISExp.fedProject v))
--           -- pPrint (ISExp.fedAnf v)
--           -- pPrint (ISExp.fedTypeTable v)
--           -- pPrint (precompile $ v)
--     _ -> do
--       putStrLn "Usage: glow <path/to/glow/frontend> <source-file.glow>"
--       exitFailure


-- interactTest :: IO ()
-- interactTest = do
--   let args = ["glow" , "/Users/Marcin/glow/dapps/coin_flip.glow"]
--   case args of
--     [exe, file] -> do
--       fed <-
--         ISExp.frontEndData
--           ISExp.FrontEndParams
--             { ISExp.fepExePath = exe,
--               ISExp.fepFile = file
--             }
--       case fed of
--         Left e -> putStrLn (ISExp.formatError e)
--         Right v -> do
--           case precompile $ v of
--              Left err -> error err
--              Right pcc -> runInteractionWithServer
--               (LocalInteractEnv
--                  pcc
--                  (LedgerPubKey "B" )
--                  "B"
--                  [GLNat 10, GLNat 2]) (fromJust $ UUID.fromString "85b3e4d2-9a48-457d-982a-936a1e846b81") 
--           -- pPrint v
--           -- pPrint (extractPrograms (ISExp.fedProject v))
--           -- pPrint (ISExp.fedAnf v)
--           -- pPrint (ISExp.fedTypeTable v)
--           -- pPrint (precompile $ v)
--     _ -> do
--       putStrLn "Usage: glow <path/to/glow/frontend> <source-file.glow>"
--       exitFailure

