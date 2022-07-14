{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Glow.Precompiled.Base where

import Glow.Gerbil.ImportSExpr
import Glow.Gerbil.Types
import Prelude
import Glow.Ast.Common
import Control.Lens (makeLenses)
import Glow.Precompiled.Mark
import qualified Data.ByteString as BS
import Glow.Precompiled.LMonad
import Glow.Precompiled.LMonadToLurk
import Data.Map.Strict (Map,(!))
-- import Data.Traversable
import Data.ByteString.Char8 (unpack)
import Glow.Runtime.Interaction.InterpretAnf (gLTypeToType)
import qualified Glow.Consensus.Lurk as GCL

data PrecompiledContract = PrecompiledContract
    { _pcParticipantNames :: [BS.ByteString]
    , _pcParamsIds :: [BS.ByteString]
    , _pcAnf :: [AnfStatement Int]
    , _pcVerifier :: String
    , _pcTypeTable :: Map BS.ByteString Type
    }
  deriving (Show)

makeLenses ''PrecompiledContract


paramsWithTypes :: PrecompiledContract -> [(String, GCL.GLType)]
paramsWithTypes pc =
  [ (unpack pcn , gLTypeToType ((_pcTypeTable pc) ! pcn)  ) | pcn <- (_pcParamsIds pc)]
  


precompile :: FrontEndData -> Either String PrecompiledContract
precompile fed =
  case fedAnf fed of
    [DefineInteraction _ (AnfInteractionDef pNames _ aNames stmnts)
     , Return (TrvExpr (TrexConst CUnit))] -> do
      
         (_ , y) <- markANF 1 stmnts
         z <- mkVerifier aNames <$> (toLMonad y)
         return (PrecompiledContract (map idBS pNames) (map idBS aNames) y z (fedTypeTable fed)) 
    _ -> Left "not implemented: multiple interactions"
    
  



-- coinFlipPC :: PrecompiledContract
-- coinFlipPC = ?

