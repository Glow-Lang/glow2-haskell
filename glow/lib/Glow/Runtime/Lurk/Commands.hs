{-# LANGUAGE TupleSections #-}
module Glow.Runtime.Lurk.Commands where


import Prelude
import Glow.Precompiled.Base

import Data.UUID

import Glow.Consensus.Lurk
import Control.Monad
import Control.Monad.IO.Class

import Glow.Ast.Common
import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey))

import qualified Data.Text.Lazy as T

import qualified Data.Map as M

import Control.Lens

import qualified Data.Aeson as A

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import qualified Data.ByteString.Char8 as B
-- LMEnv


parametersPrompt :: ([(String , GLType)]) -> IO [(String , GLValue)]
parametersPrompt = traverse $ \(pName , pTy) -> do  
  putStrLn $ "Enter value of param " ++ pName ++ " (" ++ show pTy ++ ")"
  (pName,)  <$> case pTy of
                     GLNatT -> GLNat <$> readLn
                     GLBoolT -> GLBool <$> readLn
                     GLPFT  -> GLPF <$> readLn
                     DigestT  -> DigestOf <$> readLn
                     GLUnitT -> return GLUnit

participantsPrompt :: ([Id]) -> IO [(Id , LedgerPubKey)]
participantsPrompt = traverse $ \(roleName) -> do  
  putStrLn $ "Enter pubKey for role " ++ B.unpack (idBS roleName) 
  (roleName,)  <$> LedgerPubKey . B.pack <$> getLine
                   

deployContract :: PrecompiledContract -> [GLValue] -> [(Id , LedgerPubKey)] -> IO (Either String UUID)
deployContract pc params ptps' = do
  let ptps = M.fromList ptps'
      lme =
          LMEnv
            ([ ptps M.! (Id pn) | pn <- pc ^. pcParticipantNames])
            params
            (T.pack $ pc ^. pcVerifier)

  manager <- newManager defaultManagerSettings

  initialRequest <- parseRequest "http://localhost:3000/deploy"
  let request = initialRequest { method = "POST", requestBody = RequestBodyLBS $ A.encode lme }

  response <- httpLbs request manager

  case (A.decode (responseBody response) :: Maybe (Either String UUID)) of
     Nothing -> return (Left (show (responseBody response)))
     Just x -> return x
      
