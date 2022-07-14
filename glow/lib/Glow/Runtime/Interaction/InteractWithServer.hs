{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Glow.Runtime.Interaction.InteractWithServer where

import Glow.Consensus.Lurk

import Control.Monad.RWS
-- import System.IO

import Control.Lens (makeLenses,Getter,to,(^.),(%=),(.=),use,(.~),(%~),Lens',Lens)
-- import Data.Function

import Prelude

import Glow.Precompiled.Base

import Glow.Runtime.Interaction.InterpretAnf

import Glow.Ast.Common

import Data.Proxy

import Data.List as L

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromJust,catMaybes,isJust,mapMaybe,isNothing)

import Data.Functor ((<&>))

import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey))
-- import Text.SExpression as S
import qualified Data.Text as T
-- import qualified Data.Text.Lazy.IO as TIO
import Text.Read

-- import Glow.Consensus.Lurk
-- import qualified GHC.Generics as G
-- import Data.Aeson (ToJSON,FromJSON)
-- import Data.Aeson.Types (parseEither)
-- import qualified Data.Aeson as A
-- import qualified System.Process as P
-- import Text.RawString.QQ
import Data.UUID
-- import Data.UUID.V4

import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BS

import Glow.Runtime.Interaction.Interact

import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
    
data WithServerIEnv = WithServerIEnv
     { _wsieLocalInteractEnv :: LocalInteractEnv
     , _wsieUUID :: UUID
     , _wsieLastCall :: Maybe Call
     -- , _wsieHost :: String
     -- , _wsiePort :: Int 
     }
makeLenses ''WithServerIEnv


type CmdLineWithServer = RWST WithServerIEnv () LocalState IO

instance ParticipantM CmdLineWithServer LocalState where
  env = asks (^. wsieLocalInteractEnv)

  localState _ = id

  getConsensusState = do 
    cId <- asks (^. wsieUUID)
    e <- env
    liftIO $
        runReq defaultHttpConfig $ do
            (x :: JsonResponse (Maybe LMState)) <- req
                            GET (http "localhost" /: "contractState" /: (T.pack (toString cId) ))
                            NoReqBody  (jsonResponse) (port 3000)
            case (responseBody x) of
                Nothing -> error "unable to get consensus state!"
                Just y -> return y

  sendCall c = do 
    cId <- asks (^. wsieUUID)
    liftIO $
        runReq defaultHttpConfig $ void $ do
            req POST (http "localhost" /: "interact" /: (T.pack (toString cId) ))
                            (ReqBodyJson c) ignoreResponse (port 3000)


  onWait = void $ liftIO getLine

  onError s = error s

  afterCall cl = do
    void $ liftIO getLine
    -- lc <- asks (^. wsieLastCall)
    -- if (lc == Just cl)
    -- then liftIO $ ((putStrLn "call failed, press enter to repeat") >> void getLine)
    -- else (return ())

  promptInput msg ty = do
    liftIO $ do putStrLn (show msg)
                putStrLn (show ty)
    liftIO $ case ty of
                     GLNatT -> GLNat <$> readLn
                     GLBoolT -> GLBool <$> readLn
                     GLPFT  -> GLPF <$> readLn
                     DigestT  -> DigestOf <$> readLn
                     GLUnitT -> return GLUnit
    -- case readMaybe l of
    --   Nothing -> promptInput msg ty
    --   Just x -> return x
    
        
runInteractionWithServer :: LocalInteractEnv -> UUID -> IO ()
runInteractionWithServer lie cid =
  void (runRWST (runInteraction (Proxy :: Proxy LocalState) (Proxy :: Proxy CmdLineWithServer))
         (WithServerIEnv lie cid Nothing) initialLocalState)
  
