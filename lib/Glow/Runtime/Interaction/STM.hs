-- | STM-based implementations of interfaces from "Glow.Runtime.Interaction"
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Glow.Runtime.Interaction.STM
    ( STMInteraction
    , STMHandle
    , STMServer
    , STMParticipantId
    , newServer
    , newHandle
    ) where

import Glow.Prelude
import Glow.Runtime.Interaction

import Control.Concurrent.Classy

type MsgChan m d = TChan (STM m) (MessageWithParticipant (STMInteraction d))

data STMInteraction d

instance Interaction (STMInteraction d) where
    type ParticipantId (STMInteraction d) = STMParticipantId
    type Data (STMInteraction d) = d

data STMHandle m d = STMHandle
    { hSubmitChan  :: MsgChan m d
    , hListenChan  :: MsgChan m d
    , hParticipant :: STMParticipantId
    }

newtype STMParticipantId
    = STMParticipantId Integer
    deriving(Show, Eq, Ord)

data STMServer m d = STMServer
    { sNextParticipant :: TVar (STM m) Integer
    , sEmitChan        :: MsgChan m d
    , sReceiveChan     :: MsgChan m d
    }

newServer :: MonadConc m => STM m (STMServer m d)
newServer = do
    nextId <- newTVar 0
    emit <- newBroadcastTChan
    receive <- newTChan
    pure STMServer
        { sNextParticipant = nextId
        , sEmitChan = emit
        , sReceiveChan = receive
        }

newParticipantId :: MonadConc m => STMServer m d -> STM m STMParticipantId
newParticipantId s = do
    let tvar = sNextParticipant s
    result <- readTVar tvar
    writeTVar tvar $! result + 1
    pure (STMParticipantId result)

newHandle :: MonadConc m => STMServer m d -> STM m (STMHandle m d)
newHandle s = do
    p <- newParticipantId s
    listen <- dupTChan (sEmitChan s)
    pure STMHandle
        { hSubmitChan = sReceiveChan s
        , hListenChan = listen
        , hParticipant = p
        }

instance MonadConc m => Handle (STMHandle m d) (STMInteraction d) where
    type HandleM (STMHandle m d) = m

    myParticipantId = hParticipant
    submit h msg = atomically $
        writeTChan (hSubmitChan h) MessageWithParticipant
            { mwpMessage = msg
            , mwpParticipant = hParticipant h
            }
    listenNext h = atomically $ readTChan (hListenChan h)

instance MonadConc m => ConsensusServer (STMServer m d) (STMInteraction d) where
    type ServerM (STMServer m d) = m

    receive s = atomically $ readTChan (sReceiveChan s)
    emit s = atomically . writeTChan (sEmitChan s)
