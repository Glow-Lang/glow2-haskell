-- | In-memory implementations of interfaces from "Glow.Runtime.Interaction"
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Glow.Runtime.Interaction.InMemory
    ( InMemoryHandle
    , InMemoryServer
    , InMemoryParticipantId
    , newServer
    , newHandle
    ) where

import Glow.Prelude
import Glow.Runtime.Interaction

import Control.Concurrent.Classy

type MsgChan m d = TChan (STM m) (MessageWithParticipant (InMemoryHandle m d))

data InMemoryHandle m d = InMemoryHandle
    { imhSubmitChan  :: MsgChan m d
    , imhListenChan  :: MsgChan m d
    , imhParticipant :: InMemoryParticipantId
    }

newtype InMemoryParticipantId
    = InMemoryParticipantId Integer
    deriving(Show, Eq, Ord)

data InMemoryServer m d = InMemoryServer
    { imsNextParticipant :: TVar (STM m) Integer
    , imsEmitChan        :: MsgChan m d
    , imsReceiveChan     :: MsgChan m d
    }

newServer :: MonadConc m => STM m (InMemoryServer m d)
newServer = do
    nextId <- newTVar 0
    emit <- newBroadcastTChan
    receive <- newTChan
    pure InMemoryServer
        { imsNextParticipant = nextId
        , imsEmitChan = emit
        , imsReceiveChan = receive
        }

newParticipantId :: MonadConc m => InMemoryServer m d -> STM m InMemoryParticipantId
newParticipantId s = do
    let tvar = imsNextParticipant s
    result <- readTVar tvar
    writeTVar tvar $! result + 1
    pure (InMemoryParticipantId result)

newHandle :: MonadConc m => InMemoryServer m d -> STM m (InMemoryHandle m d)
newHandle s = do
    p <- newParticipantId s
    listen <- dupTChan (imsEmitChan s)
    pure InMemoryHandle
        { imhSubmitChan = imsReceiveChan s
        , imhListenChan = listen
        , imhParticipant = p
        }

instance MonadConc m => Handle (InMemoryHandle m d) where
    type HandleM (InMemoryHandle m d) = m
    type ParticipantId (InMemoryHandle m d) = InMemoryParticipantId
    type Data (InMemoryHandle m d) = d

    myParticipantId = imhParticipant
    submit h msg = atomically $
        writeTChan (imhSubmitChan h) MessageWithParticipant
            { mwpMessage = msg
            , mwpParticipant = imhParticipant h
            }
    listenNext h = atomically $ readTChan (imhListenChan h)

instance MonadConc m => ConsensusServer (InMemoryServer m d) (InMemoryHandle m d) where
    type ServerM (InMemoryServer m d) = m

    receive s = atomically $ readTChan (imsReceiveChan s)
    emit s = atomically . writeTChan (imsEmitChan s)
