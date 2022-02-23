-- | STM-based implementations of interfaces from "Glow.Runtime.Interaction"
module Glow.Runtime.Interaction.STM
  ( STMHandle,
    STMServer,
    STMParticipantId,
    newSTMServer,
    newSTMHandle,
    toConsensusServer,
    toHandle,
  )
where

import Control.Concurrent.Classy
import Glow.Prelude
import Glow.Runtime.Interaction

type MsgChan m d = TChan (STM m) (MessageWithParticipant STMParticipantId d)

data STMHandle m d = STMHandle
  { hSubmitChan :: MsgChan m d,
    hListenChan :: MsgChan m d,
    hParticipant :: STMParticipantId
  }

newtype STMParticipantId
  = STMParticipantId Integer
  deriving (Show, Eq, Ord)

data STMServer m d = STMServer
  { sNextParticipant :: TVar (STM m) Integer,
    sEmitChan :: MsgChan m d,
    sReceiveChan :: MsgChan m d
  }

newSTMServer :: MonadConc m => STM m (STMServer m d)
newSTMServer = do
  nextId <- newTVar 0
  emit <- newBroadcastTChan
  receive <- newTChan
  pure
    STMServer
      { sNextParticipant = nextId,
        sEmitChan = emit,
        sReceiveChan = receive
      }

newParticipantId :: MonadConc m => STMServer m d -> STM m STMParticipantId
newParticipantId s = do
  let tvar = sNextParticipant s
  result <- readTVar tvar
  writeTVar tvar $! result + 1
  pure (STMParticipantId result)

newSTMHandle :: MonadConc m => STMServer m d -> STM m (STMHandle m d)
newSTMHandle s = do
  p <- newParticipantId s
  listen <- dupTChan (sEmitChan s)
  pure
    STMHandle
      { hSubmitChan = sReceiveChan s,
        hListenChan = listen,
        hParticipant = p
      }

toHandle :: MonadConc m => STMHandle m d -> Handle m STMParticipantId d
toHandle h =
  Handle
    { myParticipantId = hParticipant h,
      submit = \msg ->
        atomically $
          writeTChan
            (hSubmitChan h)
            MessageWithParticipant
              { mwpMessage = msg,
                mwpParticipant = hParticipant h
              },
      listenNext = atomically $ readTChan (hListenChan h)
    }

toConsensusServer :: MonadConc m => STMServer m d -> ConsensusServer m STMParticipantId d
toConsensusServer s =
  ConsensusServer
    { receive = atomically $ readTChan (sReceiveChan s),
      emit = atomically . writeTChan (sEmitChan s)
    }
