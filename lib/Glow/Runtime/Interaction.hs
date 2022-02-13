{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Glow.Runtime.Interaction where

import Glow.Prelude

import qualified Data.Map.Strict as M

-- | A message sent to a consensus.
data Message ih = Message
    { messageData           :: Data ih
    -- ^ The data payload for the message.
    , messageAssetTransfers :: M.Map (ParticipantId ih) Integer
    -- ^ Differences in balances for each participant induced by
    -- this message. TODO: maybe make this more abstract, to
    -- support multiple assets, NFTs, etc.
    }

-- | A message, paired with the participant who sent it.
data MessageWithParticipant ih = MessageWithParticipant
    { message     :: Message ih
    , participant :: ParticipantId ih
    }

-- | An interaction 'Handle' is used by participants to communicate
-- with the consensus for an interaction.
class Handle ih where
    -- | @'HandleM' i@ is an effect monad in which the handle may be
    -- used.
    type HandleM ih a

    -- | An identifier for a participant.
    type ParticipantId ih

    -- | The data portion of a message.
    type Data ih

    -- | Get the participant that is associated with messages
    -- sent using this handle.
    myParticipantId :: ih -> ParticipantId ih

    -- | Send a message to the consensus.
    submit :: ih -> Message ih -> HandleM ih ()

    -- | Wait for the next message committed to the consensus.
    listenNext :: ih -> HandleM ih (MessageWithParticipant ih)


-- | A 'ConsensusServer' is used to handle the IO needed to
-- act as a consensus. A typical caller of this interface is
-- a proxy for some consensus running on a blockchain.
class ConsensusServer s ih | s -> ih where
    -- | Effect monad to interact with the server context.
    type ServerM s a

    -- | Receive the next message sent to the consensus.
    receive :: s -> MessageWithParticipant ih

    -- | Accept a message and notify participants of it.
    emit :: s -> MessageWithParticipant ih -> ServerM s ()
