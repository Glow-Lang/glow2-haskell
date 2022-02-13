{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Glow.Runtime.Interaction where

import Glow.Prelude

import qualified Data.Map.Strict as M

-- | A message sent to a consensus.
data Message i = Message
    { messageData           :: Data i
    -- ^ The data payload for the message.
    , messageAssetTransfers :: M.Map (ParticipantId i) Integer
    -- ^ Differences in balances for each participant induced by
    -- this message. TODO: maybe make this more abstract, to
    -- support multiple assets, NFTs, etc.
    }

deriving instance (Show (Data i), Show (ParticipantId i)) => Show (Message i)
deriving instance (Eq (Data i), Eq (ParticipantId i)) => Eq (Message i)

-- | A message, paired with the participant who sent it.
data MessageWithParticipant i = MessageWithParticipant
    { mwpMessage     :: Message i
    , mwpParticipant :: ParticipantId i
    }

deriving instance (Show (Data i), Show (ParticipantId i)) => Show (MessageWithParticipant i)
deriving instance (Eq (Data i), Eq (ParticipantId i)) => Eq (MessageWithParticipant i)

-- | An interaction is a (usually phantom) type used to
-- carry information about a particular type of interaction.
class Interaction i where
    -- | An identifier for a participant.
    type ParticipantId i

    -- | The data portion of a message.
    type Data i

-- | An interaction 'Handle' is used by participants to communicate
-- with the consensus for an interaction of type @i@.
class Interaction i => Handle h i | h -> i where
    -- | @'HandleM' h@ is an effect monad in which the handle may be
    -- used.
    type HandleM h :: * -> *

    -- | Get the participant that is associated with messages
    -- sent using this handle.
    myParticipantId :: h -> ParticipantId i

    -- | Send a message to the consensus.
    submit :: h -> Message i -> HandleM h ()

    -- | Wait for the next message committed to the consensus.
    listenNext :: h -> HandleM h (MessageWithParticipant i)


-- | A 'ConsensusServer' is used to handle the IO needed to
-- act as a consensus. A typical caller of this interface is
-- a proxy for some consensus running on a blockchain.
class Interaction i => ConsensusServer s i | s -> i where
    -- | Effect monad to interact with the server context.
    type ServerM s :: * -> *

    -- | Receive the next message sent to the consensus.
    receive :: s -> ServerM s (MessageWithParticipant i)

    -- | Accept a message and notify participants of it.
    emit :: s -> MessageWithParticipant i -> ServerM s ()
