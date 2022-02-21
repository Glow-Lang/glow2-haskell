module Glow.Runtime.Interaction where

import qualified Data.Map.Strict as M
import Glow.Prelude

-- | A message sent to a consensus, with value type @d@ and participant
-- ids @p@.
data Message p d = Message
  { -- | The data payload for the message.
    messageData :: d,
    -- | Differences in balances for each participant induced by
    -- this message. TODO: maybe make this more abstract, to
    -- support multiple assets, NFTs, etc.
    messageAssetTransfers :: M.Map p Integer
  }
  deriving (Show, Eq)

-- | A message, paired with the participant who sent it.
data MessageWithParticipant p d = MessageWithParticipant
  { mwpMessage :: Message p d,
    mwpParticipant :: p
  }
  deriving (Show, Eq)

-- | An interaction 'Handle' is used by participants to communicate
-- with the consensus for an interaction. It has participant ids @p@,
-- value type @d@, and operates in effect monad @m@.
data Handle m p d = Handle
  { -- | Get the participant that is associated with messages
    -- sent using this handle.
    myParticipantId :: p,
    -- | Send a message to the consensus.
    submit :: Message p d -> m (),
    -- | Wait for the next message committed to the consensus.
    listenNext :: m (MessageWithParticipant p d)
  }

-- | A 'ConsensusServer' is used to handle the IO needed to
-- act as a consensus. A typical caller of this interface is
-- a proxy for some consensus running on a blockchain.
--
-- Parameters are as for 'Handle'.
data ConsensusServer m p d = ConsensusServer
  { -- | Receive the next message sent to the consensus.
    receive :: m (MessageWithParticipant p d),
    -- | Accept a message and notify participants of it.
    emit :: MessageWithParticipant p d -> m ()
  }
