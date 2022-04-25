{-# LANGUAGE KindSignatures #-}

-- | Half-baked implementation of state channels, in the form of
-- a wrapper around a state channel interaction that lets you treat
-- it like the normal (non-state channel) interaction it governs.
--
-- There are holes in the design, and it doesn't yet run, but this
-- is enough for me (@zenhack) to convince myself that the abstractions
-- in "Glow.Runtime.Interaction" are roughly suitable for dealing with
-- state channels.
--
-- We could maybe canibalize parts of this for a real implementation,
-- but we would want to step back and work through the protocol design
-- carefully; this is sortof slapped together based on my rough intuition
-- of how state channels work.
module Glow.StateChannel where

import Control.Concurrent.Classy
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Glow.Consensus.StateChannel as SC
import Glow.Prelude
import Glow.Runtime.Interaction
import Numeric.Natural (Natural)

data Proposer (m :: Type -> Type) s i = Proposer

propose :: MonadConc m => Proposer m s p -> SC.Agreement s p -> m (SC.Agreement s p)
propose _ = pure

-- TODO: actually talk to the other participants somehow.

data WrappedHandle m s p d = WrappedHandle
  { whHandle :: Handle m p (SC.SCMessage s p d),
    whProposer :: Proposer m s p,
    whTransition :: s -> MessageWithParticipant p d -> Maybe s,
    whState :: TVar (STM m) (WrappedHandleState s),
    whIncomingProposals :: TChan (STM m) (ProposalMsg m s p d)
  }

data ProposalMsg m s p d = ProposalMsg
  { pmOldVersion :: !Natural,
    pmMessage :: MessageWithParticipant p d,
    pmReply :: SC.Proposal s -> SC.Proof s p -> STM m ()
  }

data WrappedHandleState s = WrappedHandleState
  { whsState :: SC.State s
  }

wrapHandle :: MonadConc m => WrappedHandle m s p d -> Handle m p d
wrapHandle wh =
  Handle
    { myParticipantId = myParticipantId (whHandle wh),
      submit = \msg -> do
        let mwp =
              MessageWithParticipant
                { mwpMessage = msg,
                  mwpParticipant = myParticipantId (whHandle wh)
                }
        s <- atomically $ readTVar (whState wh)
        case whTransition wh (SC.stateUnderlying (whsState s)) mwp of
          Nothing ->
            -- TODO: handle errors more systematically.
            error "Invalid message"
          Just newState -> do
            agreement <-
              propose
                (whProposer wh)
                SC.Agreement
                  { SC.agreeProposal =
                      SC.Proposal
                        { SC.proposeVersion = SC.stateVersion (whsState s) + 1,
                          SC.proposeState = newState
                        },
                    SC.agreeProofs = Map.empty -- TODO
                  }
            submit
              (whHandle wh)
              Message
                { messageData = SC.SCMAgreement agreement,
                  messageAssetTransfers = Map.empty
                },
      listenNext = do
        -- TODO:
        --
        -- Wait for either:
        --
        -- See if there are any messages coming from the underlying
        --   handle, and respond accordingly.
        --   * If we get something that is older than our currently stored
        --     agreement, we should post the agreement.
        --   * If we get something newer than our currently stored agreement:
        --     * In the case of SCMDirect, this means we should replace our local
        --       state and return the message.
        --     * In the case of SCMAgreement, we should validate it and skip over
        --       it; in order to be valid it needs to have our signature on it,
        --       in which case we will have already seen it and returned it via
        --       acceptProposal, so we should just keep waiting for something
        --       new.
        -- Otherwise, try acceptProposal; if it works, stash the agreement and
        --   return the message.
        mwp <- listenNext (whHandle wh)
        atomically $ do
          case messageData (mwpMessage mwp) of
            SC.SCMAgreement _ ->
              error "TODO"
            SC.SCMDirect _ ->
              error "TODO"
    }

-- | Accept and sign off on an incoming proposal (if valid).
acceptProposal :: MonadConc m => WrappedHandle m s p d -> STM m (MessageWithParticipant p d)
acceptProposal wh = do
  pm <- readTChan (whIncomingProposals wh)
  whs <- readTVar (whState wh)
  let scState = whsState whs
  unless
    (pmOldVersion pm == SC.stateVersion scState)
    retry
  case whTransition wh (SC.stateUnderlying (whsState whs)) (pmMessage pm) of
    Nothing -> retry
    Just newState -> do
      -- TODO: store the new agreement locally.
      --
      -- We also need to rework the protocol so that we actually have
      -- all the proofs on the final agreement, in case we need to be
      -- the one to post it.
      pmReply
        pm
        SC.Proposal
          { SC.proposeState = newState,
            SC.proposeVersion = SC.stateVersion scState + 1
          }
        SC.Proof
      pure $ pmMessage pm
