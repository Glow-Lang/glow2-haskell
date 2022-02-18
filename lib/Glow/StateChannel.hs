{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
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

import qualified Glow.Consensus.StateChannel as SC
import           Glow.Prelude
import           Glow.Runtime.Interaction

import           Control.Concurrent.Classy
import qualified Data.Map.Strict           as M
import           Numeric.Natural           (Natural)

data Proposer (m :: * -> *) s i = Proposer

propose :: MonadConc m => Proposer m s i -> SC.Agreement s i -> m (SC.Agreement s i)
propose _ = pure
    -- TODO: actually talk to the other participants somehow.

data WrappedHandle h s i = WrappedHandle
    { whHandle            :: h
    , whProposer          :: Proposer (HandleM h) s i
    , whTransition        :: s -> MessageWithParticipant i -> Maybe s
    , whState             :: TVar (STM (HandleM h)) (WrappedHandleState h s i)
    , whIncomingProposals :: TChan (STM (HandleM h)) (ProposalMsg (HandleM h) s i)
    }

data ProposalMsg m s i = ProposalMsg
    { pmOldVersion :: !Natural
    , pmMessage    :: MessageWithParticipant i
    , pmReply      :: SC.Proposal s -> SC.Proof s i -> STM m ()
    }

data WrappedHandleState h s i = WrappedHandleState
    { whsState :: SC.State s
    }

instance (Interaction i, MonadConc (HandleM h), Handle h (SC.StateChannel s i)) => Handle (WrappedHandle h s i) i where
    type HandleM (WrappedHandle h s i) = HandleM h

    myParticipantId = myParticipantId . whHandle

    submit wh msg = do
        let mwp = MessageWithParticipant
                { mwpMessage = msg
                , mwpParticipant = myParticipantId wh
                }
        s <- atomically $ readTVar (whState wh)
        case whTransition wh (SC.stateUnderlying (whsState s)) mwp of
            Nothing ->
                -- TODO: handle errors more systematically.
                error "Invalid message"
            Just newState -> do
                agreement <- propose (whProposer wh) SC.Agreement
                    { SC.agreeProposal = SC.Proposal
                        { SC.proposeVersion = SC.stateVersion (whsState s) + 1
                        , SC.proposeState = newState
                        }
                    , SC.agreeProofs = M.empty -- TODO
                    }
                submit (whHandle wh) Message
                    { messageData = SC.SCMAgreement agreement
                    , messageAssetTransfers = M.empty
                    }
    listenNext wh = do
        -- TODO:
        --
        -- Wait for either:
        --
        -- * See if there are any messages coming from the underlying
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
        -- * Otherwise, try acceptProposal; if it works, stash the agreement and
        --   return the message.
        mwp <- listenNext (whHandle wh)
        atomically $ do
            case messageData (mwpMessage mwp) of
                SC.SCMAgreement _ ->
                    error "TODO"
                SC.SCMDirect _ ->
                    error "TODO"

-- | Accept and sign off on an incoming proposal (if valid).
acceptProposal ::
    ( Handle (WrappedHandle h s i) i
    , MonadConc (HandleM h)
    )
    => WrappedHandle h s i
    -> STM (HandleM h) (MessageWithParticipant i)
acceptProposal wh = do
    pm <- readTChan (whIncomingProposals wh)
    whs <- readTVar (whState wh)
    let scState = whsState whs
    unless (pmOldVersion pm == SC.stateVersion scState)
        retry
    case whTransition wh (SC.stateUnderlying (whsState whs)) (pmMessage pm) of
        Nothing -> retry
        Just newState -> do
            -- TODO: store the new agreement locally.
            --
            -- We also need to rework the protocol so that we actually have
            -- all the proofs on the final agreement, in case we need to be
            -- the one to post it.
            pmReply pm SC.Proposal
                { SC.proposeState = newState
                , SC.proposeVersion = SC.stateVersion scState + 1
                }
                SC.Proof
            pure $ pmMessage pm
