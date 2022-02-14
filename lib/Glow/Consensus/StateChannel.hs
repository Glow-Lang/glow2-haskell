{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Glow.Consensus.StateChannel where

import qualified Glow.Consensus.Local     as Local
import           Glow.Prelude
import           Glow.Runtime.Interaction

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.Void       (Void)
import           Numeric.Natural (Natural)

data SCMessage s i
    = SCMAgreement (Agreement s i)
    | SCMDirect (DirectMessage i)

data DirectMessage i = DirectMessage
    { dmVersion :: !Natural
    , dmMessage :: MessageWithParticipant i
    }

data Agreement s i = Agreement
    { agreeProposal :: Proposal s
    , agreeProofs   :: M.Map (ParticipantId i) (Proof s i)
    }

-- TODO: actually put something here; in a real implementation it would
-- probably be a signature of some kind.
data Proof s i = Proof

data Proposal s = Proposal
    { proposeVersion :: !Natural
    , proposeState   :: s
    }

data StateChannel s i

instance Interaction i => Interaction (StateChannel s i) where
    type Data (StateChannel s i) = SCMessage s i
    type ParticipantId (StateChannel s i) = ParticipantId i

data State s = State
    { stateUnderlying  :: s
    , stateVersion     :: !Natural
    , stateByAgreement :: !Bool
    }

runStateMachine ::
    ( ConsensusServer srv (StateChannel state i)
    , Monad (ServerM srv)
    , Ord (ParticipantId i)
    )
    => S.Set (ParticipantId i)
    -> state
    -> (state -> MessageWithParticipant i -> Maybe state)
    -> srv
    -> ServerM srv Void
runStateMachine allParticipants init transition server =
    let transition' = wrapStateMachine allParticipants transition
        init' = State
            { stateUnderlying = init
            , stateVersion = 0
            , stateByAgreement = False
            }
    in
    Local.runStateMachine init' transition' server

-- | Verifies that an agreement is valid given the set of participants,
-- and if so returns the proposal.
validateAgreement
    :: Ord (ParticipantId i)
    => S.Set (ParticipantId i) -> Agreement s i -> Maybe (Proposal s)
validateAgreement allParticipants agreement =
    if S.fromList (M.keys (agreeProofs agreement)) == allParticipants then
        Just (agreeProposal agreement)
    else
        Nothing

wrapStateMachine
    :: Ord (ParticipantId i)
    => S.Set (ParticipantId i)
    -> (s -> MessageWithParticipant i -> Maybe s)
    -> (State s -> MessageWithParticipant (StateChannel s i) -> Maybe (State s))
wrapStateMachine allParticipants transition =
    \state mwp ->
        case messageData (mwpMessage mwp) of
            SCMAgreement agreement -> do
                Proposal { proposeVersion = ver, proposeState = newState }
                    <- validateAgreement allParticipants agreement
                when (ver < stateVersion state) $ Nothing
                Just State
                    { stateUnderlying = newState
                    , stateVersion = ver
                    , stateByAgreement = True
                    }
            SCMDirect DirectMessage { dmVersion = ver, dmMessage = msg }
                | ver <= stateVersion state -> Nothing
                | otherwise ->
                    transition (stateUnderlying state) msg
                    & fmap
                        (\newState -> State
                            { stateUnderlying = newState
                            , stateVersion = ver
                            , stateByAgreement = False
                            }
                        )
