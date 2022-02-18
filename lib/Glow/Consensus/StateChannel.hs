module Glow.Consensus.StateChannel where

import qualified Glow.Consensus.Local     as Local
import           Glow.Prelude
import           Glow.Runtime.Interaction

import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Data.Void       (Void)
import           Numeric.Natural (Natural)

data SCMessage s p d
    = SCMAgreement (Agreement s p)
    | SCMDirect (DirectMessage p d)

data DirectMessage p d = DirectMessage
    { dmVersion :: !Natural
    , dmMessage :: MessageWithParticipant p d
    }

data Agreement s p = Agreement
    { agreeProposal :: Proposal s
    , agreeProofs   :: M.Map p (Proof s p)
    }

-- TODO: actually put something here; in a real implementation it would
-- probably be a signature of some kind.
data Proof s p = Proof

data Proposal s = Proposal
    { proposeVersion :: !Natural
    , proposeState   :: s
    }

data State s = State
    { stateUnderlying  :: s
    , stateVersion     :: !Natural
    , stateByAgreement :: !Bool
    }

runStateMachine ::
    ( Monad m
    , Ord p
    )
    => S.Set p
    -> s
    -> (s -> MessageWithParticipant p d -> Maybe s)
    -> ConsensusServer m p (SCMessage s p d)
    -> m Void
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
    :: Ord p
    => S.Set p -> Agreement s p -> Maybe (Proposal s)
validateAgreement allParticipants agreement =
    if S.fromList (M.keys (agreeProofs agreement)) == allParticipants then
        Just (agreeProposal agreement)
    else
        Nothing

wrapStateMachine
    :: Ord p
    => S.Set p
    -> (s -> MessageWithParticipant p d -> Maybe s)
    -> (State s -> MessageWithParticipant p (SCMessage s p d) -> Maybe (State s))
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
