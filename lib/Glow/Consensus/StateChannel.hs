{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Glow.Consensus.StateChannel where

import qualified Glow.Consensus.Local     as Local
import           Glow.Prelude
import           Glow.Runtime.Interaction

import qualified Data.Set        as S
import           Data.Void       (Void)
import           Numeric.Natural (Natural)

data SCMessage s i
    = SCMAgreement (Agreement s)
    | SCMDirect (DirectMessage i)

data DirectMessage i = DirectMessage
    { dmVersion :: !Natural
    , dmMessage :: MessageWithParticipant i
    }

data Agreement s = Agreement
    { agreeVersion :: !Natural
    , agreeState   :: s
    }

data StateChannel s i

instance Interaction i => Interaction (StateChannel s i) where
    type Data (StateChannel s i) = SCMessage s i
    type ParticipantId (StateChannel s i) = ParticipantId i

data State h s = State
    { stateUnderlying  :: s
    , stateVersion     :: !Natural
    , stateByAgreement :: !Bool
    }

runStateMachine
    :: (ConsensusServer srv (StateChannel state i), Monad (ServerM srv))
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

wrapStateMachine
    :: S.Set (ParticipantId i)
    -> (s -> MessageWithParticipant i -> Maybe s)
    -> (State h s -> MessageWithParticipant (StateChannel s i) -> Maybe (State h s))
wrapStateMachine allParticipants transition =
    \state mwp ->
        case messageData (mwpMessage mwp) of
            SCMAgreement Agreement { agreeVersion = ver, agreeState = newState }
                | ver < stateVersion state -> Nothing
                | otherwise ->
                    -- TODO: check signatures or something.
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
