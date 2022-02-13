{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Glow.Consensus.Local where

import Glow.Prelude
import Glow.Runtime.Interaction

-- | Manage the consensus using a state machine; given:
--
-- * an initial state, and
-- * a function which takes the current state and returns
--   a new state (or fails)
--
-- This handles the IO for a consensus server using the
-- given state machine.
runStateMachine
    :: (ConsensusServer s h, Monad (ServerM s))
    => state
    -> (state -> MessageWithParticipant h -> Maybe state)
    -> s
    -> ServerM s a
runStateMachine oldState transition server = do
    msg <- receive server
    case transition oldState msg of
        Nothing ->
            -- failed validation; don't change.
            runStateMachine oldState transition server
        Just newState -> do
            emit server msg
            runStateMachine newState transition server
