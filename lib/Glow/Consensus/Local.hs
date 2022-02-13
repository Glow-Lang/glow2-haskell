{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module Glow.Consensus.Local where

import Data.Void (Void)

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
    :: (ConsensusServer srv i, Monad (ServerM srv))
    => state
    -> (state -> MessageWithParticipant i -> Maybe state)
    -> srv
    -> ServerM srv Void
runStateMachine oldState transition server = do
    msg <- receive server
    case transition oldState msg of
        Nothing ->
            -- failed validation; don't change.
            runStateMachine oldState transition server
        Just newState -> do
            emit server msg
            runStateMachine newState transition server
