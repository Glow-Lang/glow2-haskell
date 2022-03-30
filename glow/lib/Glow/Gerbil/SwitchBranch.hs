{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Glow.Gerbil.SwitchBranch where

import Control.Monad.State (State, return)
import Control.Monad.Extra (concatMapM)
import Glow.Gerbil.Fresh
import Glow.Gerbil.Types as GT
import GHC.Generics hiding (Datatype)
import Glow.Prelude

data StraightStatement
  = StrtDefine ByteString Expression
  | StrtAtParticipant GlowValueRef StraightStatement
  | StrtSetParticipant GlowValueRef
  | StrtPublish GlowValueRef [GlowValueRef]
  | StrtDeposit GlowValueRef AssetMap
  | StrtWithdraw GlowValueRef AssetMap
  | StrtIgnore Expression
  | StrtRequire GlowValueRef
  | StrtReturn Expression
  deriving stock (Generic, Eq, Show)

data JBStatement pattern
  = JbsLabel ByteString
  | JbsStraight StraightStatement
  | JbsBranchNotMatch GlowValueRef pattern ByteString
  deriving stock (Generic, Eq, Show)

statementsSwitchToShallowBranch :: [GT.AnfStatement] -> State UnusedTable [JBStatement ShallowPattern]
statementsSwitchToShallowBranch = statementsDeepBranchToShallow . statementsSwitchToDeepBranch

statementsSwitchToDeepBranch :: [GT.AnfStatement] -> [JBStatement GT.Pattern]
statementsSwitchToDeepBranch _ = error "TODO"

statementsDeepBranchToShallow :: [JBStatement GT.Pattern] -> State UnusedTable [JBStatement ShallowPattern]
-- TODO: if state is needed to generate new labels, change to concatMapM or something like that
statementsDeepBranchToShallow = concatMapM statementDeepBranchToShallow

statementDeepBranchToShallow :: JBStatement GT.Pattern -> State UnusedTable [JBStatement ShallowPattern]
statementDeepBranchToShallow = \case
  JbsLabel l -> return [JbsLabel l]
  JbsStraight s -> return [JbsStraight s]
  JbsBranchNotMatch e p l -> do
    (p1, xp2s) <- patternDeepToShallow p
    let s1 = JbsBranchNotMatch e p1 l
    s2 <- concatMapM statementDeepBranchToShallow [JbsBranchNotMatch (Variable x) p2 l | (x, p2) <- xp2s]
    return (s1 : s2)

patternDeepToShallow :: GT.Pattern -> State UnusedTable (ShallowPattern, [(ByteString, GT.Pattern)])
patternDeepToShallow (Pattern p) = case p of
  PwTrivial p1 -> return (PwTrivial p1, [])
  PwAppCtor ctor p2s ->
    do xs <- genTemps p2s
       return ((PwAppCtor ctor (map VarPat xs)), (zip xs p2s))

