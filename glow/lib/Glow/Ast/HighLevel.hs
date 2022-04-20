{-# LANGUAGE LambdaCase #-}

-- | This module defines a high-level intermediate representation;
-- at this point we fork off between high-level targets like lurk
-- and pluts, and lower-level, machine-like targets.
--
-- The high-level IR is recognizably lambda calculus. Noteworthy
-- properties include:
--
-- * Everything is still in ANF
-- * Lambdas have explicit capture lists (this will help when
--   translating to low level IRs, and may help optimize
--   serialization on plutus; see detailed discussion below).
-- * The IR is typed, and we track certain kinds of effects
--   via the type system. This facilitates type-directed
--   translation to monadic effects, or actual side-effects
--   depending on the target.
module Glow.Ast.HighLevel where

import qualified Data.Map.Strict as M
import Glow.Ast.Common
import Glow.Prelude

data Program a = Program
  { progFuncs :: M.Map Id (Lambda a),
    -- | Name of entry point in progFuncs.
    progMain :: Id
  }

data Type a
  = TyTuple a [Type a]
  | TyFunc a [Type a] (Type a) EffType
  | -- | Function with an explicit capture list:
    TyFuncPtr a [Type a] [Type a] (Type a) EffType
  deriving (Show, Read, Eq)

-- | An effect type. these are nested; a function with
-- effect type 'EtStateUpdates' can be called from within
-- a function with effect type 'EtSetParticipant', using
-- the 'ExLift' operator, but not vice-versa. The Ord is
-- such that x >= y iff y may be invoked from x.
data EffType
  = -- | No effects permitted.
    EtNone
  | -- | Abort the current transaction.
    EtAbort
  | -- | Update balances and other contract state.
    EtStateUpdates
  | -- | Query the execution environment.
    EtQuery
  | -- | Switch participants. This causes a transaction commit.
    EtSetParticipant
  deriving (Show, Read, Eq, Enum, Bounded)

data Expr a
  = ExLet a Id (Expr a) (Expr a)
  | ExApply a Id [Id]
  | -- | Lift an "less-effectful" computation into a "more effectful" one.
    ExLift a (Expr a)
  | ExEffOp a (EffOp a)
  | ExConst a Constant
  | ExBuiltin a Builtin
  | -- | Construct a closure from a lambda (with explicit capture list) and
    -- a set of values for the captured variables.
    ExCapture a Id [Id]
  deriving (Show, Read, Eq)

data Lambda a = Lambda
  { lamCaptures :: [(Id, Type a)],
    lamParams :: [(Id, Type a)],
    lamReturnType :: Type a,
    lamEffectType :: EffType,
    lamBody :: Expr a
  }

data Builtin
  = BAdd
  | BSub
  | BMul
  | -- TODO: fill out other arithmetic operators
    BOr
  | BAnd
  -- TODO: fill out other logical operators
  -- TODO: fill out any other operators.
  deriving (Show, Read, Eq)

effOpType :: EffOp a -> EffType
effOpType = \case
  EffGetParticipant _ -> EtQuery
  EffSetParticipant _ _ -> EtSetParticipant
  EffDeposit _ _ -> EtStateUpdates
  EffWithdraw _ _ _ -> EtStateUpdates
  EffRequire _ _ -> EtAbort

data EffOp a
  = EffGetParticipant a
  | EffSetParticipant a Id
  | EffDeposit a Id
  | EffWithdraw a Id Id
  | EffRequire a Id
  -- TODO: fill out any other effects.
  deriving (Show, Read, Eq)
