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
-- * All side effects happen in an explicit effect monad, chained
--   together with a monadic bind operator. Most high level
--   constructs in Glow with simple operational semantics
--   still exist as primitives at this level (e.g. ADTs/match,
--   tuples, if/else, etc).
-- * The IR is not type checked. Down the line, we will likely
--   introduce a typed variant, but we leave this for future work.
module Glow.Ast.HighLevel where

import Glow.Ast.Common
import Glow.Prelude

data Expr a
  = ExLet a Var (Expr a) (Expr a)
  | ExLambda a [Var] [Var] (Expr a)
  | ExApply a Var [Var]
  | ExEffBind a Var Var
  | ExEffPure a Var
  | ExEffOp a (EffOp a)
  | ExConst a Constant
  | ExBuiltin a Builtin
  deriving (Show, Read, Eq)

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

data EffOp a
  = EffGetParticipant a
  | EffSetParticipant a Var
  | EffDeposit a Var
  | EffWithdraw a Var Var
  | EffRequire a Var
  -- TODO: fill out any other effects.
  deriving (Show, Read, Eq)

data Constant
  = CBool !Bool
  | CInteger IntType !Integer
  deriving (Show, Read, Eq)
