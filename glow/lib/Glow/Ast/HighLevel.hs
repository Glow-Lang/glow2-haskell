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
