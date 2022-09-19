-- | Module: Glow.Ast.LiftedFunctions
--
-- This module defines an Ast which is the output of the FunctionLift pass.
--
-- The Function-Lift pass takes ANF as input and produces Lifted-Functions as
-- output.
--
-- The most notable difference from ANF is that there are multiple tiers of
-- statements:
--
-- * 'TopStmt' for statements directly at the top level of a module, including
--   interaction definitions, function definitions and type definitions.
-- * 'BodyStmt' for statements in the bodies of interactions, functions, and
--   conditionals, including consensus and at-participant statements.
-- * 'PartStmt' for statements inside at-participant statements, and inside
--   conditionals within.
--
-- The 'TopStmt' function definitions ('TsDefLambda') include
-- capture-parameters before the function-parameters, and the 'Expr' includes a
-- form 'ExCapture' to supply capture-arguments to produce a closure.
module Glow.Ast.LiftedFunctions where

import Glow.Ast.Common
import Glow.Gerbil.Types (Pat, Record, Type, Variant)
import Glow.Prelude

data Module = Module [TopStmt]
  deriving (Show, Read, Eq)

data TopStmt
  = TsBodyStmt BodyStmt
  | -- Note: in the grammar there are both (deftype id type) and
    -- (deftype (id tyvar ...) type); here we just combine them, where the
    -- first variant has an empty list (likewise for defdata).
    TsDefType Id [Id] Type
  | TsDefData Id [Id] [Variant]
  | TsDefInteraction Id InteractionDef
  | -- | participant id (if any), function id, function def:
    TsDefLambda (Maybe Id) Id (Lambda BodyStmt)
  deriving (Show, Read, Eq)

data InteractionDef = InteractionDef
  { idParticipants :: [Id],
    idAssets :: [Id],
    idParams :: [Id],
    idBody :: [BodyStmt]
  }
  deriving (Show, Read, Eq)

data BodyStmt
  = BsPartStmt (Maybe Id) PartStmt
  | BsWithdraw Id (Record TrivExpr)
  | BsDeposit Id (Record TrivExpr)
  | BsPublish Id Id
  | BsSwitch (Switch BodyStmt)
  deriving (Show, Read, Eq)

data PartStmt
  = PsLabel Id
  | PsDebugLabel Id
  | PsDef Id Expr
  | PsIgnore Expr
  | PsReturn Expr
  | PsRequire TrivExpr
  | PsAssert TrivExpr
  | PsSwitch (Switch PartStmt)
  deriving (Show, Read, Eq)

data Switch stmt = Switch
  { swArg :: TrivExpr,
    swBranches :: [(Pat, [stmt])]
  }
  deriving (Show, Read, Eq)

data Expr
  = ExTriv TrivExpr
  | ExDot TrivExpr Id
  | ExList [TrivExpr]
  | ExTuple [TrivExpr]
  | ExRecord (Record TrivExpr)
  | -- | Probably obvious suggestion: maybe generalize this to other binary operators?
    ExEq TrivExpr TrivExpr
  | ExInput Type TrivExpr
  | -- | Question: can digest actually take multiple arguments? What does that do?
    ExDigest [TrivExpr]
  | ExSign TrivExpr
  | ExCapture TrivExpr [TrivExpr]
  | ExApp TrivExpr [TrivExpr]
  deriving (Show, Read, Eq)

data Lambda stmt = Lambda
  { lamCaptures :: [Id],
    -- | N.B. this representation allows nullary functions; is that what we intend?
    lamParams :: [Id],
    lamBody :: [stmt]
  }
  deriving (Show, Read, Eq)
