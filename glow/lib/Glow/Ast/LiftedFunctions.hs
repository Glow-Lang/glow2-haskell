module Glow.Ast.LiftedFunctions where

import qualified Data.Map.Strict as M
import Glow.Ast.Common
import Glow.Gerbil.Types (Type, Variant)
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

newtype Record val = Record (M.Map Id val)
  deriving (Show, Read, Eq)

data Lambda stmt = Lambda
  { lamCaptures :: [Id],
    -- | N.B. this representation allows nullary functions; is that what we intend?
    lamParams :: [Id],
    lamBody :: [stmt]
  }
  deriving (Show, Read, Eq)

data Pat
  = PTypeAnno Pat Type
  | PVar Id
  | PAppCtor Id [Pat]
  | PWild
  | PList [Pat]
  | PTuple [Pat]
  | PRecord (Record Pat)
  | POr [Pat]
  | PConst Constant
  deriving (Show, Read, Eq)
