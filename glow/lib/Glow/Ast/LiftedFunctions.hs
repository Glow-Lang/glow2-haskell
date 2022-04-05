module Glow.Ast.LiftedFunctions where

import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import Data.Text.Lazy as LT
import Glow.Prelude

newtype Id = Id LT.Text
  deriving (Show, Read, Eq, Ord)

newtype TypeVar = TypeVar Id
  deriving (Show, Read, Eq, Ord)

data Module = Module [TopStmt]
  deriving (Show, Read, Eq)

data TopStmt
  = TsBodyStmt BodyStmt
  | -- Note: in the grammar there are both (deftype id type) and
    -- (deftype (id tyvar ...) type); here we just combine them, where the
    -- first variant has an empty list (likewise for defdata).
    TsDefType Id [TypeVar] Type
  | TsDefData Id [TypeVar] [Variant]
  | TsDefInteraction Id InteractionDef
  | TsDefLambda Id (Lambda BodyStmt)
  | TsAtPart Id (Lambda PartStmt)
  deriving (Show, Read, Eq)

data InteractionDef = InteractionDef
  { idParticipants :: [Id],
    idAssets :: [Id],
    idParams :: [Id],
    idBody :: [BodyStmt]
  }
  deriving (Show, Read, Eq)

data BodyStmt
  = BsPartStmt PartStmt
  | BsWithdraw Id (Record ArgExpr)
  | BsDeposit Id (Record ArgExpr)
  | BsPublish Id ArgExpr
  | BsSwitch (Switch BodyStmt)
  deriving (Show, Read, Eq)

data PartStmt
  = PsLabel Id
  | PsDebugLabel Id
  | PsIgnore Expr
  | PsReturn Expr
  | PsRequire ArgExpr
  | PsAssert ArgExpr
  | PsSwitch (Switch PartStmt)
  deriving (Show, Read, Eq)

data Switch stmt = Switch
  { swArg :: ArgExpr,
    swBranches :: NonEmpty (Pat, NonEmpty stmt)
  }
  deriving (Show, Read, Eq)

data Variant = Variant Id [Type]
  deriving (Show, Read, Eq)

data Expr
  = ExArg ArgExpr
  | ExDot ArgExpr Id
  | ExList [ArgExpr]
  | ExTuple [ArgExpr]
  | ExRecord (Record ArgExpr)
  | -- | Probably obvious suggestion: maybe generalize this to other binary operators?
    ExEq ArgExpr ArgExpr
  | ExInput Type ArgExpr
  | -- | Question: can digest actually take multiple arguments? What does that do?
    ExDigest [ArgExpr]
  | ExSign ArgExpr
  | ExCapture ArgExpr (NonEmpty ArgExpr)
  | ExApp ArgExpr [ArgExpr]
  deriving (Show, Read, Eq)

newtype Record val = Record (M.Map Id val)
  deriving (Show, Read, Eq)

data Lambda stmt = Lambda
  { lamCaptures :: [Id],
    -- | N.B. this representation allows nullary functions; is that what we intend?
    lamParams :: [Id],
    lamBody :: NonEmpty stmt
  }
  deriving (Show, Read, Eq)

data ArgExpr
  = AEConst Constant
  | AEEmptyTuple
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

data Type
  = TyId Id [Type] -- both id and (id type ...)
  | TyVar TypeVar
  | TyTuple [Type]
  | TyRecord (Record Type)
  | TyFunc [Type] Type
  deriving (Show, Read, Eq)

data Constant
  = CInteger !Integer
  | CByteString !BS.ByteString
  | CBool !Bool
  deriving (Show, Read, Eq)
