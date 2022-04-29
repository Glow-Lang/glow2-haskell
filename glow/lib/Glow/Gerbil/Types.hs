{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glow.Gerbil.Types where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import GHC.Generics hiding (Datatype)
import Glow.Ast.Common (Id, Constant, TrivExpr)
import Glow.Prelude

-- TODO: variable cleanup, only keep live variables between each transaction
type GlowProjectContract = Map ExecutionPoint ([ProjectStatement], Maybe ExecutionPoint)

type VariableMap = Map Id GlowValue

type DatatypeMap = Map Id [(Id, Integer)]

type AssetMap = Record TrivExpr

type ExecutionPoint = Id

type Record val = Map Id val

-- | An MLsub type, as emitted by the frontend.
data Type
  = TyArrow [Type] Type
  | TyName Id
  | TyNameSubtype Id Type
  | TyTuple [Type]
  | TyVar Id
  | TyApp Type [Type]
  | TyRecord (Record Type)
  | -- | TyUnknown is a placeholder until we actually support parsing everything;
    -- it is convienient to be able print out a larger type which has un-parsable
    -- bits, so we can see what of a program we handle and what we don't.
    TyUnknown ByteString
  deriving (Eq, Read, Show)

-- TODO: support lambdas with CPS
data Statement interactionDef
  = Label Id
  | DebugLabel Id
  | DefineInteraction Id interactionDef
  | Define Id Expression
  | DefineFunction Id [Id] [(Statement interactionDef)]
  | -- Note: in the grammar there are both (deftype id type) and
    -- (deftype (id tyvar ...) type); here we just combine them, where the
    -- first variant has an empty list (likewise for defdata).
    DefineType Id [Id] Type
  | DefineDatatype Id [Id] [Variant]
  | AtParticipant Id (Statement interactionDef)
  | SetParticipant Id
  | Publish Id [Id]
  | Deposit Id AssetMap
  | Withdraw Id AssetMap
  | Ignore Expression
  | Require TrivExpr
  | Return Expression
  | Switch TrivExpr [(Pat, [(Statement interactionDef)])]
  deriving stock (Generic, Eq, Show)

data AnfInteractionDef = AnfInteractionDef
  { aidParticipantNames :: [Id],
    aidAssetNames :: [Id],
    aidArgumentNames :: [Id],
    aidBody :: [AnfStatement]
  }
  deriving stock (Generic, Eq, Show)

type AnfStatement = Statement AnfInteractionDef

data ProjectInteractionDef = ProjectInteractionDef
  { pidParticipantNames :: [Id],
    pidAssetNames :: [Id],
    pidArgumentNames :: [Id],
    pidInteractions :: [(Maybe Id, [ProjectStatement])]
  }
  deriving stock (Generic, Eq, Show)

type ProjectStatement = Statement ProjectInteractionDef

data Variant = Variant Id [Type]
  deriving stock (Generic, Show, Read, Eq)

data Expression
  = ExpectPublished Id
  | Digest [TrivExpr]
  | Sign TrivExpr
  | Input Type TrivExpr
  | EqlExpr TrivExpr TrivExpr
  | AppExpr TrivExpr [TrivExpr]
  | TrvExpr TrivExpr
  deriving stock (Generic, Eq, Show)

data GlowValue
  = Constructor Id Integer [GlowValue]
  | PubKey LedgerPubKey
  | Signature LedgerSignature
  | ByteString ByteString
  | Integer Integer
  | Boolean Bool
  | Unit
  deriving stock (Generic, Eq, Show)

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

newtype LedgerPubKey = LedgerPubKey ByteString
  deriving stock (Generic, Eq, Show)
  deriving newtype (IsString)

newtype LedgerSignature = LedgerSignature ByteString
  deriving stock (Generic, Eq, Show)
  deriving newtype (IsString)
