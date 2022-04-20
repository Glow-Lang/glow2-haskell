{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glow.Gerbil.Types where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import GHC.Generics hiding (Datatype)
import Glow.Ast.Common (Constant, TrivExpr)
import Glow.Prelude

-- TODO: variable cleanup, only keep live variables between each transaction
type GlowProjectContract = M.Map ExecutionPoint ([ProjectStatement], Maybe ExecutionPoint)

type VariableMap = M.Map ByteString GlowValue

type ProjectFunctionMap = M.Map ByteString (ByteString, [ProjectStatement])

type DatatypeMap = M.Map ByteString [(ByteString, Integer)]

type AssetMap = M.Map ByteString TrivExpr

type ProjectFunction = (ByteString, [ProjectStatement])

type ExecutionPoint = ByteString

-- | An MLsub type, as emitted by the frontend.
data Type
  = TyArrow [Type] Type
  | TyName ByteString
  | TyNameSubtype ByteString Type
  | TyTuple [Type]
  | TyVar ByteString
  | TyApp Type [Type]
  | TyRecord (M.Map ByteString Type)
  | -- | TyUnknown is a placeholder until we actually support parsing everything;
    -- it is convienient to be able print out a larger type which has un-parsable
    -- bits, so we can see what of a program we handle and what we don't.
    TyUnknown ByteString
  deriving (Eq, Read, Show)

-- TODO: support lambdas with CPS
data Statement interactionDef
  = Label ByteString
  | DebugLabel ByteString
  | DefineInteraction ByteString interactionDef
  | Define ByteString Expression
  | DefineFunction ByteString [ByteString] [(Statement interactionDef)]
  | -- Note: in the grammar there are both (deftype id type) and
    -- (deftype (id tyvar ...) type); here we just combine them, where the
    -- first variant has an empty list (likewise for defdata).
    DefineType ByteString [ByteString] Type
  | DefineDatatype ByteString [ByteString] [Variant]
  | AtParticipant TrivExpr (Statement interactionDef)
  | SetParticipant TrivExpr
  | Publish TrivExpr [TrivExpr]
  | Deposit TrivExpr AssetMap
  | Withdraw TrivExpr AssetMap
  | Ignore Expression
  | Require TrivExpr
  | Return Expression
  | Switch TrivExpr [(Pattern, [(Statement interactionDef)])]
  deriving stock (Generic, Eq, Show)

data AnfInteractionDef = AnfInteractionDef
  { aidParticipantNames :: [ByteString],
    aidAssetNames :: [ByteString],
    aidArgumentNames :: [ByteString],
    aidBody :: [AnfStatement]
  }
  deriving stock (Generic, Eq, Show)

type AnfStatement = Statement AnfInteractionDef

data ProjectInteractionDef = ProjectInteractionDef
  { pidParticipantNames :: [ByteString],
    pidAssetNames :: [ByteString],
    pidArgumentNames :: [ByteString],
    pidInteractions :: [(ByteString, [ProjectStatement])]
  }
  deriving stock (Generic, Eq, Show)

type ProjectStatement = Statement ProjectInteractionDef

data Variant = Variant ByteString [Type]
  deriving stock (Generic, Show, Read, Eq)

data Expression
  = ExpectPublished ByteString
  | Digest [TrivExpr]
  | Sign TrivExpr
  | Input Type TrivExpr
  | EqlExpr TrivExpr TrivExpr
  | AppExpr TrivExpr [TrivExpr]
  | TrvExpr TrivExpr
  deriving stock (Generic, Eq, Show)

data GlowValue
  = Constructor ByteString Integer [GlowValue]
  | PubKey LedgerPubKey
  | Signature LedgerSignature
  | ByteString ByteString
  | Integer Integer
  | Boolean Bool
  | Unit
  deriving stock (Generic, Eq, Show)

data Pattern
  = VarPat ByteString
  | ValPat Constant
  deriving stock (Generic, Eq, Show)

newtype LedgerPubKey = LedgerPubKey ByteString
  deriving stock (Generic, Eq, Show)
  deriving newtype (IsString)

newtype LedgerSignature = LedgerSignature ByteString
  deriving stock (Generic, Eq, Show)
  deriving newtype (IsString)
