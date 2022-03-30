{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}

module Glow.Gerbil.Types where

import Control.Monad.Fail (fail)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.ByteString.Base64.Lazy as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map.Strict as M
import GHC.Generics hiding (Datatype)
import Glow.Prelude

-- Wrapper, so we can define instances. (Do we actually need JSON instances?
-- maybe just drop this?)
newtype ByteString = WrappedByteString {toLBS :: LBS.ByteString}
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Semigroup)

instance Show ByteString where
  show = show . toLBS

-- TODO: variable cleanup, only keep live variables between each transaction
type GlowProjectContract = M.Map ExecutionPoint ([ProjectStatement], Maybe ExecutionPoint)

type VariableMap = M.Map ByteString GlowValue

type ProjectFunctionMap = M.Map ByteString (ByteString, [ProjectStatement])

type DatatypeMap = M.Map ByteString [(ByteString, Integer)]

type AssetMap = M.Map ByteString GlowValueRef

type ProjectFunction = (ByteString, [ProjectStatement])

type ExecutionPoint = ByteString

instance ToJSON ByteString where
  toJSON =
    toLBS
      >>> B16.encode
      >>> LBS8.unpack
      >>> toJSON

instance FromJSON ByteString where
  parseJSON v = do
    str <- parseJSON v
    case B16.decode (LBS.pack str) of
      Right bs -> pure (WrappedByteString bs)
      Left e -> fail e

-- | An MLsub type, as emitted by the frontend.
data Type
  = TyArrow [Type] Type
  | TyName ByteString
  | TyNameSubtype ByteString Type
  | TyTuple [Type]
  | -- | TyUnknown is a placeholder until we actually support parsing everything;
    -- it is convienient to be able print out a larger type which has un-parsable
    -- bits, so we can see what of a program we handle and what we don't.
    TyUnknown ByteString
  -- TODO:
  --
  -- - [x] type:name
  -- - [x] type:name-subtype
  -- - [ ] type:var
  -- - [ ] type:app
  -- - [ ] type:tuple
  -- - [ ] type:record
  -- - [x] type:arrow
  deriving (Eq, Show)

-- TODO: support lambdas with CPS
data Statement interactionDef
  = Label ByteString
  | DebugLabel ByteString
  | DefineInteraction interactionDef
  | Define ByteString Expression
  | DefineFunction ByteString [ByteString] [(Statement interactionDef)]
  | DefineDatatype ByteString [(ByteString, Integer)]
  | AtParticipant GlowValueRef (Statement interactionDef)
  | SetParticipant GlowValueRef
  | Publish GlowValueRef [GlowValueRef]
  | Deposit GlowValueRef AssetMap
  | Withdraw GlowValueRef AssetMap
  | Ignore Expression
  | Require GlowValueRef
  | Return Expression
  | Switch GlowValueRef [(Pattern, [(Statement interactionDef)])]
  deriving stock (Generic, Eq, Show)

-- deriving (FromJSON, ToJSON)

data AnfInteractionDef = AnfInteractionDef
  { aidParticipantNames :: [ByteString],
    aidAssetNames :: [ByteString],
    aidArgumentNames :: [ByteString],
    aidBody :: [AnfStatement]
  }
  deriving stock (Generic, Eq, Show)

-- deriving (FromJSON, ToJSON)

type AnfStatement = Statement AnfInteractionDef

data ProjectInteractionDef = ProjectInteractionDef
  { pidParticipantNames :: [ByteString],
    pidAssetNames :: [ByteString],
    pidArgumentNames :: [ByteString],
    pidInteractions :: [(ByteString, [ProjectStatement])]
  }
  deriving stock (Generic, Eq, Show)

-- deriving (FromJSON, ToJSON)

type ProjectStatement = Statement ProjectInteractionDef

data Expression
  = ExpectPublished ByteString
  | Digest [GlowValueRef]
  | Sign GlowValueRef
  | Input Type GlowValueRef
  | EqlExpr GlowValueRef GlowValueRef
  | AppExpr GlowValueRef [GlowValueRef]
  | TrvExpr GlowValueRef
  deriving stock (Generic, Eq, Show)

-- deriving anyclass (FromJSON, ToJSON)

-- TODO: how to encode expected type?
data GlowValueRef
  = Explicit GlowValue
  | Variable ByteString
  deriving stock (Generic, Eq, Show)

-- deriving anyclass (FromJSON, ToJSON)

data GlowValue
  = Constructor ByteString Integer [GlowValue]
  | PubKey LedgerPubKey
  | Signature LedgerSignature
  | ByteString ByteString
  | Integer Integer
  | Boolean Bool
  | Unit
  deriving stock (Generic, Eq, Show)

-- deriving anyclass (FromJSON, ToJSON) -- ToSchema, ToArgument)

data TrivialPattern
  = VarPat ByteString
  | ValPat GlowValue
  deriving stock (Generic, Eq, Show)

data PatternWith pattern
  = PwTrivial TrivialPattern
  | PwAppCtor ByteString [pattern]
  deriving stock (Generic, Eq, Show)

newtype Pattern = Pattern (PatternWith Pattern)
  deriving stock (Generic, Eq, Show)
type ShallowPattern = PatternWith TrivialPattern

newtype LedgerPubKey = LedgerPubKey ByteString
  deriving stock (Generic, Eq, Show)
  deriving newtype (IsString)

-- deriving anyclass (FromJSON, ToJSON) -- ToSchema, ToArgument)

newtype LedgerSignature = LedgerSignature ByteString
  deriving stock (Generic, Eq, Show)
  deriving newtype (IsString)

-- deriving anyclass (FromJSON, ToJSON) -- ToSchema, ToArgument)

{-
data GlowDatum = GlowDatum
  { gdContract :: GlowProjectContract,
    gdVariableMap :: VariableMap,
    -- separate from variable map to prevent mutual recursion
    gdFunctionMap :: FunctionMap,
    gdDatatypeMap :: DatatypeMap,
    gdExecutionPoint :: Maybe ExecutionPoint,
    gdDeadline :: Ledger.Slot
  }
  deriving stock (Generic, Eq, Show)
-}
