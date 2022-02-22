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
  deriving stock (Show, Read, Eq, Ord)
  deriving newtype (IsString)

-- TODO: variable cleanup, only keep live variables between each transaction
type GlowContract = M.Map ExecutionPoint ([Statement], Maybe ExecutionPoint)

type VariableMap = M.Map ByteString GlowValue

type FunctionMap = M.Map ByteString (ByteString, [Statement])

type DatatypeMap = M.Map ByteString [(ByteString, Integer)]

type Function = (ByteString, [Statement])

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

-- TODO: support lambdas with CPS
data Statement
  = Label ByteString
  | DebugLabel ByteString
  | Declare ByteString
  | DefineInteraction [ByteString] [ByteString] [(ByteString, [Statement])]
  | Define ByteString Expression
  | DefineFunction ByteString ByteString [Statement]
  | DefineDatatype ByteString [(ByteString, Integer)]
  | SetParticipant GlowValueRef
  | ExpectDeposited GlowValueRef
  | ExpectWithdrawn GlowValueRef GlowValueRef
  | AddToDeposit GlowValueRef
  | AddToWithdraw GlowValueRef GlowValueRef
  | Ignore Expression
  | Require GlowValueRef
  | Return GlowValueRef
  deriving stock (Generic, Eq, Show)

-- deriving (FromJSON, ToJSON)

data Expression
  = ExpectPublished ByteString
  | IsValidSignature GlowValueRef GlowValueRef GlowValueRef
  | Apply ByteString GlowValueRef
  | NoOp
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
  { gdContract :: GlowContract,
    gdVariableMap :: VariableMap,
    -- separate from variable map to prevent mutual recursion
    gdFunctionMap :: FunctionMap,
    gdDatatypeMap :: DatatypeMap,
    gdExecutionPoint :: Maybe ExecutionPoint,
    gdDeadline :: Ledger.Slot
  }
  deriving stock (Generic, Eq, Show)
-}
