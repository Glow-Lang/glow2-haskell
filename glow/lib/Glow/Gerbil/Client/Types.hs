{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Glow.Gerbil.Client.Types where

import Data.Aeson hiding (Value)
import qualified Data.Map.Strict as M
import GHC.Generics
import Glow.Ast.Common (Id)
import Glow.Gerbil.Types
import Glow.Prelude

type SExprString = String

--------------------------------------------
-- Incoming representation from endpoints --
--------------------------------------------

data RawCreateParams = RawCreateParams
  { source :: SExprString, -- project.sexp
    initialVariableMap :: SExprString, -- initial arguments to initialize contract interaction
    rawTimeoutLength :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data RawMoveParams = RawMoveParams
  { rawVariableMap :: SExprString,
    rawEntryPoint :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

------------------------------------------
-- Representation within smart contract --
------------------------------------------
-- NOTE: Eventually parsing logic should be glow's responsibility
-- and this should be expected shape of data at endpoints.

data CreateParams = CreateParams
  { datatypes :: DatatypeMap,
    participants :: M.Map Id LedgerPubKey, -- TODO: type synonym for this
    arguments :: VariableMap,
    contract :: GlowProjectContract, -- consensus program
    timeoutLength :: Integer
  }
  deriving stock (Generic, Eq, Show)

-- TODO: deriving currently fails since our ByteString wrapper doesn't implement FromJSONKey
-- deriving anyclass (FromJSON, ToJSON)

data MoveParams = MoveParams
  { variableMap :: VariableMap,
    entryPoint :: String
  }
  deriving stock (Generic, Eq, Show)

-- deriving anyclass (FromJSON, ToJSON)
