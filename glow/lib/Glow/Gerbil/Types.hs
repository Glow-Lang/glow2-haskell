{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Glow.Gerbil.Types where

import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import GHC.Generics hiding (Datatype)
import Glow.Ast.Common
import Glow.Prelude
import Data.Traversable

-- TODO: variable cleanup, only keep live variables between each transaction
type GlowProjectContract pt = Map ExecutionPoint ([ProjectStatement pt], Maybe ExecutionPoint)

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
data Statement interactionDef pt
  = Label Id
  | DebugLabel Id
  | DefineInteraction Id interactionDef
  | Define Id Expression
  | DefineFunction Id [Id] [(Statement interactionDef pt)]
  | -- Note: in the grammar there are both (deftype id type) and
    -- (deftype (id tyvar ...) type); here we just combine them, where the
    -- first variant has an empty list (likewise for defdata).
    DefineType Id [Id] Type
  | DefineDatatype Id [Id] [Variant]
  | AtParticipant Id (Statement interactionDef pt)
  | SetParticipant Id
  | Publish pt Id [Id]
  | Deposit pt Id AssetMap
  | Withdraw pt Id AssetMap
  | Ignore Expression
  | Require TrivExpr
  | Return Expression
  | Switch TrivExpr [(Pat, [(Statement interactionDef pt)])]
  deriving stock (Generic, Eq, Show,Functor,Foldable,Traversable)

data AnfInteractionDef pt = AnfInteractionDef
  { aidParticipantNames :: [Id],
    aidAssetNames :: [Id],
    aidArgumentNames :: [Id],
    aidBody :: [AnfStatement pt]
  }
  deriving stock (Generic, Eq, Show)

-- instance Functor AnfInteractionDef where
--   fmap f x = x  {aidBody = fmap (fmap f) (aidBody x)} 

type AnfStatement pt = Statement (AnfInteractionDef pt) pt

data ProjectInteractionDef pt = ProjectInteractionDef
  { pidParticipantNames :: [Id],
    pidAssetNames :: [Id],
    pidArgumentNames :: [Id],
    pidInteractions :: [(Maybe Id, [ProjectStatement pt])]
  }
  deriving stock (Generic, Eq, Show)

type ProjectStatement pt = Statement (ProjectInteractionDef pt) pt


data Variant = Variant Id [Type]
  deriving stock (Generic, Show, Read, Eq)

data Expression
  = ExpectPublished Id
  | Digest [TrivExpr]
  | Sign TrivExpr
  | Input Type TrivExpr
  | EqlExpr TrivExpr TrivExpr
  | AppExpr Id [TrivExpr]
  | TrvExpr TrivExpr
  deriving stock (Generic, Eq, Show)

data PureExpression
  = PDigest [TrivExpr]
  | PEqlExpr TrivExpr TrivExpr
  | PAppExpr Id [TrivExpr]
  | PTrvExpr TrivExpr
  deriving stock (Generic, Eq, Show)

toPure :: Expression -> Either String PureExpression
toPure = \case
    ExpectPublished _ -> Left "ExpectPublished"
    Digest xs -> Right $ PDigest xs
    Sign _ -> Left "Sign"
    Input _ _ -> Left "Input"
    EqlExpr x y -> Right $ PEqlExpr x y 
    AppExpr x xs -> Right $ PAppExpr x xs
    TrvExpr x -> Right $ PTrvExpr x

fromPure :: PureExpression -> Expression
fromPure = \case
    PDigest xs -> Digest xs
    PEqlExpr x y -> EqlExpr x y 
    PAppExpr x xs -> AppExpr x xs
    PTrvExpr x -> TrvExpr x

data GlowValue
  = Constructor Id Integer [GlowValue]
  | PubKey LedgerPubKey
  | Signature LedgerSignature
  | ByteString ByteString
  | Integer Integer
  | Boolean Bool
  | Unit
  deriving stock (Generic, Eq, Show)

constantToValue :: Constant -> GlowValue 
constantToValue = \case
  CBool x -> Boolean x
  CByteString x -> ByteString x
  CInt _ x -> Integer x
  CUnit -> Unit

constantToTypedValue :: Constant -> GlowValue 
constantToTypedValue = \case
  CBool x -> Boolean x
  CByteString x -> ByteString x
  CInt _ x -> Integer x
  CUnit -> Unit


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

newtype LedgerPubKey = LedgerPubKey { lpkBS :: ByteString }
  deriving stock (Generic, Eq, Show, Read,Ord)
  deriving newtype (IsString)

newtype LedgerPrivKey = LedgerPrivKey { lprkBS :: ByteString }
  deriving stock (Generic, Eq, Show, Read,Ord)
  deriving newtype (IsString)


newtype LedgerSignature = LedgerSignature ByteString
  deriving stock (Generic, Eq, Show, Read,Ord)
  deriving newtype (IsString)
