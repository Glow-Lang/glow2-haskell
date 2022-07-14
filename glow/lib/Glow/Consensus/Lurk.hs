{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glow.Consensus.Lurk where


import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.IO as TIO
import Glow.Prelude
-- import Text.SExpression as S 

import qualified Data.List as L

-- import qualified Glow.Ast.Targets.Lurk as O

-- import Prelude (read)

import qualified Data.Text as TT

-- import Glow.Translate.LurkToSExpr
import Control.Lens (makeLenses,(.~),(^.),(%~))
-- import qualified Control.Lens as L

import qualified GHC.Generics as G
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText)
import Text.SExpression as S
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey),lpkBS)


import Glow.Ast.Targets.Lurk as LT
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack,pack)

type ParticipantId = Int

data GLType = GLNatT | GLBoolT | GLStringT | GLPFT | DigestT | GLUnitT 
  deriving (Show , Read , Eq , G.Generic, Ord)

instance ToJSON GLType where
instance FromJSON GLType where

data GLValue = GLNat Int | GLBool Bool | GLString T.Text | GLPF Int | DigestOf GLValue | GLUnit 
  deriving (Show , Read , Eq , G.Generic, Ord)

instance ToJSON GLValue where
instance FromJSON GLValue where


prettyGLValue :: GLValue -> String 
prettyGLValue = \case
  GLNat k -> show k
  GLBool k -> show k
  GLString s -> show s
  GLPF k -> show k L.++ "(PF)"
  DigestOf x -> "dig(" L.++ (prettyGLValue x) L.++  ")" 
  GLUnit -> "()" 



instance ToJSON ByteString where
  toJSON = toJSON . unpack
  
instance FromJSON ByteString where 
  parseJSON x = pack <$> parseJSON x
  
instance ToJSON LedgerPubKey where
instance FromJSON LedgerPubKey where

instance ToJSONKey LedgerPubKey where
  toJSONKey = toJSONKeyText ( TT.pack . unpack . lpkBS) 
  

instance FromJSONKey LedgerPubKey where
  fromJSONKey = FromJSONKeyText (LedgerPubKey . pack . TT.unpack)
  
type LurkSource = T.Text

data GLContainer = GLContainer
  { _participantsIds :: ([String])
  , _stateTransitionVerifier :: LurkSource
  , _signature :: [GLType]
  }
  deriving (Show, Read, Eq)
makeLenses ''GLContainer


data Action =
    Withdraw Int
  | Deposit Int
  | Publish GLValue
 deriving (Show , Read, G.Generic, Eq , Ord)

instance ToJSON Action where
instance FromJSON Action where


data Call = Call
  { _desiredStateId :: Int
  , _caller :: LedgerPubKey
  , _action :: Action
  }
 deriving (Show , Read, G.Generic,Eq)
makeLenses ''Call

instance ToJSON Call where
instance FromJSON Call where

-- interationLibHead :: String
-- interationLibHead = "(let ((glow-code "

-- interationLibFooter :: String
-- interationLibFooter = "))(current-env))"



render :: SExpr -> String
render = \case
  S.Atom x -> x
  S.List x -> "(" L.++ (L.intercalate " " (render <$> x)) L.++ ")"
  _ -> "not implemented"
  --ConsList
  -- Number x -> pack (show x)

renderGLValue :: GLValue -> SExpr
renderGLValue = \case
  GLNat 0 -> S.Atom "NIL"
  GLNat k -> S.List $ map (\case
                              '1' -> S.Atom "T"
                              _ -> S.Atom "NIL")
                              
                        (reverse $ showIntAtBase 2 intToDigit k "")
  GLBool k -> if k then (S.Atom "T") else (S.Atom "NIL")
  GLString k -> Atom (show k)
  GLPF k -> Atom (show k)
  DigestOf x -> S.List [S.Atom "DIGEST" , renderGLValue x ]
  GLUnit -> S.Atom "'glow-unit-lit" 

translateGLValue :: GLValue -> LT.Expr ()
translateGLValue = \case
  GLNat 0 -> LT.ExNil ()
  GLNat k -> LT.ExQuote () $ S.List $ map (\case
                              '1' -> S.Atom "T"
                              _ -> S.Atom "NIL")
                              
                        (reverse $ showIntAtBase 2 intToDigit k "")
        -- LT.mkConsList $ map (\case
        --                       '1' -> LT.ExT ()
        --                       _ -> LT.ExNil ())
                              
        --                 (reverse $ showIntAtBase 2 intToDigit k "")
  GLBool k -> if k then (LT.ExT ()) else LT.ExNil ()
  GLString s -> (LT.ExString () $ s)
  GLPF k -> LT.ExFieldElem () k
  DigestOf x -> LT.mkConsList $ [LT.ExQuote () (S.Atom "DIGEST") , translateGLValue x ]
  GLUnit -> LT.ExQuote () (S.Atom "glow-unit-lit")




data LMState = LMState
  { _publicValues :: [GLValue]
  , _stateId :: Int
  }
 deriving (Show,G.Generic)
makeLenses ''LMState

instance ToJSON LMState where
instance FromJSON LMState where

  
initialLMState :: LMState
initialLMState = LMState
  { _publicValues = []
  , _stateId = 0
  }


data LMEnv = LMEnv
  { _identities :: [LedgerPubKey]
  , _interactionParameters :: [GLValue]
  , _stateTransitionVerifierSrc :: LurkSource
  }
 deriving (Show,G.Generic)
makeLenses ''LMEnv


instance ToJSON LMEnv where
instance FromJSON LMEnv where



data DeployedContract = DeployedContract
  { _initializationData :: LMEnv
  , _currentState :: LMState
  }
 deriving (Show, G.Generic)
makeLenses ''DeployedContract

instance ToJSON DeployedContract where
instance FromJSON DeployedContract where



  
makeStateChange :: Call -> LMState -> LMState 
makeStateChange c s = 
  let s' = (stateId .~ c ^. desiredStateId) s
  in (case c ^. action of
       Withdraw _ -> s'  
       Deposit _ -> s'
       Publish v -> (publicValues %~ (L.++[v])) s')  
