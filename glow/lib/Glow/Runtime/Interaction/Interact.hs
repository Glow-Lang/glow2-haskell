{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Glow.Runtime.Interaction.Interact where

import Glow.Consensus.Lurk

import Control.Monad.RWS
-- import System.IO

import Control.Lens (makeLenses,Getter,to,(^.),(%=),(.=),use,(.~),(%~),Lens',Lens)
-- import Data.Function

import Prelude

import Glow.Precompiled.Base

import Glow.Runtime.Interaction.InterpretAnf

import Glow.Ast.Common

import Data.Proxy

import Data.List as L

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromJust,catMaybes,isJust,mapMaybe,isNothing)

import Data.Functor ((<&>))

import Glow.Gerbil.Types (LedgerPubKey(LedgerPubKey))
-- import Text.SExpression as S
-- import qualified Data.Text.Lazy as T
-- import qualified Data.Text.Lazy.IO as TIO


-- import Glow.Consensus.Lurk
-- import qualified GHC.Generics as G
-- import Data.Aeson (ToJSON,FromJSON)
-- import Data.Aeson.Types (parseEither)
-- import qualified Data.Aeson as A
-- import qualified System.Process as P
-- import Text.RawString.QQ
import Data.UUID
-- import Data.UUID.V4

import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as BS


data LocalInteractEnv = LocalInteractEnv
     { _lieContract :: PrecompiledContract
     , _lieMyIdentity :: LedgerPubKey
     , _lieMyRole :: ByteString
     , _lieInteractionParameters :: [GLValue]
     }
makeLenses ''LocalInteractEnv

  
class (Monad m , MonadState s m) => ParticipantM m s where
  
  
  env :: m LocalInteractEnv

  getConsensusState :: m LMState
  
  localState :: Proxy m -> Lens' s LocalState

  promptInput :: String -> GLType -> m GLValue 

  onError :: String -> m ()

  onWait :: m ()
  onWait = return ()
  
  afterCall :: Call -> m ()


  sendCall :: Call -> m ()

  execFlag :: Proxy s -> Proxy m -> m (Either String (Maybe ExecFlag))
  execFlag ps pm = do
    cs <- getConsensusState
    e <- env
    ls <- use (localState pm)
    let pc = e ^. lieContract
        prms = zip (pc ^. pcParamsIds) (e ^. lieInteractionParameters)
        ef = computeExecFlag
                     (pc ^. pcTypeTable)
                     (Id $ e ^. lieMyRole)
                     cs
                     ls
                     prms
                     (pc ^. pcAnf)        
    return ef

  runInteraction :: Proxy s -> Proxy m -> m ()
  runInteraction ps pm = do
    e <- env
    ef <- execFlag ps pm
    case ef of
      Left s -> onError s
      Right ef ->
         case ef of
           Nothing -> return ()
           Just x ->
             case x of
               SendCall a j ->
                 let cl = Call j (e ^. lieMyIdentity) a
                 in (sendCall cl) >> afterCall cl >> runInteraction ps pm
               PromptInput msg ty -> do
                 v <- promptInput msg ty
                 (localState pm . lsInputs) %= (++ [v])
                 runInteraction ps pm
               Wait -> onWait >> runInteraction ps pm
    
type CmdLineI = RWST LocalInteractEnv () LocalState IO

-- instance ParticipantM CmdLineI LocalState where
--   env = ask
--   consensusStateL' _ = sssConsensusState

--   localStateL _ i f sss = 
--     fmap (\ls' -> (sssLocalState %~ M.insert i ls') sss ) $ f $ fromJust (M.lookup i (sss ^. sssLocalState))

--   promptInput i _ _ = 
--     (use $ sssPendingInputsFor i) >>=
--      \case
--         [] -> error "run out of inputs!"
--         (x : xs) -> do
--            sssPendingInputsFor i .= xs
--            return x
--   onCall _ c = do
--     cs <- use $ consensusStateL' (Proxy :: Proxy ScriptedSim)  
--     tell ([show cs,show c, "\n"])
  
-- runScriptedSim :: PrecompiledContract -> [GLValue] -> [(Id,(LedgerPubKey , [GLValue]))] -> IO [String]
-- runScriptedSim pc params x' =
--   let x = M.fromList x'
--   in fmap snd $ evalRWST (runSim Proxy Proxy :: (ScriptedSim ())) 
--       (LocalSimEnv pc (fmap fst x) params)
--       (ScriptedSimState (fmap snd x) (fmap (\_ -> LocalState []) x) initialLMState)
     
