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
module Glow.Runtime.Interaction.Sim where

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

-- import qualified Data.ByteString.Char8 as BS


data LocalSimEnv = LocalSimEnv
     { _lseContract :: PrecompiledContract
     , _lseIdentities :: Map Id LedgerPubKey
     , _lseInteractionParameters :: [GLValue]
     }
makeLenses ''LocalSimEnv
  
class (Monad m , MonadState s m) => ConsensusSim m s where

  env :: m LocalSimEnv

  localStateL :: Proxy m -> Id -> (Lens' s LocalState)

  
  consensusStateL' :: Proxy m -> Lens' s LMState

  onCall :: Proxy s -> Call -> m ()
  onCall _ _ = return () 

  afterCall :: Proxy s -> Call -> m ()
  afterCall _ _ = return () 


  promptInput :: Id -> String -> GLType -> m GLValue 

  getExecFlags :: Proxy s -> m (Maybe (Maybe Call , [(Id,String,GLType)])) 
  getExecFlags _ = do
    e <- env
    let pc = e ^. lseContract
        allI = M.keys (e ^. lseIdentities)
    cs <- use $ consensusStateL' (Proxy :: Proxy m)
    lSts <- forM allI $ \i -> do
                  x <- use $ localStateL (Proxy :: Proxy m) i
                  return (i , x)
    let prms = zip (pc ^. pcParamsIds) (e ^. lseInteractionParameters)
    let efs = fmap (L.sortOn snd) $
               forM lSts $ \(i , lst) -> 
                  (computeExecFlag
                     (pc ^. pcTypeTable)
                     i
                     cs
                     lst
                     prms
                     (pc ^. pcAnf) <&> (i,))

    let xxs =
          case efs of
               Left err -> error err
               Right ((i , Just (SendCall a j)) : (_ , Just (SendCall _ _) ) : _) -> error "two possible calls!"
               Right ((i , Just (SendCall a j)) : xs) -> Just (Just (Call j ((e ^. lseIdentities) M.! i) a) , xs)  
               Right xs -> if all (isNothing . snd) xs
                           then Nothing
                           else Just (Nothing , xs)    
    return $ fmap (\(x , xs ) -> (x , mapMaybe (\(i , y) -> case y of
                                          Just (PromptInput msg ty) -> Just (i , msg ,  ty)
                                          Just (Wait) -> Nothing 
                                          Just (SendCall _ _) -> error "imposible"
                                          Nothing -> error "imposible"
                               
                               )
                       $ filter (isJust . snd) xs)) xxs
  
  runSim :: Proxy s -> Proxy m -> m () 
  runSim p m' = do
    getExecFlags p >>=
     \case
       Nothing -> return ()
       Just (Nothing , []) -> error "imposible"
       Just (_, (i,ss,t) : _) -> do
         v <- promptInput i ss t
         (localStateL m' i . lsInputs) %= (++ [v])
         runSim p m'
         
       Just (Just x , _) -> (onCall p x) >> (consensusStateL' m' %= makeStateChange x) >> (afterCall p x) >> runSim p m'
     
data ScriptedSimState = ScriptedSimState
     { _sssPendingInputs :: Map Id [GLValue]
     , _sssLocalState :: Map Id LocalState
     , _sssConsensusState :: LMState
     }
makeLenses ''ScriptedSimState     
  
sssPendingInputsFor :: Id -> Lens' ScriptedSimState [GLValue] 
sssPendingInputsFor i f sss = 
          fmap (\ls' -> (sssPendingInputs %~ M.insert i ls') sss ) $ f $ fromJust (M.lookup i (sss ^. sssPendingInputs))
      
type ScriptedSim = RWST LocalSimEnv [String] ScriptedSimState IO

instance ConsensusSim ScriptedSim ScriptedSimState where
  env = ask
  consensusStateL' _ = sssConsensusState

  localStateL _ i f sss = 
    fmap (\ls' -> (sssLocalState %~ M.insert i ls') sss ) $ f $ fromJust (M.lookup i (sss ^. sssLocalState))

  promptInput i msg _ = do
    -- ((use sssPendingInputs) >>= (liftIO . putStrLn . show))
    -- ((use sssLocalState) >>= (liftIO . putStrLn . show))
    (use $ sssPendingInputsFor i) >>=
     \case
        [] -> error $ "run out of inputs! " ++ msg
        (x : xs) -> do
           sssPendingInputsFor i .= xs
           return x
  onCall _ c = do
    cs <- use $ consensusStateL' (Proxy :: Proxy ScriptedSim)  
    tell ([show cs,show c])

  afterCall _ c = do
    cs <- use $ consensusStateL' (Proxy :: Proxy ScriptedSim)  
    tell ([show cs, "\n"])

  
runScriptedSim :: PrecompiledContract -> [GLValue] -> [(Id,(LedgerPubKey , [GLValue]))] -> IO [String]
runScriptedSim pc params x' =
  let x = M.fromList x'
  in fmap snd $ evalRWST (runSim Proxy Proxy :: (ScriptedSim ())) 
      (LocalSimEnv pc (fmap fst x) params)
      (ScriptedSimState (fmap snd x) (fmap (\_ -> LocalState []) x) initialLMState)
     
