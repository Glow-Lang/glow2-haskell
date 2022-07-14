{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Glow.Runtime.Interaction.Base where

import Glow.Consensus.Lurk

import Control.Monad.RWS
-- import System.IO

import Control.Lens (makeLenses,Getter,to,(^.),(%=),use,(.~),(%~),Lens')
-- import Data.Function


import Prelude

import Glow.Precompiled.Base

import Glow.Runtime.Interaction.InterpretAnf



-- import Data.List as L
-- import qualified Data.Map as M

-- import Data.Maybe (fromMaybe)

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



-- executeCall :: (MonadIO m , MonadState ConsensusState m , MonadReader UUID m) =>  Call -> m () 













-- data LocalEnv = LocalEnv
--      { _lsContract :: PrecompiledContract
--      , _identities :: [Identity]
--      , _interactionParametersWN :: [GLValue]
--      , _ownIdentity :: Identity
--      }

-- data LocalState = LocalState
--      { _lsInputs :: [GLValue]
--      }


  
  

-- data ExecFlag =
--      Wait
--    | PromptInput String GLType
--    | SendCall Action Int


-- -- class (MonadState LocalState m , MonadReader LocalEnv m) => LIM m where 
-- --   emitCall :: ExecFlag -> m ()
-- --   -- updateState ::

-- -- type ConsensusSim = RWST LocalEnv [LocalState] LocalState IO 

-- data LocalSimEnv = LocalSimEnv
--      { _lseContract :: PrecompiledContract
--      , _lseIdentities :: [Identity]
--      , _lseInteractionParametersWN :: [GLValue]
--      }

  
-- class (Monad m , MonadState s m) => ConsensusSim m s where

--   env :: m LocalSimEnv

--   localStateL :: Identity -> m (Lens' s LocalState)
--   consensusStateL :: Identity -> m (Lens' s LMState)

--   promptInput :: Identity -> GLType -> m GLValue 
  
  

  
--   -- emitCall :: ExecFlag -> m ()
--   -- emitCall = \case
--   --   Wait -> error "imposible in ConsensusSim"
--   --   PromptInput s t -> undefined
--   --   SendCall a i -> do
--   --     modiy makeStateChange

