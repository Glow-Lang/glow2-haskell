{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Glow.Consensus.LurkMock where

import Control.Monad.RWS
-- import Control.Monad.Loops
import System.IO

import Control.Lens
import System.Console.Haskeline
import Data.Function
import System.Console.ANSI

import Data.Map (Map)
import qualified Data.Map as M

import Prelude

import Data.List
import Data.Maybe
import Text.Read (readMaybe)

import Numeric
import Data.Char

import Text.SExpression as S
-- import qualified Data.Text.Lazy as T


import System.Process.Typed as P
-- import qualified Data.ByteString.Lazy as LBS
-- import Data.ByteString.Lazy.Char8(unpack)

type ParticipantId = Int


data GLValue = GLNat Int | GLBool Bool | GLPF Int | DigestOf GLValue 
  deriving (Show , Read)

renderGLValue :: GLValue -> SExpr
renderGLValue = \case
  GLNat k -> S.List $ map (\case
                              '1' -> S.Atom "T"
                              _ -> S.List [])
                              
                        (reverse $ showIntAtBase 2 intToDigit k "")
  GLBool k -> if k then (S.Atom "T") else S.List []
  GLPF k -> Atom (show k)
  DigestOf x -> S.List [S.Atom "DIGEST" , renderGLValue x ] 
  
  

data LMState = LMState
  { _balances :: Map ParticipantId Int
  , _interactionParameters :: [GLValue]
  , _publicValues :: [GLValue]
  , _stateId :: Int
  }
 deriving (Show)
makeLenses ''LMState


data Action =
    Withdraw Int
  | Deposit Int
  | Publish GLValue
 deriving (Show , Read)

data Call = Call
  { _desiredStateId :: Int
  , _caller :: ParticipantId
  , _action :: Action
  }
 deriving (Show)
makeLenses ''Call

emptyLMState = LMState
  { _balances = M.empty
  , _publicValues = []
  , _interactionParameters = []
  , _stateId = 0
  }

cfInitialLMState = LMState
  { _balances = M.empty
  , _publicValues = []
  , _interactionParameters = [GLNat 10 , GLNat 2]
  , _stateId = 0
  }


renderCall :: Call -> SExpr
renderCall c =
  case c ^. action of
    Publish _ ->
      S.List [ S.Atom "PUBLISH"
             , S.Atom (show $ c ^. desiredStateId)
             , S.Atom (show $ c ^. caller ) ]
    _ -> let a = case c ^. action of
                     Withdraw x -> S.List [S.Atom "WITHDRAW" , renderGLValue $ GLNat x]
                     Deposit x -> S.List [S.Atom "DEPOSIT" , renderGLValue $ GLNat x]
                     _ -> S.List [] --imposible case
         in S.List
             [ S.Atom "ACTION"
             , S.Atom (show $ c ^. desiredStateId)
             , S.Atom (show $ c ^. caller )
             , a  ]


crudeQuote :: SExpr -> SExpr
crudeQuote = S.Atom . ('\'':) . render

mkLurkInput :: LMState -> Call -> SExpr
mkLurkInput s c = 
  let p = S.List (fmap renderGLValue (s ^. publicValues))
      a = renderCall c
  in S.List [ S.Atom "run-glow"
            , crudeQuote $ p
            , S.List (S.Atom "cf-code" : (fmap (crudeQuote . renderGLValue) (s ^. interactionParameters)))
            , if (s ^. stateId ==0) then S.List [] else S.Atom (show $ s ^. stateId)
            , crudeQuote $ a]  
  

verifyCall :: Call -> LM Bool 
verifyCall c =
  do s <- get
     liftIO $ putStrLn $ "\n Input for verifier:"
     liftIO $ putStrLn $ render $ mkLurkInput s c
     _ <- liftIO $ getLine
     x <- liftIO $ callLurk $ mkLurkInput s c
     -- liftIO $ putStrLn x
     -- _ <- liftIO $ getLine
     case x of
       "T" -> do
                 liftIO $ putStrLn $ "\nConfirmed validity of call, will execute state change."
                 _ <- liftIO $ getLine
                 return True
       _ -> do
                 liftIO $ putStrLn $ "\nUnable to confirm validity of call, won't execute state change."
                 _ <- liftIO $ getLine
                 return False


executeCall :: Call -> LM () 
executeCall c = do
  (stateId .= c ^. desiredStateId )
  case c ^. action of
    Withdraw w -> balances . at (c ^. caller) %= Just . (subtract w)  . fromMaybe 0
    Deposit d -> balances . at (c ^. caller) %= Just . (+ d)  . fromMaybe 0
    Publish v -> publicValues %= (++[v]) 

someCall = Call 5 0 (Withdraw 22)
  
someState =
   emptyLMState
   & interactionParameters .~  [(GLNat 10), (GLNat 2)]
   & stateId .~ 4 
   & publicValues .~  [DigestOf (GLNat 7), (GLNat 7), (GLNat 7)]
   & balances . at 0 ?~ 4
   & balances . at 1 ?~ 7



type LM = InputT (RWST () () LMState (IO))

instance MonadState LMState LM where
  get = lift get
  put = lift . put

printState :: LM ()
printState = do
  s <- get
  forM_
    [ "published :  " ++ (show $ s ^. publicValues)
    , "balances  :  " ++ (show $ s ^. balances)
    , "state id  :  " ++ (show $ s ^. stateId)
    ] (liftIO . putStrLn) 



interaction :: LM ()
interaction = do
  liftIO $ clearScreen
  liftIO $ setCursorPosition 3 0
  printState
  liftIO $ cursorDownLine 5
  mbC <- readCall
  case mbC of
    Nothing -> pure ()
    Just x -> do
       isVerified <- verifyCall x
       when isVerified $ executeCall x


readCall :: LM (Maybe Call)
readCall = do
  ((>>= readMaybe) <$> (getInputLine "participant? >")) >>= \case
      Nothing -> pure Nothing
      Just pId -> ((>>= readMaybe) <$> (getInputLine "desired state? >")) >>= \case
                     Nothing -> pure Nothing
                     Just sId ->  ((>>= readMaybe) <$> (getInputLine "action? >")) >>= \case
                         Nothing -> pure Nothing
                         Just a -> pure $ Just (Call sId pId a)

mainLM :: LM ()
mainLM = do
  forever interaction




completionLM :: CompletionFunc (RWST () () LMState IO)
completionLM = completeWord Nothing [' '] $
   \case
     x -> do
       liftIO $ clearScreen
       -- liftIO $ cursorUpLine 2
       pure $ (simpleCompletion <$> filter (isPrefixOf x) ["xxxxx","xyyy"])
     -- _ -> pure $ (simpleCompletion <$> [])

render :: SExpr -> String
render = \case
  S.Atom x -> x
  S.List x -> "(" <> (intercalate " " (render <$> x)) <> ")"
  _ -> "not implemented"


callLurk :: SExpr -> IO String
callLurk x =

   P.withProcessWait
   (   P.setStdin P.createPipe
     $ P.setStdout P.createPipe
     $ P.proc ("/Users/Marcin/lurk/bin/lurkx") []) $
       (\lp ->
          let lpOut = (P.getStdout lp)
              readLoop :: IO ()
              readLoop =  do
                b <- hWaitForInput lpOut 400
                if b then hGetChar lpOut >> readLoop
                     else return ()
                

          in do
             readLoop
             readLoop

             
             hPutStrLn (P.getStdin lp) ":load \"/Users/marcin/glow/agda/Glow/Simple/Lurk/host.lurk\""
             hFlush (P.getStdin lp)

             readLoop

             hPutStrLn (P.getStdin lp) (render x)
             hFlush (P.getStdin lp)
             
             y <- hGetLine (P.getStdout lp)

             hPutStrLn (P.getStdin lp) ":quit"
             hFlush (P.getStdin lp)             

             return y
          )

       where
         
         

runLM :: IO ()
runLM =
    void
    $ execRWST
      (runInputT (defaultSettings & setComplete completionLM ) mainLM )
        () cfInitialLMState


