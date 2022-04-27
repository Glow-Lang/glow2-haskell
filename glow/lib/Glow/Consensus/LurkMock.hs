{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Glow.Consensus.LurkMock where

import Control.Monad.RWS
-- import Control.Monad.Loops
import System.IO

import Control.Lens
import System.Console.Haskeline
import Data.Function
import System.Console.ANSI
import System.Environment (getArgs)

-- import Data.Map (Map)
-- import qualified Data.Map as M

import Prelude

import Data.List as L
-- import Data.Maybe
import Text.Read (readMaybe)

import Numeric
import Data.Char

import Text.SExpression as S
-- import qualified Data.Text.Lazy as T

import Glow.Translate.LurkFromAgdaDev 

import System.Process.Typed as P
-- import qualified Data.ByteString.Lazy as LBS
-- import Data.ByteString.Lazy.Char8(unpack)
import Text.RawString.QQ




renderGLValue :: GLValue -> SExpr
renderGLValue = \case
  GLNat k -> S.List $ map (\case
                              '1' -> S.Atom "T"
                              _ -> S.List [])
                              
                        (reverse $ showIntAtBase 2 intToDigit k "")
  GLBool k -> if k then (S.Atom "T") else S.List []
  GLPF k -> Atom (show k)
  DigestOf x -> S.List [S.Atom "DIGEST" , renderGLValue x ]
  GLUnit -> S.Atom "'glow-unit-lit" 

  

data LMEnv = LMEnv
  { _participantsLabels :: [String]
  , _interactionParametersWN :: [(String , GLValue)]
  , _contractFileName :: FilePath
  , _fileToLogActions :: FilePath
  }
 deriving (Show)
makeLenses ''LMEnv

 
interactionParameters :: Getter LMEnv [GLValue] 
interactionParameters = interactionParametersWN . (to (map snd)) 

data LMState = LMState
  { _publicValues :: [GLValue]
  , _stateId :: Int
  }
 deriving (Show)
makeLenses ''LMState

initialLMState :: LMState
initialLMState = LMState
  { _publicValues = []
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

mkLurkInput :: LMEnv -> LMState -> Call -> SExpr
mkLurkInput e s c = 
  let p = S.List (fmap renderGLValue (s ^. publicValues))
      a = renderCall c
  in S.List [ S.Atom "run-glow"
            , crudeQuote $ p
            , S.List (S.Atom "glow-code" : (fmap (crudeQuote . renderGLValue) (e ^. interactionParameters)))
            , if (s ^. stateId ==0) then S.List [] else S.Atom (show $ s ^. stateId)
            , crudeQuote $ a]  
  

verifyCall :: (MonadIO m , MonadState LMState m , MonadReader LMEnv m) => Call -> m Bool 
verifyCall c =
  do e <- ask
     s <- get
     liftIO $ putStrLn $ "\n Input for verifier:"
     liftIO $ putStrLn $ render $ mkLurkInput e s c
     _ <- liftIO $ getLine
     x <- liftIO $ callLurk (e ^. contractFileName) $ mkLurkInput e s c
     -- liftIO $ putStrLn x
     -- _ <- liftIO $ getLine
     case x of
       "T" -> do
                 liftIO $ putStrLn $ "\nConfirmed validity of call, will execute state change."
                 _ <- liftIO $ getLine
                 return True
       lurkOutput -> do
                 liftIO $ putStrLn $ lurkOutput
                 liftIO $ putStrLn $ ""
                 liftIO $ putStrLn $ "\nUnable to confirm validity of call, won't execute state change."
                 _ <- liftIO $ getLine
                 return False

emitAction :: (MonadIO m , MonadState LMState m , MonadReader LMEnv m , MonadWriter [String] m) =>  String ->  m ()
emitAction x = do
  withSideConsole (\h -> hPutStrLn h x)
  tell [x]
    
executeCall :: (MonadIO m , MonadState LMState m , MonadReader LMEnv m , MonadWriter [String] m) =>  Call -> m () 
executeCall c = do 
  (stateId .= c ^. desiredStateId )
  case c ^. action of
    Withdraw _ -> emitAction $ show $ c ^. action 
    Deposit _ -> emitAction $ show $ c ^. action
    Publish v -> publicValues %= (++[v]) 

type LMS = RWST LMEnv [String] LMState (IO)

type LM = InputT LMS



instance MonadState LMState LM where
  get = lift get
  put = lift . put

instance MonadReader LMEnv LM where
  ask = lift ask
  local f = mapInputT (local f)

-- TODO :: Fix this!!
instance MonadWriter [String] LM where
  tell = lift . tell
  listen = undefined    
  pass = undefined

printState :: LM ()
printState = do
  e <- ask
  s <- get
  forM_
    [ "params :  " ++ (L.intercalate ", "
                          $ [ pName ++ " = " ++ prettyGLValue pVal
                            | (pName , pVal) <- e ^. interactionParametersWN])
    , ""
    , "published :  " ++ (L.intercalate ", " $ L.map prettyGLValue $ s ^. publicValues)
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


  
readOrAbort :: Read a => String -> LM (Maybe a)
readOrAbort info = do
  minput <- getInputLine (info ++ " >")
  case minput of
    Nothing -> (readOrAbort info)
    Just ":a" -> return Nothing
    Just x ->
       case (readMaybe x) of
         Nothing -> do liftIO $ putStrLn "error parsing, try again or enter :a to abort"
                       (readOrAbort info)
         Just x' -> return (Just x')


data ActionType = W | D | P
  deriving (Read)                  

readCall :: LM (Maybe Call)
readCall = do
  (readOrAbort "participant?") >>= \case
      Nothing -> pure Nothing
      Just pId -> (readOrAbort "desired state?") >>= \case
                     Nothing -> pure Nothing
                     Just sId -> fmap (Call sId pId) <$> (
                       (readOrAbort "Withdraw(W)/Deposit(D)/Publish(P)?") >>= \case
                         Nothing -> pure Nothing
                         Just W -> (fmap Withdraw) <$> readOrAbort "amount?"
                         Just D -> (fmap Deposit) <$> readOrAbort "amount?"
                         Just P -> (fmap Publish) <$> readOrAbort "value?")


withSideConsole :: (MonadIO m , MonadState LMState m , MonadReader LMEnv m) =>  (Handle -> IO()) -> m ()
withSideConsole x = do
  f <- asks (_fileToLogActions)
  liftIO $ withFile f WriteMode x

mainLM :: LM ()
mainLM = do
  withSideConsole hClearScreen
  forever interaction




completionLM :: CompletionFunc (RWST LMEnv [String] LMState IO)
completionLM = completeWord Nothing [' '] $
   \case
     x -> do
       liftIO $ clearScreen
       -- liftIO $ cursorUpLine 2
       pure $ (simpleCompletion <$> filter (isPrefixOf x) ["xxxxx","xyyy"])
     -- _ -> pure $ (simpleCompletion <$> [])


callLurk :: FilePath -> SExpr -> IO String
callLurk fp x =

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

             
             hPutStrLn (P.getStdin lp) ":load \"/Users/marcin/glow/agda/Glow/Simple/Lurk/glow.lurk\""
             hFlush (P.getStdin lp)
             readLoop

             hPutStrLn (P.getStdin lp) (":load \""++ fp ++ "\"")
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

parametersPrompt :: ([(String , GLType)]) -> IO [(String , GLValue)]
parametersPrompt = traverse $ \(pName , pTy) -> do  
  putStrLn $ "Enter value of param " ++ pName ++ " (" ++ show pTy ++ ")"
  (pName,)  <$> case pTy of
                     GLNatT -> GLNat <$> readLn
                     GLBoolT -> GLBool <$> readLn
                     GLPFT  -> GLPF <$> readLn
                     DigestT  -> DigestOf <$> readLn
                     GLUnitT -> return GLUnit
    
                
initialization :: FilePath -> IO LMEnv
initialization iName = do
  (GLContainer ptps lurkFileName paramsTy) <- pickContainerFromAgdaFile iName
  clearScreen
  params <- parametersPrompt paramsTy
  return $ LMEnv ptps params lurkFileName "/dev/ttys005"  


runLMInteractive :: LMEnv -> IO ()
runLMInteractive e = 
  void $ execRWST
    (runInputT (defaultSettings & setComplete completionLM ) mainLM )
      e initialLMState


-- interactionTestInitialization :: FilePath -> IO LMEnv
-- interactionTestInitialization iName = do
--   (GLContainer ptps lurkFileName paramsTy) <- pickContainerFromAgdaFile iName
--   clearScreen
--   params <- parametersPrompt paramsTy
--   return $ LMEnv ptps params lurkFileName "/dev/ttys005"  



runLMTest :: GLInteractionTest -> IO ()
runLMTest glit =
  void $ do
    let (GLContainer ptps lurkFileName _b) = glit ^. contract
        -- todo verify format of parameters!!
        initEnv = LMEnv ptps (glit ^. parameters) lurkFileName "/dev/ttys005"  
    (mbInvalidCall , _) <-
        evalRWST (findInvalidCall (glit ^. calls))
             initEnv initialLMState
    case mbInvalidCall of
       Nothing -> putStrLn "all calls validated sucesfully"
       Just x -> (putStrLn $ "invalidCall : " ++ show x)

  where
    findInvalidCall :: [Call] -> LMS (Maybe Call)
    findInvalidCall = \case
      [] -> pure Nothing
      (x : xs) ->
         do validQ <- verifyCall x
            if validQ
            then executeCall x >> findInvalidCall xs
            else pure $ Just x

helpContent :: String
helpContent = [r|
wrong aguments!

- to run interactive mode :
    pass name of the labelled GLContainer value in Agda file
- to run interaction test :
    add the -t parameter and then
    pass name of the labelled GLInteractionTest value in Agda file
    
            |]
            
runLM :: IO ()
runLM = getArgs >>=
  \case
    [fName] -> 
        initialization fName >>= runLMInteractive
    ["-t" , fName] ->
        pickTestCaseFromAgdaFile fName >>= runLMTest
    _ -> putStrLn $ helpContent
            
