{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Glow.Runtime.Interaction.InterpretAnf where


import Control.Monad.RWS

import Control.Lens (makeLenses,Getter,to,(^.),(%=),use,(.~),(%~),Lens')

import Glow.Ast.Common
import Glow.Gerbil.Types

import Prelude

import Data.Map as M
import qualified Data.Map.Strict as MS


import Glow.Runtime.Interaction.BuiltIns
import qualified Glow.Consensus.Lurk as GCL
import qualified GHC.Generics as G

import Data.Text.Lazy (unpack)
import qualified Data.Text.Lazy as DTL
import Glow.Precompiled.LMonad (headAsset)
import Glow.Precompiled.LMonadToLurk (constantToGLValue)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as DBS8

import qualified Data.Char as DC

import qualified Data.List as L

data ExecFlag =
     SendCall GCL.Action Int
   | PromptInput String GCL.GLType
   | Wait
  deriving (Eq , Ord , Show)


data LocalState = LocalState
     { _lsInputs :: [GCL.GLValue]
     }
  deriving (Show)
makeLenses ''LocalState

initialLocalState = LocalState []

foldUntil :: (b -> a -> Either b c) -> b -> [ a ] -> Maybe c
foldUntil _ _ [] = Nothing 
foldUntil f b (x : xs) =
  case f b x of
    Left b' -> foldUntil f b' xs
    Right y -> Just y

foldUntilM :: Monad m => (b -> a -> m (Either b c)) -> b -> [ a ] -> m (Maybe c)
foldUntilM _ _ [] = return Nothing 
foldUntilM f b (x : xs) =
  f b x >>= 
  \case 
    Left b' -> foldUntilM f b' xs
    Right y -> return (Just y)

foldUntilM' :: Monad m => (b -> a -> m (Either b (Either c [a]))) -> b -> [ a ] -> m (Maybe c)
foldUntilM' _ _ [] = return Nothing 
foldUntilM' f b (x : xs) =
  f b x >>= 
  \case 
    Left b' -> foldUntilM' f b' xs
    Right (Left y) -> return (Just y)
    Right (Right ys) -> foldUntilM' f b (ys ++ xs)


data InterpretationState =  InterpretationState
  { _isScope :: Map Id GlowValue
  , _isInputs :: [GCL.GLValue]
  , _isPublished :: [GCL.GLValue]
  , _isSeed :: Integer
  }
makeLenses ''InterpretationState

gLValueToValue :: GCL.GLValue -> GlowValue 
gLValueToValue = \case
  GCL.GLNat k -> Integer (fromIntegral k)
  GCL.GLBool k -> Boolean k
  GCL.GLString s -> ByteString $ pack (unpack s)
  GCL.GLPF k -> error "not implemented"
  GCL.DigestOf x -> ByteString $ pack (show x) 
  GCL.GLUnit -> Unit


valueToGLValue :: Type -> GlowValue -> GCL.GLValue
valueToGLValue ty = \case
  Constructor _ _ _ -> error "not implemented"
  PubKey _ -> error "not implemented"
  Signature _  -> error "not implemented"
  ByteString x ->
    case gLTypeToType ty of
       GCL.DigestT -> GCL.DigestOf (read (DBS8.unpack x))
       GCL.GLStringT -> GCL.GLString (DTL.pack (DBS8.unpack x))
       _ -> error ("not implemented: " ++ show ty)
  Integer i -> GCL.GLNat $ fromIntegral i
  Boolean b -> GCL.GLBool b
  Unit -> GCL.GLUnit

valueToMaybeConstant :: GlowValue -> Maybe Constant
valueToMaybeConstant (Integer x) = Just (CInt IntType { itSigned = True , itNumBits = 8 } x)
valueToMaybeConstant _ = Nothing
  
gLTypeToType :: Type -> GCL.GLType  
gLTypeToType = \case
   TyName (Id x) ->
     case x of
       "Nat" -> GCL.GLNatT
       "Int" -> GCL.GLNatT
       "Digest" -> GCL.DigestT
       "Bool" -> GCL.GLBoolT
       "String" -> GCL.GLStringT
       "Unit" -> GCL.GLUnitT
   TyUnknown ("Atom \"Nat\"") -> GCL.GLNatT
   x -> error ("fatal, not implemented" ++ show x) 
   
computeExecFlag :: MS.Map ByteString Type -> Id -> GCL.LMState -> LocalState -> [(ByteString , GCL.GLValue)]
                    ->  [AnfStatement Int] -> Either String (Maybe ExecFlag) 
computeExecFlag typeTable myId cs ls params =
   foldUntilM' f (InterpretationState
                  (fmap gLValueToValue $ M.mapKeys Id (M.fromList params))
                  (ls ^. lsInputs)
                  (cs ^. GCL.publicValues) 0)

  where
    currentStateId = cs ^. GCL.stateId

    scopeTypeLookup :: Id -> Maybe Type
    scopeTypeLookup i = MS.lookup (idBS i) typeTable 

    evt :: InterpretationState -> TrivExpr -> Either String GlowValue
    evt s  = \case
      TrexVar i ->
         case M.lookup i (s ^. isScope) of
           Nothing -> Left (show i ++ " not in scope")
           Just v -> Right v
      TrexConst c -> return (constantToValue c)

    right' = Right . Left   

    ev :: InterpretationState -> Expression -> Either String (Either (Type , String) GlowValue)
    ev s = \case
      ExpectPublished i -> Left "fatal - not expected" -- exclude with typesafe ANF 
      Digest [x] -> do
        x' <- evt s x
        case valueToMaybeConstant x' of
          Nothing -> Left "not implemented"
          Just x'' -> return $ Right $ gLValueToValue $ (GCL.DigestOf (constantToGLValue x'')) 
      Digest _ -> Left "not implemented - multiple values in digest"
      Sign _ -> Left "not implemented - signature - TODO"
      Input ty msg -> do
        msg' <- evt s msg
        case msg' of
           ByteString msg'' -> return $ Left (ty , DBS8.unpack msg'')
           _ -> Left "fatal - msg not a string"
          -- (x : xs) ->
          --       return $ Right $ gLValueToValue x
      EqlExpr x y -> do
        x' <- evt s x
        y' <- evt s y
        return $ Right $ Boolean (x' == y')
      AppExpr f args -> 
          Right <$> (evalBuiltIn (idBS f) =<< mapM (evt s) args)
        
      TrvExpr x -> Right <$> evt s x
      

    action s j i x =  
        if j < currentStateId + 1
        then return $ Left s
        else return $ right' $ if i == myId
             then SendCall x j
             else Wait

  
    f s = \case
      Define i (AppExpr (Id "randomUInt256") []) -> do
         let v =  fromIntegral $ Prelude.foldr (+) 0  $ Prelude.map DC.ord (show (s ^. isSeed) ++ show myId)
             
         return (Left ( (isSeed .~ v + 1) $ (isScope %~ M.insert i (Integer v)) s))
      
      Define i e -> do
         e' <- ev s e
         case e' of
           Left (ty , msg) ->
             case (s ^. isInputs) of
               [] -> return (right' (PromptInput msg (gLTypeToType ty)))
               (pi : pis) -> return (Left ( (isInputs .~ pis) ((isScope %~ M.insert i (gLValueToValue pi)) s)))
                   
           Right v -> return (Left ((isScope %~ M.insert i v) s))
           

      Publish j i [k] ->
        if j < currentStateId + 1
        then case (s ^. isPublished) of
               [] -> Left "bad consensus state"
               (v : vs) -> return $ Left $
                              ((( isPublished .~ vs) . (isScope %~ M.insert k (gLValueToValue v))) s)
        else if i == myId
             then case (M.lookup k (s ^. isScope) , scopeTypeLookup k) of
                     (Just v , Just ty) -> return $ right' (SendCall (GCL.Publish (valueToGLValue ty v)) j) 
                     _ -> Left ("fatal error, variable to be published not found in scope : "
                            ++ show k ++ "\n"
                            ++ show (s ^. isScope))
             else return $ right' Wait
      Publish _ _ _ -> Left "todo: mulitple publish"
    

      Deposit j i am -> do
        z <- (fromPure <$> headAsset am) >>= ev s
        case z of
          Right (Integer z') -> action s j i (GCL.Deposit (fromIntegral z'))
          _ -> Left $ "fatal - amount of founds not a number : " ++ show z
        

      Withdraw j i am -> do
        z <- (fromPure <$> headAsset am) >>= ev s
        case z of
          Right (Integer z') -> action s j i (GCL.Withdraw (fromIntegral z'))
          _ -> Left $ "fatal - amount of founds not a number : " ++ show z


      -- TODO handle it prperly , by additional ExecFlag maybe?
      Require te -> return $ Left s

      -- should be ok for now
      Return e -> return $ Left s

      Switch te [(PConst (CBool True) , tb),(PConst (CBool False) , tf)] -> do
        te' <- evt s te
        case te' of
          Boolean b -> return $ Right $ Right $ if b then tb else tf
          _ -> error $ "fatail - condition did not evaluated to Bool! : " ++ show te'  

      Switch _ _ -> Left ("todo: shiwthfc other than if")

      AtParticipant pId y ->
        if pId == myId
        then f s y 
        else return (Left s)
             
      _ -> return $ Left s






      
  

--   where
--     h :: 

-- execFlag :: Scope -> [AnfStatement Int] -> ExecFlag
-- execFlag s [] = End
-- execFlag s (x : xs) =
--   case h s x of
--     Left ef -> ef
--     Right s' -> execFlag s' xs
--   where
     
    
--     h :: Scope -> AnfStatement Int -> Either ExecFlag Scope
--     h s = \case
--       Define i e -> undefined
--          -- Right ((defs %~ Just pId ) s , [y])
--   --     Publish j i [k] -> 
--   --       Right $ Just (Just k , (ExpectPub j i))
--   --     Publish _ _ _ -> Left "todo: mulitple publish"
    

--   --     Deposit j i am -> do
--   --       a <- headAsset am
--   --       nb $ (Action j i (DepositA a)) 
                      
--   --     Withdraw j i am -> do
--   --       a <- headAsset am
--   --       nb $ (Action j i (WithdrawA a))

--   --     Require te ->
--   --       nb $ (RequireLM (PTrvExpr te))


--   --     Return e -> do
--   --        e' <- toPure e
--   --        nb (Pure e')

--   --     Switch te [(PConst (CBool True) , tb),(PConst (CBool False) , tf)] -> do
--   --       btLM <- toLMonad tb
--   --       bfLM <- toLMonad tf
--   --       nb $ Branch (PTrvExpr te) btLM bfLM

--   --     Switch _ _ -> Left ("todo: shiwthfc other than if")

--       AtParticipant pId y -> h ((currentParticipant .~ Just pId ) s) y
      
--       _ -> Right s

--   --     -- AtParticipant pId s -> fmap (AtParticipant pId) <$> mbMarkANF k s
--   --     -- Label i -> Right (k , Label i)
--   --     -- DebugLabel i -> Right (k , DebugLabel i)
--   --     -- DefineInteraction _ _ -> Left "not implemented: DefineInteraction"
--   --     -- DefineFunction _ _ _ -> Left "not implemented: DefineFunction"
--   --     -- DefineType _ _ _ -> Left "not implemented: DefineType"
--   --     -- DefineDatatype _ _ _ -> Left "not implemented: DefineDatatype"
--   --     -- SetParticipant _ -> Left "imposible: SetParticipant"
--   --         -- Ignore _ -> Left "not implemented: Ignore"


--   --   -- f :: Scope -> [AnfStatement Int] -> Either ExecFlag Scope
--   --   -- f s [] = End


--   -- -- \case
--   -- -- [] -> return End
--   -- -- (x : xs) -> End
      

  
  


