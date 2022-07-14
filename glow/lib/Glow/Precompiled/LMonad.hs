{-# LANGUAGE LambdaCase #-}
module Glow.Precompiled.LMonad where

import Prelude

import Glow.Ast.Common
-- import qualified Data.ByteString as BS

import Glow.Gerbil.Types
import Data.Map.Strict (toList)
import Data.Maybe (catMaybes)
-- import Data.Foldable (foldlM)



data Action = WithdrawA Expr | DepositA Expr
     deriving (Show)


type PID = Id
type EID = Int

type Expr = PureExpression



data LMonad =
    Action EID PID Action
  | RequireLM Expr
  | ExpectPub EID PID
  | Bind Id LMonad LMonad
  | Next LMonad LMonad
  | Pure Expr
  | Branch Expr LMonad LMonad
  deriving (Show)

headAsset :: AssetMap -> Either String Expr
headAsset x = 
  case (toList x) of
    [( Id { idBS = "DefaultToken" } , e)] -> toPure (TrvExpr e)
    _ -> Left "multiple tokens not implemented"



toLMonad :: [AnfStatement Int] -> Either String (LMonad)
toLMonad ss' = do
  lms <- catMaybes <$> traverse h ss'
  case reverse lms of
    [] -> Left "empty interaction"
    (s : ss) -> return $ snd $ foldr g s (reverse ss)


  where

    nb x = Right $ Just (Nothing , x)

    g :: ((Maybe Id) , LMonad) -> ((Maybe Id) , LMonad) -> ((Maybe Id) , LMonad)
    g (mbXId , xVal) =
      fmap (case mbXId of
                 Nothing -> Next xVal
                 Just xId -> Bind xId xVal)
      
    h :: AnfStatement Int -> Either String (Maybe ((Maybe Id) , LMonad))
    h = \case
      Define i e -> do 
        e' <- toPure e
        Right $ Just (Just i , (Pure e'))

      Publish j i [k] -> 
        Right $ Just (Just k , (ExpectPub j i))
      Publish _ _ _ -> Left "todo: mulitple publish"
    

      Deposit j i am -> do
        a <- headAsset am
        nb $ (Action j i (DepositA a)) 
                      
      Withdraw j i am -> do
        a <- headAsset am
        nb $ (Action j i (WithdrawA a))

      -- Require te ->
      --   nb $ (RequireLM (PTrvExpr te))


      Return e -> do
         e' <- toPure e
         nb (Pure e')

      Switch te [(PConst (CBool True) , tb),(PConst (CBool False) , tf)] -> do
        btLM <- toLMonad tb
        bfLM <- toLMonad tf
        nb $ Branch (PTrvExpr te) btLM bfLM

      Switch _ _ -> Left ("todo: shiwthfc other than if")

      _ -> Right Nothing

      -- AtParticipant pId s -> fmap (AtParticipant pId) <$> mbMarkANF k s
      -- Label i -> Right (k , Label i)
      -- DebugLabel i -> Right (k , DebugLabel i)
      -- DefineInteraction _ _ -> Left "not implemented: DefineInteraction"
      -- DefineFunction _ _ _ -> Left "not implemented: DefineFunction"
      -- DefineType _ _ _ -> Left "not implemented: DefineType"
      -- DefineDatatype _ _ _ -> Left "not implemented: DefineDatatype"
      -- SetParticipant _ -> Left "imposible: SetParticipant"
          -- Ignore _ -> Left "not implemented: Ignore"
