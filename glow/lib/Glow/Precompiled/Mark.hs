{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Glow.Precompiled.Mark where


import Glow.Gerbil.Types
import Prelude



mbMarkANF :: Int -> AnfStatement () -> Either String (Int , AnfStatement Int) 
mbMarkANF k = 
  \case
    Label i -> Right (k , Label i)
    DebugLabel i -> Right (k , DebugLabel i)
    DefineInteraction _ _ -> Left "not implemented: DefineInteraction"
    Define i e -> Right (k , Define i e) 
    DefineFunction _ _ _ -> Left "not implemented: DefineFunction"
    DefineType _ _ _ -> Left "not implemented: DefineType"
    DefineDatatype _ _ _ -> Left "not implemented: DefineDatatype"
    AtParticipant pId s -> fmap (AtParticipant pId) <$> mbMarkANF k s
    SetParticipant _ -> Left "imposible: SetParticipant" 
    Publish _ i ids -> Right $ (k + 1 , Publish k i ids)
    Deposit _ i am -> Right $ (k + 1 , Deposit k i am)
    Withdraw _ i am -> Right $ (k + 1 , Withdraw k i am)
    Ignore _ -> Left "not implemented: Ignore"
    Require te -> Right $ (k , Require te)
    Return e ->   Right $ (k , Return e)
    Switch te css -> 
        fmap (fmap (Switch te)) $
           mapAccumLM (\k' (p , sl) -> (fmap (p ,)) <$> (markANF k' sl)) k css 
 

mapAccumLM :: Monad m => (a -> b -> m(a, c)) -> a -> [b] -> m(a, [c])
mapAccumLM _ a [] = return (a, [])
mapAccumLM f a (x:xs) = do
  (a', c) <- f a x
  (a'', cs) <- mapAccumLM f a' xs
  return (a'', c:cs)

markANF :: Int -> [AnfStatement ()] -> Either String (Int , [AnfStatement Int])
markANF k stmnts = mapAccumLM mbMarkANF k stmnts 


