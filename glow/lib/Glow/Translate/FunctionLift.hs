{-# LANGUAGE LambdaCase #-}

module Glow.Translate.FunctionLift where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State)
import Data.Set (Set, union, unions, intersection, (\\))
import qualified Data.Set as Set (fromList, toAscList, empty, singleton)
import qualified Data.Map.Strict as Map
import Glow.Ast.Common
import Glow.Ast.LiftedFunctions
import Glow.Gerbil.Fresh
import Glow.Gerbil.Types (Pat(..))
import qualified Glow.Gerbil.Types as GGT
import Glow.Prelude

-- While traversing, accumulate local variables.
-- On GGT.DefineFunction, get free variables,
-- captures are free local variables.
-- Lift top def with capture parameters,
-- supply capture arguments for local def.
-- When one closure references another closure,
-- that other closure must be included in the captures,
-- so consider function definitions as local defs,
-- even if those functions can be lifted later without
-- captures.

-- TODO: would a combined state monad for accumulating both UnusedTable inforamtion
--       and TopStmts be better?
type LiftState a = State UnusedTable ([TopStmt], a)

functionLift :: [GGT.AnfStatement] -> State UnusedTable Module
-- TODO: if the Module type changes to use a Map of top-level functions,
--       the functions should be filtered and collected into the Map here
functionLift s = Module <$> liftTopStmts Nothing Set.empty s

liftTopStmts ::  Maybe Id -> Set Id -> [GGT.AnfStatement] -> State UnusedTable [TopStmt]
liftTopStmts mpart locals = concatMapM (liftTopStmt mpart locals)

liftTopStmt :: Maybe Id -> Set Id -> GGT.AnfStatement -> State UnusedTable [TopStmt]
-- when mpart is (Just p), must produce TsBodyStmt BsPartStmt (Just p)
liftTopStmt mpart locals = \case
  -- cases for TopStmt
  GGT.AtParticipant p s ->
    (case mpart of
      Nothing -> liftTopStmt (Just p) locals s
      Just _ -> error ("@ at-participant not allowed, already within a participant"))
  GGT.DefineInteraction i (GGT.AnfInteractionDef ps as xs bs) ->
    (case mpart of
      Nothing -> do
       (ts, bs2) <- liftBodyStmts mpart (locals `union` Set.fromList ps `union` Set.fromList as `union` Set.fromList xs) bs
       pure (ts <> [TsDefInteraction i (InteractionDef ps as xs bs2)])
      Just _ -> error ("interaction definition not allowed within a participant: " <> show i))
  GGT.DefineFunction f xs bs -> do
    (ts, bs2) <- liftBodyStmts mpart (locals `union` Set.singleton f `union` Set.fromList xs) bs
    (case Set.toAscList (intersection locals (usedVars bs \\ Set.fromList xs)) of
      [] -> pure (ts <> [TsDefLambda mpart f (Lambda [] xs bs2)])
      cs -> do
        f2 <- freshId f
        pure (ts
                <> [TsDefLambda mpart f2 (Lambda cs xs bs2),
                    TsBodyStmt (BsPartStmt mpart (PsDef f (ExCapture (TrexVar f2) (TrexVar <$> cs))))]))
  GGT.DefineType f xs b -> pure [TsDefType f xs b]
  GGT.DefineDatatype f xs vs -> pure [TsDefData f xs vs]
  -- cases for BodyStmt
  GGT.Publish p xs -> pure [TsBodyStmt (BsPublish p x) | x <- xs]
  GGT.Deposit p am -> pure [TsBodyStmt (BsDeposit p am)]
  GGT.Withdraw p am -> pure [TsBodyStmt (BsWithdraw p am)]
  GGT.Switch a cs -> 
    (case mpart of
      -- case for BsSwitch
      Nothing -> do
        (ts, cs2) <- liftSwitchCases mpart locals liftBodyStmts cs
        pure (ts <> [TsBodyStmt (BsSwitch (Switch a cs2))])
      -- case for PsSwitch
      Just _ -> do
        (ts, cs2) <- liftSwitchCases mpart locals liftPartStmts cs
        pure (ts <> [TsBodyStmt (BsPartStmt mpart (PsSwitch (Switch a cs2)))]))
  -- cases for PartStmt
  GGT.Label bs -> pure [TsBodyStmt (BsPartStmt mpart (PsLabel bs))]
  GGT.DebugLabel _ -> pure []
  GGT.SetParticipant _ -> pure []
  GGT.Define x e -> pure [TsBodyStmt (BsPartStmt mpart (PsDef x (translateExpr e)))]
  GGT.Ignore e -> pure [TsBodyStmt (BsPartStmt mpart (PsIgnore (translateExpr e)))]
  GGT.Require a -> pure [TsBodyStmt (BsPartStmt mpart (PsRequire a))]
  GGT.Return e -> pure [TsBodyStmt (BsPartStmt mpart (PsReturn (translateExpr e)))]

liftBodyStmts :: Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [BodyStmt]
liftBodyStmts mpart locals stmts = do
  stmts2 <- liftTopStmts mpart (locals `union` localDefs stmts) stmts
  pure (splitBody stmts2)

splitBody :: [TopStmt] -> ([TopStmt], [BodyStmt])
splitBody stmts =
  let rs = splitBody1 <$> stmts
   in (concatMap fst rs, concatMap snd rs)

splitBody1 :: TopStmt -> ([TopStmt], [BodyStmt])
-- leave TsBodyStmt, lift everything else
splitBody1 = \case
  TsBodyStmt b -> ([], [b])
  t -> ([t], [])

liftPartStmts :: Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [PartStmt]
liftPartStmts mpart locals stmts = do
  (ts, bs) <- liftBodyStmts mpart locals stmts
  pure (ts, [ps | BsPartStmt _ ps <- bs])

liftSwitchCases :: Maybe Id -> Set Id -> (Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [stmt]) -> [(Pat, [GGT.AnfStatement])] -> LiftState [(Pat, [stmt])]
liftSwitchCases mpart locals liftStmts cs = do
  tcs2 <- traverse (liftSwitchCase mpart locals liftStmts) cs
  pure (concatMap fst tcs2, map snd tcs2)

liftSwitchCase :: Maybe Id -> Set Id -> (Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [stmt]) -> (Pat, [GGT.AnfStatement]) -> LiftState (Pat, [stmt])
liftSwitchCase mpart locals liftStmts (pat, stmts) = do
  (ts, bs) <- liftStmts mpart (locals `union` patVars pat) stmts
  pure (ts, (pat, bs))

----------

translateExpr :: GGT.Expression -> Expr
translateExpr = \case
  GGT.Digest as -> ExDigest as
  GGT.Sign a -> ExSign a
  GGT.Input t a -> ExInput t a
  GGT.EqlExpr a b -> ExEq a b
  GGT.AppExpr f as -> ExApp f as
  GGT.TrvExpr a -> ExTriv a
  e@(GGT.ExpectPublished _) ->
    error ("Glow.Translate.FunctionLift.translateExpr: bad ExpectPublished expression in ANF input: " <> show e <> "\n"
           <> "  expected Publish statements instead")

----------

localDefs :: [GGT.AnfStatement] -> Set Id
localDefs = unionMap localDefs1

localDefs1 :: GGT.AnfStatement -> Set Id
-- include function definitions, but not interaction definitions
-- include localDefs in switch bodies, but not pattern variables
localDefs1 = \case
  GGT.Define x _ -> Set.singleton x
  GGT.DefineFunction f _ _ -> Set.singleton f
  GGT.AtParticipant _ s -> localDefs1 s
  GGT.Switch _ cs -> unionMap (localDefs . snd) cs
  _ -> Set.empty

usedVars :: [GGT.AnfStatement] -> Set Id
usedVars = unionMap usedVars1

usedVars1 :: GGT.AnfStatement -> Set Id
usedVars1 = \case
  GGT.DefineInteraction _ (GGT.AnfInteractionDef _ _ _ bs) -> usedVars bs
  GGT.Define _ e -> usedVarsExpr e
  GGT.DefineFunction _ _ bs -> usedVars bs
  GGT.AtParticipant p s -> Set.singleton p `union` usedVars1 s
  GGT.SetParticipant p -> Set.singleton p
  GGT.Publish p xs -> Set.singleton p `union` Set.fromList xs
  GGT.Deposit p am -> Set.singleton p `union` usedVarsAM am
  GGT.Withdraw p am -> Set.singleton p `union` usedVarsAM am
  GGT.Ignore e -> usedVarsExpr e
  GGT.Require a -> usedVarsGVR a
  GGT.Return e -> usedVarsExpr e
  GGT.Switch a cs -> usedVarsGVR a `union` unionMap (usedVars . snd) cs
  _ -> Set.empty

usedVarsExpr :: GGT.Expression -> Set Id
usedVarsExpr = \case
  GGT.ExpectPublished x -> Set.singleton x
  GGT.Digest as -> unionMap usedVarsGVR as
  GGT.Sign a -> usedVarsGVR a
  GGT.Input _ a -> usedVarsGVR a
  GGT.EqlExpr a b -> usedVarsGVR a `union` usedVarsGVR b
  GGT.AppExpr f as -> usedVarsGVR f `union` unionMap usedVarsGVR as
  GGT.TrvExpr a -> usedVarsGVR a

usedVarsGVR :: TrivExpr -> Set Id
usedVarsGVR = \case
  TrexVar x -> Set.singleton x
  TrexConst _ -> Set.empty

usedVarsAM :: GGT.AssetMap -> Set Id
usedVarsAM = unionMap usedVarsGVR . Map.elems

unionMap :: Ord b => (a -> Set b) -> [a] -> Set b
unionMap f as = unions (map f as)

patVars :: Pat -> Set Id
patVars = \case
  PVar x -> Set.singleton x
  PTypeAnno p _ -> patVars p
  PAppCtor _ ps -> unionMap patVars ps
  PList ps -> unionMap patVars ps
  PTuple ps -> unionMap patVars ps
  POr ps -> unionMap patVars ps
  PRecord m -> unionMap patVars (Map.elems m)
  PWild -> Set.empty
  PConst _ -> Set.empty
