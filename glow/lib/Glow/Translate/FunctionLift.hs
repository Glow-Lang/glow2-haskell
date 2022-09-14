{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Glow.Translate.FunctionLift where

import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State (get, put)
import Control.Monad.Trans.RWS.CPS (RWS, censor, execRWS, listen, tell)
import qualified Control.Monad.Trans.RWS.CPS as RWS (state)
import Data.DList (DList)
import qualified Data.DList as DList (fromList, toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set, intersection, union, unions, (\\))
import qualified Data.Set as Set (empty, fromList, singleton, toAscList)
import Glow.Ast.Common
import Glow.Ast.LiftedFunctions
import Glow.Gerbil.Fresh
import Glow.Gerbil.Types (Pat (..))
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

type LiftState a = RWS () (DList TopStmt) UnusedTable a

rwSA :: State s a -> RWS r w s a
-- RWS runState
rwSA = RWS.state . runState

srWS :: Monoid w => RWS () w s () -> State s w
-- State execRWS, output W not A
srWS m = do
  s1 <- State.get
  let (s2, w) = execRWS m () s1
  State.put s2
  pure w

interceptAW :: (Monoid w) => RWS r w s a -> RWS r w s (a, w)
interceptAW = censor (const mempty) . listen

----------

functionLift :: [GGT.AnfStatement] -> State UnusedTable Module
-- TODO: if the Module type changes to use a Map of top-level functions,
--       the functions should be filtered and collected into the Map here
functionLift s =
  Module . DList.toList <$> srWS (liftTopStmts Nothing Set.empty s)

liftTopStmts :: Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState ()
liftTopStmts mpart locals = traverse_ (liftTopStmt mpart locals)

liftTopStmt :: Maybe Id -> Set Id -> GGT.AnfStatement -> LiftState ()
-- when mpart is (Just p), must produce TsBodyStmt BsPartStmt (Just p)
liftTopStmt mpart locals = \case
  -- cases for TopStmt
  GGT.AtParticipant p s ->
    ( case mpart of
        Nothing -> liftTopStmt (Just p) locals s
        Just _ -> error ("@ at-participant not allowed, already within a participant")
    )
  GGT.DefineInteraction i (GGT.AnfInteractionDef ps as xs bs) ->
    ( case mpart of
        Nothing -> do
          bs2 <- liftBodyStmts mpart (locals `union` Set.fromList ps `union` Set.fromList as `union` Set.fromList xs) bs
          tell [TsDefInteraction i (InteractionDef ps as xs bs2)]
        Just _ -> error ("interaction definition not allowed within a participant: " <> show i)
    )
  GGT.DefineFunction f xs bs -> do
    bs2 <- liftBodyStmts mpart (locals `union` Set.singleton f `union` Set.fromList xs) bs
    ( case Set.toAscList (intersection locals (usedVars bs \\ Set.fromList xs)) of
        [] -> tell [TsDefLambda mpart f (Lambda [] xs bs2)]
        cs -> do
          f2 <- rwSA (freshId f)
          tell
            [ TsDefLambda mpart f2 (Lambda cs xs bs2),
              TsBodyStmt (BsPartStmt mpart (PsDef f (ExCapture (TrexVar f2) (TrexVar <$> cs))))
            ]
      )
  GGT.DefineType f xs b -> tell [TsDefType f xs b]
  GGT.DefineDatatype f xs vs -> tell [TsDefData f xs vs]
  -- cases for BodyStmt
  GGT.Publish p xs -> tell (DList.fromList [TsBodyStmt (BsPublish p x) | x <- xs])
  GGT.Deposit p am -> tell [TsBodyStmt (BsDeposit p am)]
  GGT.Withdraw p am -> tell [TsBodyStmt (BsWithdraw p am)]
  GGT.Switch a cs ->
    ( case mpart of
        -- case for BsSwitch
        Nothing -> do
          cs2 <- liftSwitchCases mpart locals liftBodyStmts cs
          tell [TsBodyStmt (BsSwitch (Switch a cs2))]
        -- case for PsSwitch
        Just _ -> do
          cs2 <- liftSwitchCases mpart locals liftPartStmts cs
          tell [TsBodyStmt (BsPartStmt mpart (PsSwitch (Switch a cs2)))]
    )
  -- cases for PartStmt
  GGT.Label bs -> tell [TsBodyStmt (BsPartStmt mpart (PsLabel bs))]
  GGT.DebugLabel _ -> tell []
  GGT.SetParticipant _ -> tell []
  GGT.Define x e -> tell [TsBodyStmt (BsPartStmt mpart (PsDef x (translateExpr e)))]
  GGT.Ignore e -> tell [TsBodyStmt (BsPartStmt mpart (PsIgnore (translateExpr e)))]
  GGT.Require a -> tell [TsBodyStmt (BsPartStmt mpart (PsRequire a))]
  GGT.Return e -> tell [TsBodyStmt (BsPartStmt mpart (PsReturn (translateExpr e)))]

liftBodyStmts :: Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [BodyStmt]
liftBodyStmts mpart locals stmts = do
  ((), stmts2) <- interceptAW (liftTopStmts mpart (locals `union` localDefs stmts) stmts)
  splitBody (DList.toList stmts2)

splitBody :: [TopStmt] -> LiftState [BodyStmt]
splitBody = concatMapM splitBody1

splitBody1 :: TopStmt -> LiftState [BodyStmt]
-- leave TsBodyStmt, lift everything else
splitBody1 = \case
  TsBodyStmt b -> pure [b]
  t -> tell [t] *> pure []

liftPartStmts :: Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [PartStmt]
liftPartStmts mpart locals stmts = do
  bs <- liftBodyStmts mpart locals stmts
  pure [ps | BsPartStmt _ ps <- bs]

liftSwitchCases :: Maybe Id -> Set Id -> (Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [stmt]) -> [(Pat, [GGT.AnfStatement])] -> LiftState [(Pat, [stmt])]
liftSwitchCases mpart locals liftStmts =
  traverse (liftSwitchCase mpart locals liftStmts)

liftSwitchCase :: Maybe Id -> Set Id -> (Maybe Id -> Set Id -> [GGT.AnfStatement] -> LiftState [stmt]) -> (Pat, [GGT.AnfStatement]) -> LiftState (Pat, [stmt])
liftSwitchCase mpart locals liftStmts (pat, stmts) = do
  bs <- liftStmts mpart (locals `union` patVars pat) stmts
  pure (pat, bs)

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
    error
      ( "Glow.Translate.FunctionLift.translateExpr: bad ExpectPublished expression in ANF input: "
          <> show e
          <> "\n"
          <> "  expected Publish statements instead"
      )

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
