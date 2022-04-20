{-# LANGUAGE LambdaCase #-}

module Glow.Translate.FunctionLift where

import Control.Monad (return, mapM)
import Control.Monad.Extra (concatMapM)
import Control.Monad.State (State)
import Data.ByteString (ByteString)
import Data.List (intersect, union, (\\))
import qualified Data.Map.Strict as Map
import Glow.Ast.Common
import Glow.Ast.LiftedFunctions
import Glow.Gerbil.Fresh
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
functionLift s = Module <$> liftTopStmts Nothing [] s

liftTopStmts ::  Maybe Id -> [ByteString] -> [GGT.AnfStatement] -> State UnusedTable [TopStmt]
liftTopStmts mpart locals = concatMapM (liftTopStmt mpart locals)

liftTopStmt :: Maybe Id -> [ByteString] -> GGT.AnfStatement -> State UnusedTable [TopStmt]
-- when mpart is (Just p), must produce TsBodyStmt BsPartStmt (Just p)
liftTopStmt mpart locals = \case
  -- cases for TopStmt
  GGT.AtParticipant (GGT.Variable p) s ->
    (case mpart of
      Nothing -> liftTopStmt (Just (Id p)) locals s
      Just _ -> error ("@ at-participant not allowed, already within a participant"))
  GGT.DefineInteraction i (GGT.AnfInteractionDef ps as xs bs) ->
    (case mpart of
      Nothing -> do
       (ts, bs2) <- liftBodyStmts mpart (locals `union` ps `union` as `union` xs) bs
       return (ts <> [TsDefInteraction (Id i) (InteractionDef (Id <$> ps) (Id <$> as) (Id <$> xs) bs2)])
      Just _ -> error ("interaction definition not allowed within a participant: " <> show i))
  GGT.DefineFunction f xs bs -> do
    (ts, bs2) <- liftBodyStmts mpart (locals `union` [f] `union` xs) bs
    (case intersect locals (usedVars bs \\ xs) of
      [] -> return (ts <> [TsDefLambda mpart (Id f) (Lambda [] (Id <$> xs) bs2)])
      cs -> do
        f2 <- fresh f
        return (ts
                <> [TsDefLambda mpart (Id f2) (Lambda (Id <$> cs) (Id <$> xs) bs2),
                    TsBodyStmt (BsPartStmt mpart (PsDef (Id f) (ExCapture (AEVar (Id f2)) (AEVar . Id <$> cs))))]))
  GGT.DefineType f xs b -> return [TsDefType (Id f) (Id <$> xs) b]
  GGT.DefineDatatype f xs vs -> return [TsDefData (Id f) (Id <$> xs) vs]
  -- cases for BodyStmt
  GGT.Publish (GGT.Variable p) xs -> return [TsBodyStmt (BsPublish (Id p) (Id x)) | GGT.Variable x <- xs]
  GGT.Deposit (GGT.Variable p) am -> return [TsBodyStmt (BsDeposit (Id p) (translateAssetMap am))]
  GGT.Withdraw (GGT.Variable p) am -> return [TsBodyStmt (BsWithdraw (Id p) (translateAssetMap am))]
  GGT.Switch a cs -> 
    (case mpart of
      -- case for BsSwitch
      Nothing -> do
        let a2 = translateArgExpr a
        (ts, cs2) <- liftSwitchCases mpart locals liftBodyStmts cs
        return (ts <> [TsBodyStmt (BsSwitch (Switch a2 cs2))])
      -- case for PsSwitch
      Just _ -> do
        let a2 = translateArgExpr a
        (ts, cs2) <- liftSwitchCases mpart locals liftPartStmts cs
        return (ts <> [TsBodyStmt (BsPartStmt mpart (PsSwitch (Switch a2 cs2)))]))
  -- cases for PartStmt
  GGT.Label bs -> return [TsBodyStmt (BsPartStmt mpart (PsLabel (Id bs)))]
  GGT.DebugLabel _ -> return []
  GGT.SetParticipant _ -> return []
  GGT.Define x e -> return [TsBodyStmt (BsPartStmt mpart (PsDef (Id x) (translateExpr e)))]
  GGT.Ignore e -> return [TsBodyStmt (BsPartStmt mpart (PsIgnore (translateExpr e)))]
  GGT.Require a -> return [TsBodyStmt (BsPartStmt mpart (PsRequire (translateArgExpr a)))]
  GGT.Return e -> return [TsBodyStmt (BsPartStmt mpart (PsReturn (translateExpr e)))]
  s -> error ("Glow.Translate.FunctionLift.liftTopStmt: unexpected statement" <> show s)

liftBodyStmts :: Maybe Id -> [ByteString] -> [GGT.AnfStatement] -> LiftState [BodyStmt]
liftBodyStmts mpart locals stmts = do
  stmts2 <- liftTopStmts mpart (locals `union` localDefs stmts) stmts
  return (splitBody stmts2)

splitBody :: [TopStmt] -> ([TopStmt], [BodyStmt])
splitBody stmts =
  let rs = splitBody1 <$> stmts
   in (concatMap fst rs, concatMap snd rs)

splitBody1 :: TopStmt -> ([TopStmt], [BodyStmt])
-- leave TsBodyStmt, lift everything else
splitBody1 = \case
  TsBodyStmt b -> ([], [b])
  t -> ([t], [])

liftPartStmts :: Maybe Id -> [ByteString] -> [GGT.AnfStatement] -> LiftState [PartStmt]
liftPartStmts mpart locals stmts = do
  (ts, bs) <- liftBodyStmts mpart locals stmts
  return (ts, [ps | BsPartStmt _ ps <- bs])

liftSwitchCases :: Maybe Id -> [ByteString] -> (Maybe Id -> [ByteString] -> [GGT.AnfStatement] -> LiftState [stmt]) -> [(GGT.Pattern, [GGT.AnfStatement])] -> LiftState [(Pat, [stmt])]
liftSwitchCases mpart locals liftStmts cs = do
  tcs2 <- mapM (liftSwitchCase mpart locals liftStmts) cs
  return (concatMap fst tcs2, map snd tcs2)

liftSwitchCase :: Maybe Id -> [ByteString] -> (Maybe Id -> [ByteString] -> [GGT.AnfStatement] -> LiftState [stmt]) -> (GGT.Pattern, [GGT.AnfStatement]) -> LiftState (Pat, [stmt])
liftSwitchCase mpart locals liftStmts (pat, stmts) = do
  (ts, bs) <- liftStmts mpart (locals `union` patVars pat) stmts
  return (ts, ((translatePat pat), bs))

----------

translateExpr :: GGT.Expression -> Expr
translateExpr = \case
  GGT.Digest as -> ExDigest (translateArgExpr <$> as)
  GGT.Sign a -> ExSign (translateArgExpr a)
  GGT.Input t a -> ExInput t (translateArgExpr a)
  GGT.EqlExpr a b -> ExEq (translateArgExpr a) (translateArgExpr b)
  GGT.AppExpr f as -> ExApp (translateArgExpr f) (translateArgExpr <$> as)
  GGT.TrvExpr a -> ExArg (translateArgExpr a)
  e@(GGT.ExpectPublished _) ->
    error ("Glow.Translate.FunctionLift.translateExpr: bad ExpectPublished expression in ANF input: " <> show e <> "\n"
           <> "  expected Publish statements instead")

translateArgExpr :: GGT.GlowValueRef -> ArgExpr
translateArgExpr = \case
  GGT.Explicit GGT.Unit -> AEEmptyTuple
  GGT.Explicit v -> AEConst (translateConst v)
  GGT.Variable x -> AEVar (Id x)

translateConst :: GGT.GlowValue -> Constant
translateConst = \case
  GGT.Integer i -> CInt (intType i) i
  GGT.Boolean b -> CBool b
  GGT.ByteString s -> CByteString s
  v -> error ("TODO: resolve mismatch between GGT.GlowValue and Constant,\n"
              <> "  by restricting GGT.GlowValue and/or expanding Constant,\n"
              <> "  or merging them into a single common type.\n"
              <> "  at: " <> show v)

translateAssetMap :: GGT.AssetMap -> (Record ArgExpr)
translateAssetMap am = Record (Map.mapKeys Id (Map.map translateArgExpr am))

translatePat :: GGT.Pattern -> Pat
-- TODO: expand GGT.Pattern to express more patterns that Pat can express,
--       or merge them into a single common type
translatePat = \case
  GGT.VarPat x -> PVar (Id x)
  GGT.ValPat v -> PConst (translateConst v)

----------

localDefs :: [GGT.AnfStatement] -> [ByteString]
localDefs = unionMap localDefs1

localDefs1 :: GGT.AnfStatement -> [ByteString]
-- include function definitions, but not interaction definitions
-- include localDefs in switch bodies, but not pattern variables
localDefs1 = \case
  GGT.Define x _ -> [x]
  GGT.DefineFunction f _ _ -> [f]
  GGT.AtParticipant _ s -> localDefs1 s
  GGT.Switch _ cs -> unionMap (localDefs . snd) cs
  _ -> []

usedVars :: [GGT.AnfStatement] -> [ByteString]
usedVars = unionMap usedVars1

usedVars1 :: GGT.AnfStatement -> [ByteString]
usedVars1 = \case
  GGT.DefineInteraction _ (GGT.AnfInteractionDef _ _ _ bs) -> usedVars bs
  GGT.Define _ e -> usedVarsExpr e
  GGT.DefineFunction _ _ bs -> usedVars bs
  GGT.AtParticipant p s -> usedVarsGVR p `union` usedVars1 s
  GGT.SetParticipant p -> usedVarsGVR p
  GGT.Publish p xs -> usedVarsGVR p `union` unionMap usedVarsGVR xs
  GGT.Deposit p am -> usedVarsGVR p `union` usedVarsAM am
  GGT.Withdraw p am -> usedVarsGVR p `union` usedVarsAM am
  GGT.Ignore e -> usedVarsExpr e
  GGT.Require a -> usedVarsGVR a
  GGT.Return e -> usedVarsExpr e
  GGT.Switch a cs -> usedVarsGVR a `union` unionMap (usedVars . snd) cs
  _ -> []

usedVarsExpr :: GGT.Expression -> [ByteString]
usedVarsExpr = \case
  GGT.ExpectPublished x -> [x]
  GGT.Digest as -> unionMap usedVarsGVR as
  GGT.Sign a -> usedVarsGVR a
  GGT.Input _ a -> usedVarsGVR a
  GGT.EqlExpr a b -> usedVarsGVR a `union` usedVarsGVR b
  GGT.AppExpr f as -> usedVarsGVR f `union` unionMap usedVarsGVR as
  GGT.TrvExpr a -> usedVarsGVR a

usedVarsGVR :: GGT.GlowValueRef -> [ByteString]
usedVarsGVR = \case
  GGT.Variable x -> [x]
  GGT.Explicit _ -> []

usedVarsAM :: GGT.AssetMap -> [ByteString]
usedVarsAM = unionMap usedVarsGVR . Map.elems

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f as = foldr union [] (map f as)

patVars :: GGT.Pattern -> [ByteString]
patVars = \case
  GGT.VarPat x -> [x]
  GGT.ValPat _ -> []