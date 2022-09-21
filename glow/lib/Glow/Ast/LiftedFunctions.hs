{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module: Glow.Ast.LiftedFunctions
--
-- This module defines an Ast which is the output of the FunctionLift pass.
--
-- The Function-Lift pass takes ANF as input and produces Lifted-Functions as
-- output.
--
-- The most notable difference from ANF is that there are multiple tiers of
-- statements:
--
-- * 'TopStmt' for statements directly at the top level of a module, including
--   interaction definitions, function definitions and type definitions.
-- * 'BodyStmt' for statements in the bodies of interactions, functions, and
--   conditionals, including consensus and at-participant statements.
-- * 'PartStmt' for statements inside at-participant statements, and inside
--   conditionals within.
--
-- The 'TopStmt' function definitions ('TsDefLambda') include
-- capture-parameters before the function-parameters, and the 'Expr' includes a
-- form 'ExCapture' to supply capture-arguments to produce a closure.
module Glow.Ast.LiftedFunctions where

import Data.Set (Set)
import qualified Data.Set as Set
import Glow.Ast.Classes
import Glow.Ast.Common
import Glow.Gerbil.Types (Pat, Record, Type, Variant, patVars)
import Glow.Prelude
import Prelude (head)

data Module a = Module [TopStmt a]
  deriving (Show, Read, Eq)

data TopStmt a
  = TsBodyStmt (BodyStmt a)
  | -- Note: in the grammar there are both (deftype id type) and
    -- (deftype (id tyvar ...) type); here we just combine them, where the
    -- first variant has an empty list (likewise for defdata).
    TsDefType a Id [Id] Type
  | TsDefData a Id [Id] [Variant]
  | TsDefInteraction a Id (InteractionDef a)
  | -- | participant id (if any), function id, function def:
    TsDefLambda a (Maybe Id) Id (Lambda (BodyStmt a))
  deriving (Show, Read, Eq)

class HasMeta f => IsStmt f where
  stmtsWithFreeVars :: [f a] -> [f (Set Id)]

instance HasMeta TopStmt where
  getMeta = \case
    TsBodyStmt s -> getMeta s
    TsDefType m _ _ _ -> m
    TsDefData m _ _ _ -> m
    TsDefInteraction m _ _ -> m
    TsDefLambda m _ _ _ -> m
  setMeta m = \case
    TsBodyStmt s -> TsBodyStmt $ setMeta m s
    TsDefType _ name params ty -> TsDefType m name params ty
    TsDefData _ name params variants -> TsDefData m name params variants
    TsDefInteraction _ name idef -> TsDefInteraction m name idef
    TsDefLambda _ maybePart name lam -> TsDefLambda m maybePart name lam

stmtsFreeVars :: HasMeta f => [f (Set Id)] -> Set Id
stmtsFreeVars = \case
  [] -> Set.empty
  (s : _) -> getMeta s

instance IsStmt TopStmt where
  stmtsWithFreeVars = go
    where
      go [] = []
      go (TsDefType _ _ _ _ : _) = error "TODO: TsDefType"
      go (TsDefData _ _ _ _ : _) = error "TODO: TsDefData"
      go (TsDefInteraction _ name idef : ss) =
        let idef' = interactionDefWithFreeVars idef
            defFvs = interactionDefFreeVars idef'
            ss' = stmtsWithFreeVars ss
            fvs = defFvs `Set.union` (Set.delete name (stmtsFreeVars ss'))
         in TsDefInteraction fvs name idef' : ss'
      go (TsDefLambda _ participant fName lam : tss) =
        let body' = stmtsWithFreeVars (lamBody lam)
            tail' = stmtsWithFreeVars tss
            lam' =
              Lambda
                { lamCaptures = lamCaptures lam,
                  lamParams = lamParams lam,
                  lamBody = body'
                }
            defFvs =
              Set.union
                (foldMap Set.singleton participant)
                ( stmtsFreeVars body'
                    `Set.difference` Set.unions
                      [ Set.fromList $ lamParams lam,
                        Set.fromList $ lamCaptures lam,
                        Set.singleton fName
                      ]
                )
            tailFvs =
              Set.delete fName (stmtsFreeVars tail')
            retFvs = Set.union defFvs tailFvs
         in TsDefLambda retFvs participant fName lam' : tail'
      go (TsBodyStmt (BsPartStmt _ maybePart (PsDef _ v e)) : tss) =
        let tss' = go tss
            psFvs = exFreeVars e
            fvs =
              Set.unions
                [ foldMap Set.singleton maybePart,
                  psFvs,
                  Set.delete v (stmtsFreeVars tss')
                ]
         in TsBodyStmt (BsPartStmt fvs maybePart (PsDef psFvs v e)) : tss'
      go (TsBodyStmt b : ss) =
        let ss' = stmtsWithFreeVars ss
            b' = head $ stmtsWithFreeVars [b]
            ssFvs = stmtsFreeVars ss'
            headFvs = getMeta b'
         in TsBodyStmt (setMeta (Set.union ssFvs headFvs) b') : ss'

data InteractionDef a = InteractionDef
  { idParticipants :: [Id],
    idAssets :: [Id],
    idParams :: [Id],
    idBody :: [BodyStmt a]
  }
  deriving (Show, Read, Eq)

interactionDefWithFreeVars :: InteractionDef a -> InteractionDef (Set Id)
interactionDefWithFreeVars InteractionDef {..} =
  InteractionDef {idBody = stmtsWithFreeVars idBody, ..}

interactionDefFreeVars :: InteractionDef (Set Id) -> Set Id
interactionDefFreeVars InteractionDef {..} =
  stmtsFreeVars idBody
    `Set.difference` Set.unions
      [ Set.fromList idParticipants,
        Set.fromList idAssets,
        Set.fromList idParams
      ]

data BodyStmt a
  = BsPartStmt a (Maybe Id) (PartStmt a)
  | BsWithdraw a Id (Record TrivExpr)
  | BsDeposit a Id (Record TrivExpr)
  | BsPublish a Id Id
  | BsSwitch a (Switch a (BodyStmt a))
  deriving (Show, Read, Eq)

instance HasMeta BodyStmt where
  getMeta = \case
    BsPartStmt m _ _ -> m
    BsWithdraw m _ _ -> m
    BsDeposit m _ _ -> m
    BsPublish m _ _ -> m
    BsSwitch m _ -> m
  setMeta m = \case
    BsPartStmt _ p s -> BsPartStmt m p s
    BsWithdraw _ name record -> BsWithdraw m name record
    BsDeposit _ name record -> BsDeposit m name record
    BsPublish _ x y -> BsPublish m x y
    BsSwitch _ sw -> BsSwitch m sw

instance IsStmt BodyStmt where
  stmtsWithFreeVars = go
    where
      go [] = []
      go (BsPartStmt _ maybePart (PsDef _ v e) : ss) =
        let ss' = go ss
            psFvs = exFreeVars e
            fvs =
              Set.unions
                [ foldMap Set.singleton maybePart,
                  psFvs,
                  Set.delete v (stmtsFreeVars ss')
                ]
         in BsPartStmt fvs maybePart (PsDef psFvs v e) : ss'
      go (BsPartStmt _ maybePart p : ss) =
        let p' = head $ stmtsWithFreeVars [p]
         in chainTail
              ss
              (\fvs -> BsPartStmt fvs maybePart p')
              (Set.union (getMeta p') (foldMap Set.singleton maybePart))
      go (BsWithdraw _ v record : ss) =
        chainTail
          ss
          (\fvs -> BsWithdraw fvs v record)
          (Set.insert v (foldMap trexFreeVars record))
      go (BsDeposit _ v record : ss) =
        chainTail
          ss
          (\fvs -> BsDeposit fvs v record)
          (Set.insert v (foldMap trexFreeVars record))
      go (BsPublish _ x y : ss) =
        chainTail
          ss
          (\fvs -> BsPublish fvs x y)
          (Set.fromList [x, y])
      go (BsSwitch _ sw : ss) =
        let sw' = switchWithFreeVars sw
            fvsSw = swMeta sw'
         in chainTail
              ss
              (\fvs -> BsSwitch fvs sw')
              fvsSw
      chainTail :: [BodyStmt a] -> (Set Id -> BodyStmt (Set Id)) -> Set Id -> [BodyStmt (Set Id)]
      chainTail ss ctor headFvs =
        let ss' = go ss
            fvs = Set.union headFvs (stmtsFreeVars ss')
         in ctor fvs : ss'

data PartStmt a
  = PsLabel a Id
  | PsDebugLabel a Id
  | PsDef a Id Expr
  | PsIgnore a Expr
  | PsReturn a Expr
  | PsRequire a TrivExpr
  | PsAssert a TrivExpr
  | PsSwitch a (Switch a (PartStmt a))
  deriving (Show, Read, Eq)

instance HasMeta PartStmt where
  getMeta = \case
    PsLabel m _ -> m
    PsDebugLabel m _ -> m
    PsDef m _ _ -> m
    PsIgnore m _ -> m
    PsReturn m _ -> m
    PsRequire m _ -> m
    PsAssert m _ -> m
    PsSwitch m _ -> m
  setMeta m = \case
    PsLabel _ lbl -> PsLabel m lbl
    PsDebugLabel _ lbl -> PsDebugLabel m lbl
    PsDef _ v e -> PsDef m v e
    PsIgnore _ e -> PsIgnore m e
    PsReturn _ e -> PsReturn m e
    PsRequire _ e -> PsRequire m e
    PsAssert _ e -> PsAssert m e
    PsSwitch _ sw -> PsSwitch m sw

instance IsStmt PartStmt where
  stmtsWithFreeVars = go
    where
      go [] = []
      go (PsLabel _ _ : ss) = go ss
      go (PsDebugLabel _ _ : ss) = go ss
      go (PsDef _ v e : ss) =
        let ss' = go ss
            fvs = Set.union (exFreeVars e) (Set.delete v (stmtsFreeVars ss'))
         in PsDef fvs v e : ss'
      go (PsIgnore _ e : ss) = chainEx PsIgnore exFreeVars e ss
      go (PsReturn _ e : ss) = chainEx PsReturn exFreeVars e ss
      go (PsRequire _ e : ss) = chainEx PsRequire trexFreeVars e ss
      go (PsAssert _ e : ss) = chainEx PsAssert trexFreeVars e ss
      go (PsSwitch _ sw : ss) =
        let sw' = switchWithFreeVars sw
            swFvs = swMeta sw'
         in chainTail
              ss
              (\fvs -> PsSwitch fvs sw')
              swFvs

      chainTail ss ctor headFvs =
        let ss' = go ss
            fvs = Set.union headFvs (stmtsFreeVars ss')
         in ctor fvs : ss'

      chainEx :: (Set Id -> e -> PartStmt (Set Id)) -> (e -> Set Id) -> e -> [PartStmt a] -> [PartStmt (Set Id)]
      chainEx ctor getEx e ss =
        chainTail
          ss
          (\fvs -> ctor fvs e)
          (getEx e)

data Switch a stmt = Switch
  { swMeta :: a,
    swArg :: TrivExpr,
    swBranches :: [(Pat, [stmt])]
  }
  deriving (Show, Read, Eq)

switchWithFreeVars :: IsStmt f => Switch a (f a) -> Switch (Set Id) (f (Set Id))
switchWithFreeVars sw =
  let branches = map goBranch (swBranches sw)
      branchFvs =
        foldMap
          (\(_, fvs, _) -> fvs)
          branches
   in Switch
        { swMeta = Set.union (trexFreeVars (swArg sw)) branchFvs,
          swArg = swArg sw,
          swBranches = [(p, stmts) | (p, _, stmts) <- branches]
        }
  where
    goBranch (p, ss) =
      let ss' = stmtsWithFreeVars ss
          fvs = stmtsFreeVars ss'
       in (p, Set.difference fvs (patVars p), ss')

data Expr
  = ExTriv TrivExpr
  | ExDot TrivExpr Id
  | ExList [TrivExpr]
  | ExTuple [TrivExpr]
  | ExRecord (Record TrivExpr)
  | -- | Probably obvious suggestion: maybe generalize this to other binary operators?
    ExEq TrivExpr TrivExpr
  | ExInput Type TrivExpr
  | -- | Question: can digest actually take multiple arguments? What does that do?
    ExDigest [TrivExpr]
  | ExSign TrivExpr
  | ExCapture TrivExpr [TrivExpr]
  | ExApp TrivExpr [TrivExpr]
  deriving (Show, Read, Eq)

exFreeVars :: Expr -> Set Id
exFreeVars (ExTriv e) = trexFreeVars e
exFreeVars (ExDot e _) = trexFreeVars e
exFreeVars (ExList es) = foldMap trexFreeVars es
exFreeVars (ExTuple es) = foldMap trexFreeVars es
exFreeVars (ExRecord es) = foldMap trexFreeVars es
exFreeVars (ExEq x y) = Set.union (trexFreeVars x) (trexFreeVars y)
exFreeVars (ExInput _ty e) = trexFreeVars e
exFreeVars (ExDigest es) = foldMap trexFreeVars es
exFreeVars (ExSign e) = trexFreeVars e
exFreeVars (ExCapture e es) = foldMap trexFreeVars (e : es)
exFreeVars (ExApp e es) = foldMap trexFreeVars (e : es)

data Lambda stmt = Lambda
  { lamCaptures :: [Id],
    -- | N.B. this representation allows nullary functions; is that what we intend?
    lamParams :: [Id],
    lamBody :: [stmt]
  }
  deriving (Show, Read, Eq)
