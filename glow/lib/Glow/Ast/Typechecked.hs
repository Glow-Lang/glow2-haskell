{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- | AST fo  Glow's surface syntax.
module Glow.Ast.Typechecked where

import qualified Data.Text.Lazy as LT
import Glow.Prelude
import Numeric.Natural (Natural)


-- preamble ;)
-- The GADTs and data kinds extensions of the Haskell Language is a pathway to many abilities some consider to be unnatural.

-- idea :
-- surface ast with subtyping
-- internal ast without subtyping

------------ 

data Participant = Participant

data EContext =
    Consensus | Private Participant

data Symbol = Symbole

data Variable (c :: EContext) = Variable LT.Text

data PubRel (cOut :: EContext) (cIn :: EContext) where
  AtPart :: PubRel Consensus (Private pa)
  AtNothing :: PubRel cOut cIn 

-- bindingRules :: Stmnt cAfter -> Maybe (BindingRulesCheck cBefore cAfter)
-- bindingRules = undefined

data Stmnt (c0 :: EContext) (c1 :: EContext) where
  BindingS :: BStmnt c0 c1 -> Stmnt c0 c1
  NonBindingS :: NBStmnt c -> Stmnt c c

data BStmnt (c0 :: EContext) (c1 :: EContext) where
  BSLet :: PubRel cOut cIn -> Symbol -> Expr cOut -> BStmnt cOut cIn
  BSPublish :: Variable (Private pa) -> BStmnt Consensus Consensus

data NBStmnt (c :: EContext) where
  NBSExpr :: Expr c -> NBStmnt c
  NBSStmnt :: NBStmnts -> NBStmnt Consensus

data NBStmnts where
  NBSSRequire :: Expr Consensus -> NBStmnts
  NBSSDeposit :: Participant -> Natural -> NBStmnts
  NBSSWithdraw :: Participant -> Natural -> NBStmnts

data Expr (c :: EContext) where
  Var :: Variable c -> Expr c
  BodyE :: Body c0 cN -> Expr cN -> Expr c0
  Lit :: Expr c
  App :: Expr c
  Input :: Expr (Private pa)
  Sign :: Expr (Private pa)

data Body (c0 :: EContext) (c1 :: EContext) where
  BNil :: Body c c
  BCons :: Body c0 cN -> Stmnt cN cM -> Body c0 cM

---------

-- data BStmnt (Γ : Context) : Type₀


-- data NBStmnt (Γ : Context) : Type₀

-- data NBStmnt+Expr (Γ : Context) : Type₀

-- data Expr (Γ : Context) (Τ : GType): Type₀

-- data Arg (Γ : Context) (Τ : GType): Type₀

-- Args : (Γ : Context) (Τs : List GType) → Type₀


-- bindingMechanics : {Γ : Context} → BStmnt Γ → List ContextEntry 

-- bindingMechanics' : (Γ : Context) → Stmnt Γ → Context 


-- record Body (Γ : _) (Τ : _ ) : Type₀ where
--   pattern
--   inductive
--   constructor bodyR
--   field
--     stmnts : Linked' bindingMechanics' Γ
--     expr : Expr (foldLinked' stmnts) Τ

-- open Body public

-- data Arg Γ Τ where
--   var-a : DefinedSymbolOfTy Γ Τ → Arg Γ Τ
--   lit-a : GTypeAgdaRep Τ → Arg Γ Τ


-- data Expr Γ Τ where
--   var : DefinedSymbolOfTy Γ Τ → Expr Γ Τ
--   body : Body Γ Τ → Expr Γ Τ
--   lit : GTypeAgdaRep Τ → Expr Γ Τ
--   _$'_ : (x : BI Τ) → Args Γ (getBI-Dm x) → Expr Γ Τ
--   input : String → {_ : PM (IsNotConsensus Γ) } → Expr Γ Τ
--   sign : Arg Γ Digest → {_ : PM (IsNotConsensus Γ) } → {_ : Signature PM≡ Τ} → Expr Γ Τ


--   -- this is temporary solution, this constructors cannot apear in code, and are introduced on some passes, this distinction must be typesafe in the future! 
--   receivePublished : DishonestParticipantId → {_ : PM (IsConsensus Γ) } → Expr Γ Τ

--   if_then_else_ : Expr Γ Bool → Expr Γ Τ → Expr Γ Τ → Expr Γ Τ

-- data Stmnt Γ where
--   -- not necessary binding, but rather context changing
--   bindingS : BStmnt Γ → Stmnt Γ
--   nonBindingS : NBStmnt+Expr Γ → Stmnt Γ

-- data BStmnt Γ where
--                 -- warning: scope in "ce" is interpreted in unusual way!
--                 -- (TODO : consider speical type here)
--   BS-let : (ce : ContextEntry) → {asn : PM  (AllowedScopeNarrowing Γ (scope ce) )}
--               → Expr (narrow Γ (scope ce) asn) (type ce) → BStmnt Γ    
--   BS-publish! : (p : HonestParticipantId) → (PrivateSymbolOf Γ p)
--                          → {_ : PM ( IsConsensus Γ ) } →  BStmnt Γ

-- data NBStmnt Γ where
--   NBS-require! : Expr Γ Bool → NBStmnt Γ
--   NBS-deposit! : ParticipantId → Expr Γ Nat → NBStmnt Γ
--   NBS-withdraw! : ParticipantId → Expr Γ Nat → NBStmnt Γ
--   -- this is temporary solution, this constructors cannot apear in code, and are introduced on some passes, this distinction must be typesafe in the future!

--   NBS-publishVal! : HonestParticipantId → Identifier → NBStmnt Γ

-- data NBStmnt+Expr Γ where
--   stmntNBS : NBStmnt Γ → {_ : PM ( IsConsensus Γ ) } →  NBStmnt+Expr Γ
--   exprNBS : ∀ {Τ} → Expr Γ Τ → NBStmnt+Expr Γ
