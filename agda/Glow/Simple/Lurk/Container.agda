{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.Container where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat
open import Cubical.Data.Fin
open import Cubical.Data.Int
open import Cubical.Data.Prod
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe)
open import Cubical.Data.Bool hiding (if_then_else_)  renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive
-- open import Cubical.Functions.Logic

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Glow.Simple.ContextMore

open import Glow.Simple.VarSubst

open import Glow.Simple.ParamsSubst

-- open import Glow.Simple.Monad


open import Cubical.HITs.Interval

open import Glow.ListDecProps

open import Cubical.Categories.Category

open import Glow.Simple.Lurk.HaskellInterface

open import Cubical.Data.BinNat.BinNat



record GLContainer : Type₀ where
  constructor GLContainerC
  field
    participantsIds : AList String
    lurkConsensusCode : LurkAST.Expr AList String Unit
    interactionParameters : AList (String × String )
    -- assets : AList String



data GLValue : Type₀ where
  GLNat : ℕ → GLValue
  GLBool : 𝟚 → GLValue
  GLPF : ℕ → GLValue
  DigestOf : GLValue → GLValue
  GLUnit : GLValue 

data Action : Type₀ where
  Withdraw : ℕ → Action
  Deposit : ℕ → Action
  Publish : GLValue → Action

record CallTy : Type₀ where
  constructor CallC
  field
    desiredStateId : ℕ
    caller : ℕ
    action : Action

record GLInteractionTest : Type₀ where
   constructor GLInteractionTestC
   field
     contract : GLContainer
     parameters : AList (String × GLValue)
     calls : AList CallTy
