
{-# OPTIONS --cubical  #-}
module Glow.Simple.UnsafeProjectOut where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Foundations.CartesianKanOps

open import Cubical.Data.Nat
open import Cubical.Data.Int
open import Cubical.Data.Prod renaming (map to prod-map)
open import Cubical.Data.Sum renaming (elim to sum-elim ; rec to sum-rec ; map to sum-map)
open import Cubical.Data.List renaming (map to map-List)


open import Cubical.Data.Maybe renaming (rec to recMaybe )
open import Cubical.Data.Bool hiding (if_then_else_ ; _≟_) renaming (Bool to 𝟚)

open import Cubical.Data.Empty renaming (elim to empty-elim ; rec to empty-rec ;  ⊥ to Empty )


open import Cubical.Data.Nat.Order.Recursive hiding (_≟_)
import Cubical.Functions.Logic as Lo

open import Cubical.Relation.Nullary.Base renaming (¬_ to IsEmpty)

open import Glow.Linked

open import Glow.Simple.AST

open import Glow.DecEqMore

open import Cubical.HITs.Interval

open import Glow.ListDecProps

open import Glow.Simple.ContextMore

module UnsafeProjectOut {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
              {BuiltInsIndex : Type₀} {{IsDiscrete-BuiltInsIndex : IsDiscrete BuiltInsIndex}}
              {builtIns : BuiltIns' BuiltInsIndex {{IsDiscrete-BuiltInsIndex}}} where

  prop-mode = one
  
  open AST Identifier builtIns prop-mode

  open PropMode prop-mode 

  -- open InteractionHead

  makeDistrusted' : (ps : List (Identifier × ParticipantModality))
                   → PM (_ , UniqueByDec≡ proj₁ ps , isProp-UniqueBy _ _ )
                   → ∀ nm → PM ( IsHonestParticipantId {ps} nm )  
                           →  Σ (List (Identifier × ParticipantModality))
                                  λ ps → PM ( IsDistrustedParticipantId {ps} nm )
                                    × PM (_ , UniqueByDec≡ proj₁ ps , isProp-UniqueBy _ _ )
  makeDistrusted' l x nm w =
    ExistMemberAs-mapExisting-help {{eqTest (IsDiscrete-Identifier)}}
     l x w (λ x₁ q → (proj₁ x₁ , distrusted) , proj₁ q , refl)
       (λ a _ → refl)
       (λ a a' x₁ x₂ → sym (proj₁ x₁) ∙ proj₁ x₂)
       λ a a' x₁ x₂ x₃ → proj₁ x₁ ∙ x₂ ,
         sum-rec (λ a₁ → empty-elim (x₃ (proj₁ x₁ ∙ x₂ , (sym a₁)))) (sym) (dichotomyBool (proj₂ a))  

  makeDistrustedΣ : (ih : InteractionHead) → HonestParticipantId ih 
                           →  Σ InteractionHead DistrustedParticipantId 
  AST.participantsWM (fst (makeDistrustedΣ ih hp)) =
     (fst (makeDistrusted' (AST.participantsWM ih) (AST.uniquePtcpnts ih) (pId-nameHon hp) (pId-isInHon hp)))
  AST.parameters (fst (makeDistrustedΣ ih hp)) = AST.parameters ih
  AST.uniqueParams (fst (makeDistrustedΣ ih hp)) = AST.uniqueParams ih
  AST.uniquePtcpnts (fst (makeDistrustedΣ ih hp)) = 
     proj₂ (snd (makeDistrusted' (AST.participantsWM ih) (AST.uniquePtcpnts ih) (pId-nameHon hp) (pId-isInHon hp)))
  snd (makeDistrustedΣ ih hp) =
    AST.pId ((pId-nameHon hp))
       {proj₁ (snd (makeDistrusted' (AST.participantsWM ih) (AST.uniquePtcpnts ih) (pId-nameHon hp) (pId-isInHon hp)))}

  makeDistrusted : (ih : InteractionHead) → HonestParticipantId ih → InteractionHead   
  makeDistrusted ih = fst ∘ makeDistrustedΣ ih

-- makeDistrusted' l x nm w
 
  makeDistrusted'-ParticipantId : ∀ pp → ∀ nm → ∀ x nm' w 
           → ⟨ IsParticipantId {map-List proj₁ pp} nm ⟩
           → ⟨ IsParticipantId {map-List proj₁ (fst (makeDistrusted' pp x nm' w))} nm ⟩  
  makeDistrusted'-ParticipantId (x₂ ∷ pp) nm x nm' (inl x₃) xx = xx
  makeDistrusted'-ParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inl x₁) = inl x₁
  makeDistrusted'-ParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inr x₁) =
     inr (proj₁ x₁ , (makeDistrusted'-ParticipantId pp nm x₄ nm' (proj₂ x₃) (proj₂ x₁)))

  makeDistrusted'-DistrustedParticipantId : ∀ pp → ∀ nm → ∀ x nm' w 
           → ⟨ IsDistrustedParticipantId {pp} nm ⟩
           → ⟨ IsDistrustedParticipantId {(fst (makeDistrusted' pp x nm' w))} nm ⟩  
  makeDistrusted'-DistrustedParticipantId (x₂ ∷ pp) nm x nm' (inl x₃) (inl x₁) = inl (proj₁ x₁ , refl)
  makeDistrusted'-DistrustedParticipantId (x₂ ∷ pp) nm x nm' (inl x₃) (inr x₁) =
     inr ((λ x₄ → proj₁ x (ExistMemberAs→ (λ a x₅ → sym (proj₁ x₄) ∙ proj₁ x₅) (λ a → _ ≟ _) (proj₂ x₁))) , (proj₂ x₁))
     --inr ((λ x₄ → {!proj₁ x₄ ∙ (sym (proj₁ x₃))!}) , (proj₂ x₁))
  makeDistrusted'-DistrustedParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inl x₁) = inl x₁
  makeDistrusted'-DistrustedParticipantId (x₂ ∷ pp) nm (x , x₄) nm' (inr x₃) (inr x₁) =
     inr (proj₁ x₁ , (makeDistrusted'-DistrustedParticipantId pp nm x₄ nm' (proj₂ x₃) (proj₂ x₁)))


  makeDistrusted-ParticipantId : (ih : InteractionHead) → ∀ hp → 
                                    ParticipantId ih → ParticipantId (makeDistrusted ih hp)
  makeDistrusted-ParticipantId ih hp (AST.pId name₁ {y}) =
     AST.pId name₁ {makeDistrusted'-ParticipantId _ _ (AST.uniquePtcpnts ih) _ _ y}

    
  makeDistrusted-DistrustedParticipantId : (ih : InteractionHead) → ∀ hp → 
                                    DistrustedParticipantId ih → DistrustedParticipantId (makeDistrusted ih hp)
  makeDistrusted-DistrustedParticipantId ih hp (AST.pId name₁ {y}) =
     AST.pId name₁ {makeDistrusted'-DistrustedParticipantId _ _ (AST.uniquePtcpnts ih) _ _ y}

  CtxTrans' : (ptps : List (Identifier × ParticipantModality))
            → Maybe (HonestParticipantId' {ptps})
            → (HonestParticipantId' {ptps})
            → DecPropΣ
  CtxTrans' [] x (AST.pId name₁ {()}) 
  CtxTrans' (x₃ ∷ ptps) nothing (_) = Unit-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inl x})) (AST.pId name₁ {inl x₁}) = Empty-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inl x})) (AST.pId name₁ {inr x₁}) = Unit-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inr x})) (AST.pId name₁ {inl x₁}) = Unit-dp
  CtxTrans' (x₃ ∷ ptps) (just (AST.pId name₂ {inr x})) (AST.pId name₁ {inr x₁}) = 
     CtxTrans' (ptps) (just (AST.pId name₂ {proj₂ x})) (AST.pId name₁ {proj₂ x₁})

  
  CtxTrans'-cases : (ptps : List (Identifier × ParticipantModality)) → ∀ {q}
            → ∀ nm
            → (z : (⟨ IsHonestParticipantId {ptps} nm ⟩))
            → (hp : (HonestParticipantId' {ptps}))
            → ⟨ CtxTrans' ptps (just (AST.pId nm {z})) hp ⟩
            → (⟨ IsHonestParticipantId  {(fst (makeDistrusted' ptps q (pId-nameHon hp) (pId-isInHon hp)))} nm ⟩ )
  CtxTrans'-cases (x₁ ∷ ptps) nm (inr x₂) (AST.pId name₁ {inl x₃}) x = inr ((true≢false ∘ proj₂) , (proj₂ x₂))
  CtxTrans'-cases (x₁ ∷ ptps) nm (inl x₂) (AST.pId name₁ {inr x₃}) x = inl x₂
  CtxTrans'-cases (x₁ ∷ ptps) {q} nm (inr x₂) (AST.pId name₁ {inr x₃}) x =
     inr (proj₁ x₂ , CtxTrans'-cases (ptps) {proj₂ q} nm (proj₂ x₂) (AST.pId name₁ {proj₂ x₃}) x)

  
  CtxTrans'-cases¬ : (ptps : List (Identifier × ParticipantModality)) → ∀ {q}
            → ∀ nm
            → (z : (⟨ IsHonestParticipantId {ptps} nm ⟩))
            → (hp : (HonestParticipantId' {ptps}))
            → (⟨ CtxTrans' ptps (just (AST.pId nm {z})) hp ⟩ → Empty)
            → (⟨ IsDistrustedParticipantId  {(fst (makeDistrusted' ptps q (pId-nameHon hp) (pId-isInHon hp)))} nm ⟩ )
  CtxTrans'-cases¬ (x₁ ∷ ptps) nm (inr x₂) (AST.pId name₁ {inl x₃}) x = empty-elim (x _)
  CtxTrans'-cases¬ (x₁ ∷ ptps) nm (inl x₂) (AST.pId name₁ {inr x₃}) x = empty-elim (x _)
  CtxTrans'-cases¬ (x₁ ∷ ptps) {q} nm (inr x₂) (AST.pId name₁ {inr x₃}) x =
     let z = λ a → proj₁ q ((ExistMemberAs→ (λ a₁ x₄ → (sym (proj₁ a) ∙ (proj₁ x₄))) (λ _ → _ ≟ _) (proj₂ x₂)))
     in inr ( z , CtxTrans'-cases¬ (ptps) {proj₂ q} nm (proj₂ x₂) (AST.pId name₁ {proj₂ x₃}) x)
  CtxTrans'-cases¬ (x₁ ∷ ptps) {q} nm (inl x₂) (AST.pId name₁ {inl x₃}) x = inl ((proj₁ x₂) , refl)

  ctxTrans-hlp : ∀ {ih : _} {hp : HonestParticipantId ih} → (nm : Identifier)
       
       → (xx : ExistMemberAs
             (λ x₁ → (nm ≡ proj₁ x₁) × (true ≡ proj₂ x₁))
             (participantsWM ih))

       → ⟨ CtxTrans' (participantsWM ih) (just (AST.pId nm {xx})) hp ⟩
         
       → ExistMemberAs
             (λ x₁ → (nm ≡ proj₁ x₁) × (true ≡ proj₂ x₁))
             (ExistMemberAs-mapExisting (λ x₁ x₂ → proj₁ x₁ , false)
              (participantsWM ih) (AST.pId-isInHon _ _ _ hp))
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {AST.pId name₁ {inl x₂}} nm (inl x₃) ()
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {AST.pId name₁ {inl x₂}} nm (inr x₃) _ =
     inr ( true≢false ∘ proj₂ , (proj₂ x₃))
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁} {AST.pId name₁ {inr x₂}} nm (inl x₃) _ = inl x₃
  ctxTrans-hlp {AST.interactionHead (x₁ ∷ participantsWM₁) parameters₁ {yy} {x₄ , x₅}} {AST.pId name₁ {inr x₂}} nm (inr x₃) x = 
     inr ( proj₁ x₃
       , ctxTrans-hlp {AST.interactionHead (participantsWM₁) parameters₁ {yy} {x₅}} {AST.pId name₁ {proj₂ x₂}} nm (proj₂ x₃) x)


  ctxTrans-hlp-CE' : ∀ {ih : _} → ∀ {entries₁} → ∀ hp → ∀ nm → (q : ⟨ IsHonestParticipantId {participantsWM ih} nm ⟩) →  
                        Maybe
                          ⟨ ×-dp ( IsHonestParticipantId {participantsWM (makeDistrusted ih hp)} nm )
                          
                           (CtxTrans' (participantsWM ih)
                                   (narrowScope ih
                                   (con entries₁ nothing) (just (pId nm {q})) tt)
                                   hp) ⟩ 
  ctxTrans-hlp-CE' {ih} {entries₁} hp nm q =
      dec-rec' _ just (const nothing)
        (proj₁ (snd ( ×-dp ( IsHonestParticipantId {participantsWM (makeDistrusted ih hp)} nm )
                          
                           (CtxTrans' (participantsWM ih)
                                   (narrowScope ih
                                   (con entries₁ nothing) (just (pId nm {q})) tt)
                                   hp))) )

  ctxTrans-hlp-CE : ∀ {ih : _} → ∀ hp → ContextEntry ih → Maybe (ContextEntry (makeDistrusted ih hp))
  ctxTrans-hlp-CE hp (AST.ice nothing name₁ type₁) = just (AST.ice nothing name₁ type₁)
  ctxTrans-hlp-CE ih@{AST.interactionHead (x ∷ xs) pms} hp (AST.ice (just zz@(AST.pId nm' {yy'})) name₁ type₁) =
    dec-rec' _ (just ∘ (λ b → ice (just (pId nm' {b})) name₁ type₁) ∘ ctxTrans-hlp {ih = ih} {hp = hp} nm' yy')
     (const nothing) (proj₁ (snd (CtxTrans' _ (just zz) hp )) )

  scopeTrans : ∀ {ih : _} {hp} → (sc : Scope ih) → ⟨ CtxTrans' (participantsWM ih) sc hp ⟩ → Scope (makeDistrusted ih hp)
  scopeTrans {ih} {hp} nothing x = nothing
  scopeTrans {ih} {hp} (just (AST.pId name₁ {yy})) x = just (AST.pId name₁ {ctxTrans-hlp {ih} {hp} name₁ yy x })



  module U = Unsafe

  module _ {ih : _} (hp : _)  where

    private _>>=_ = bind-Maybe


    {-# TERMINATING #-}
    projectOutE : U.Expr ih → U.Expr (makeDistrusted ih hp)
    projectOutA : U.Arg ih → U.Arg (makeDistrusted ih hp)
    projectOutS : U.Stmnt ih → Maybe (U.Stmnt (makeDistrusted ih hp))

    projectOutE (U.var x x₁) = (U.var x x₁)
    projectOutE (U.body (U.bodyR stmnts₁ expr₁)) = (U.body (U.bodyR (filterMap projectOutS stmnts₁)  (projectOutE expr₁)))
    projectOutE (U.lit x) = (U.lit x)
    projectOutE (x U.$' x₁) = (x U.$' (map-List projectOutA x₁))
    projectOutE (U.input x x₁) = (U.input x x₁)
    projectOutE (U.sign x) = (U.sign (projectOutA x))
    projectOutE (U.receivePublished x x₁) = (U.receivePublished x (makeDistrusted-DistrustedParticipantId ih hp x₁))
    projectOutE (U.if x then x₁ else x₂) = (U.if (projectOutE x) then (projectOutE x₁) else (projectOutE x₂))
    
    projectOutA (U.var-a x) = (U.var-a x)
    projectOutA (U.lit-a x) = (U.lit-a x)
 
    projectOutS (U.bindingS x) with x
    ... | U.BS-let nothing x₂ x₃ x₄ = just (U.bindingS (U.BS-let nothing x₂ x₃ (projectOutE x₄)))
    ... | U.BS-let sc@(just _) x₂ x₃ x₄ = do
      ct' ← mbDec (CtxTrans' (participantsWM ih) sc hp)
      just (U.bindingS (U.BS-let (scopeTrans {ih} {hp} sc ct' ) x₂ x₃ (projectOutE x₄)))
    ... | U.BS-publish! (AST.pId nm) t x₂ with mbDec (IsHonestParticipantId {participants = participantsWM (makeDistrusted ih hp)} nm)
    ... | nothing = just (U.bindingS (U.BS-let nothing x₂ t (U.receivePublished t (snd (makeDistrustedΣ ih hp)))))
    ... | just x₃ = just (U.bindingS (U.BS-publish! (AST.pId nm {x₃}) t x₂)) 

    
    projectOutS (U.nonBindingS (U.stmntNBS x)) = map-Maybe (U.nonBindingS ∘ U.stmntNBS) (h x)
      where
        h : U.NBStmnt ih → Maybe (U.NBStmnt (makeDistrusted ih hp))
        h (U.NBS-require! x) = just (U.NBS-require! (projectOutE x))
        h (U.NBS-deposit! x x₁) = just (U.NBS-deposit! (makeDistrusted-ParticipantId ih hp x ) (projectOutE x₁))
        h (U.NBS-withdraw! x x₁) = just (U.NBS-withdraw! (makeDistrusted-ParticipantId ih hp x) (projectOutE x₁))
        h (U.NBS-publishVal! x x₁) = nothing -- TODO : figure out semantics of this, and sort out its safety
    projectOutS (U.nonBindingS (U.exprNBS x)) = just (U.nonBindingS (U.exprNBS (projectOutE x)))

    projectOut : List (U.Stmnt ih) → List (U.Stmnt (makeDistrusted ih hp))
    projectOut = filterMap projectOutS


  -- makeDistrustedAll : InteractionHead → InteractionHead 
  -- makeDistrustedAll x = {!x!}

  -- projectOutAll : {!!}
  -- projectOutAll = {!!}

  -- CtxTrans : {ih : InteractionHead} → HonestParticipantId ih → Context ih → Type₀
  -- CtxTrans {ih} hp Γ =
  --      ⟨ CtxTrans' (participantsWM ih) (scope' Γ) hp ⟩
  --       × Σ ℕ λ k → BTFS (λ Τ → (λ y → ⟨ isNothing-dp (scope y) ⟩ × (Τ ≡ type y) ))
  --           (name)
  --           k (filterMap (ctxTrans-hlp-CE {ih} hp) (AST.entries Γ))  
  --        -- List (BTF (filterMap (ctxTrans-hlp-CE {ih} hp) (AST.entries Γ)))


  -- ctxTrans : ∀ {ih : _} {hp} → (Γ : Context ih) → CtxTrans hp Γ → Context (makeDistrusted ih hp)
  -- AST.entries (ctxTrans Γ x) = btfs _ _ (snd (proj₂ x))
  -- AST.scope' (ctxTrans {ih} {hp} Γ x) = scopeTrans {ih} {hp} (_) (proj₁ x)
