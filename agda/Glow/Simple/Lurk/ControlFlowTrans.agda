{-# OPTIONS --cubical  #-}
module Glow.Simple.Lurk.ControlFlowTrans where

open import Agda.Builtin.String
open import Agda.Builtin.Char
-- open import Agda.Builtin.List

open import Cubical.Foundations.Everything 

open import Cubical.Data.Nat renaming (_+_ to _ℕ+_)
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

open import Glow.Simple.Lurk.Translation

open import Glow.Simple.Example



data LabeledValue[_][_] {ℓ} {A : Type ℓ} : String → A → Type ℓ where
  labeledValue[_][_]labeledValueEnd : (label : String) → (a : A) → LabeledValue[ label ][ a ] 

mkLV : ∀ {ℓ label} {A : Type ℓ} → {a : A} → LabeledValue[ label ][ a ]
mkLV {label = label} {a = a} = labeledValue[ label ][ a ]labeledValueEnd

open import Glow.Simple.UnsafeProjectOut

module _ {Identifier : Type₀} {{IsDiscrete-Identifier : IsDiscrete Identifier}}
            {BuilitInsIndex : Type₀} {{IsDiscrete-BuilitInsIndex : IsDiscrete BuilitInsIndex}}
              {builtIns : BuiltIns' BuilitInsIndex {{IsDiscrete-BuilitInsIndex}}} where


  everyIsDishonest-lem : (ptpsIds : List (Identifier)) → ∀ name →
                           ExistMemberAs (PathP (λ x₁ → Identifier) name)
                                  (map-List proj₁ (map-List (_, false) ptpsIds)) →
                           ExistMemberAs (λ x₁ → (name ≡ proj₁ x₁) × (false ≡ proj₂ x₁))
                                (map-List (_, false) ptpsIds)

  everyIsDishonest-lem [] name ()
  everyIsDishonest-lem (x ∷ ptpsIds) name (inl x₁) = inl (x₁ , refl)
  everyIsDishonest-lem (x ∷ ptpsIds) name (inr x₁) = inr (proj₁ x₁ ∘ proj₁ , (everyIsDishonest-lem ptpsIds name (proj₂ x₁)))


  
  --TODO: parametrisation by abstracted fields of interactionHead may not be good idea,
  --      instead try refactoring to parametrization via InteractionHead + apropriate predicate of dishonesty
  module MonadicControlFlow (ptpsIds : List (Identifier)) (prms : _) (uniquePrms : _) where

    ptps : List (Identifier × ParticipantModality)
    ptps = map-List (_, dishonest) ptpsIds
    
    module MonadicControlFlowUP (uniquePtps : _) where

      ptps'' = ptps

      ih'' : AST.InteractionHead Identifier builtIns one
      ih'' = (AST.interactionHead ptps prms {uniquePrms} {uniquePtps}) 

      open AST.InteractionHead {Identifier} {builtIns = builtIns} {one} ih''

   

      open SubstAll {Identifier} {builtIns = builtIns} {ptps = ptps}
      open SubstOne {Identifier} {builtIns = builtIns} {ptps = ptps}

   -- Context :   (A : Int) :: (B : Int) :: (C : Bool) :: []
   --             (A : 3)   :: (B : 2) :: (C : True ) :: []



   -- PublicContext : (A : Int ) :: { ( (B : Int) :: (C : Int)  ) , (D : Int)  } :: []
   --                 (A : 3) :: ( B : 2) :: (C : 4) :: []
   --                 (A : 2) :: (D : 4) :: []


      data PublicContext : Type₀ where
        PC[] : PublicContext
        _PC∷_ : (GType) ⊎ (PublicContext × PublicContext) → PublicContext → PublicContext 
        -- ⟨_∶_⟩PC∷_ : Identifier → GType → PublicContext → PublicContext
        -- [_∨_]bPC∷_ : PublicContext → PublicContext → PublicContext → PublicContext

      {-# TERMINATING #-}
      PCRec : PublicContext → Type₀
      PCRec PC[] = Unit
      PCRec (x PC∷ x₁) = (sum-elim GTypeAgdaRep (λ b → PCRec (proj₁ b) ⊎ PCRec (proj₂ b)) x) × PCRec x₁ 

      _PC++_ : PublicContext → PublicContext → PublicContext
      _PC++_ PC[] x₁ = x₁
      _PC++_ (x PC∷ x₂) x₁ = x PC∷ (x₂ PC++  x₁)
      
      StmntPC : ∀ {Γ} → Stmnt Γ → PublicContext

      StatementsPC : ∀ {Γ} → Statements Γ → PublicContext
      ExprPC : ∀ {Γ Τ} → Expr Γ Τ → PublicContext

      StatementsPC []L = PC[]
      StatementsPC (h ∷L x) = StmntPC h  PC++ StatementsPC x 

      ExprPC (AST.var x) = PC[]
      ExprPC (AST.body (AST.bodyR stmnts₁ expr₁)) = StatementsPC stmnts₁ PC++ ExprPC expr₁
      ExprPC (AST.lit x) = PC[]
      ExprPC (x AST.$' x₁) = PC[]
      ExprPC (AST.input x) = PC[]
      ExprPC (AST.sign x) = PC[]
      ExprPC {Τ = Τ} (AST.receivePublished x) = inl (Τ) PC∷ PC[]
      ExprPC (AST.if x then x₁ else x₂) = inr (ExprPC x₁ , ExprPC x₂) PC∷ PC[]

      StmntPC {Γ} (AST.bindingS x) = w x
         where
          w : BStmnt Γ → PublicContext
          w (AST.BS-let ce x) = ExprPC x
          w (AST.BS-publish! p x) = PC[] 
      StmntPC {Γ} (AST.nonBindingS (AST.stmntNBS x)) = PC[]
        -- where
        --   w : NBStmnt Γ → PublicContext
        --   w (AST.NBS-require! x) = PC[]
        --   w (AST.NBS-deposit! x x₁) = PC[]
        --   w (AST.NBS-withdraw! x x₁) = PC[]
        --   w (AST.NBS-publishVal! x x₁) = PC[] -- <- imposible!!
      StmntPC {Γ} (AST.nonBindingS (AST.exprNBS x)) = ExprPC x

      -- evalWithPC : ∀ {Γ Τ} → (e : Expr Γ Τ) → (r : Rec Γ) → PCRec (ExprPC e) → GTypeAgdaRep Τ
      -- evalWithPC = {!!}

      -- evalWithPC : ∀ {Τ} → (e : Expr (con [] nothing) Τ) → PCRec (ExprPC e) → GTypeAgdaRep Τ
      -- evalWithPC e x = {!!}




      record PCExpr (pc : PublicContext) (Τ : GType) : Type₀ where
        field
           e : Expr (con [] nothing) Τ
           pc≡ : ExprPC e ≡ pc


      FreeExpr = Expr (con [] nothing)

      PID = DishonestParticipantId
      
      -- TODO : try to parametrise be all definitions from AST
      module _ {Expr : Context → GType → Type₀} {IsPureE : ∀ {Γ Τ} → Expr Γ Τ → DecPropΣ} where
  
        data Action' (Γ : Context) : Type₀ where
          withdrawA : (e : Expr Γ Nat) → (⟨ IsPureE e ⟩) → Action' Γ
          depositA : (e : Expr Γ Nat) → (⟨ IsPureE e ⟩) → Action' Γ 

        _,_⦂_ : Context → Identifier → GType → Context
        Γ , x ⦂ Τ = addToContext Γ (AST.ice nothing x Τ)

        data LMonad' (A : Type₀) (Γ : Context) (Τ : GType) : Type₀ where
          action : A → PID → Action' Γ → Τ ≡ Unitᵍ → LMonad' A Γ Τ
          require : (e : Expr Γ Bool) → (⟨ IsPureE e ⟩) → Τ ≡ Unitᵍ → LMonad' A Γ Τ
          expectPub : A → PID → LMonad' A Γ Τ
          bind : ∀ {x Τ'} → LMonad' A Γ Τ' → LMonad' A (Γ , x ⦂ Τ') Τ → LMonad' A Γ Τ
          next : ∀ {Τ'} → LMonad' A Γ Τ' → LMonad' A Γ Τ → LMonad' A Γ Τ
          pure : (e : Expr Γ Τ) → (⟨ IsPureE e ⟩) → LMonad' A Γ Τ
          branch : (e : Expr Γ Bool) → (⟨ IsPureE e ⟩) → LMonad' A Γ Τ → LMonad' A Γ Τ → LMonad' A Γ Τ

        labelStates : ∀ {A Γ Τ} → ℕ → LMonad' A Γ Τ → (Σ ℕ λ _ → LMonad' ℕ Γ Τ) 
        labelStates n (action x x₁ x₂ x₃) = (suc n) , (action n x₁ x₂ x₃)
        labelStates n (require e x x₁) = n , (require e x x₁)
        labelStates n (expectPub x x₁) = (suc n) , (expectPub n x₁)
        labelStates n (bind x x₁) =
          let (n₁ , x') = labelStates n x
              (n₂ , x₁') = labelStates n₁ x₁
          in (n₂ , bind x' x₁')
        labelStates n (next x x₁) =
          let (n₁ , x') = labelStates n x
              (n₂ , x₁') = labelStates n₁ x₁
          in (n₂ , next x' x₁')
        labelStates n (pure e x) = n , pure e x
        labelStates n (branch e y x x₁) =
          let (n₁ , x') = labelStates n x
              (n₂ , x₁') = labelStates n₁ x₁
          in (n₂ , (branch e y x' x₁'))

        labelStates' : ∀ {A Γ Τ} → LMonad' A Γ Τ → (LMonad' ℕ Γ Τ)  
        labelStates' = snd ∘ labelStates 1

      Action = Action' {Expr} {IsPureE}
      LMonad = LMonad' {Expr} {IsPureE}

      UAction = Action' {λ _ _ → Unsafe.Expr} {λ _ → Unit-dp }
      ULMonad = LMonad' {λ _ _ → Unsafe.Expr} {λ _ → Unit-dp }


      everyIsDishonest : ParticipantId → PID
      everyIsDishonest (AST.pId name {x}) = AST.pId name {everyIsDishonest-lem _ _ x}




      module tryTranslation where
        private       
          _>>=_ = bind-Maybe

        mbIsPureE : ∀ {Γ Τ} → (e : Expr Γ Τ) → Maybe ⟨ IsPureE e ⟩
        mbIsPureE e = mbDec (IsPureE e)

        toLMonadE : ∀ Γ Τ → Expr Γ Τ → Maybe (LMonad Unit Γ Τ)
        toLMonadNBS : ∀ Γ → NBStmnt+Expr Γ → Maybe (LMonad Unit Γ Unitᵍ)

        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-require! x)) =  do
          ispure-x ← mbIsPureE x
          just ((require x ispure-x refl))
        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-deposit! x x₁)) = do
          ispure-x₁ ← mbIsPureE x₁
          just ((action tt (everyIsDishonest x) (depositA x₁ ispure-x₁) refl))
        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-withdraw! x x₁)) = do
          ispure-x₁ ← mbIsPureE x₁
          just ((action tt (everyIsDishonest x) (withdrawA x₁ ispure-x₁) refl))
          -- e ← toLMonadE _ _ x₁ -- TODO make it work for impure exprs!
          -- just (bind e ((action tt (everyIsDishonest x) (withdrawA {!!} {!!}) refl)))
        toLMonadNBS Γ (AST.stmntNBS (AST.NBS-publishVal! x x₁)) = nothing
        toLMonadNBS Γ (AST.exprNBS x) = do
          e ←  (toLMonadE _ _ x) 
          just (next e (pure (lit tt) tt))
        -- toLMonadS : ∀ Γ → Stmnt Γ → Maybe (LMonad Unit Γ Unitᵍ)

        toLMonadE Γ Τ (AST.var x) = just (pure (var x) tt)
        
        toLMonadE Γ Τ (AST.body (AST.bodyR []L expr₁)) = toLMonadE Γ Τ expr₁
        toLMonadE (AST.con entries₁ nothing) Τ (AST.body (AST.bodyR (AST.bindingS (AST.BS-let (AST.ice nothing name type) x) ∷L stmnts₁) expr₁)) = 
          do e' ← toLMonadE _ type x
             es' ← toLMonadE _ _ (body (bodyR stmnts₁ expr₁ ))
             just (bind e' es')
        toLMonadE (AST.con entries₁ (just x₁)) Τ (AST.body (AST.bodyR (AST.bindingS (AST.BS-let (AST.ice nothing name type) x) ∷L stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (AST.body (AST.bodyR (AST.bindingS (AST.BS-let (AST.ice (just x₁) name type) x) ∷L stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (body (bodyR (bindingS (BS-publish! p x) ∷L stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (body (bodyR (nonBindingS x ∷L stmnts₁) expr₁)) =
          do e' ← toLMonadNBS _ x
             es' ← toLMonadE _ _ (body (bodyR stmnts₁ expr₁ ))
             just (next e' es')
        
        toLMonadE Γ Τ (AST.lit x) = just (pure (lit x) tt)
        toLMonadE Γ Τ (x AST.$' x₁) = just (pure (x AST.$' x₁) tt)
        toLMonadE Γ Τ (AST.input x) = nothing
        toLMonadE Γ Τ (AST.sign x) = nothing
        toLMonadE Γ Τ (AST.receivePublished x) = just (expectPub _ x)
        toLMonadE Γ Τ (AST.if e then e₁ else e₂) = do
          ispure-e ← mbIsPureE e
          e₁' ← toLMonadE Γ _ e₁
          e₂' ← toLMonadE Γ _ e₂
          just (branch e ispure-e e₁' e₂')

        toLMonad : ∀ Γ → Statements Γ → Maybe (LMonad Unit Γ Unitᵍ)
        toLMonad Γ x = toLMonadE Γ Unitᵍ (body (AST.bodyR x (lit tt)))


      module tryTranslationUnsafe where
        private       
          _>>=_ = bind-Maybe

        module U = Unsafe

        mbIsPureE : (e : U.Expr) → Maybe ⟨ Unit-dp ⟩
        mbIsPureE e = just _


        toLMonadE : ∀ Γ Τ → (e : U.Expr) → Maybe (ULMonad Unit Γ Τ)
        toLMonadNBS : ∀ Γ → U.NBStmnt+Expr → Maybe (ULMonad Unit Γ Unitᵍ)

        toLMonadNBS Γ (U.stmntNBS (U.NBS-require! x)) =  do
          ispure-x ← mbIsPureE x
          just ((require x ispure-x refl))
        toLMonadNBS Γ (U.stmntNBS (U.NBS-deposit! x x₁)) = do
          ispure-x₁ ← mbIsPureE x₁
          just ((action tt (everyIsDishonest x) (depositA x₁ ispure-x₁) refl))
        toLMonadNBS Γ (U.stmntNBS (U.NBS-withdraw! x x₁)) = do
          ispure-x₁ ← mbIsPureE x₁
          just ((action tt (everyIsDishonest x) (withdrawA x₁ ispure-x₁) refl))

        toLMonadNBS Γ (U.stmntNBS (U.NBS-publishVal! x x₁)) = nothing
        toLMonadNBS Γ (U.exprNBS x) = do
          e ←  (toLMonadE _ (U.Τ? x) x) 
          just (next e (pure (U.lit (record { gType = Unitᵍ ; gValue = tt })) tt))


        toLMonadE Γ Τ (U.var t x) = just (pure (U.var t x) tt)
        
        toLMonadE Γ Τ (U.body (U.bodyR [] expr₁)) = toLMonadE Γ Τ expr₁
        toLMonadE Γ Τ (U.body (U.bodyR (U.bindingS (U.BS-let nothing name type x) ∷ stmnts₁) expr₁)) = 
          do e' ← toLMonadE _ type x
             es' ← toLMonadE _ _ (U.body (U.bodyR stmnts₁ expr₁ ))
             just (bind {x = name} e' es')

        toLMonadE Γ Τ (U.body (U.bodyR (U.bindingS (U.BS-let (just x₁) name type x) ∷ stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (U.body (U.bodyR (U.bindingS (U.BS-publish! p _ x) ∷ stmnts₁) expr₁)) = nothing
        toLMonadE Γ Τ (U.body (U.bodyR (U.nonBindingS x ∷ stmnts₁) expr₁)) =
          do e' ← toLMonadNBS _ x
             es' ← toLMonadE _ _ (U.body (U.bodyR stmnts₁ expr₁ ))
             just (next e' es')
        
        toLMonadE Γ Τ (U.lit x) = just (pure (U.lit x) tt)
        toLMonadE Γ Τ (x U.$' x₁) = just (pure (x U.$' x₁) tt)
        toLMonadE Γ Τ (U.input _ x) = nothing
        toLMonadE Γ Τ (U.sign x) = nothing
        toLMonadE Γ Τ (U.receivePublished t x) = just (expectPub _ x)
        toLMonadE Γ Τ (U.if e then e₁ else e₂) = do
          ispure-e ← mbIsPureE e
          e₁' ← toLMonadE Γ _ e₁
          e₂' ← toLMonadE Γ _ e₂
          just (branch e ispure-e e₁' e₂')

        toLMonad : ∀ Γ → List (U.Stmnt) → Maybe (ULMonad Unit Γ Unitᵍ)
        toLMonad Γ x = toLMonadE Γ Unitᵍ (U.body (U.bodyR x (U.lit (record { gType = Unitᵍ ; gValue = _ }))))



module ToLurkCF (ptpsIds : List (String)) (prms : _) (uniquePrms : _) (uniquePtpnts : _) where

  open import Glow.Simple.ASTDef 


  open AST-String one 

  open MonadicControlFlow.MonadicControlFlowUP {String} {builtIns = Basic-BuiltIns} (ptpsIds) prms uniquePrms (uniquePtpnts) public

  module L = LurkAST List String Unit

  bi-renderer : (Τ : GType) → BI Τ → String
  bi-renderer Τ (AST.bi' bIndex) = bIndex


  module T = Translate.unsafe {String} {builtIns = Basic-BuiltIns} {ptpsIds} {prms} {uniquePrms} {uniquePtpnts}
                tt "cons" bi-renderer

  module LH = LurkAST AList String Unit


  PID→LExpr : PID → L.Expr
  PID→LExpr z = L.ExFieldElem _ (DishonestParticipantId→ℕ ih'' z)

  -- TODO : merge those modules (may  require better parametrisation of LMonad')
  module safeAST where
    module T' = T.safeAST



    toLurkGlowcode : ∀ {Γ Τ} → LMonad ℕ Γ Τ → L.Expr
    toLurkGlowcode (action x x₁ x₂ Τ≡Unitᵍ) =
       T.appS "action" (L.ExFieldElem _ x ∷ PID→LExpr x₁ ∷ [ h x₂ ])
       where
         h : Action _ → L.Expr
         h (withdrawA x _) = T.appS "withdraw" [ T'.translateE x ]
         h (depositA x _) = T.appS "deposit" [ T'.translateE x ]

    toLurkGlowcode (expectPub x x₁) = T.appS "publish" (L.ExFieldElem _ x ∷ [ PID→LExpr x₁ ])
    toLurkGlowcode (next x x₁) = T.appS "next" (toLurkGlowcode x ∷ [ toLurkGlowcode x₁ ])
    toLurkGlowcode (bind {x = s} x x₁) =
       T.appS "bind" (toLurkGlowcode x ∷ [ h ])
       where
         h : L.Expr
         h = L.ExLambda _
                 [ L.SymbolC s ]
                 (toLurkGlowcode x₁)
    toLurkGlowcode (pure x _) = T.appS "pure" [ T'.translateE x ]
    toLurkGlowcode (branch x _ x₁ x₂) = L.ExIf _ (T'.translateE x) (toLurkGlowcode x₁) (toLurkGlowcode x₂)
    toLurkGlowcode (require e x x₁) = T.appS "require" ([ T'.translateE e ])



    fixListImp : L.Expr →  LH.Expr
    fixListImp = LurkASTchangeListImp.mapLiImp List AList map-List toAList

    toLurkGlowcode' : ∀ {Γ Τ} → LMonad ℕ Γ Τ → LH.Expr
    toLurkGlowcode' = fixListImp ∘ T.addSignature ∘ toLurkGlowcode

  module unsafeAST where
    module T' = T.unsafeAST

    module U = Unsafe


    toLurkGlowcode : ∀ {Γ Τ} → ULMonad ℕ Γ Τ → L.Expr
    toLurkGlowcode (action x x₁ x₂ Τ≡Unitᵍ) =
       T.appS "action" (L.ExFieldElem _ x ∷ PID→LExpr x₁ ∷ [ h x₂ ])
       where
         h : UAction _ → L.Expr
         h (withdrawA x _) = T.appS "withdraw" [ T'.translateE x ]
         h (depositA x _) = T.appS "deposit" [ T'.translateE x ]

    toLurkGlowcode (expectPub x x₁) = T.appS "publish" (L.ExFieldElem _ x ∷ [ PID→LExpr x₁ ])
    toLurkGlowcode (next x x₁) = T.appS "next" (toLurkGlowcode x ∷ [ toLurkGlowcode x₁ ])
    toLurkGlowcode (bind {x = s} x x₁) =
       T.appS "bind" (toLurkGlowcode x ∷ [ h ])
       where
         h : L.Expr
         h = L.ExLambda _
                 [ L.SymbolC s ]
                 (toLurkGlowcode x₁)
    toLurkGlowcode (pure x _) = T.appS "pure" [ T'.translateE x ]
    toLurkGlowcode (branch x _ x₁ x₂) = L.ExIf _ (T'.translateE x) (toLurkGlowcode x₁) (toLurkGlowcode x₂)
    toLurkGlowcode (require e x x₁) = T.appS "require" ([ T'.translateE e ])



    fixListImp : L.Expr →  LH.Expr
    fixListImp = LurkASTchangeListImp.mapLiImp List AList map-List toAList

    toLurkGlowcode' : ULMonad ℕ (con [] nothing) Unitᵍ → LH.Expr
    toLurkGlowcode' = fixListImp ∘ T.addSignature ∘ toLurkGlowcode



module examplesAB where

  open import Glow.Simple.ASTDef 


  open AST-String one 


  ptps = ("A" ∷ "B" ∷ [])
  uPtps = (toWitness {Q = UniqueByDec≡ proj₁ (map-List (_, false) ("A" ∷ "B" ∷ []))} tt)


  module noParams where
    open ToLurkCF ptps [] tt* uPtps 

    open LH public

    open import Cubical.Data.Unit renaming (tt to TU)

    idA idB : PID
    idA = pId "A" {toWitnessDP ((IsDishonestParticipantId {ptps''} "A")) tt}
    idB = pId "B" {toWitnessDP ((IsDishonestParticipantId {ptps''} "B")) tt}


    testLM : LMonad ℕ (con [] nothing) Unitᵍ 
    testLM = bind {x = "z"} {Τ' = Nat} (next (next (action 0 idA (withdrawA < 2 > tt ) refl)
                        (action 1 idB (depositA < 2 > tt ) refl))
                    (pure < 4 > tt))
                   (bind {x = "zz"} {Τ' = Nat} (expectPub 2 idA)
                       (action 3 idB (withdrawA < 3 > tt) refl))

    testLGC : LH.Expr
    testLGC = (safeAST.toLurkGlowcode' testLM)


    testOutput : {!!}
    testOutput = {! testLGC!}


  module coinFlip where
    open ToLurkCF ptps ( (AST.iwt "wagerAmount" Nat ∷ AST.iwt "escrowAmount" Nat ∷ []))
                    (toWitness {Q = UniqueByDec≡ IdentifierWithType.name ((AST.iwt "wagerAmount" Nat ∷ AST.iwt "escrowAmount" Nat ∷ []))} tt) uPtps 

    open LH public

    open import Cubical.Data.Unit renaming (tt to TU)


    idA idB : PID
    idA = pId "A" {toWitnessDP ((IsDishonestParticipantId {ptps''} "A")) tt}
    idB = pId "B" {toWitnessDP ((IsDishonestParticipantId {ptps''} "B")) tt}

    testCoinFlip : Statements ih'' (con [] nothing)
    testCoinFlip = coinFlipConsensusCode


    testOutput : LH.Expr
    testOutput = fromJust (map-Maybe (safeAST.toLurkGlowcode' ∘ labelStates') (tryTranslation.toLMonad _ coinFlipConsensusCode))

    testOutput' : LH.Expr
    testOutput' = fromJust (map-Maybe (unsafeAST.toLurkGlowcode' ∘ labelStates') (tryTranslationUnsafe.toLMonad _ (ToUnsafe.stmntsF _ coinFlipConsensusCode)))

    zz : LabeledValue[ "coinFlip" ][ testOutput ] 
    zz = {!mkLV!}

    zz' : LabeledValue[ "coinFlipU" ][ testOutput' ]
    zz' = {!addLabel testOutput'!}

  module coinFlipProj where
    open ToLurkCF ("A" ∷ "B" ∷ []) (AST.iwt "wagerAmount" Nat ∷ AST.iwt "escrowAmount" Nat ∷ [])
                   ( (toWitness {Q = UniqueByDec≡ IdentifierWithType.name ((AST.iwt "wagerAmount" Nat ∷ AST.iwt "escrowAmount" Nat ∷ []))} tt))
                   uPtps

    open LH public

    open import Glow.Simple.Lurk.Container

    open import Cubical.Data.Unit renaming (tt to TU)

    module U = Unsafe

    module UPO = UnsafeProjectOut

    testCoinFlipProjected : List (U.Stmnt ih'')
    testCoinFlipProjected = fixProofs' (List ∘ U.Stmnt) (
        UPO.projectOut (pId "B" {toWitnessDP (IsHonestParticipantId {("A" , false) ∷ ("B" , true) ∷ []} "B") tt}) 
         (UPO.projectOut (pId "A" {toWitnessDP (IsHonestParticipantId {("A" , true) ∷ ("B" , true) ∷ []} "A") tt})
            (ToUnsafe.stmntsF (interactionHead (("A" , honest) ∷ ("B" , honest) ∷ []) ((iwt "wagerAmount" Nat) ∷ (iwt "escrowAmount" Nat) ∷ [])) coinFlipCode)))

    testOutput' : LH.Expr
    testOutput' = fromJust (map-Maybe (unsafeAST.toLurkGlowcode' ∘ labelStates') (tryTranslationUnsafe.toLMonad _ (testCoinFlipProjected)))

    putIntoContainer : LH.Expr → GLContainer
    putIntoContainer x = GLContainerC (toAList ptps) x (toAList (("wagerAmount" , "Nat") ∷ [ "escrowAmount" , "Nat" ]))
    
    zz : LabeledValue[ "coinFlipP" ][ putIntoContainer testOutput' ]
    zz = {!mkLV!}

    coinFlipTest : GLInteractionTest
    coinFlipTest =
      GLInteractionTestC
        (putIntoContainer testOutput')
        ((toAList (("wagerAmount" , GLNat 10) ∷ [ "escrowAmount" , GLNat 2 ])))
        (toAList (CallC 1 0 (Publish (DigestOf (GLNat 7)))
                ∷ CallC 2 0 (Deposit 12)
                ∷ []))

    zzT : LabeledValue[ "coinFlipPTest" ][ coinFlipTest ]
    zzT = {!mkLV!}