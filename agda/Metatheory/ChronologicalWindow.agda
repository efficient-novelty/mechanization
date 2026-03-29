{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.ChronologicalWindow where

open import Cubical.Foundations.Prelude
open import Cubical.Data.Empty.Base using (⊥)

open import Core.Nat renaming (ℕ to Nat)
open import Core.Sequence using (Vec; []; _∷_)
open import Metatheory.Obligations
  using ( Not
        ; Fin
        ; fzero
        ; fsuc
        ; mapVec
        ; injectFinLeft
        ; FactorsThroughWindow
        ; ChronologicalWindowAt
        )
open import Metatheory.KanSubsumption
  using ( HornCandidate
        ; horn-candidate
        ; StructuralObligation
        ; depth2-boundary
        ; extend-remote-layer
        ; allFin
        ; structural-horn-language
        )

data ⊤ : Type where
  tt : ⊤

⊥-elim : {A : Type} → ⊥ → A
⊥-elim ()

head : {A : Type} {n : Nat} → Vec A (suc n) → A
head (x ∷ xs) = x

tail : {A : Type} {n : Nat} → Vec A (suc n) → Vec A n
tail (x ∷ xs) = xs

second : {A : Type} {n : Nat} → Vec A (suc (suc n)) → A
second xs = head (tail xs)

Fin-head-code : {n : Nat} → Fin (suc n) → Type
Fin-head-code fzero = ⊤
Fin-head-code (fsuc i) = ⊥

fzero≠fsuc : {n : Nat} {i : Fin n} → fzero ≡ fsuc i → ⊥
fzero≠fsuc p = subst Fin-head-code p tt

Nat-zero-code : Nat → Type
Nat-zero-code zero = ⊤
Nat-zero-code (suc n) = ⊥

zero≠suc : {n : Nat} → zero ≡ suc n → ⊥
zero≠suc p = subst Nat-zero-code p tt

no-one-layer-factorization :
  (localLayers : Vec (Fin (suc zero)) (suc (suc zero))) →
  mapVec (injectFinLeft (suc zero) (suc zero)) localLayers ≡
    allFin (suc (suc zero)) →
  ⊥
no-one-layer-factorization (fzero ∷ fzero ∷ []) q =
  fzero≠fsuc (cong second q)

primitive-obligations-factor-through-last-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  ChronologicalWindowAt (structural-horn-language u u0) (suc (suc zero))
primitive-obligations-factor-through-last-two u u0 = record
  { primitive-support-factors = λ where
      horn-candidate zero depth2-boundary costProof → record
        { localLayers = allFin (suc (suc zero))
        ; factorization = refl
        }
      horn-candidate (suc offset) (extend-remote-layer boundary fiber) costProof →
        ⊥-elim (zero≠suc costProof)
  }

one-layer-window-insufficient :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Not (ChronologicalWindowAt (structural-horn-language u u0) (suc zero))
one-layer-window-insufficient u u0 window =
  no-one-layer-factorization
    (FactorsThroughWindow.localLayers factors)
    (FactorsThroughWindow.factorization factors)
  where
    factors =
      ChronologicalWindowAt.primitive-support-factors window
        horn-candidate (suc zero) depth2-boundary refl

two-layer-chronological-window :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  ChronologicalWindowAt (structural-horn-language u u0) (suc (suc zero))
two-layer-chronological-window = primitive-obligations-factor-through-last-two

chronological-markov-blanket :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  ChronologicalWindowAt (structural-horn-language u u0) (suc (suc zero))
chronological-markov-blanket = two-layer-chronological-window
