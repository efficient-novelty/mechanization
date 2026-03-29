{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.UpperBound where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (StabilizesAt)
open import Metatheory.KanSubsumption
  using ( StructuralObligation
        ; HornCandidate
        ; horn-candidate
        ; HornExtensionWitness
        ; hornWitness
        ; mkHornExtensionFiber
        ; depth2-boundary
        ; extend-remote-layer
        ; structural-weaken
        ; structural-horn-language
        )

collapse-to-depth-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  StructuralObligation u u0 (2 + offset) →
  StructuralObligation u u0 2
collapse-to-depth-two u u0 zero boundary =
  boundary
collapse-to-depth-two u u0 (suc offset) (extend-remote-layer boundary _) =
  collapse-to-depth-two u u0 offset boundary

extend-from-depth-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  StructuralObligation u u0 2 →
  StructuralObligation u u0 (2 + offset)
extend-from-depth-two u u0 zero boundary =
  boundary
extend-from-depth-two u u0 (suc offset) boundary =
  structural-weaken u u0 (2 + offset)
    (extend-from-depth-two u u0 offset boundary)

collapse-after-extend :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  (boundary : StructuralObligation u u0 2) →
  collapse-to-depth-two u u0 offset
    (extend-from-depth-two u u0 offset boundary)
    ≡ boundary
collapse-after-extend u u0 zero boundary =
  refl
collapse-after-extend u u0 (suc offset) boundary =
  collapse-after-extend u u0 offset boundary

extend-after-collapse :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  (o : StructuralObligation u u0 (2 + offset)) →
  extend-from-depth-two u u0 offset
    (collapse-to-depth-two u u0 offset o)
    ≡ o
extend-after-collapse u u0 zero depth2-boundary =
  refl
extend-after-collapse u u0 (suc offset)
  (extend-remote-layer boundary (mkHornExtensionFiber hornWitness)) =
  cong
    (λ b → extend-remote-layer b (mkHornExtensionFiber hornWitness))
    (extend-after-collapse u u0 offset boundary)

structural-obligation-set-equivalence :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  Iso (StructuralObligation u u0 (2 + offset))
      (StructuralObligation u u0 2)
structural-obligation-set-equivalence u u0 offset = record
  { fun = collapse-to-depth-two u u0 offset
  ; inv = extend-from-depth-two u u0 offset
  ; rightInv = collapse-after-extend u u0 offset
  ; leftInv = extend-after-collapse u u0 offset
  }

structural-stabilizes-at-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  StabilizesAt (structural-horn-language u u0) 2
structural-stabilizes-at-two u u0 = record
  { stabilize = λ where
      horn-candidate offset →
        structural-obligation-set-equivalence u u0 offset
  }
