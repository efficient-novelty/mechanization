{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.ExactDepth where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)
open import Cubical.Data.Empty.Base using (⊥)

open import Core.Nat renaming (ℕ to Nat)
open import Core.Sequence using (Vec; _∷_)
open import Metatheory.Obligations
  using ( Not
        ; Fin
        ; FactorsThroughWindow
        ; ChronologicalWindowAt
        ; StabilizesAt
        ; HasCoherenceDepth
        ; HasChronologicalWindowSize
        ; _<_
        ; s≤s
        )
open import Metatheory.KanSubsumption
  using ( horn-candidate
        ; StructuralObligation
        ; depth2-boundary
        ; structural-horn-language
        )
open import Metatheory.UpperBound using (structural-stabilizes-at-two)
open import Metatheory.ChronologicalWindow
  using ( one-layer-window-insufficient
        ; two-layer-chronological-window
        )
open import Metatheory.AdjunctionBarrier
  using ( ExplicitBinarySealingObstruction
        ; explicit-binary-sealing-obstruction
        ; TriangleIdentityCorollary
        ; triangle-identity-corollary
        )

no-structural-obligation-at-zero :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  {u : I → Partial φ A} {u0 : A [ φ ↦ u i0 ]} →
  StructuralObligation u u0 zero → ⊥
no-structural-obligation-at-zero ()

no-structural-obligation-at-one :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  {u : I → Partial φ A} {u0 : A [ φ ↦ u i0 ]} →
  StructuralObligation u u0 (suc zero) → ⊥
no-structural-obligation-at-one ()

stabilization-at-zero-impossible :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Not (StabilizesAt (structural-horn-language u u0) zero)
stabilization-at-zero-impossible u u0 stabilization =
  no-structural-obligation-at-zero
    (Iso.fun
      (StabilizesAt.stabilize stabilization horn-candidate (suc (suc zero)))
      depth2-boundary)

stabilization-at-one-impossible :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Not (StabilizesAt (structural-horn-language u u0) (suc zero))
stabilization-at-one-impossible u u0 stabilization =
  no-structural-obligation-at-one
    (Iso.fun
      (StabilizesAt.stabilize stabilization horn-candidate (suc zero))
      depth2-boundary)

no-zero-layer-factorization :
  (localLayers : Vec (Fin zero) (suc (suc zero))) → ⊥
no-zero-layer-factorization (() ∷ _)

window-at-zero-impossible :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Not (ChronologicalWindowAt (structural-horn-language u u0) zero)
window-at-zero-impossible u u0 window =
  no-zero-layer-factorization (FactorsThroughWindow.localLayers factors)
  where
    factors =
      ChronologicalWindowAt.primitive-support-factors window
        horn-candidate (suc (suc zero)) depth2-boundary refl

not-less-than-two :
  (n : Nat) → Not ((suc (suc n)) < (suc (suc zero)))
not-less-than-two n (s≤s (s≤s ()))

structural-coherence-depth-exactly-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  HasCoherenceDepth (structural-horn-language u u0) (suc (suc zero))
structural-coherence-depth-exactly-two u u0 = record
  { stabilizesAt = structural-stabilizes-at-two u u0
  ; leastDepth = λ where
      zero _ → stabilization-at-zero-impossible u u0
      (suc zero) _ → stabilization-at-one-impossible u u0
      (suc (suc d)) lt → λ _ → not-less-than-two d lt
  }

structural-chronological-window-size-exactly-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  HasChronologicalWindowSize (structural-horn-language u u0) (suc (suc zero))
structural-chronological-window-size-exactly-two u u0 = record
  { windowAt = two-layer-chronological-window u u0
  ; leastWindow = λ where
      zero _ → window-at-zero-impossible u u0
      (suc zero) _ → one-layer-window-insufficient u u0
      (suc (suc d)) lt → λ _ → not-less-than-two d lt
  }

cubical-binary-sealing-obstruction : ExplicitBinarySealingObstruction
cubical-binary-sealing-obstruction = explicit-binary-sealing-obstruction

cubical-triangle-identity-corollary : TriangleIdentityCorollary
cubical-triangle-identity-corollary = triangle-identity-corollary

cubical-coherence-depth-exactly-two = structural-coherence-depth-exactly-two

cubical-chronological-window-size-exactly-two =
  structural-chronological-window-size-exactly-two
