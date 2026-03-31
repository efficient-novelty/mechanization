{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.TwoDFoundations where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)

open import Core.Nat renaming (ℕ to Nat)
open import Core.AffineRecurrence
  using ( payload2
        ; Delta-bootstrap
        ; Delta-bootstrap-step
        ; U-bootstrap
        ; U-bootstrap-is-fibonacci
        )
open import Metatheory.Obligations
  using ( Not
        ; ObligationLanguage
        ; PrimitiveEliminatesAbove
        ; HasPrimitiveDepth
        ; StabilizesAt
        ; HasCoherenceDepth
        ; HasChronologicalWindowSize
        ; requiresPrimitive
        ; requiresPrimitive≠derived
        ; _<_
        ; s≤s
        )
open import Metatheory.KanSubsumption
  using ( horn-candidate
        ; StructuralObligation
        ; depth2-boundary
        ; telescopic-remote-comparison-derived
        ; structural-horn-language
        )
open import Metatheory.ExactDepth
  using ( no-structural-obligation-at-one
        ; structural-coherence-depth-exactly-two
        ; structural-chronological-window-size-exactly-two
        ; cubical-binary-sealing-obstruction
        ; cubical-triangle-identity-corollary
        )
open import Metatheory.AdjunctionBarrier
  using ( ExplicitBinarySealingObstruction
        ; TriangleIdentityCorollary
        )

inverseIso :
  {ℓ ℓ' : Level} {A : Type ℓ} {B : Type ℓ'} →
  Iso A B → Iso B A
inverseIso α = record
  { fun = Iso.inv α
  ; inv = Iso.fun α
  ; rightInv = Iso.leftInv α
  ; leftInv = Iso.rightInv α
  }

composeIso :
  {ℓA ℓB ℓC : Level}
  {A : Type ℓA} {B : Type ℓB} {C : Type ℓC} →
  Iso A B → Iso B C → Iso A C
composeIso α β = record
  { fun = λ a → Iso.fun β (Iso.fun α a)
  ; inv = λ c → Iso.inv α (Iso.inv β c)
  ; rightInv = λ c →
      cong (Iso.fun β) (Iso.rightInv α (Iso.inv β c))
      ∙ Iso.rightInv β c
  ; leftInv = λ a →
      cong (Iso.inv α) (Iso.leftInv β (Iso.fun α a))
      ∙ Iso.leftInv α a
  }

not-less-than-two-abstract :
  (n : Nat) → Not ((suc (suc n)) < 2)
not-less-than-two-abstract n (s≤s (s≤s ()))

record PrimitiveWindow2DFoundation {ℓC ℓO : Level}
  (L : ObligationLanguage ℓC ℓO) : Type (ℓ-max ℓC ℓO) where
  open ObligationLanguage L

  field
    primitiveBinaryWitness    : Candidate
    primitiveBinaryObligation : O primitiveBinaryWitness 2
    primitiveBinaryCost       :
      primitiveCost primitiveBinaryObligation ≡ requiresPrimitive
    eliminatesAboveTwo        : PrimitiveEliminatesAbove L 2
    twoLayerWindow            : HasChronologicalWindowSize L 2

open PrimitiveWindow2DFoundation public

record FullyCoupled2DFoundation {ℓC ℓO : Level}
  (L : ObligationLanguage ℓC ℓO) : Type (ℓ-max ℓC ℓO) where
  open ObligationLanguage L

  field
    primitiveWindowFoundation : PrimitiveWindow2DFoundation L
    binaryWitness             : Candidate
    binaryNotUnary            : Not (Iso (O binaryWitness 2) (O binaryWitness 1))
    stabilizesAtTwo           : StabilizesAt L 2

open FullyCoupled2DFoundation public

module _ {ℓC ℓO : Level} {L : ObligationLanguage ℓC ℓO} where

  open ObligationLanguage L

  no-primitive-depth-one :
    (F : PrimitiveWindow2DFoundation L) →
    Not (PrimitiveEliminatesAbove L 1)
  no-primitive-depth-one F elimination =
    requiresPrimitive≠derived
      (sym (primitiveBinaryCost F) ∙
        PrimitiveEliminatesAbove.eliminate elimination
          (primitiveBinaryWitness F) zero (primitiveBinaryObligation F))

  no-primitive-depth-zero :
    (F : PrimitiveWindow2DFoundation L) →
    Not (PrimitiveEliminatesAbove L 0)
  no-primitive-depth-zero F elimination =
    requiresPrimitive≠derived
      (sym (primitiveBinaryCost F) ∙
        PrimitiveEliminatesAbove.eliminate elimination
          (primitiveBinaryWitness F) (suc zero)
          (primitiveBinaryObligation F))

  primitive-depth-two-law-for-2d-foundations :
    (F : PrimitiveWindow2DFoundation L) →
    HasPrimitiveDepth L 2
  primitive-depth-two-law-for-2d-foundations F = record
    { eliminatesAbove = eliminatesAboveTwo F
    ; leastPrimitiveDepth = λ where
        zero _ → no-primitive-depth-zero F
        (suc zero) _ → no-primitive-depth-one F
        (suc (suc d)) lt → λ _ → not-less-than-two-abstract d lt
    }

  no-unary-collapse :
    (F : FullyCoupled2DFoundation L) →
    Not (StabilizesAt L 1)
  no-unary-collapse F stabilization =
    binaryNotUnary F
      (StabilizesAt.stabilize stabilization (binaryWitness F) 1)

  no-zero-collapse :
    (F : FullyCoupled2DFoundation L) →
    Not (StabilizesAt L 0)
  no-zero-collapse F stabilization =
    binaryNotUnary F
      (composeIso
        (StabilizesAt.stabilize stabilization (binaryWitness F) 2)
        (inverseIso
          (StabilizesAt.stabilize stabilization (binaryWitness F) 1)))

  depth-two-law-for-2d-foundations :
    (F : FullyCoupled2DFoundation L) →
    HasCoherenceDepth L 2
  depth-two-law-for-2d-foundations F = record
    { stabilizesAt = stabilizesAtTwo F
    ; leastDepth = λ where
        zero _ → no-zero-collapse F
        (suc zero) _ → no-unary-collapse F
        (suc (suc d)) lt → λ _ → not-less-than-two-abstract d lt
    }

  chronological-window-size-two-for-2d-foundations :
    (F : PrimitiveWindow2DFoundation L) →
    HasChronologicalWindowSize L 2
  chronological-window-size-two-for-2d-foundations = twoLayerWindow

record ConstantPayloadDepthTwoLaw (c : Nat) : Type where
  field
    integration-latency-step :
      (n : Nat) →
      Delta-bootstrap c (suc (suc n))
        ≡ Delta-bootstrap c (suc n) + Delta-bootstrap c n + payload2 c
    shifted-sequence-fibonacci :
      (n : Nat) →
      U-bootstrap c (suc (suc n))
        ≡ U-bootstrap c (suc n) + U-bootstrap c n

open ConstantPayloadDepthTwoLaw public

constant-payload-depth-two-law :
  (c : Nat) → ConstantPayloadDepthTwoLaw c
constant-payload-depth-two-law c = record
  { integration-latency-step = Delta-bootstrap-step c
  ; shifted-sequence-fibonacci = U-bootstrap-is-fibonacci c
  }

binary-obligation-set-not-unary :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Not (Iso (StructuralObligation u u0 2) (StructuralObligation u u0 1))
binary-obligation-set-not-unary u u0 α =
  no-structural-obligation-at-one (Iso.fun α depth2-boundary)

structural-primitive-eliminates-above-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  PrimitiveEliminatesAbove (structural-horn-language u u0) 2
structural-primitive-eliminates-above-two u u0 = record
  { eliminate = λ where
      horn-candidate offset o →
        telescopic-remote-comparison-derived u u0 offset o
  }

cubical-binary-coherence-source : ExplicitBinarySealingObstruction
cubical-binary-coherence-source = cubical-binary-sealing-obstruction

cubical-triangle-coherence-source : TriangleIdentityCorollary
cubical-triangle-coherence-source = cubical-triangle-identity-corollary

cubical-primitive-window-2d-foundation :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  PrimitiveWindow2DFoundation (structural-horn-language u u0)
cubical-primitive-window-2d-foundation u u0 = record
  { primitiveBinaryWitness = horn-candidate
  ; primitiveBinaryObligation = depth2-boundary
  ; primitiveBinaryCost = refl
  ; eliminatesAboveTwo = structural-primitive-eliminates-above-two u u0
  ; twoLayerWindow = structural-chronological-window-size-exactly-two u u0
  }

cubical-2d-foundation :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  FullyCoupled2DFoundation (structural-horn-language u u0)
cubical-2d-foundation u u0 = record
  { primitiveWindowFoundation =
      cubical-primitive-window-2d-foundation u u0
  ; binaryWitness = horn-candidate
  ; binaryNotUnary = binary-obligation-set-not-unary u u0
  ; stabilizesAtTwo =
      HasCoherenceDepth.stabilizesAt
        (structural-coherence-depth-exactly-two u u0)
  }

cubical-primitive-depth-two-law-for-2d-foundations :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  HasPrimitiveDepth (structural-horn-language u u0) 2
cubical-primitive-depth-two-law-for-2d-foundations u u0 =
  primitive-depth-two-law-for-2d-foundations
    (cubical-primitive-window-2d-foundation u u0)

cubical-depth-two-law-for-2d-foundations :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  HasCoherenceDepth (structural-horn-language u u0) 2
cubical-depth-two-law-for-2d-foundations u u0 =
  depth-two-law-for-2d-foundations (cubical-2d-foundation u u0)

cubical-chronological-window-size-two-for-2d-foundations :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  HasChronologicalWindowSize (structural-horn-language u u0) 2
cubical-chronological-window-size-two-for-2d-foundations u u0 =
  chronological-window-size-two-for-2d-foundations
    (cubical-primitive-window-2d-foundation u u0)
