{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.MuInvariance where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations
  using ( Fin
        ; Not
        ; ObligationLanguage
        ; derived
        ; requiresPrimitive
        ; requiresPrimitive≠derived
        )
open import Metatheory.CanonicalTelescope
open import Metatheory.TraceCostNormalForm
open import Metatheory.PresentationEquivalence
open import Metatheory.ComputationalReplacement as Replacement

private
  variable
    ℓ : Level
    k : Nat

TransparentlyGenerated :
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  FieldIndex (traceTelescope Γ) →
  Type
TransparentlyGenerated Γ φ =
  primitiveCost (traceField Γ φ) ≡ derived

RequiresPrimitive :
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  FieldIndex (traceTelescope Γ) →
  Type
RequiresPrimitive Γ φ =
  Not (TransparentlyGenerated Γ φ)

mu-preserved-by-presentation-step :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
mu-preserved-by-presentation-step =
  presentation-step-preserves-primitive-cost

mu-invariant-under-presentation-equivalence :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationEquivalent Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
mu-invariant-under-presentation-equivalence =
  presentation-equivalence-preserves-primitive-cost

derived-field-deletion-preserves-mu :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
derived-field-deletion-preserves-mu =
  mu-preserved-by-presentation-step

requires-primitive-field-essential :
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  (φ : FieldIndex (traceTelescope Γ)) →
  primitiveCost (traceField Γ φ) ≡ requiresPrimitive →
  RequiresPrimitive Γ φ
requires-primitive-field-essential Γ φ primitiveWitness derivedWitness =
  requiresPrimitive≠derived
    (Cubical.Foundations.Prelude.sym primitiveWitness ∙ derivedWitness)

computational-replacement-preserves-mu :
  {ℓC ℓO ℓN ℓP : Level}
  {L : ObligationLanguage ℓC ℓO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : Replacement.CanonicalTraceSignature
         {lC = ℓC} {lO = ℓO} {lN = ℓN} L X k) →
  (P Q : Replacement.TracePresentation {lP = ℓP} S) →
  (φ : Replacement.PresentedTraceField P) →
  Replacement.presentedPrimitiveCost S P φ ≡ derived →
  Replacement.ComputationalReplacementResult S P Q φ
computational-replacement-preserves-mu =
  Replacement.computational-replacement-preserves-canonical-presentation
