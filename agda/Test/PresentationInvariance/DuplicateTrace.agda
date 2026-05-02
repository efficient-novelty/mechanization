{-# OPTIONS --cubical --safe --guardedness #-}

module Test.PresentationInvariance.DuplicateTrace where

open import Cubical.Foundations.Prelude using (Level; _≡_)
open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (requiresPrimitive)
open import Metatheory.CanonicalTelescope
open import Metatheory.TraceCostNormalForm
open import Metatheory.PresentationEquivalence
open import Metatheory.MuInvariance

duplicate-derived-delete-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
duplicate-derived-delete-generator = duplicateDerivedDelete

duplicate-derived-delete-preserves-mu :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
duplicate-derived-delete-preserves-mu =
  derived-field-deletion-preserves-mu

primitive-fields-remain-essential :
  {ℓ : Level} {k : Nat}
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  (φ : FieldIndex (traceTelescope Γ)) →
  primitiveCost (traceField Γ φ) ≡ requiresPrimitive →
  RequiresPrimitive Γ φ
primitive-fields-remain-essential =
  requires-primitive-field-essential
