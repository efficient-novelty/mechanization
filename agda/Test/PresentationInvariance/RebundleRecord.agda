{-# OPTIONS --cubical --safe --guardedness #-}

module Test.PresentationInvariance.RebundleRecord where

open import Cubical.Foundations.Prelude using (Level; _≡_)
open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.TraceCostNormalForm
open import Metatheory.PresentationEquivalence
open import Metatheory.MuInvariance

rebundle-record-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
rebundle-record-generator = bundleRecord

rebundle-record-preserves-trace-support :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  traceFieldCount Γ ≡ traceFieldCount Δ
rebundle-record-preserves-trace-support =
  presentation-step-preserves-trace-support

rebundle-record-preserves-mu :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
rebundle-record-preserves-mu = mu-preserved-by-presentation-step
