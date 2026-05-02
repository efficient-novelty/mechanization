{-# OPTIONS --cubical --safe --guardedness #-}

module Test.PresentationInvariance.SplitShell where

open import Cubical.Foundations.Prelude using (Level; _≡_)
open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.TraceCostNormalForm
open import Metatheory.PresentationEquivalence
open import Metatheory.MuInvariance

split-shell-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
split-shell-generator = splitRecord

split-shell-preserves-trace-support :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  traceFieldCount Γ ≡ traceFieldCount Δ
split-shell-preserves-trace-support =
  presentation-step-preserves-trace-support

split-shell-preserves-mu :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
split-shell-preserves-mu = mu-preserved-by-presentation-step
