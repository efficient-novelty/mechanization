{-# OPTIONS --cubical --safe --guardedness #-}

module Test.PresentationInvariance.CurryUncurry where

open import Cubical.Foundations.Prelude using (Level; _≡_)
open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.TraceCostNormalForm
open import Metatheory.PresentationEquivalence
open import Metatheory.MuInvariance

step-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
step-generator = curryPi

curry-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
curry-generator = curryPi

uncurry-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
uncurry-generator = uncurryPi

curry-uncurry-preserves-trace-support :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  traceFieldCount Γ ≡ traceFieldCount Δ
curry-uncurry-preserves-trace-support =
  presentation-step-preserves-trace-support

curry-uncurry-preserves-mu :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
curry-uncurry-preserves-mu = mu-preserved-by-presentation-step
