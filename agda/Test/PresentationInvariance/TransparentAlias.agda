{-# OPTIONS --cubical --safe --guardedness #-}

module Test.PresentationInvariance.TransparentAlias where

open import Cubical.Foundations.Prelude using (Level; _≡_)
open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.TraceCostNormalForm
open import Metatheory.PresentationEquivalence
open import Metatheory.MuInvariance

transparent-alias-insert-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
transparent-alias-insert-generator = transparentAliasInsert

transparent-alias-delete-generator :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  TraceSupportPreserved Γ Δ →
  PrimitiveCostPreserved Γ Δ →
  PresentationStep Γ Δ
transparent-alias-delete-generator = transparentAliasDelete

transparent-alias-preserves-trace-support :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  traceFieldCount Γ ≡ traceFieldCount Δ
transparent-alias-preserves-trace-support =
  presentation-step-preserves-trace-support

transparent-alias-preserves-mu :
  {ℓ : Level} {k : Nat}
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
transparent-alias-preserves-mu = mu-preserved-by-presentation-step
