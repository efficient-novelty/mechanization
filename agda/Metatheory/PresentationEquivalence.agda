{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.PresentationEquivalence where

import Cubical.Foundations.Prelude as P
open P using (Type; Level; ℓ-suc; _≡_; _∙_)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations
  using (Fin; HistoricalSupport; PrimitiveCost)
open import Metatheory.CanonicalTelescope
open import Metatheory.TraceCostNormalForm

private
  variable
    ℓ : Level
    k : Nat

record TraceSupportPreserved
  (Γ Δ : CanonicalTraceCostNormalForm ℓ k) :
  Type (ℓ-suc ℓ) where
  constructor mkTraceSupportPreserved
  field
    fieldCount-preserved :
      traceFieldCount Γ ≡ traceFieldCount Δ
    primitiveCount-preserved :
      primitiveTraceCount Γ ≡ primitiveTraceCount Δ
    derivedCount-preserved :
      derivedTraceCount Γ ≡ derivedTraceCount Δ

open TraceSupportPreserved public

record PrimitiveCostPreserved
  (Γ Δ : CanonicalTraceCostNormalForm ℓ k) : Type where
  constructor mkPrimitiveCostPreserved
  field
    primitive-cost-count-preserved :
      primitiveTraceCount Γ ≡ primitiveTraceCount Δ

open PrimitiveCostPreserved public

data PresentationStep
  {ℓ : Level} {k : Nat}
  (Γ Δ : CanonicalTraceCostNormalForm ℓ k) :
  Type (ℓ-suc ℓ) where
  reassocSigma :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  splitRecord :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  bundleRecord :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  curryPi :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  uncurryPi :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  transportField :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  transparentAliasInsert :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  transparentAliasDelete :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ
  duplicateDerivedDelete :
    TraceSupportPreserved Γ Δ →
    PrimitiveCostPreserved Γ Δ →
    PresentationStep Γ Δ

data PresentationEquivalent
  {ℓ : Level} {k : Nat} :
  CanonicalTraceCostNormalForm ℓ k →
  CanonicalTraceCostNormalForm ℓ k →
  Type (ℓ-suc ℓ) where
  refl :
    {Γ : CanonicalTraceCostNormalForm ℓ k} →
    PresentationEquivalent Γ Γ
  sym :
    {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
    PresentationEquivalent Γ Δ →
    PresentationEquivalent Δ Γ
  trans :
    {Γ Δ Θ : CanonicalTraceCostNormalForm ℓ k} →
    PresentationEquivalent Γ Δ →
    PresentationEquivalent Δ Θ →
    PresentationEquivalent Γ Θ
  step :
    {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
    PresentationStep Γ Δ →
    PresentationEquivalent Γ Δ

step-support :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  TraceSupportPreserved Γ Δ
step-support (reassocSigma support pc) = support
step-support (splitRecord support pc) = support
step-support (bundleRecord support pc) = support
step-support (curryPi support pc) = support
step-support (uncurryPi support pc) = support
step-support (transportField support pc) = support
step-support (transparentAliasInsert support pc) = support
step-support (transparentAliasDelete support pc) = support
step-support (duplicateDerivedDelete support pc) = support

step-primitive-cost :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  PrimitiveCostPreserved Γ Δ
step-primitive-cost (reassocSigma support pc) = pc
step-primitive-cost (splitRecord support pc) = pc
step-primitive-cost (bundleRecord support pc) = pc
step-primitive-cost (curryPi support pc) = pc
step-primitive-cost (uncurryPi support pc) = pc
step-primitive-cost (transportField support pc) = pc
step-primitive-cost (transparentAliasInsert support pc) = pc
step-primitive-cost (transparentAliasDelete support pc) = pc
step-primitive-cost (duplicateDerivedDelete support pc) = pc

presentation-step-preserves-trace-support :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  traceFieldCount Γ ≡ traceFieldCount Δ
presentation-step-preserves-trace-support s =
  fieldCount-preserved (step-support s)

presentation-step-preserves-primitive-cost :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationStep Γ Δ →
  primitiveTraceCount Γ ≡ primitiveTraceCount Δ
presentation-step-preserves-primitive-cost s =
  primitive-cost-count-preserved (step-primitive-cost s)

presentation-equivalence-preserves-trace-fields :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationEquivalent Γ Δ →
  traceFieldCount Γ ≡ traceFieldCount Δ
presentation-equivalence-preserves-trace-fields refl = P.refl
presentation-equivalence-preserves-trace-fields (sym eq) =
  P.sym (presentation-equivalence-preserves-trace-fields eq)
presentation-equivalence-preserves-trace-fields (trans eq₁ eq₂) =
  presentation-equivalence-preserves-trace-fields eq₁ ∙
  presentation-equivalence-preserves-trace-fields eq₂
presentation-equivalence-preserves-trace-fields (step s) =
  presentation-step-preserves-trace-support s

presentation-equivalence-preserves-primitive-cost :
  {Γ Δ : CanonicalTraceCostNormalForm ℓ k} →
  PresentationEquivalent Γ Δ →
  primitiveTraceCount Γ ≡ primitiveTraceCount Δ
presentation-equivalence-preserves-primitive-cost refl = P.refl
presentation-equivalence-preserves-primitive-cost (sym eq) =
  P.sym (presentation-equivalence-preserves-primitive-cost eq)
presentation-equivalence-preserves-primitive-cost (trans eq₁ eq₂) =
  presentation-equivalence-preserves-primitive-cost eq₁ ∙
  presentation-equivalence-preserves-primitive-cost eq₂
presentation-equivalence-preserves-primitive-cost (step s) =
  presentation-step-preserves-primitive-cost s

same-normal-form-support :
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  TraceSupportPreserved Γ Γ
same-normal-form-support Γ = record
  { fieldCount-preserved = P.refl
  ; primitiveCount-preserved = P.refl
  ; derivedCount-preserved = P.refl
  }

same-normal-form-primitive-cost :
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  PrimitiveCostPreserved Γ Γ
same-normal-form-primitive-cost Γ = record
  { primitive-cost-count-preserved = P.refl
  }
