{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.TraceCostNormalForm where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations
  using ( Fin
        ; HistoricalSupport
        ; PrimitiveCost
        ; derived
        ; requiresPrimitive
        )
open import Metatheory.CanonicalTelescope

private
  variable
    ℓ : Level
    k : Nat

record TraceCostField (k : Nat) : Type where
  constructor mkTraceCostField
  field
    support              : HistoricalSupport k
    arity                : Nat
    arityMatchesSupport  : arity ≡ HistoricalSupport.arity support
    primitiveCost        : PrimitiveCost

open TraceCostField public

record TraceCostSubtelescope
  {ℓ : Level} {k : Nat}
  (Γ : CanonicalTelescope ℓ)
  (traceField : FieldIndex Γ → TraceCostField k) :
  Type (ℓ-suc ℓ) where
  constructor mkTraceCostSubtelescope
  field
    subfieldCount : Nat
    parentField   : Fin subfieldCount → FieldIndex Γ
    subfieldAt    : Fin subfieldCount → Type ℓ

open TraceCostSubtelescope public

record CanonicalTraceCostNormalForm
  (ℓ : Level) (k : Nat) : Type (ℓ-suc ℓ) where
  constructor mkCanonicalTraceCostNormalForm
  field
    traceTelescope : CanonicalTelescope ℓ
    traceField     : FieldIndex traceTelescope → TraceCostField k

    primitiveTraceCount :
      Nat
    primitiveTraceAt :
      Fin primitiveTraceCount → FieldIndex traceTelescope
    primitiveTraceWitness :
      (i : Fin primitiveTraceCount) →
      primitiveCost (traceField (primitiveTraceAt i)) ≡ requiresPrimitive

    derivedTraceCount :
      Nat
    derivedTraceAt :
      Fin derivedTraceCount → FieldIndex traceTelescope
    derivedTraceWitness :
      (i : Fin derivedTraceCount) →
      primitiveCost (traceField (derivedTraceAt i)) ≡ derived

open CanonicalTraceCostNormalForm public

traceFieldCount :
  CanonicalTraceCostNormalForm ℓ k → Nat
traceFieldCount Γ =
  canonical-telescope-cardinality (traceTelescope Γ)

trace-cost-normal-form-cardinality :
  CanonicalTraceCostNormalForm ℓ k → Nat
trace-cost-normal-form-cardinality = traceFieldCount

primitive-trace-subtelescope :
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  TraceCostSubtelescope (traceTelescope Γ) (traceField Γ)
primitive-trace-subtelescope Γ = record
  { subfieldCount = primitiveTraceCount Γ
  ; parentField = primitiveTraceAt Γ
  ; subfieldAt = λ i →
      CanonicalTelescope.fieldAt (traceTelescope Γ) (primitiveTraceAt Γ i)
  }

derived-trace-subtelescope :
  (Γ : CanonicalTraceCostNormalForm ℓ k) →
  TraceCostSubtelescope (traceTelescope Γ) (traceField Γ)
derived-trace-subtelescope Γ = record
  { subfieldCount = derivedTraceCount Γ
  ; parentField = derivedTraceAt Γ
  ; subfieldAt = λ i →
      CanonicalTelescope.fieldAt (traceTelescope Γ) (derivedTraceAt Γ i)
  }

mu-of-trace-cost-normal-form :
  CanonicalTraceCostNormalForm ℓ k → Nat
mu-of-trace-cost-normal-form Γ = primitiveTraceCount Γ
