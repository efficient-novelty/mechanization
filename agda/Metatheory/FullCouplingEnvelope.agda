{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.FullCouplingEnvelope where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin)
open import Metatheory.SparseDependencyRecurrence
  using ( CouplingFootprint
        ; SparseWindowedContext
        ; SparseWindowedRecurrence
        ; mkCouplingFootprint
        ; mkSparseWindowedContext
        ; dependencyCount
        ; sparse-windowed-recurrence
        ; ≤-refl
        )
open import Metatheory.TwoDFoundations
  using ( ConstantPayloadDepthTwoLaw
        ; constant-payload-depth-two-law
        )

record FullCouplingEnvelope (n : Nat) : Type where
  constructor mkFullCouplingEnvelope
  field
    envelopeFootprint :
      CouplingFootprint n
    coversPreviousWindow :
      dependencyCount envelopeFootprint ≡ n

open FullCouplingEnvelope public

full-coupling-envelope : (n : Nat) → FullCouplingEnvelope n
full-coupling-envelope n = record
  { envelopeFootprint =
      mkCouplingFootprint n (λ i → i) (≤-refl n)
  ; coversPreviousWindow = refl
  }

full-coupling-sparse-context :
  (n : Nat) →
  (Fin n → Nat) →
  Nat →
  SparseWindowedContext
full-coupling-sparse-context n layerCost payload =
  mkSparseWindowedContext
    n
    (envelopeFootprint (full-coupling-envelope n))
    layerCost
    payload

full-coupling-specializes-sparse-recurrence :
  (n : Nat) →
  (layerCost : Fin n → Nat) →
  (payload : Nat) →
  SparseWindowedRecurrence
    (full-coupling-sparse-context n layerCost payload)
full-coupling-specializes-sparse-recurrence n layerCost payload =
  sparse-windowed-recurrence
    (full-coupling-sparse-context n layerCost payload)

full-coupling-depth-two-affine-law :
  (c : Nat) → ConstantPayloadDepthTwoLaw c
full-coupling-depth-two-affine-law =
  constant-payload-depth-two-law
