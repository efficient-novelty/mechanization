{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.SparseDependencyRecurrence where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations
  using ( Fin
        ; fzero
        ; fsuc
        ; ObligationLanguage
        ; _≤_
        ; z≤n
        ; s≤s
        ; costOf
        )
open import Metatheory.InterfaceCalculus
  using ( LibraryState
        ; TransparentDevelopment
        ; transparent-integration-latency
        ; transparent-definitions-have-zero-integration-latency
        )
open import Metatheory.UniversalRecurrence
  using ( WindowedRecurrenceContext
        ; UniversalAffineRecurrence
        ; universal-affine-recurrence
        )

private
  variable
    ℓC ℓO ℓI ℓP ℓT ℓU : Level

≤-refl : (n : Nat) → n ≤ n
≤-refl zero = z≤n
≤-refl (suc n) = s≤s (≤-refl n)

sumFin : (n : Nat) → (Fin n → Nat) → Nat
sumFin zero f = zero
sumFin (suc n) f = f fzero + sumFin n (λ i → f (fsuc i))

record CouplingFootprint (n : Nat) : Type where
  constructor mkCouplingFootprint
  field
    dependencyCount       : Nat
    dependsOn             : Fin dependencyCount → Fin n
    dependencyCount≤window : dependencyCount ≤ n

open CouplingFootprint public

zero-coupling-footprint : (n : Nat) → CouplingFootprint n
zero-coupling-footprint n = record
  { dependencyCount = zero
  ; dependsOn = λ ()
  ; dependencyCount≤window = z≤n
  }

record SparseWindowedContext : Type where
  constructor mkSparseWindowedContext
  field
    windowDepth : Nat
    footprint   : CouplingFootprint windowDepth
    layerCost   : Fin (dependencyCount footprint) → Nat
    payload     : Nat

open SparseWindowedContext public

sparse-footprint-cost : SparseWindowedContext → Nat
sparse-footprint-cost C =
  sumFin (dependencyCount (footprint C)) (layerCost C)

sparse-next-latency : SparseWindowedContext → Nat
sparse-next-latency C = payload C + sparse-footprint-cost C

record SparseWindowedRecurrence (C : SparseWindowedContext) : Type where
  constructor mkSparseWindowedRecurrence
  field
    footprintCost :
      Nat
    recurrenceLaw :
      footprintCost ≡ payload C + sparse-footprint-cost C

open SparseWindowedRecurrence public

sparse-windowed-recurrence :
  (C : SparseWindowedContext) → SparseWindowedRecurrence C
sparse-windowed-recurrence C = record
  { footprintCost = sparse-next-latency C
  ; recurrenceLaw = refl
  }

universal-windowed-recurrence-as-sparse-envelope :
  {L : ObligationLanguage ℓC ℓO} {d : Nat} →
  (C : WindowedRecurrenceContext
         {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d) →
  UniversalAffineRecurrence C
universal-windowed-recurrence-as-sparse-envelope =
  universal-affine-recurrence

record TransparentZeroFootprint
  {B : LibraryState ℓI}
  (U : TransparentDevelopment {ℓU = ℓU} B) :
  Type (ℓ-suc (ℓ-max ℓI ℓU)) where
  constructor mkTransparentZeroFootprint
  field
    transparentFootprint :
      CouplingFootprint zero
    zeroLatency :
      costOf (transparent-integration-latency U) ≡ zero

open TransparentZeroFootprint public

transparent-growth-zero-footprint :
  {B : LibraryState ℓI}
  (U : TransparentDevelopment {ℓU = ℓU} B) →
  TransparentZeroFootprint U
transparent-growth-zero-footprint U = record
  { transparentFootprint = zero-coupling-footprint zero
  ; zeroLatency = transparent-definitions-have-zero-integration-latency U
  }

data ZeroOrSparseFootprint (n : Nat) : Type where
  zero-orthogonal :
    ZeroOrSparseFootprint n
  sparse-orthogonal :
    (F : CouplingFootprint n) →
    dependencyCount F ≤ n →
    ZeroOrSparseFootprint n

orthogonal-extension-zero-or-sparse :
  {n : Nat} →
  (F : CouplingFootprint n) →
  ZeroOrSparseFootprint n
orthogonal-extension-zero-or-sparse F =
  sparse-orthogonal F (dependencyCount≤window F)

orthogonal-extension-below-full-envelope :
  {n : Nat} →
  (F : CouplingFootprint n) →
  dependencyCount F ≤ n
orthogonal-extension-below-full-envelope F = dependencyCount≤window F
