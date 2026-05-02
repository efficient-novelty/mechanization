{-# OPTIONS --cubical --safe --guardedness #-}

module CaseStudies.SparseDatatype where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin; fzero; _≤_; z≤n; s≤s)
open import Metatheory.SparseDependencyRecurrence
open import CaseStudies.Common

sparse-datatype-footprint : CouplingFootprint three
sparse-datatype-footprint =
  mkCouplingFootprint
    one
    (λ i -> fzero)
    (s≤s z≤n)

sparse-datatype-layer-cost : Fin one -> Nat
sparse-datatype-layer-cost i = one

sparse-datatype-context : SparseWindowedContext
sparse-datatype-context =
  mkSparseWindowedContext
    three
    sparse-datatype-footprint
    sparse-datatype-layer-cost
    one

sparse-datatype-recurrence :
  SparseWindowedRecurrence sparse-datatype-context
sparse-datatype-recurrence =
  sparse-windowed-recurrence sparse-datatype-context

sparse-datatype-zero-or-sparse :
  ZeroOrSparseFootprint three
sparse-datatype-zero-or-sparse =
  orthogonal-extension-zero-or-sparse sparse-datatype-footprint

sparse-datatype-below-full-envelope :
  dependencyCount sparse-datatype-footprint ≤ three
sparse-datatype-below-full-envelope =
  orthogonal-extension-below-full-envelope sparse-datatype-footprint

sparse-datatype-summary : CaseStudySummary
sparse-datatype-summary =
  mkCaseStudySummary
    one
    one
    one
    zero
    zero
    zero
    Unit
    one
    sparse-local
