{-# OPTIONS --cubical --safe --guardedness #-}

module Test.SaturationTest where

open import Cubical.Foundations.Prelude

open import Core.Nat
open import Core.Sequence
open import Saturation.ExportedSchema
open import Saturation.ObligationDuality
open import Saturation.Axiom

-- ============================================
-- Concrete Saturated Witnesses for Steps 1-8
-- ============================================

-- Each witness uses mkSaturated, which requires a
-- SchemaSet (Δ k). The Vec length IS the cardinality
-- proof, so cost-match goes through by refl.

sat1 : Saturated 1
sat1 = mkSaturated 1 schemas-step1

sat2 : Saturated 2
sat2 = mkSaturated 2 schemas-step2

sat3 : Saturated 3
sat3 = mkSaturated 3 schemas-step3

sat4 : Saturated 4
sat4 = mkSaturated 4 schemas-step4

sat5 : Saturated 5
sat5 = mkSaturated 5 schemas-step5

sat6 : Saturated 6
sat6 = mkSaturated 6 schemas-step6

sat7 : Saturated 7
sat7 = mkSaturated 7 schemas-step7

sat8 : Saturated 8
sat8 = mkSaturated 8 schemas-step8

-- ============================================
-- Cardinality Proofs (all refl)
-- ============================================

-- These verify that each schema set has exactly Δ k elements.

_ : exportCount sat1 ≡ 1
_ = refl

_ : exportCount sat2 ≡ 1
_ = refl

_ : exportCount sat3 ≡ 2
_ = refl

_ : exportCount sat4 ≡ 3
_ = refl

_ : exportCount sat5 ≡ 5
_ = refl

_ : exportCount sat6 ≡ 8
_ = refl

_ : exportCount sat7 ≡ 13
_ = refl

_ : exportCount sat8 ≡ 21
_ = refl

-- ============================================
-- Interface-Size Recurrence Checks
-- ============================================

-- Verify Δ(k+1) = Δ(k) + Δ(k-1) for k = 2..7.
-- These are the concrete instances of the Fibonacci
-- recurrence that saturation must respect.

_ : Δ 3 ≡ Δ 2 + Δ 1
_ = refl

_ : Δ 4 ≡ Δ 3 + Δ 2
_ = refl

_ : Δ 5 ≡ Δ 4 + Δ 3
_ = refl

_ : Δ 6 ≡ Δ 5 + Δ 4
_ = refl

_ : Δ 7 ≡ Δ 6 + Δ 5
_ = refl

_ : Δ 8 ≡ Δ 7 + Δ 6
_ = refl

-- ============================================
-- Inductive Step Verification
-- ============================================

-- Verify that saturation-step correctly produces
-- witnesses from the concrete base cases.

sat3-from-step : Saturated 3
sat3-from-step = saturation-step 0 sat2 sat1

sat4-from-step : Saturated 4
sat4-from-step = saturation-step 1 sat3 sat2

sat5-from-step : Saturated 5
sat5-from-step = saturation-step 2 sat4 sat3

-- ============================================
-- Full Axiom Instance
-- ============================================

-- Construct the canonical SaturationAxiom from concrete witnesses.

testAxiom : SaturationAxiom
testAxiom = canonicalSaturationAxiom sat1 sat2

-- Derive all-saturated from the axiom
testAllSat : (k : ℕ) → Saturated (suc k)
testAllSat = all-saturated testAxiom

-- Verify the derived witnesses match expected cardinalities
_ : exportCount (testAllSat 0) ≡ 1
_ = refl

_ : exportCount (testAllSat 1) ≡ 1
_ = refl

_ : exportCount (testAllSat 2) ≡ 2
_ = refl

_ : exportCount (testAllSat 3) ≡ 3
_ = refl

_ : exportCount (testAllSat 4) ≡ 5
_ = refl
