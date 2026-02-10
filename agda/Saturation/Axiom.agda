{-# OPTIONS --cubical --safe --guardedness #-}

module Saturation.Axiom where

open import Cubical.Foundations.Prelude

open import Core.Nat
open import Core.Sequence
open import ObligationGraph.Interface
open import ObligationGraph.Recurrence
open import Saturation.ExportedSchema
open import Saturation.ObligationDuality

-- ============================================
-- The Saturation Axiom
-- ============================================

-- The Saturation Axiom states that every Genesis step
-- exports exactly Δ k schemas to the interface basis.
-- It is given by:
--   (1) Base cases for steps 1 and 2
--   (2) An inductive step: if steps n and n-1 are
--       saturated, then step n+1 is saturated
--
-- The inductive step follows from the obligation-schema
-- duality (saturation-step) in ObligationDuality.

record SaturationAxiom : Type where
  field
    base1 : Saturated 1
    base2 : Saturated 2
    step  : (n : ℕ) → Saturated (suc (suc n)) → Saturated (suc n)
                     → Saturated (suc (suc (suc n)))

open SaturationAxiom public

-- ============================================
-- All Steps are Saturated
-- ============================================

-- Given the axiom, every step k ≥ 1 is saturated.
-- We proceed by strong induction (two-step).

all-saturated : SaturationAxiom → (k : ℕ) → Saturated (suc k)
all-saturated ax zero     = base1 ax
all-saturated ax (suc zero) = base2 ax
all-saturated ax (suc (suc n)) =
  step ax n (all-saturated ax (suc n)) (all-saturated ax n)

-- ============================================
-- Saturation Implies Recurrence
-- ============================================

-- If every step is saturated, then the cost-match fields
-- chain together to give us the Fibonacci recurrence.
-- This is already proved definitionally (Δ-recurrence),
-- but here we show it follows from the axiom structure.

saturation-implies-recurrence : SaturationAxiom
  → (n : ℕ) → Δ (suc (suc (suc n))) ≡ Δ (suc (suc n)) + Δ (suc n)
saturation-implies-recurrence ax n =
  -- The recurrence is definitional from Δ = fib
  Δ-recurrence n

-- ============================================
-- The Canonical Axiom Instance
-- ============================================

-- We can construct the canonical instance using
-- saturation-step from ObligationDuality.

canonicalSaturationAxiom : Saturated 1 → Saturated 2 → SaturationAxiom
canonicalSaturationAxiom s1 s2 = record
  { base1 = s1
  ; base2 = s2
  ; step  = saturation-step
  }
