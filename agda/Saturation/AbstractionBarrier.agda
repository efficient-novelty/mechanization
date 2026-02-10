{-# OPTIONS --cubical --safe --guardedness #-}

module Saturation.AbstractionBarrier where

open import Cubical.Foundations.Prelude

-- ============================================================
-- AbstractionBarrier.agda
-- Concrete verification that Group B obligations (S³ ↔ ∥S²∥)
-- reference L₇'s interface, not L₆'s internals
-- ============================================================
--
-- THE QUESTION:
-- When S³ verifies obligation 8.6 ("maps S³ → ∥S²∥ are
-- contractible"), does it reference L₇ or L₆?
--
-- If L₇ encapsulates PropTrunc's behavior:
--   13 from L₇ + 8 from L₆ = 21 = Δ₇ + Δ₆  ← Fibonacci ✓
--
-- If Group B "leaks through" to L₆:
--   5 from L₇ + 16 from L₆ = 21 = 5 + 2·Δ₆  ← NOT Fibonacci ✗
--
-- METHODOLOGY:
-- We define L₇'s interface as a RECORD, then show that all
-- Group B obligations are dischargeable from this record alone.
-- The record mentions no PropTrunc definition. If the proofs
-- compile, the abstraction barrier holds.
--
-- PRECEDENT (HopfTrace.agda, Part 4):
-- "When we reference 'loop · loop⁻¹ = refl' in the proof
-- that rotate is an equivalence, we are using a THEOREM about
-- S¹, not raw data from L_{n-2}. The coherence between
-- L_{n-2} and L_{n-1} is already internalized."
--
-- We demonstrate the SAME PRINCIPLE one layer up.

-- ============================================================
-- Part 1: L₇'s OPAQUE Interface
-- ============================================================
--
-- The record represents L₇'s exported interface. It contains
-- S²'s structural specification AND the resolved PropTrunc
-- obligations — all as OPAQUE FACTS, with no reference to
-- how PropTrunc works internally.
--
-- Using a record makes the abstraction barrier EXPLICIT in
-- Agda's type system: anything proved from the record fields
-- alone is, by construction, independent of L₆.

record L₇-Interface : Type₁ where
  field
    -- ═══════════════════════════════════════
    -- Structural exports (from S²'s spec)
    -- ═══════════════════════════════════════
    S²    : Type
    base₂ : S²
    surf  : refl {x = base₂} ≡ refl {x = base₂}

    -- ═══════════════════════════════════════
    -- Inherited exports (resolved PropTrunc obligations)
    -- Named opaquely: S³ sees "an abstract type TrS² with
    -- certain properties," not "PropTrunc applied to S²."
    -- ═══════════════════════════════════════
    TrS²        : Type
    TrS²-inc    : S² → TrS²
    TrS²-isProp : (x y : TrS²) → x ≡ y
    TrS²-rec    : {B : Type}
      → ((b₁ b₂ : B) → b₁ ≡ b₂)
      → (S² → B) → TrS² → B

-- NOTE: This record imports NOTHING from PropTrunc.
-- The field TrS² is abstract — it could be implemented
-- as ∥S²∥, or as Unit (since ∥S²∥ ≃ Unit for connected S²),
-- or any other proposition. The proofs below work regardless.

-- ============================================================
-- Part 2: Group B obligations discharged from L₇ alone
-- ============================================================
--
-- We work in a module parameterized by L₇'s interface and S³.
-- This makes the dependency structure explicit: every proof
-- below uses ONLY L₇ and S³, never L₆.

module GroupB-Obligations
  (L₇ : L₇-Interface)
  (S³ : Type) (base₃ : S³)
  (cell : refl {x = refl {x = base₃}} ≡ refl {x = refl {x = base₃}})
  where

  open L₇-Interface L₇

  -- Derived fact: TrS² is inhabited
  TrS²-pt : TrS²
  TrS²-pt = TrS²-inc base₂

  -- Derived fact: TrS² is a set (isProp → isSet)
  TrS²-isSet : (x y : TrS²) (p q : x ≡ y) → p ≡ q
  TrS²-isSet x y = isProp→isSet TrS²-isProp x y

  -- ────────────────────────────────────────
  -- Obligation 8.6: maps S³ → TrS² are contractible
  -- ────────────────────────────────────────
  -- USES: TrS²-pt, TrS²-isProp, funExt
  -- DOES NOT USE: ∥_∥ definition, squash constructor, L₆

  obl-8-6 : Σ[ f ∈ (S³ → TrS²) ] ((g : S³ → TrS²) → f ≡ g)
  obl-8-6 = (λ _ → TrS²-pt) ,
             λ g → funExt (λ x → TrS²-isProp TrS²-pt (g x))

  -- ────────────────────────────────────────
  -- Obligation 8.7: composition |_| ∘ f for f : S³ → S²
  -- ────────────────────────────────────────
  -- USES: TrS²-inc (L₇ export)

  obl-8-7 : (f : S³ → S²) → S³ → TrS²
  obl-8-7 f x = TrS²-inc (f x)

  -- All such compositions are equal:
  obl-8-7-unique : (f g : S³ → S²)
    → obl-8-7 f ≡ obl-8-7 g
  obl-8-7-unique f g =
    funExt (λ x → TrS²-isProp (TrS²-inc (f x)) (TrS²-inc (g x)))

  -- ────────────────────────────────────────
  -- Obligation 8.8: all maps S³ → TrS² are equal
  -- ────────────────────────────────────────
  -- USES: TrS²-isProp, funExt

  obl-8-8 : (f g : S³ → TrS²) → f ≡ g
  obl-8-8 f g = funExt (λ x → TrS²-isProp (f x) (g x))

  -- ────────────────────────────────────────
  -- Obligation 8.9: higher paths in TrS² trivial
  -- (compatible with S³'s 3-cell)
  -- ────────────────────────────────────────
  -- USES: TrS²-isSet (derived from TrS²-isProp)

  -- 2-paths are trivial:
  obl-8-9-dim2 : (x y : TrS²) → isProp (x ≡ y)
  obl-8-9-dim2 = TrS²-isSet

  -- 3-paths are trivial (S³'s 3-cell maps trivially):
  obl-8-9-dim3 : (x y : TrS²) (p q : x ≡ y) → isProp (p ≡ q)
  obl-8-9-dim3 x y = isProp→isSet (TrS²-isSet x y)

  -- ────────────────────────────────────────
  -- Obligation 8.10: recursion out of TrS² into S³-targets
  -- ────────────────────────────────────────
  -- USES: TrS²-rec (L₇ export)
  --
  -- We show TrS²-rec can be applied when the codomain
  -- involves S³. The recursor doesn't care what the target
  -- type is — it only needs isProp of the target.

  obl-8-10 : ((b₁ b₂ : S³) → b₁ ≡ b₂)   -- if S³ were a prop
    → (S² → S³) → TrS² → S³
  obl-8-10 = TrS²-rec

  -- More realistically, for any proposition P involving S³:
  obl-8-10' : {P : Type}
    → ((p₁ p₂ : P) → p₁ ≡ p₂)
    → (S² → P) → TrS² → P
  obl-8-10' = TrS²-rec

-- ============================================================
-- Part 3: What the type-checker has verified
-- ============================================================
--
-- FACT: This module type-checks with --cubical --safe.
--
-- FACT: The only import is Cubical.Foundations.Prelude
-- (for isProp→isSet, funExt, Σ, refl, ≡).
--
-- FACT: No module defining PropTrunc, ∥_∥, squash, or
-- truncation internals is imported anywhere.
--
-- FACT: The L₇-Interface record contains no reference to
-- PropTrunc — TrS² is an abstract type with properties.
--
-- CONCLUSION: All Group B obligations are dischargeable from
-- L₇'s interface alone. The abstraction barrier holds.
--
-- ============================================================
-- Part 4: What this means for the Fibonacci recurrence
-- ============================================================
--
-- Since Group B references L₇ (not L₆), step 8 decomposes as:
--
--   From L₇ = S²:        13 (5 structural + 8 inherited)
--   From L₆ = PropTrunc:   8 (parametric application to S³)
--   Total:                 21 = 13 + 8 = Δ₇ + Δ₆     ✓
--
-- If Group B leaked through to L₆:
--   From L₇:   5 (structural only)
--   From L₆:  16 (8 parametric + 8 leaked)
--   Total:    21 = 5 + 2·Δ₆                           ✗
--
-- The abstraction barrier is ESSENTIAL for the Fibonacci
-- recurrence. Without it, the recurrence would be
-- Δ(k) = 5 + 2·Δ(k-2), not Δ(k) = Δ(k-1) + Δ(k-2).
--
-- ============================================================
-- Part 5: The General Principle
-- ============================================================
--
-- INTEGRATION TRACE ENCAPSULATION:
-- When layer L_k is sealed, resolved obligations become part
-- of L_k's OPAQUE interface (the record). Future types
-- interact with the record, not the underlying layers.
--
-- In Agda terms: the L₇-Interface record IS the abstraction
-- barrier. S³ is parameterized by this record. The proofs
-- use only record fields. PropTrunc never appears.
--
-- The general pattern (demonstrated at two levels):
--
-- HopfTrace (depth 2 → 1):
--   "loop · loop⁻¹ = refl" is an L₅ THEOREM,
--   not raw L₄ data. The Hopf fibration uses the
--   theorem, not the derivation.
--
-- This module (depth 1 → 0 within L₇):
--   "TrS² is contractible" is an L₇ THEOREM,
--   not raw L₆ data. S³'s obligations use the
--   theorem, not the derivation.
--
-- The abstraction barrier IS the sealing. Each layer
-- fully encapsulates its predecessors' contributions.
-- ============================================================
