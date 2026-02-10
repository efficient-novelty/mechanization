{-# OPTIONS --cubical --safe --guardedness #-}

module Saturation.Enumeration where

open import Cubical.Foundations.Prelude

open import Core.Nat
open import Saturation.CellPresentation

-- ============================================
-- The Question This Module Addresses
-- ============================================

-- Can we DERIVE the schema count |S(L_k)| = Δ(k) from the
-- cell presentation of each Genesis step, rather than just
-- asserting vectors of the right length?
--
-- Answer: PARTIALLY. For steps 1-5, a formula based on
-- type-theoretic principles gives the correct count. For
-- steps 6-8, the structural formula falls short — the gap
-- represents "interaction schemas" with the growing library
-- that we cannot yet enumerate mechanically.

-- ============================================
-- Type Classification
-- ============================================

-- Each Genesis step introduces a type of one of three kinds.
-- THIS CLASSIFICATION REQUIRES TYPE-THEORETIC JUDGMENT —
-- it is not derivable from the cell presentation alone.

data TypeClass : Type where
  BaseType          : TypeClass
    -- Universe-like foundational types.
    -- Exports only raw existential data (dim0Cells).
    -- Examples: Universe (U₀), Unit (𝟏/★).

  OrdinaryInductive : TypeClass
    -- Standard inductive types / type formers where the
    -- elimination principle is UNIQUELY DETERMINED by the
    -- constructors (universal property of inductive types).
    -- Elim + β do not count as separate exports.
    -- Exports: totalCells (= all independently specifiable data).
    -- Examples: Identity type (formation + refl), Π (formation + λ + app).

  HigherInductive   : TypeClass
    -- Higher inductive types where the elimination principle
    -- is an ADDITIONAL AXIOM (not derivable from constructors)
    -- and each higher constructor's computation rule is also
    -- an additional axiom.
    -- Exports: formation + constructors + elim + one β per higher cell.
    -- Examples: S¹, S², PropTrunc, S³.

-- ============================================
-- Schema Count Formula
-- ============================================

-- Count of higher-dimensional cells (dim ≥ 1)
higherCells : CellPresentation → ℕ
higherCells cp = dim1Cells cp + dim2Cells cp + dim3PlusCells cp

-- Structural schema count from cell presentation + type class.
--
-- BaseType:          dim0Cells
--   (the foundational datum: one schema per point-level entity)
--
-- OrdinaryInductive: totalCells
--   (all cells represent independently specifiable data;
--    elim/β are derivable from these via universal property)
--
-- HigherInductive:   1 + totalCells + 1 + higherCells
--   = formation + constructors + eliminator + higher β-rules
--   (for HITs, elim is an axiom, and each path/surface/higher
--    constructor needs an explicit computation rule;
--    point-constructor β-rules are definitional so not counted)

structuralCount : TypeClass → CellPresentation → ℕ
structuralCount BaseType          cp = dim0Cells cp
structuralCount OrdinaryInductive cp = totalCells cp
structuralCount HigherInductive   cp =
  suc (totalCells cp + suc (higherCells cp))
  -- = 1 + totalCells + 1 + higherCells
  -- written as suc/suc for Agda computation

-- ============================================
-- Genesis Step Classifications
-- ============================================

-- IMPORTANT: These assignments encode type-theoretic judgments.
-- Each is justified below, but the justification is informal.

genesisClass : ℕ → TypeClass
genesisClass 1 = BaseType
  -- Universe: not an inductive type. Exports one datum (U₀).
genesisClass 2 = BaseType
  -- Unit: trivially inductive. Exports one datum (★).
  -- Formation (𝟏 : U₀) fulfills a Universe obligation,
  -- not a new export. Elim is trivially determined.
genesisClass 3 = OrdinaryInductive
  -- Identity type: formation (_≡_) + constructor (refl).
  -- J eliminator is uniquely determined by refl.
  -- In cubical Agda, J is derivable from path primitives.
genesisClass 4 = OrdinaryInductive
  -- Π type: formation (Π) + intro (λ) + elim (app).
  -- β and η are definitional.
genesisClass 5 = HigherInductive
  -- Circle S¹: first HIT. Formation + base + loop +
  -- S¹-elim (axiom) + β-loop (axiom).
  -- β-base is definitional.
genesisClass 6 = HigherInductive
  -- PropTrunc ∥-∥: HIT with |_| + squash + coherence disc.
genesisClass 7 = HigherInductive
  -- S²: HIT with base + surf (2-cell).
genesisClass 8 = HigherInductive
  -- S³: HIT with base + cell (3-cell).
genesisClass _ = BaseType

-- ============================================
-- Derived Structural Count
-- ============================================

genesisStructural : ℕ → ℕ
genesisStructural k = structuralCount (genesisClass k) (genesisCellPres k)

-- ============================================
-- RESULT 1: Structural count matches Δ for steps 1-5
-- ============================================

-- For steps 1-5, the structural formula accounts for ALL
-- exported schemas. No interaction schemas are needed.
-- All proofs are by refl (definitional equality).

structural-matches-1 : genesisStructural 1 ≡ Δ 1    -- 1 ≡ 1
structural-matches-1 = refl

structural-matches-2 : genesisStructural 2 ≡ Δ 2    -- 1 ≡ 1
structural-matches-2 = refl

structural-matches-3 : genesisStructural 3 ≡ Δ 3    -- 2 ≡ 2
structural-matches-3 = refl

structural-matches-4 : genesisStructural 4 ≡ Δ 4    -- 3 ≡ 3
structural-matches-4 = refl

structural-matches-5 : genesisStructural 5 ≡ Δ 5    -- 5 ≡ 5
structural-matches-5 = refl

-- ============================================
-- RESULT 2: Structural count falls SHORT for steps 6-8
-- ============================================

-- For steps 6-8, the structural formula under-counts.
-- The gap represents interaction schemas with the library
-- that we cannot enumerate from cell presentations alone.

structural-at-6 : genesisStructural 6 ≡ 7    -- Δ 6 = 8
structural-at-6 = refl

structural-at-7 : genesisStructural 7 ≡ 5    -- Δ 7 = 13
structural-at-7 = refl

structural-at-8 : genesisStructural 8 ≡ 5    -- Δ 8 = 21
structural-at-8 = refl

-- The interaction gap: structural + gap ≡ Δ
-- (verified by refl = definitional computation)

interaction-gap-6 : genesisStructural 6 + 1 ≡ Δ 6
interaction-gap-6 = refl

interaction-gap-7 : genesisStructural 7 + 8 ≡ Δ 7
interaction-gap-7 = refl

interaction-gap-8 : genesisStructural 8 + 16 ≡ Δ 8
interaction-gap-8 = refl

-- ============================================
-- Interpretation of Results
-- ============================================

-- Steps 1-5: Saturation is DERIVABLE from the HoTT specification
-- of each type. The structural formula gives the correct count
-- because interaction schemas are zero (the library is too small
-- for nontrivial cross-type obligations to arise).
--
-- Steps 6-8: Saturation REQUIRES additional interaction schemas
-- that grow with the library. The gaps (1, 8, 16) cannot be
-- derived from cell presentations alone.
--
-- Pattern: as the library grows, interaction schemas DOMINATE.
-- At step 7: structural = 5, interaction = 8 (61% of total).
-- At step 8: structural = 5, interaction = 16 (76% of total).
--
-- CONCLUSION: The saturation assumption |S(L_k)| = Δ(k) is:
-- (a) Derivable for k ≤ 5 from standard HIT specification theory
-- (b) An empirical claim for k ≥ 6 that depends on interaction
--     schemas growing to fill the Fibonacci gap
-- (c) NOT mechanically derivable from cell presentations alone
--     for the general case
--
-- The paper should state saturation as an axiom of the model,
-- noting that it is verifiable (by exhaustive enumeration) for
-- small k but becomes an increasingly strong claim as k grows.
