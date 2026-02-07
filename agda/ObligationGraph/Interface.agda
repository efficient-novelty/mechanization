{-# OPTIONS --cubical --safe --guardedness #-}

module ObligationGraph.Interface where

open import Cubical.Foundations.Prelude

open import Core.Nat

-- ============================================
-- Obligation Graphs and Schemas
-- ============================================

-- A Schema is a finite set of obligation nodes.
-- For Phase 1, we use the abstract representation (just cardinality).
-- Phase 2 will connect this to actual Agda type definitions.

-- Abstract Schema: just its cardinality
Schema : Type
Schema = ℕ

-- The Interface Basis for a d-window system
-- This is I^(d)_n = ⊎_{j=0}^{d-1} S(L_{n-j})
--
-- For d=2: I^(2)_n = S(L_n) ⊎ S(L_{n-1})
-- The disjoint union ensures obligations to different layers
-- are type-theoretically distinct.

-- Disjoint union of two schemas (as finite sets)
-- |S₁ ⊎ S₂| = |S₁| + |S₂|
schemaSum : Schema → Schema → Schema
schemaSum s₁ s₂ = s₁ + s₂

-- The 2-dimensional interface: disjoint union of current and previous
Interface² : Schema → Schema → Schema
Interface² current previous = schemaSum current previous

-- ============================================
-- The Saturation Assumption
-- ============================================

-- SATURATION ASSUMPTION (Axiom of the model):
-- The integration cost of the next structure equals
-- the full cardinality of the available interface.
--
-- This is stated as an axiom, making explicit where
-- the model's assumptions live.

-- For a concrete history, the next Δ equals the interface size
-- This is definitional given the saturation assumption:
--   Δ(n+1) = |I^(d)_n|

-- For d=2:
--   Δ(n+1) = |S(L_n)| + |S(L_{n-1})| = Δ(n) + Δ(n-1)

-- ============================================
-- Obligation Graph Structure (for Phase 2)
-- ============================================

-- A more concrete representation for when we connect to real types
record ObligationNode : Type where
  field
    dimension : ℕ           -- 0 = point, 1 = path, 2 = surface, ...
    sourceLayer : ℕ         -- which layer of history this comes from

-- An ObligationGraph is a finite set of obligation nodes
-- For Phase 1, we only care about its cardinality
ObligationGraph : Type
ObligationGraph = ℕ  -- Abstract: just the count
