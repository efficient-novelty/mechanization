{-# OPTIONS --cubical --safe --guardedness #-}

module Saturation.ObligationDuality where

open import Cubical.Foundations.Prelude

open import Core.Nat
open import Core.Sequence
open import ObligationGraph.Interface
open import Saturation.ExportedSchema

-- ============================================
-- Obligation Nodes
-- ============================================

-- An Obligation is an unfilled cell in the interface basis
-- that a new layer must resolve.

record Obligation : Type where
  field
    oblDim            : ℕ    -- dimension of the obligation
    oblInterfaceIndex : ℕ    -- position in the interface
    oblSourceLayer    : ℕ    -- which layer generated the obligation

open Obligation public

-- ============================================
-- Saturated Witness
-- ============================================

-- A witness that step k is saturated: it exports exactly Δ k schemas.
-- The Vec-indexed SchemaSet enforces that exportCount is the actual
-- number of schemas, and cost-match proves this equals Δ k.

record Saturated (k : ℕ) : Type where
  field
    exportCount   : ℕ
    exportSchemas : SchemaSet exportCount
    cost-match    : exportCount ≡ Δ k

open Saturated public

-- ============================================
-- Obligation-Schema Duality
-- ============================================

-- The duality principle: each obligation in the interface basis
-- corresponds to exactly one exported schema in the new layer.
-- Under saturation, the number of exported schemas equals the
-- total interface size.

-- Interface size for a 2-window system at step n+1:
-- |I²_{n+1}| = Δ(n) + Δ(n-1) = Δ(n+1)

-- This is already captured by Δ-recurrence in Core.Nat:
-- Δ (suc (suc (suc n))) ≡ Δ (suc (suc n)) + Δ (suc n)

-- ============================================
-- Saturation Step
-- ============================================

-- Given that steps n and n-1 are saturated, and the system
-- is in d=2 mode, the next step is also saturated with
-- exportCount = Δ(n) + Δ(n-1) = Δ(n+1).

-- Helper: concatenate two SchemaSet vectors
_++S_ : {m n : ℕ} → SchemaSet m → SchemaSet n → SchemaSet (m + n)
[]       ++S ys = ys
(x ∷ xs) ++S ys = x ∷ (xs ++S ys)

infixr 5 _++S_

-- The duality witness: interface obligations produce schemas
-- The concatenated schema set from layers n and n-1 has
-- cardinality Δ(n) + Δ(n-1).

saturation-step : (n : ℕ)
  → Saturated (suc (suc n))
  → Saturated (suc n)
  → Saturated (suc (suc (suc n)))
saturation-step n sat-n sat-n-1 = record
  { exportCount   = Δ (suc (suc n)) + Δ (suc n)
  ; exportSchemas = coerced-n ++S coerced-n-1
  ; cost-match    = sym (Δ-recurrence n)
  }
  where
  -- Transport the schema sets along cost-match to get
  -- SchemaSet (Δ k) from SchemaSet (exportCount sat-k).
  coerced-n : SchemaSet (Δ (suc (suc n)))
  coerced-n = subst SchemaSet (cost-match sat-n) (exportSchemas sat-n)

  coerced-n-1 : SchemaSet (Δ (suc n))
  coerced-n-1 = subst SchemaSet (cost-match sat-n-1) (exportSchemas sat-n-1)

-- ============================================
-- Direct Saturated witness construction
-- ============================================

-- Build a Saturated witness directly from a concrete SchemaSet
-- when the length matches Δ k by computation.

mkSaturated : (k : ℕ) → SchemaSet (Δ k) → Saturated k
mkSaturated k schemas = record
  { exportCount   = Δ k
  ; exportSchemas = schemas
  ; cost-match    = refl
  }
