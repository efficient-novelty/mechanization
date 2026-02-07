{-# OPTIONS --guardedness --without-K #-}

module Oracle.Kappa where

-- ============================================
-- Note: This module uses --without-K instead of --cubical
-- because Agda 2.8.0's reflection API has compatibility
-- issues with --cubical (InfectiveImport errors).
--
-- IMPORTANT: First run requires --ignore-all-interfaces flag
-- to clear cached interface files:
--   agda --ignore-all-interfaces Oracle/Kappa.agda
-- ============================================

-- ============================================
-- Reflection and Built-in Imports
-- ============================================

open import Agda.Builtin.Reflection public
open import Agda.Builtin.List public
open import Agda.Builtin.String public
open import Agda.Builtin.Bool public
open import Agda.Builtin.Unit public
open import Agda.Builtin.Nat public

-- Identity type (standard, not cubical)
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}

infix 4 _≡_

-- ============================================
-- List utilities
-- ============================================

len : {A : Set} → List A → Nat
len [] = zero
len (x ∷ xs) = suc (len xs)

-- ============================================
-- The κ-Oracle: Measuring Effort
-- ============================================

-- κ(X) measures the "effort" to define a type X.
-- From the implementation plan:
--
-- κ(X) = (number of point constructors)
--      + (number of path constructors)
--      + (number of higher path constructors)
--      + (number of computation rules / β-reductions)
--
-- For Phase 2, we start with: κ = number of constructors

-- ============================================
-- Core Reflection Functions
-- ============================================

-- Pattern matching helper for Definition
-- (Replaces case expressions which don't work with reflection API)
handleDef : Definition → TC Nat
handleDef (data-type pars cs) = returnTC (len cs)
handleDef (record-type c fs)  = returnTC (suc (len fs))
handleDef (function cs)       = returnTC (len cs)
handleDef (data-cons d q)     = returnTC 1
handleDef axiom               = returnTC 0
handleDef prim-fun            = returnTC 0

-- Count constructors of a data type
-- Uses bindTC instead of do notation
countCons : Name → TC Nat
countCons n = bindTC (getDefinition n) handleDef

-- ============================================
-- Macro for computing κ at compile time
-- ============================================

-- Helper that unifies with a nat literal
unifyNat : Term → Nat → TC ⊤
unifyNat hole n = unify hole (lit (nat n))

-- Macro that computes κ and returns it as a natural number
κ-macro : Name → Term → TC ⊤
κ-macro x hole = bindTC (countCons x) (unifyNat hole)

macro
  κ : Name → Term → TC ⊤
  κ = κ-macro

-- ============================================
-- Test Types
-- ============================================

-- Unit type
data ⊤' : Set where
  tt' : ⊤'

-- Boolean type
data Bool' : Set where
  true'  : Bool'
  false' : Bool'

-- Three-element type
data Tri : Set where
  one : Tri
  two : Tri
  three : Tri

-- Four-element type (simulating Torus constructor count)
data Quad : Set where
  q1 : Quad
  q2 : Quad
  q3 : Quad
  q4 : Quad

-- ============================================
-- κ Tests
-- ============================================

κ-⊤' : Nat
κ-⊤' = κ ⊤'

κ-Bool' : Nat
κ-Bool' = κ Bool'

κ-Tri : Nat
κ-Tri = κ Tri

κ-Quad : Nat
κ-Quad = κ Quad

-- ============================================
-- Verification
-- ============================================

-- Unit has 1 constructor
test-⊤' : κ-⊤' ≡ 1
test-⊤' = refl

-- Bool has 2 constructors
test-Bool' : κ-Bool' ≡ 2
test-Bool' = refl

-- Tri has 3 constructors
test-Tri : κ-Tri ≡ 3
test-Tri = refl

-- Quad has 4 constructors (like Torus)
test-Quad : κ-Quad ≡ 4
test-Quad = refl

-- ============================================
-- Reference κ Values from Paper
-- ============================================

-- Expected values from the implementation plan:
-- | Type       | Points | Paths | Higher | Comp | κ |
-- |------------|--------|-------|--------|------|---|
-- | Unit (𝟏)   | 1      | 0     | 0      | 0    | 1 |
-- | Bool (𝟐)   | 2      | 0     | 0      | 0    | 2 |
-- | S¹         | 1      | 1     | 0      | 0    | 2 |
-- | S²         | 1      | 0     | 1      | 0    | 2 |
-- | Torus      | 1      | 2     | 1      | 0    | 4 |
-- | Σ-type     | 1      | 0     | 0      | 2    | 3 |
-- | Π-type     | 1      | 0     | 0      | 1    | 2 |

-- Reference values
κ-Unit-expected : Nat
κ-Unit-expected = 1

κ-Bool-expected : Nat
κ-Bool-expected = 2

κ-Circle-expected : Nat
κ-Circle-expected = 2  -- base + loop

κ-Sphere2-expected : Nat
κ-Sphere2-expected = 2  -- base + surf

κ-Torus-expected : Nat
κ-Torus-expected = 4  -- base + p + q + surf
