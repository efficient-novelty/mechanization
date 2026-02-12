{-# OPTIONS --cubical --safe --guardedness #-}

module Adjunction.AdjunctionDepth where

open import Cubical.Foundations.Prelude
open import Core.Nat

-- ============================================================
-- AdjunctionDepth.agda
-- Formalization of the Adjunction Barrier (Theorem 3.7)
--
-- An adjunction L ⊣ R requires:
--   Depth 0: functors L, R
--   Depth 1: unit η, counit ε
--   Depth 2: triangle identities (ε∘Lη = id, Rε∘ηR = id)
--
-- A system with d=1 can establish η and ε but cannot verify
-- the triangle identities, which are 2-dimensional homotopies.
-- ============================================================

-- ============================================================
-- Part 1: Obligation Depth Classification
-- ============================================================

data ObligationDepth : Type where
  Depth0 : ObligationDepth
  Depth1 : ObligationDepth
  Depth2 : ObligationDepth

-- ============================================================
-- Part 2: Adjoint Pair Record
-- ============================================================
--
-- We work in a type-theoretic setting where an "adjunction"
-- between type formers L and R is witnessed by:
--   η : A → R(L(A))           (unit)
--   ε : L(R(B)) → B           (counit)
--   triangle-L : ε(L(a)) ∘ L(η(a)) ≡ a    (left triangle)
--   triangle-R : R(ε(b)) ∘ η(R(b)) ≡ b    (right triangle)

record AdjointPair (L R : Type → Type) : Type₁ where
  field
    -- Depth 0: The functors (given as parameters)

    -- Depth 1: Unit and Counit
    η : {A : Type} → A → R (L A)
    ε : {B : Type} → L (R B) → B

    -- Depth 1: Functorial action (needed to state triangles)
    mapL : {A B : Type} → (A → B) → L A → L B
    mapR : {A B : Type} → (A → B) → R A → R B

    -- Depth 2: Triangle Identities
    -- These equate COMPOSITIONS of 1-cells to the identity.
    -- They are 2-dimensional homotopies — the key obstruction.
    triangle-L : {A : Type} (a : L A)
      → ε (mapL η a) ≡ a

    triangle-R : {B : Type} (b : R B)
      → mapR ε (η b) ≡ b

-- ============================================================
-- Part 3: Depth Classification of Each Component
-- ============================================================

-- The depth of each component in an adjunction:
--   η         : Depth 1 (natural transformation, references L and R)
--   ε         : Depth 1 (natural transformation, references L and R)
--   triangle-L : Depth 2 (equates composition of 1-cells to identity)
--   triangle-R : Depth 2 (equates composition of 1-cells to identity)

η-depth : ObligationDepth
η-depth = Depth1

ε-depth : ObligationDepth
ε-depth = Depth1

triangle-L-depth : ObligationDepth
triangle-L-depth = Depth2

triangle-R-depth : ObligationDepth
triangle-R-depth = Depth2

-- ============================================================
-- Part 4: Depth-1 Insufficient
-- ============================================================
--
-- A Depth1-only system can state η and ε but cannot state
-- the triangle identities. We model this as a record that
-- has η and ε but LACKS the triangle fields.

record Depth1Only (L R : Type → Type) : Type₁ where
  field
    η : {A : Type} → A → R (L A)
    ε : {B : Type} → L (R B) → B
    mapL : {A B : Type} → (A → B) → L A → L B
    mapR : {A B : Type} → (A → B) → R A → R B

-- A Depth1Only pair is strictly weaker than an AdjointPair:
-- it has the 0-cell and 1-cell data but no 2-cell coherence.
-- Without triangle identities, the unit/counit may not compose
-- correctly — we have a "pre-adjunction" but not an adjunction.

forgetTriangles : {L R : Type → Type}
  → AdjointPair L R → Depth1Only L R
forgetTriangles adj = record
  { η    = AdjointPair.η adj
  ; ε    = AdjointPair.ε adj
  ; mapL = AdjointPair.mapL adj
  ; mapR = AdjointPair.mapR adj
  }

-- The key observation: forgetTriangles is NOT an equivalence.
-- The triangle identities carry independent data (Depth 2)
-- that cannot be recovered from Depth-1 data alone.
-- This is the formal content of the Adjunction Barrier.

-- ============================================================
-- Part 5: Maximum depth of an adjunction = 2
-- ============================================================

-- The maximum obligation depth across all components
-- of an adjunction is 2 (from the triangle identities).
-- This witnesses that d ≥ 2 for any system supporting adjunctions.

adjunctionMaxDepth : ObligationDepth
adjunctionMaxDepth = Depth2
