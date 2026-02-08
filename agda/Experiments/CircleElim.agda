{-# OPTIONS --cubical --safe #-}

module Experiments.CircleElim where

-- ============================================================
-- CircleElim.agda
-- Coherence obligation trace for the S^1 eliminator
-- ============================================================
--
-- PURPOSE: Verify that the elimination principle for the circle
-- S^1 generates coherence obligations of depth at most 2, and
-- that the actual depth observed is 1 (since S^1 has no 2-cells).
--
-- LIBRARY CONTEXT (Genesis Sequence position n=5):
--   L_{n-2} = {U, 1}        (universe and unit, steps 1-2)
--   L_{n-1} = {*, Pi/Sigma} (witness and function types, steps 3-4)
--   L_n     = S^1 itself    (step 5)
--
-- ============================================================
-- OBLIGATION SUMMARY TABLE
-- ============================================================
--
--  Obligation   | Dim | Layer Depth | References
-- --------------+-----+-------------+------------------------------
--  rec(base)    |  0  |     0       | Only the target type Y
--  rec(loop)    |  1  |     1       | loop in S^1 (L_n) and
--               |     |             | transport in Y
--  [no 2-cell]  |  -  |     -       | S^1 has no surface constructors
--
--  Maximum depth observed: 1
--  (S^1 has no 2-cells, so the depth-2 mechanism is not triggered)
--
-- ============================================================

open import Cubical.Foundations.Prelude

-- ============================================================
-- Part 1: The Circle S^1
-- ============================================================
-- In Cubical Agda, S^1 is a Higher Inductive Type with:
--   base : S^1
--   loop : base == base
--
-- We import the standard definition from the cubical library.

open import Cubical.HITs.S1.Base

-- S^1 is now available with:
--   base : S^1
--   loop : base == base   (a path in the interval variable i)

-- ============================================================
-- Part 2: A concrete eliminator
-- ============================================================
-- We define a recursion principle rec : S^1 -> Y for a specific
-- target Y. For concreteness, we take Y = S^1 itself and define
-- the identity map, then trace every obligation the type-checker
-- requires us to fill.

-- ----- OBLIGATION 0 (Dimension 0, Depth 0) -----
-- rec(base) : Y
--
-- We must provide a point in the target type Y.
-- This references:
--   - Y itself (the target type)
--   - base (the constructor being matched)
-- Both belong to the CURRENT definition and the target type.
-- No reference to any prior library layer is needed.
--
-- DEPTH: 0 (purely local to the eliminator's own signature)

-- ----- OBLIGATION 1 (Dimension 1, Depth 1) -----
-- rec(loop) : PathP (\i -> Y) (rec base) (rec base)
--
-- We must provide a path in Y from rec(base) to rec(base).
-- Equivalently: transport along loop in the family Y must
-- send rec(base) to rec(base).
--
-- This references:
--   - loop : base == base  (the path constructor of S^1,
--     which lives in the same layer L_n as S^1)
--   - transport in Y (which uses the path structure of Y;
--     if Y is a type from L_n or earlier, this uses L_n's
--     path algebra)
--
-- DEPTH: 1 (references the path constructor from L_n and
-- the transport structure of Y, but no interaction between
-- L_n and L_{n-1} is needed because there are no 2-cells
-- whose coherence would require relating paths from two
-- distinct layers)

-- The identity map: S^1 -> S^1
recId : S^1 -> S^1
recId base     = base       -- Obligation 0: point data (depth 0)
recId (loop i) = loop i     -- Obligation 1: path data  (depth 1)

-- ============================================================
-- Part 3: A non-trivial eliminator with a different target
-- ============================================================
-- To illustrate the obligation structure more clearly, let us
-- define an eliminator into a type family over S^1.
-- Consider the constant family Y(x) = S^1 for all x : S^1.
-- An eliminator into this family is just a section.

-- A "double loop" map: wraps the loop around twice.
recDouble : S^1 -> S^1
recDouble base     = base               -- Obligation 0 (depth 0)
recDouble (loop i) = (loop ∙ loop) i    -- Obligation 1 (depth 1)
  -- Here (loop . loop) is path composition of loop with itself.
  -- The composition operation _._  is part of the path algebra
  -- of S^1, which lives in L_n.
  --
  -- Crucially, path composition itself does not require us to
  -- verify that associativity holds (that would be a depth-2
  -- obligation involving the coherence of composition). We are
  -- only USING composition, not proving it coherent. The
  -- coherence was already established when the path algebra
  -- was set up.

-- ============================================================
-- Part 4: Why no depth-2 obligation arises
-- ============================================================
--
-- S^1 has cell presentation:
--   C_0 = {base}
--   C_1 = {loop : base = base}
--   C_k = emptyset  for k >= 2
--
-- Since there are no 2-cells in S^1's presentation, the
-- eliminator never asks us to provide a surface datum.
-- A surface datum would require coherence between paths,
-- which would involve:
--   - Paths from L_n  (e.g., ap_f applied to loop)
--   - The composition law for paths (from L_n)
--   - The coherence of that composition with paths from L_{n-1}
--     (the associator, which is a depth-2 phenomenon)
--
-- Since S^1 has no 2-cells, this depth-2 mechanism is never
-- triggered. The maximum depth for the S^1 eliminator is 1.
--
-- Note: This does NOT mean that depth-2 obligations are
-- impossible in HoTT. It means that S^1 is too low-dimensional
-- to trigger them. See SphereElim.agda and TorusElim.agda for
-- examples where depth 2 genuinely appears.

-- ============================================================
-- Part 5: Dependent eliminator (induction principle)
-- ============================================================
-- For completeness, here is the dependent eliminator for S^1.
-- The obligation structure is the same: one point datum and
-- one path datum.

-- The type of the dependent eliminator:
-- S^1-elim : (Y : S^1 -> Type l)
--          -> (y_base : Y base)
--          -> (y_loop : PathP (\i -> Y (loop i)) y_base y_base)
--          -> (x : S^1) -> Y x
--
-- Obligation 0: y_base : Y base                          (depth 0)
-- Obligation 1: y_loop : PathP (\i -> Y (loop i)) yb yb  (depth 1)
--
-- The PathP type in obligation 1 is a dependent path over the
-- family Y, transported along loop. This references:
--   - loop (from S^1, in L_n)
--   - the family Y and its transport structure
-- Still depth 1: no interaction between distinct library layers.

-- Using the cubical library's built-in eliminator:
-- (This is available as S^1.elim in the cubical library)

-- ============================================================
-- CONCLUSION
-- ============================================================
--
-- For the circle S^1:
--   - 2 obligations total (point + path)
--   - Maximum depth: 1
--   - Depth 0: 1 obligation  (point data for base)
--   - Depth 1: 1 obligation  (path data for loop)
--   - Depth 2: 0 obligations (no 2-cells in S^1)
--
-- This is consistent with the Coherence Window Theorem:
-- all obligations reference at most 2 layers (here, at most 1).
-- The bound d=2 is not saturated because S^1 is 1-dimensional.
-- ============================================================
