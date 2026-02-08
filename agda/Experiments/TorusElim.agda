{-# OPTIONS --cubical --safe #-}

module Experiments.TorusElim where

-- ============================================================
-- TorusElim.agda
-- Coherence obligation trace for the torus T^2 eliminator
-- ============================================================
--
-- PURPOSE: The torus T^2 is the most instructive example for the
-- Coherence Window Theorem because it has constructors at ALL
-- three relevant dimensions (0, 1, 2), generating obligations
-- at depths 0, 1, and 2. It is the minimal HIT that exercises
-- the full d=2 window with 4 distinct obligations.
--
-- LIBRARY CONTEXT:
--   L_{n-2} = basic type formers (Pi, Sigma, identity types)
--   L_{n-1} = S^1 (or whatever provides the target's path algebra)
--   L_n     = T^2 being introduced
--
-- ============================================================
-- OBLIGATION SUMMARY TABLE
-- ============================================================
--
--  Obligation   | Dim | Layer Depth | References
-- --------------+-----+-------------+-------------------------------
--  rec(base)    |  0  |     0       | Target type Y only
--  rec(pathP)   |  1  |     1       | pathP in T^2 (L_n),
--               |     |             | transport in Y (L_n)
--  rec(pathQ)   |  1  |     1       | pathQ in T^2 (L_n),
--               |     |             | transport in Y (L_n)
--  rec(surf)    |  2  |     2       | surf in T^2 (L_n),
--               |     |             | composition of rec(pathP)
--               |     |             | and rec(pathQ) (L_n),
--               |     |             | coherence with Y's path
--               |     |             | algebra (L_{n-1} interaction)
--
--  Total obligations:    4
--  Maximum depth:        2
--  Depth 0 obligations:  1  (base)
--  Depth 1 obligations:  2  (pathP, pathQ)
--  Depth 2 obligations:  1  (surf)
--
-- ============================================================

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.GroupoidLaws

-- ============================================================
-- Part 1: The Torus T^2
-- ============================================================
-- The torus is a Higher Inductive Type with cell presentation:
--
--   C_0 = {base}
--   C_1 = {pathP : base = base,  pathQ : base = base}
--   C_2 = {surf : pathP . pathQ = pathQ . pathP}
--   C_k = emptyset  for k >= 3
--
-- In words: a point, two loops, and a surface witnessing that
-- the two loops commute up to homotopy.

-- We use the cubical library's torus definition.
open import Cubical.HITs.Torus.Base

-- T^2 provides:
--   point   : T^2
--   line1   : point == point
--   line2   : point == point
--   square  : PathP (\i -> line1 i == line1 i) line2 line2
--
-- The square constructor witnesses the commutativity surface:
-- it says that line2 "commutes" with line1 in the appropriate
-- path-over-path sense.

-- ============================================================
-- Part 2: The S^1 target (from a prior layer)
-- ============================================================

open import Cubical.HITs.S1.Base

-- ============================================================
-- Part 3: The eliminator and its 4 obligations
-- ============================================================
-- We define a map  f : T^2 -> S^1  and trace every obligation.

-- Consider the projection onto the first loop:
-- f(base)  = base
-- f(pathP) = loop
-- f(pathQ) = refl
-- f(surf)  = ... (must fill a square)

-- ----- OBLIGATION 0 (Dimension 0, Depth 0) -----
{- OBLIGATION 0: Point data for base
   TYPE:    f(point) : S^1
   DEPTH:   0
   REASON:  We choose a point in S^1. This references only the
            target type. No interaction with any library layer.
   LAYER REFS: none (only the target type's carrier set)
-}

-- ----- OBLIGATION 1 (Dimension 1, Depth 1) -----
{- OBLIGATION 1: Path data for line1
   TYPE:    f(line1) : f(point) ==_{S^1} f(point)
            i.e., a path  base == base  in S^1
   DEPTH:   1
   REASON:  We must provide a path in S^1 that maps to line1.
            This references:
              - line1 (a constructor of T^2, in L_n)
              - The path space of S^1 (which is L_{n-1})
            The path space of S^1 provides the TYPE of the
            obligation; line1 provides the STRUCTURE it must
            match. Both are from a single recent layer.
   LAYER REFS: L_n (line1), target type S^1 (L_{n-1})
-}

-- ----- OBLIGATION 2 (Dimension 1, Depth 1) -----
{- OBLIGATION 2: Path data for line2
   TYPE:    f(line2) : f(point) ==_{S^1} f(point)
            i.e., a path  base == base  in S^1
   DEPTH:   1
   REASON:  Structurally identical to Obligation 1. We provide
            a second path in S^1 for the second loop.
   LAYER REFS: L_n (line2), target type S^1 (L_{n-1})
-}

-- ----- OBLIGATION 3 (Dimension 2, Depth 2) -----
{- OBLIGATION 3: Surface data for square
   TYPE:    PathP (\i -> f(line1) i ==_{S^1} f(line1) i)
                  (ap f line2) (ap f line2)
   DEPTH:   2
   REASON:  THIS IS THE CRITICAL DEPTH-2 OBLIGATION.

   The surface datum must witness that our path data for line1
   and line2 are "compatible" in the sense that the image of the
   commutativity square in T^2 gives a valid commutativity
   square in S^1.

   Concretely, we must show that the square:

       f(line1)
   *---------*
   |         |
   f(line2)  f(line2)
   |         |
   *---------*
       f(line1)

   is filled in S^1. This requires:

   (a) The paths f(line1) and f(line2) from obligations 1 and 2.
       These are depth-1 data from the current step.

   (b) The path COMPOSITION in S^1:
       f(line1) . f(line2)  and  f(line2) . f(line1)
       Path composition is an operation in S^1's path algebra,
       which was established when S^1 was introduced (at L_{n-1}).

   (c) The COHERENCE of this composition: specifically, the
       commutativity condition requires understanding how paths
       in S^1 interact. If f(line1) = loop and f(line2) = refl,
       we need:
         loop . refl == refl . loop
       The proof of this uses:
         - rUnit (right unit law): loop . refl == loop
         - lUnit (left unit law):  refl . loop == loop
       These groupoid laws are part of S^1's PATH ALGEBRA.
       Their proofs reference:
         - The definition of _._  (path composition, from the
           ambient type theory -- L_{n-2} or earlier)
         - The paths in S^1 being composed (from L_{n-1})

   This is a genuine DEPTH-2 obligation because it requires the
   interaction between:
     - L_n: the square constructor of T^2 that imposes the condition
     - L_{n-1}: S^1's path algebra (loop, composition)
     - L_{n-2}: the coherence laws (unit laws, associativity) that
       were established when S^1's path algebra was sealed

   The reference to L_{n-2} goes THROUGH L_{n-1}: we use the
   groupoid laws that were proved about S^1's paths, and those
   proofs internally referenced L_{n-2}. So the depth is 2
   (L_n and L_{n-1}), with L_{n-2} appearing only indirectly.

   LAYER REFS: L_n (square), L_{n-1} (S^1's path algebra and its
               coherence, which was sealed using L_{n-2})
-}

-- The first-loop projection:
proj1 : T^2 -> S^1
proj1 (point)       = base              -- Obligation 0: depth 0
proj1 (line1 i)     = loop i            -- Obligation 1: depth 1
proj1 (line2 j)     = base              -- Obligation 2: depth 1
proj1 (square i j)  = loop i            -- Obligation 3: depth 2
  -- For proj1, the surface obligation is filled by the constant
  -- homotopy on loop: the square collapses because f(line2) = refl.
  -- The resulting square is:
  --   loop(i) at every j-coordinate
  -- which is degenerate. But it IS a surface datum, and its
  -- well-typedness requires the path algebra of S^1.

-- ============================================================
-- Part 4: A more revealing example
-- ============================================================
-- The projection proj1 has a degenerate surface datum. For a
-- non-degenerate depth-2 obligation, consider a map where both
-- paths are nontrivial.

-- Second projection: T^2 -> S^1 via the other loop
proj2 : T^2 -> S^1
proj2 (point)       = base              -- Obligation 0: depth 0
proj2 (line1 i)     = base              -- Obligation 1: depth 1
proj2 (line2 j)     = loop j            -- Obligation 2: depth 1
proj2 (square i j)  = loop j            -- Obligation 3: depth 2

-- ============================================================
-- Part 5: Diagonal map (both loops nontrivial)
-- ============================================================
-- The diagonal map sends both loops to loop.
-- Here the surface obligation is genuinely non-degenerate.

-- For the diagonal, we would need:
--   f(point)  = base
--   f(line1)  = loop
--   f(line2)  = loop
--   f(square) = a 2-path witnessing  loop . loop == loop . loop
--
-- The type of f(square) is:
--   PathP (\i -> loop i == loop i) loop loop
--
-- This is a square in S^1 whose boundary is (loop, loop, loop, loop).
-- Filling this square requires understanding the FULL path algebra
-- of S^1, including that loop commutes with itself up to homotopy.
--
-- This is a genuine depth-2 obligation:
--   - The square comes from T^2's surf (L_n)
--   - The paths loop are from S^1 (L_{n-1})
--   - The commutativity proof uses S^1's groupoid laws, which
--     were established using L_{n-2}
--
-- In fact, S^1 is a K(Z,1), so pi_2(S^1) = 0, meaning this
-- square has a UNIQUE filler. But finding that filler still
-- requires the depth-2 analysis.

-- ============================================================
-- Part 6: Why no depth-3 obligation arises
-- ============================================================
--
-- Could there be a FOURTH type of obligation beyond the 4 listed?
--
-- T^2 has no 3-cells (C_3 = emptyset), so the eliminator doesn't
-- ask for 3-dimensional data. But even if we considered the
-- coherence of the surface datum itself, we would be asking:
--
--   "Is the 2-path f(surf) compatible with the pentagon identity
--    for path composition in S^1?"
--
-- This would be a dimension-3 condition referencing:
--   - f(surf)   (our depth-2 datum)
--   - The pentagon identity for S^1's path composition
--
-- But by Mac Lane / Stasheff coherence, the pentagon identity
-- for infinity-groupoid path composition holds AUTOMATICALLY.
-- It is not an independent obligation. The space of ways to
-- satisfy it is contractible (a proposition), so it adds no
-- information.
--
-- Furthermore, any reference to L_{n-2} in this hypothetical
-- depth-3 obligation would go through L_{n-1}: the coherence
-- between L_{n-2} and L_{n-1} was already established when
-- L_{n-1} was sealed. We inherit it; we don't re-prove it.
--
-- Maximum depth: 2. No depth-3 obligation arises.

-- ============================================================
-- CONCLUSION
-- ============================================================
--
-- For the torus T^2:
--   - 4 obligations total (1 point + 2 paths + 1 surface)
--   - Maximum depth: 2
--   - Depth breakdown:
--       Depth 0: base    (1 obligation)
--       Depth 1: p, q    (2 obligations)
--       Depth 2: surf    (1 obligation)
--
-- The torus is the MINIMAL complete example:
--   - It has constructors at dimensions 0, 1, and 2
--   - Its surface datum generates a genuine depth-2 obligation
--   - It demonstrates that depth 2 is necessary and sufficient
--
-- The obligation count (4 = 1 + 2 + 1) and the depth bound (2)
-- are exactly as predicted by the Coherence Window Theorem.
-- ============================================================
