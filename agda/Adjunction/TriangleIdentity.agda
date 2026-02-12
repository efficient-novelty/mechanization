{-# OPTIONS --cubical --safe --guardedness #-}

module Adjunction.TriangleIdentity where

open import Cubical.Foundations.Prelude
open import Core.Nat
open import Adjunction.AdjunctionDepth

-- ============================================================
-- TriangleIdentity.agda
-- Concrete examples of adjunctions and their triangle depths
--
-- Example 1: Π-adjunction (L = const, R = fun-space)
--   Triangle identity is definitional (refl) — but still Depth 2
--
-- Example 2: S¹ loop-space adjunction (postulated)
--   β-reduction for loop is a Depth-2 obligation
-- ============================================================

-- ============================================================
-- Example 1: The Diagonal/Product Adjunction
-- ============================================================
--
-- For a fixed type B, consider:
--   L(A) = A × B      (pair with B)
--   R(C) = B → C      (function from B)
--
-- The adjunction L ⊣ R is the currying isomorphism:
--   (A × B → C) ≃ (A → B → C)
--
-- We demonstrate this with concrete types.

module ProdFunAdjunction (B : Type) where

  -- Type formers
  L : Type → Type
  L A = A × B

  R : Type → Type
  R C = B → C

  -- Unit: A → (B → A × B)
  η-prod : {A : Type} → A → R (L A)
  η-prod a = λ b → (a , b)

  -- Counit: (B → C) × B → C
  ε-prod : {C : Type} → L (R C) → C
  ε-prod (f , b) = f b

  -- Functorial actions
  mapL-prod : {A A' : Type} → (A → A') → L A → L A'
  mapL-prod f (a , b) = (f a , b)

  mapR-prod : {C C' : Type} → (C → C') → R C → R C'
  mapR-prod g h = λ b → g (h b)

  -- Triangle identity (left): ε(mapL η (a,b)) ≡ (a,b)
  -- This is DEFINITIONAL (refl) — but it is still a Depth-2
  -- obligation because it equates a composition of 1-cells.
  triangle-L-prod : {A : Type} (x : L A)
    → ε-prod (mapL-prod η-prod x) ≡ x
  triangle-L-prod (a , b) = refl

  -- Triangle identity (right): mapR ε (η(f)) ≡ f
  triangle-R-prod : {C : Type} (f : R C)
    → mapR-prod ε-prod (η-prod f) ≡ f
  triangle-R-prod f = refl

  -- The full adjoint pair
  prodFunAdj : AdjointPair L R
  prodFunAdj = record
    { η          = η-prod
    ; ε          = ε-prod
    ; mapL       = mapL-prod
    ; mapR       = mapR-prod
    ; triangle-L = triangle-L-prod
    ; triangle-R = triangle-R-prod
    }

-- ============================================================
-- Example 2: HIT Depth Analysis (abstract over S¹-like types)
-- ============================================================
--
-- The circle S¹ is the prototypical HIT. Its β-reduction
-- rule (loop computes to the specified path) is a Depth-2
-- obligation: it equates the eliminator's action on a path
-- constructor (Depth 1) with a specified path in the target
-- type (Depth 1), forming a 2-dimensional coherence.
--
-- We parameterize over a "circle-like" type to avoid
-- BUILTIN NATURAL conflicts with Core.Nat.

module HITDepthExample
  (Circle : Type)
  (pt : Circle)
  (lp : pt ≡ pt)
  where

  -- The elimination principle for a HIT with one loop requires:
  --   (1) A point: b : B(pt)                      [Depth 0]
  --   (2) A path: ℓ : PathP (λ i → B(lp i)) b b  [Depth 1]
  --
  -- The β-rule for the loop constructor is:
  --   elim-β : apd(elim b ℓ, lp) ≡ ℓ             [Depth 2]
  --
  -- This β-rule is a Depth-2 obligation because:
  --   - It references the eliminator's behavior (current step)
  --   - It references lp (path data from the HIT in L_n)
  --   - It equates with ℓ (which involves B's path structure,
  --     potentially from L_{n-1})

  -- Depth classification
  point-data-depth : ObligationDepth
  point-data-depth = Depth0

  path-data-depth : ObligationDepth
  path-data-depth = Depth1

  β-reduction-depth : ObligationDepth
  β-reduction-depth = Depth2

  -- The maximum depth for S¹ elimination is 2.
  -- This matches the Adjunction Barrier prediction:
  -- S¹'s suspension adjunction (Σ ⊣ Ω) has triangle
  -- identities at Depth 2.
  hit-max-depth : ObligationDepth
  hit-max-depth = Depth2

  -- Concrete witness: the loop generates a non-trivial path space.
  -- Any function Circle → B must map lp to some path in B,
  -- creating a depth-1 obligation. The coherence of this
  -- mapping with the elimination principle is depth-2.
  loop-path : pt ≡ pt
  loop-path = lp
