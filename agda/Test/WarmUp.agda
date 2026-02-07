{-# OPTIONS --cubical --safe --guardedness #-}

module Test.WarmUp where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism
open import Cubical.Data.Nat
open import Cubical.Data.Int renaming (_+_ to _+ℤ_)
open import Cubical.HITs.S1

-- ============================================
-- Warm-Up Exercises (Phase 0)
-- ============================================
-- From the implementation plan:
-- "Before touching PEN, implement these in Cubical Agda to build fluency"

-- ============================================
-- Exercise 1: Fibonacci and Sum Identity
-- ============================================

-- Define Fibonacci
fib' : ℕ → ℕ
fib' zero = 1
fib' (suc zero) = 1
fib' (suc (suc n)) = fib' (suc n) + fib' n

-- Prove: Σ fib(i) for i=0..n = fib(n+2) - 1
-- Equivalently: (Σ fib(i)) + 1 = fib(n+2)

fibSum' : ℕ → ℕ
fibSum' zero = fib' zero
fibSum' (suc n) = fib' (suc n) + fibSum' n

-- The identity
fibSum-id : (n : ℕ) → fibSum' n + 1 ≡ fib' (suc (suc n))
fibSum-id zero = refl
fibSum-id (suc n) =
  (fib' (suc n) + fibSum' n) + 1       ≡⟨ +-assoc (fib' (suc n)) (fibSum' n) 1 ⟩
  fib' (suc n) + (fibSum' n + 1)       ≡⟨ cong (fib' (suc n) +_) (fibSum-id n) ⟩
  fib' (suc n) + fib' (suc (suc n))    ≡⟨ +-comm (fib' (suc n)) (fib' (suc (suc n))) ⟩
  fib' (suc (suc n)) + fib' (suc n)    ≡⟨ refl ⟩
  fib' (suc (suc (suc n)))             ∎

-- ============================================
-- Exercise 2: The Circle S¹ and π₁(S¹) ≅ ℤ
-- ============================================

-- The Circle is defined in Cubical.HITs.S1:
-- data S¹ : Type where
--   base : S¹
--   loop : base ≡ base

-- The winding number function
-- (This is the key to proving π₁(S¹) ≅ ℤ)

-- helix : S¹ → Type
-- helix base = ℤ
-- helix (loop i) = sucPathℤ i
-- where sucPathℤ : ℤ ≃ ℤ is the successor equivalence

-- The fundamental group element from an integer
intLoop : ℤ → base ≡ base
intLoop (pos zero) = refl
intLoop (pos (suc n)) = intLoop (pos n) ∙ loop
intLoop (negsuc zero) = sym loop
intLoop (negsuc (suc n)) = intLoop (negsuc n) ∙ sym loop

-- Study: The full proof of π₁(S¹) ≅ ℤ is in
-- Cubical.HITs.S1.Properties

-- ============================================
-- Exercise 3: The Torus T²
-- ============================================

-- The Torus as a HIT:
-- data T² : Type where
--   point : T²
--   pathP : point ≡ point
--   pathQ : point ≡ point
--   surface : pathP ∙ pathQ ≡ pathQ ∙ pathP

-- Constructors: 1 point + 2 paths + 1 surface = 4
-- This matches κ(Torus) = 4 from the paper

-- The Torus can also be defined as S¹ × S¹
-- T² ≃ S¹ × S¹

-- Counting for the eliminator:
-- To define f : T² → A, you need:
-- - f(point) : A
-- - ap f pathP : f(point) ≡ f(point)
-- - ap f pathQ : f(point) ≡ f(point)
-- - ap² f surface : Square (ap f pathP ∙ ap f pathQ)
--                          (ap f pathQ ∙ ap f pathP)

-- ============================================
-- Exercise 4: Reflection API
-- ============================================

-- This exercise requires using Agda.Builtin.Reflection
-- to write a macro that inspects type definitions.

-- See Oracle/Kappa.agda for the implementation sketch.
-- The key function is:
--   countConstructors : Name → TC ℕ
-- which uses getDefinition to inspect a data type.

-- Example usage (when implemented):
-- unquote (countConstructors (quote Bool)) ≡ 2
-- unquote (countConstructors (quote ℕ)) ≡ 2

-- ============================================
-- Summary
-- ============================================

-- Exercise 1: ✓ Implemented above
-- Exercise 2: Study Cubical.HITs.S1.Properties
-- Exercise 3: Study Cubical.HITs.Torus
-- Exercise 4: See Oracle/Kappa.agda

-- Done criterion from plan:
-- "All four exercises compile. You can write a macro that,
--  given a type name, returns the number of constructors as a ℕ."
