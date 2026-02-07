{-# OPTIONS --guardedness --without-K #-}

module OpSchema where

-- ============================================
-- The Operation Schema Framework
-- ============================================
--
-- This is the main entry point for the OpSchema framework.
-- It provides:
--   - Core.agda: The OpSchema grammar (AST)
--   - Enumerate.agda: Schema enumeration
--   - Realize.agda: Realizability checking
--   - Novel.agda: Novelty checking and ν computation
--
-- Usage:
--   open import OpSchema
--
--   -- Compute ν for S¹ entering library {U, 1, ★, Π/Σ}
--   nu-circle : Nat
--   nu-circle = computeGenesisNu 5
-- ============================================

open import OpSchema.Core public
open import OpSchema.Enumerate public
open import OpSchema.Realize public
open import OpSchema.Novel public

-- ============================================
-- Convenience Functions
-- ============================================

-- Report: Compute ν for all Genesis steps 1-10
reportAllNu : List Nat
reportAllNu = map computeGenesisNu (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ 7 ∷ 8 ∷ 9 ∷ 10 ∷ [])

-- Compare computed vs expected
-- Returns list of computed values (compare manually with expectedNu)
reportComputed : List Nat
reportComputed = map computeGenesisNu (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ 7 ∷ 8 ∷ 9 ∷ 10 ∷ [])

-- Expected values for reference
reportExpected : List Nat
reportExpected = map expectedNu (1 ∷ 2 ∷ 3 ∷ 4 ∷ 5 ∷ 6 ∷ 7 ∷ 8 ∷ 9 ∷ 10 ∷ [])

-- ============================================
-- Quick Tests
-- ============================================

-- Test: Universe (n=1, expected ν=1)
test-nu-1 : Nat
test-nu-1 = computeGenesisNu 1

-- Test: Unit (n=2, expected ν=1)
test-nu-2 : Nat
test-nu-2 = computeGenesisNu 2

-- Test: Witness (n=3, expected ν=2)
test-nu-3 : Nat
test-nu-3 = computeGenesisNu 3

-- Test: Π/Σ (n=4, expected ν=5)
test-nu-4 : Nat
test-nu-4 = computeGenesisNu 4

-- Test: Circle (n=5, expected ν=7)
test-nu-5 : Nat
test-nu-5 = computeGenesisNu 5

-- ============================================
-- Debugging: Show schemas for a step
-- ============================================

-- All schemas enumerated for Circle
allSchemasCircle : List OpSchema
allSchemasCircle = enumerateSchemas descCircle (buildLibrary 4)

-- Novel schemas for Circle
novelCircle : List OpSchema
novelCircle = filterNovel allSchemasCircle descCircle (buildLibrary 4)

-- Count
countNovelCircle : Nat
countNovelCircle = length novelCircle
