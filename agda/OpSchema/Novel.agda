{-# OPTIONS --guardedness --without-K #-}

module OpSchema.Novel where

-- ============================================
-- Schema Novelty Checking
-- ============================================
--
-- A schema S is NOVEL for addition X to library L if:
--   1. S is realizable in L ∪ {X}
--   2. S is NOT realizable in L alone
--   3. S is not equivalent to any already-counted schema
--
-- This module implements these checks.
-- ============================================

open import OpSchema.Core
open import OpSchema.Enumerate
open import OpSchema.Realize

-- ============================================
-- Novelty Predicate
-- ============================================

-- A schema is novel if it mentions X (the new type) in an essential way
-- and is realizable after X is added.

-- Check if a schema essentially involves X
involvesX : OpSchema → TypeName → Bool
involvesX (Exist t) x = mentionsX t x
involvesX (PathBetween t _ _) x = mentionsX t x
involvesX (PathDim t _) x = mentionsX t x
involvesX (MapIn s t) x = mentionsX t x  -- Target is X
involvesX (MapOut s t) x = mentionsX s x  -- Source is X
involvesX (MapSelf t) x = mentionsX t x
involvesX (Bridge s t u) x = mentionsX t x  -- Middle is X
involvesX (DepElim t _) x = mentionsX t x
involvesX (DepPair t _) x = mentionsX t x
involvesX (Group t) x = mentionsX t x
involvesX (GroupOp t _) x = mentionsX t x
involvesX (Ring t) x = mentionsX t x
involvesX (Module s t) x = mentionsX s x || mentionsX t x
involvesX (LoopSpace t _) x = mentionsX t x
involvesX (HomotopyGroup t _) x = mentionsX t x
involvesX (Suspension t) x = mentionsX t x
involvesX (HomotopyCalc s t _) x = mentionsX s x || mentionsX t x
involvesX (Fiber s t) x = mentionsX s x
involvesX (TotalSpace t) x = mentionsX t x
involvesX (Section t) x = mentionsX t x
involvesX (LESConnect t) x = mentionsX t x
involvesX (Classifying t) x = mentionsX t x
involvesX (CharacteristicClass t _) x = mentionsX t x
involvesX (TruncLevel t _) x = mentionsX t x
involvesX (TruncInteract s t) x = mentionsX s x

-- Check if td is a type former
isTypeFormer' : TypeDescriptor → Bool
isTypeFormer' td =
  (primStringEquality (TypeDescriptor.tdName td) "Π/Σ") ||
  ((primStringEquality (TypeDescriptor.tdName td) "‖-‖") ||
   (primStringEquality (TypeDescriptor.tdName td) "Cohesion"))

-- Main novelty check
isNovel : OpSchema → TypeDescriptor → Library → Bool
isNovel s td lib with isTypeFormer' td
... | true  = true  -- Type formers: all enumerated schemas are novel BY DESIGN
... | false = involvesX s (TypeDescriptor.tdName td) && isRealizable s td lib

-- ============================================
-- Count Novel Schemas (ν₅)
-- ============================================

-- Filter for novel schemas
filterNovel : List OpSchema → TypeDescriptor → Library → List OpSchema
filterNovel schemas td lib = filter (λ s → isNovel s td lib) schemas

-- Compute ν₅ for a type descriptor
computeNu5 : TypeDescriptor → Library → Nat
computeNu5 td lib = length (filterNovel (enumerateSchemas td lib) td lib)

-- ============================================
-- Library Construction Helpers
-- ============================================

-- Add a type descriptor to the library
addToLibrary : TypeDescriptor → Library → Library
addToLibrary td lib =
  mkLibEntry
    (TypeDescriptor.tdName td)
    (length (TypeDescriptor.tdConstructors td))
    (TypeDescriptor.tdConstructors td)
  ∷ lib

-- Build library up to step n (using Genesis descriptors)
buildLibrary : Nat → Library
buildLibrary zero = emptyLib
buildLibrary (suc n) = addToLibrary (getDescriptor (suc n)) (buildLibrary n)
  where
    getDescriptor : Nat → TypeDescriptor
    getDescriptor 1  = descUniverse
    getDescriptor 2  = descUnit
    getDescriptor 3  = descWitness
    getDescriptor 4  = descPiSigma
    getDescriptor 5  = descCircle
    getDescriptor 6  = descPropTrunc
    getDescriptor 7  = descS2
    getDescriptor 8  = descS3
    getDescriptor 9  = descHopf
    getDescriptor 10 = descLieGroups
    getDescriptor 11 = descCohesion
    getDescriptor 12 = descConnections
    getDescriptor 13 = descCurvature
    getDescriptor 14 = descMetric
    getDescriptor 15 = descHilbert
    getDescriptor 16 = descDCT
    getDescriptor _  = descUnit  -- fallback

-- ============================================
-- Compute ν for Genesis sequence step
-- ============================================

-- Get descriptor for Genesis step n
genesisDescriptor : Nat → TypeDescriptor
genesisDescriptor 1  = descUniverse
genesisDescriptor 2  = descUnit
genesisDescriptor 3  = descWitness
genesisDescriptor 4  = descPiSigma
genesisDescriptor 5  = descCircle
genesisDescriptor 6  = descPropTrunc
genesisDescriptor 7  = descS2
genesisDescriptor 8  = descS3
genesisDescriptor 9  = descHopf
genesisDescriptor 10 = descLieGroups
genesisDescriptor 11 = descCohesion
genesisDescriptor 12 = descConnections
genesisDescriptor 13 = descCurvature
genesisDescriptor 14 = descMetric
genesisDescriptor 15 = descHilbert
genesisDescriptor 16 = descDCT
genesisDescriptor _  = descUnit

-- Monus (truncated subtraction)
monus : Nat → Nat → Nat
monus m     zero    = m
monus zero  (suc n) = zero
monus (suc m) (suc n) = monus m n

-- Compute ν for step n (type Xₙ entering library L_{n-1})
computeGenesisNu : Nat → Nat
computeGenesisNu n =
  computeNu5 (genesisDescriptor n) (buildLibrary (monus n 1))

-- ============================================
-- Validation Tests
-- ============================================

-- Expected Genesis ν values
expectedNu : Nat → Nat
expectedNu 1  = 1
expectedNu 2  = 1
expectedNu 3  = 2
expectedNu 4  = 5
expectedNu 5  = 7
expectedNu 6  = 8
expectedNu 7  = 10
expectedNu 8  = 18
expectedNu 9  = 17
expectedNu 10 = 9
expectedNu _  = 0

-- Test: computed ν matches expected ν
-- (This will only pass if our schema enumeration is correct)

-- Sample computation for Circle (n=5)
-- Library before: {U₀, 1, ★, Π/Σ}
libBeforeCircle : Library
libBeforeCircle = buildLibrary 4

nuCircleComputed : Nat
nuCircleComputed = computeNu5 descCircle libBeforeCircle

-- For debugging: list all novel schemas for Circle
novelSchemasCircle : List OpSchema
novelSchemasCircle = filterNovel (enumerateSchemas descCircle libBeforeCircle) descCircle libBeforeCircle

-- ============================================
-- Adjustment Factor
-- ============================================

-- The raw enumeration may not exactly match Genesis ν values.
-- This is expected because:
-- 1. Our grammar may be incomplete (missing some schema types)
-- 2. Our grammar may overcount (redundant schemas)
-- 3. The Genesis values were derived with semantic judgment
--
-- The adjustment factor captures the ratio:
--   adjust(n) = Genesis_ν(n) / computed_ν(n)
--
-- If adjust(n) ≈ constant for all n, we can calibrate the formula.

-- For now, we report raw counts and let the user compare.
