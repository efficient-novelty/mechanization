{-# LANGUAGE BangPatterns #-}

-- | Bounded type enumeration for PEN information-theoretic framework
--
-- This module enumerates all type expressions up to a given complexity bound.
-- Used for computing Shannon surprise ν as the count of newly inhabited types.

module Enumerate
  ( enumerateExact
  , enumerateBounded
  , atoms
  , enumeratePrograms
  , allPrograms
  , allTypes
  , typesInvolving
  , countByComplexity
  , filterTypes
  , allProgramsGated
  ) where

import Types
import Data.List (nub)

-- ============================================
-- Type Enumeration
-- ============================================

-- | Enumerate all type expressions of EXACTLY complexity k using library L
enumerateExact :: Library -> Int -> [TypeExpr]
enumerateExact lib k
  | k < 1     = []
  | k == 1    = atoms lib
  | otherwise = unaryOps lib k ++ binaryOps lib k

-- | Enumerate all type expressions of complexity <= k
enumerateBounded :: Library -> Int -> [TypeExpr]
enumerateBounded lib k = concatMap (enumerateExact lib) [1..k]

-- | Base cases: complexity 1 expressions
atoms :: Library -> [TypeExpr]
atoms lib = TUnit : TVoid : map (TRef . leName) lib

-- | Unary operations: Ω, Susp, etc.
-- Complexity = 1 + complexity of argument
unaryOps :: Library -> Int -> [TypeExpr]
unaryOps lib k = do
  let args = enumerateBounded lib (k - 1)
  arg <- args
  if complexity arg == k - 1
    then [TOmega arg, TSusp arg, TSelfId arg]
    else []

-- | Binary operations: →, ×, +, etc.
-- Complexity = 1 + sum of argument complexities
binaryOps :: Library -> Int -> [TypeExpr]
binaryOps lib k = do
  let maxArg = k - 2  -- Need at least 1 for each arg plus 1 for op
  i <- [1..maxArg]
  let j = k - 1 - i
  if j >= 1
    then do
      a <- enumerateExact lib i
      b <- enumerateExact lib j
      [TArrow a b, TProd a b, TCoprod a b]
    else []

-- ============================================
-- Smarter Enumeration (avoids duplicates)
-- ============================================

-- | Enumerate types using memoization for efficiency
enumerateMemo :: Library -> Int -> [[TypeExpr]]
enumerateMemo lib maxK = go 1
  where
    go k
      | k > maxK  = []
      | otherwise = enumerateExact lib k : go (k + 1)

-- | Get all unique types up to complexity k
allTypes :: Library -> Int -> [TypeExpr]
allTypes lib k = nub $ enumerateBounded lib k

-- ============================================
-- Type Program Enumeration (for κ)
-- ============================================

-- | Enumerate all type programs of EXACTLY cost c
enumeratePrograms :: Library -> Int -> [TypeProgram]
enumeratePrograms lib c
  | c < 1     = []
  | c == 1    = atomPrograms lib
  | otherwise = unaryPrograms lib c ++ binaryPrograms lib c ++ hitPrograms c

-- | Base programs: cost 1
atomPrograms :: Library -> [TypeProgram]
atomPrograms lib = PLitUnit : PLitVoid :
  map (PRef . leName) lib ++
  [PTypeFormerPi, PTypeFormerSigma, PTypeFormerId]

-- | Unary program operations
unaryPrograms :: Library -> Int -> [TypeProgram]
unaryPrograms lib c = do
  arg <- enumeratePrograms lib (c - 1)
  [PSusp arg, POmega arg, PDeloop arg]

-- | Binary program operations
binaryPrograms :: Library -> Int -> [TypeProgram]
binaryPrograms lib c = do
  let maxArg = c - 2
  i <- [1..maxArg]
  let j = c - 1 - i
  if j >= 1
    then do
      a <- enumeratePrograms lib i
      b <- enumeratePrograms lib j
      [PArrow a b, PProd a b, PCoprod a b, PFiber a b]
    else []

-- | HIT programs with various point/path combinations
hitPrograms :: Int -> [TypeProgram]
hitPrograms c
  | c < 2     = []
  | c == 2    = [PMakeHIT 1 []]       -- 1 point, no paths
  | c == 3    = [PMakeHIT 1 [1], PMakeHIT 2 []]  -- 1 pt + 1-path, or 2 pts
  | c == 4    = [PMakeHIT 1 [2], PMakeHIT 1 [1,1], PMakeHIT 2 [1]]
  | otherwise = []  -- Larger HITs not commonly needed

-- | Enumerate all programs up to cost c
allPrograms :: Library -> Int -> [TypeProgram]
allPrograms lib c = concatMap (enumeratePrograms lib) [1..c]

-- ============================================
-- Filtering and Analysis
-- ============================================

-- | Filter types by a predicate
filterTypes :: (TypeExpr -> Bool) -> [TypeExpr] -> [TypeExpr]
filterTypes = filter

-- | Count types at each complexity level
countByComplexity :: [TypeExpr] -> [(Int, Int)]
countByComplexity types =
  let maxC = maximum (0 : map complexity types)
  in [(k, length $ filter ((== k) . complexity) types) | k <- [1..maxC]]

-- | Get types that involve a specific library type
typesInvolving :: String -> [TypeExpr] -> [TypeExpr]
typesInvolving name = filter (involves name)
  where
    involves n TUnit = False
    involves n TVoid = False
    involves n (TRef s) = s == n
    involves n (TArrow a b) = involves n a || involves n b
    involves n (TProd a b) = involves n a || involves n b
    involves n (TCoprod a b) = involves n a || involves n b
    involves n (TId a x y) = involves n a || involves n x || involves n y
    involves n (TSelfId a) = involves n a
    involves n (TOmega a) = involves n a
    involves n (TSusp a) = involves n a
    involves n (TTrunc _ a) = involves n a
    involves n (TPi _ a b) = involves n a || involves n b
    involves n (TSigma _ a b) = involves n a || involves n b
    involves n (THIT _ _) = False
    involves n (TFiber a b) = involves n a || involves n b
    involves n (TDeloop a) = involves n a
    -- Modal operators
    involves n (TFlat a) = involves n a
    involves n (TSharp a) = involves n a
    involves n (TDisc a) = involves n a
    involves n (TPiCoh a) = involves n a
    -- Temporal operators
    involves n (TNext a) = involves n a
    involves n (TEventually a) = involves n a
    -- Differential/Axiomatic
    involves n (TInf a) = involves n a
    involves n (TTangent a) = involves n a
    involves n (TConnection a) = involves n a
    involves n (TCurvature a) = involves n a
    involves n (TMetric a) = involves n a
    involves n (THilbert a) = involves n a

-- ============================================
-- Gated Program Enumeration (for K-based novelty)
-- ============================================

-- | Enumerate programs with gated atoms/ops:
--   - PTypeFormerPi/Sigma/Id are NOT free atoms; they're only reachable via PRef when in library
--   - PTrunc 0 only available when "Trunc" is in library names
--   - Memoized by cost level to avoid redundant recursion
allProgramsGated :: Library -> Int -> [TypeProgram]
allProgramsGated lib maxC = concatMap (levels !!) [1..maxC]
  where
    libNames = map leName lib
    hasTrunc = "Trunc" `elem` libNames

    -- Lazy memoization: levels !! k gives programs of exactly cost k
    levels :: [[TypeProgram]]
    levels = map lvl [0..maxC]

    lvl :: Int -> [TypeProgram]
    lvl 0 = []
    lvl 1 = PLitUnit : PLitVoid : map (PRef . leName) lib
    lvl k = unaryAt k ++ binaryAt k ++ hitPrograms k

    unaryAt :: Int -> [TypeProgram]
    unaryAt k =
      let args = levels !! (k - 1)
      in concat
           [ [PSusp arg | arg <- args]
           , [POmega arg | arg <- args]
           , [PDeloop arg | arg <- args]
           , [PTrunc 0 arg | arg <- args, hasTrunc]
           ]

    binaryAt :: Int -> [TypeProgram]
    binaryAt k = do
      i <- [1 .. k - 2]
      let j = k - 1 - i
      if j >= 1
        then do
          a <- levels !! i
          b <- levels !! j
          [PArrow a b, PProd a b, PCoprod a b, PFiber a b]
        else []
