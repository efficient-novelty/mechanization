{-# LANGUAGE BangPatterns #-}

-- | Proof-rank computation (depth-2 enumeration + schema counting)
--
-- Implements the proof-rank ν as the number of distinct type schemas among
-- newly inhabited types at expression depth ≤ d.
--
-- A "type schema" abstracts over specific library atoms: all library types
-- are replaced by a generic "L", the new type by "X", and Ω(T)≡SelfId(T).
-- Commutative operations (×, +) are canonicalized by sorting operands.
--
-- This captures the pencil calculation's insight: ν counts independent
-- "proof technique generators" (rank, not cardinality). Two types that
-- differ only in which library atom appears (e.g., star→S1 vs Pi→S1)
-- represent the same proof technique (const function to S1).
--
-- Key design decisions (v0.5):
--   1. Atoms restricted to 2-step window {X, R_{n-1}, R_{n-2}}
--   2. Types normalized (unit erasure, void absorption, 1→A≃A)
--   3. Schema abstraction: library atoms → L, new type → X, Ω→SelfId
--   4. Commutativity: A×B and B×A map to same schema, likewise A+B and B+A
--   5. ν = number of distinct schemas = rank of novelty module

module ProofRank
  ( depth
  , normalize
  , windowAtoms
  , enumWindowBounded
  , newlyInhabitedWindow
  , proofRank
  , derivableStructural
  , buildCostMap
  , kNovelty
  ) where

import Types
import Enumerate (typesInvolving, allProgramsGated)
import Inhabitation (isNewlyInhabited, checkInhab, isInhabited)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map

-- ============================================
-- Expression Depth
-- ============================================

depth :: TypeExpr -> Int
depth TUnit = 0
depth TVoid = 0
depth (TRef _) = 0
depth (TArrow a b) = 1 + max (depth a) (depth b)
depth (TProd a b) = 1 + max (depth a) (depth b)
depth (TCoprod a b) = 1 + max (depth a) (depth b)
depth (TId a x y) = 1 + maximum [depth a, depth x, depth y]
depth (TSelfId a) = 1 + depth a
depth (TOmega a) = 1 + depth a
depth (TSusp a) = 1 + depth a
depth (TTrunc _ a) = 1 + depth a
depth (TPi _ a b) = 1 + max (depth a) (depth b)
depth (TSigma _ a b) = 1 + max (depth a) (depth b)
depth (THIT _ _) = 0
depth (TFiber a b) = 1 + max (depth a) (depth b)
depth (TDeloop a) = 1 + depth a

-- ============================================
-- Type Normalization (Section 1.3b of plan)
-- ============================================

-- | Normalize a type expression to canonical form.
-- Includes HoTT isomorphisms: A×1≃A, A+0≃A, 0→A≃1, A→1≃1, 1→A≃A.
normalize :: TypeExpr -> TypeExpr
normalize t = let t' = normStep t
              in if t' == t then t else normalize t'
  where
    -- Resolve library names to canonical constructors
    normStep (TRef "1") = TUnit              -- Library "1" IS unit
    normStep (TRef "0") = TVoid              -- Library "0" IS void
    -- Product with unit
    normStep (TProd a TUnit) = normalize a
    normStep (TProd TUnit b) = normalize b
    -- Product with void (absorption)
    normStep (TProd _ TVoid) = TVoid
    normStep (TProd TVoid _) = TVoid
    -- Coproduct with void
    normStep (TCoprod a TVoid) = normalize a
    normStep (TCoprod TVoid b) = normalize b
    -- Arrow to/from unit/void
    normStep (TArrow _ TUnit) = TUnit       -- A → 1 ≃ 1
    normStep (TArrow TVoid _) = TUnit       -- 0 → A ≃ 1
    normStep (TArrow TUnit b) = normalize b -- 1 → A ≃ A (evaluation at ★)
    -- SelfId/Omega of trivial types
    normStep (TSelfId TUnit) = TUnit
    normStep (TSelfId TVoid) = TVoid
    normStep (TOmega TUnit) = TUnit         -- Ω(1) ≃ 1 (contractible)
    normStep (TOmega TVoid) = TVoid         -- Ω(0) ≃ 0 (empty)
    normStep (TSusp TVoid) = TUnit
    -- Recursive normalization
    normStep (TArrow a b) = TArrow (normalize a) (normalize b)
    normStep (TProd a b) = TProd (normalize a) (normalize b)
    normStep (TCoprod a b) = TCoprod (normalize a) (normalize b)
    normStep (TSelfId a) = TSelfId (normalize a)
    normStep (TOmega a) = TOmega (normalize a)
    normStep (TSusp a) = TSusp (normalize a)
    normStep (TTrunc n a) = TTrunc n (normalize a)
    normStep x = x

-- ============================================
-- 2-Step Window Enumeration
-- ============================================

windowAtoms :: LibraryEntry -> Library -> [TypeExpr]
windowAtoms newType lib =
  let windowEntries = take 2 (reverse lib)
      windowRefs = map (TRef . leName) windowEntries
      newRef = TRef (leName newType)
  in nub $ [TUnit, TVoid, newRef] ++ windowRefs

availableFormers :: Library -> [String]
availableFormers lib =
  let names = map leName lib
      base = ["Arrow", "Prod", "Coprod", "SelfId"]
      withOmega = if any leHasLoop lib then "Omega" : base else base
      withSusp  = if length lib >= 5 then "Susp" : withOmega else withOmega
      withTrunc = if "Trunc" `elem` names then "Trunc" : withSusp else withSusp
  in withTrunc

enumWindowExact :: [TypeExpr] -> Library -> Int -> [TypeExpr]
enumWindowExact windowAts lib d
  | d < 0 = []
  | d == 0 = windowAts
  | otherwise = nub $ unaryOps ++ binaryOps
  where
    sub = enumWindowBounded windowAts lib (d - 1)
    formers = availableFormers lib
    subMaxD = filter (\t -> depth t == d - 1) sub

    unaryOps = concat
      [ [TOmega a | a <- subMaxD, "Omega" `elem` formers]
      , [TSusp a | a <- subMaxD, "Susp" `elem` formers]
      , [TSelfId a | a <- subMaxD]
      ]
    binaryOps = concat
      [ [TArrow a b | a <- sub, b <- sub, max (depth a) (depth b) == d - 1]
      , [TProd a b | a <- sub, b <- sub, max (depth a) (depth b) == d - 1]
      , [TCoprod a b | a <- sub, b <- sub, max (depth a) (depth b) == d - 1]
      ]

enumWindowBounded :: [TypeExpr] -> Library -> Int -> [TypeExpr]
enumWindowBounded windowAts lib d = nub $ concatMap (enumWindowExact windowAts lib) [0..d]

-- ============================================
-- Structural Derivability (kept for reference)
-- ============================================

-- | Structural derivability (not used for clustering in v0.5,
-- but kept for analysis and debugging).
derivableStructural :: TypeExpr -> TypeExpr -> Library -> Bool
derivableStructural t1 t2 _lib
  | normalEq t1 t2 = True
  | TOmega x <- t1, TSelfId y <- t2, normalEq x y = True
  | TSelfId x <- t1, TOmega y <- t2, normalEq x y = True
  | otherwise = False

-- | Check equality after normalization.
normalEq :: TypeExpr -> TypeExpr -> Bool
normalEq a b = normalize a == normalize b

-- ============================================
-- Schema Abstraction
-- ============================================

-- | Abstract a type expression to a "schema" by:
--   1. Replacing the new type's name with "X"
--   2. Replacing ALL library types with "L" (same proof technique)
--   3. Keeping Ω and SelfId DISTINCT (Ω requires loops, SelfId only needs refl)
--   4. Canonicalizing commutative operations (× and +)
schemaize :: String -> Library -> TypeExpr -> TypeExpr
schemaize newName _lib = canon . go
  where
    go TUnit = TUnit
    go TVoid = TVoid
    go (TRef name)
      | name == newName = TRef "X"
      | otherwise = TRef "L"
    go (TArrow a b) = TArrow (go a) (go b)
    go (TProd a b) = TProd (go a) (go b)
    go (TCoprod a b) = TCoprod (go a) (go b)
    go (TSelfId a) = TSelfId (go a)
    go (TOmega a) = TOmega (go a)  -- Keep Ω distinct from SelfId
    go (TSusp a) = TSusp (go a)
    go (TTrunc n a) = TTrunc n (go a)
    go (TId a x y) = TId (go a) (go x) (go y)
    go (TPi v a b) = TPi v (go a) (go b)
    go (TSigma v a b) = TSigma v (go a) (go b)
    go (THIT p d) = THIT p d
    go (TFiber a b) = TFiber (go a) (go b)
    go (TDeloop a) = TDeloop (go a)

    -- Canonicalize commutative operations by sorting operands
    canon (TProd a b) = let a' = canon a; b' = canon b
                        in if show a' <= show b'
                           then TProd a' b'
                           else TProd b' a'
    canon (TCoprod a b) = let a' = canon a; b' = canon b
                          in if show a' <= show b'
                             then TCoprod a' b'
                             else TCoprod b' a'
    canon (TArrow a b) = TArrow (canon a) (canon b)  -- NOT commutative
    canon (TSelfId a) = TSelfId (canon a)
    canon (TOmega a) = TOmega (canon a)
    canon (TSusp a) = TSusp (canon a)
    canon (TTrunc n a) = TTrunc n (canon a)
    canon x = x

-- ============================================
-- Proof-Rank (v0.5 — schema counting)
-- ============================================

-- | Newly inhabited types at depth ≤ d using the 2-step window.
-- Uses the FULL library (including newType) for available formers.
newlyInhabitedWindow :: LibraryEntry -> Library -> Int -> [TypeExpr]
newlyInhabitedWindow newType lib d =
  let windowAts = windowAtoms newType lib
      fullLib = newType : lib  -- Include new type for former availability
      types = enumWindowBounded windowAts fullLib d
      relevant = typesInvolving (leName newType) types
      normalized = nub $ map normalize relevant
      nonUnit = filter (/= TUnit) normalized
      nonVoid = filter (/= TVoid) nonUnit
  in filter (\t -> isNewlyInhabited t newType lib) nonVoid

-- | Compute proof-rank as the number of distinct type schemas.
-- Each schema = one independent "proof technique."
-- ν = number of distinct schemas = rank of the novelty module.
--
-- Returns (ν, clusters) where each "cluster" is the list of concrete
-- types that map to the same schema. Sorted by cluster size descending.
proofRank :: LibraryEntry -> Library -> Int -> (Int, [[TypeExpr]])
proofRank newType lib d =
  let newTypes = newlyInhabitedWindow newType lib d
      name = leName newType
      -- Compute schema for each newly inhabited type
      typeSchemas = [(t, normalize (schemaize name lib t)) | t <- newTypes]
      -- Group by schema
      schemaGroups = groupBySchema typeSchemas
      -- Sort by group size descending
      sorted = sortOn (negate . length . snd) schemaGroups
      clusters = map snd sorted
  in (length clusters, clusters)

-- | Group types by their schema, returning (schema, [types]) pairs.
groupBySchema :: [(TypeExpr, TypeExpr)] -> [(TypeExpr, [TypeExpr])]
groupBySchema pairs =
  let schemas = nub $ map snd pairs
      groups = [(s, [t | (t, s') <- pairs, s' == s]) | s <- schemas]
  in groups

-- ============================================
-- K-Based Novelty (compression improvement)
-- ============================================

-- | Build map from normalized TypeExpr to its minimum program cost.
-- Enumerates all gated programs up to @maxCost@ and records the cheapest
-- cost for each normalised type expression they denote.
buildCostMap :: Library -> Int -> Map.Map TypeExpr Int
buildCostMap lib maxCost =
  let progs = allProgramsGated lib maxCost
      insertProg m p =
        let expr = normalize (programToExpr p)
            cost = programCost p
        in Map.insertWith min expr cost m
  in foldl' insertProg Map.empty progs
  where
    foldl' _ z [] = z
    foldl' f !z (x:xs) = foldl' f (f z x) xs

-- | K-based novelty: count schemas of types whose Kolmogorov complexity
-- drops after adding X, and that are inhabited in B∪{X}.
--
-- Returns (schema count, clusters) like 'proofRank'.
kNovelty :: LibraryEntry -> Library -> Int -> (Int, [[TypeExpr]])
kNovelty newType lib horizon =
  let fullLib   = newType : lib
      costBefore = buildCostMap lib horizon
      costAfter  = buildCostMap fullLib horizon
      defaultK   = horizon + 1

      -- All types reachable after adding X
      allAfterTypes = Map.keys costAfter

      -- Types whose cost improved (or became newly reachable)
      improved = filter isImproved allAfterTypes
        where
          isImproved t =
            let kAfter  = Map.findWithDefault defaultK t costAfter
                kBefore = Map.findWithDefault defaultK t costBefore
            in kAfter < kBefore

      -- Filter to inhabited types (in full library)
      inhabited = filter (\t -> isInhabited (checkInhab t fullLib)) improved

      -- Filter out TUnit and TVoid
      interesting = filter (\t -> t /= TUnit && t /= TVoid) inhabited

      -- Schema abstraction and grouping
      name = leName newType
      typeSchemas = [(t, normalize (schemaize name lib t)) | t <- interesting]
      schemaGroups = groupBySchema typeSchemas
      sorted = sortOn (negate . length . snd) schemaGroups
      clusters = map snd sorted
  in (length clusters, clusters)
