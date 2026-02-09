-- | Exact ν oracle (WP 2.1)
--
-- Computes novelty using ALL library atoms at depths 1, 2, and 3,
-- establishing ground truth for whether proof-rank is approximating
-- something real.
--
-- At depth 1, after schemaization (all library refs → "L"), exact and
-- proof-rank schemas should be identical (sanity check). At depth 2,
-- new composition schemas appear — revealing what the latent bonus
-- approximates.

module ExactNu
  ( ExactNuResult(..)
  , allLibraryAtoms
  , enumExactBounded
  , computeExactNuAtDepth
  , computeExactNu
  ) where

import Types
import ProofRank (enumWindowExact, schemaize, normalize, groupBySchema)
import Inhabitation (isNewlyInhabited)
import Enumerate (typesInvolving)
import Independence (isTrivialSchema)
import qualified Data.Set as Set
import Data.List (sortOn)

-- ============================================
-- Data Types
-- ============================================

data ExactNuResult = ExactNuResult
  { enrDepth       :: Int
  , enrRawCount    :: Int                      -- newly inhabited types (pre-schema)
  , enrSchemaCount :: Int                      -- distinct non-trivial schemas
  , enrSchemas     :: [(TypeExpr, [TypeExpr])] -- (schema, concrete members)
  , enrPerDepth    :: [(Int, Int, Int)]        -- (depth, raw_count, schema_count)
  } deriving (Show)

-- ============================================
-- Atom Collection
-- ============================================

-- | All library atoms — the key difference from proof-rank's windowAtomsD
-- which takes only the last d entries. This uses the FULL library.
allLibraryAtoms :: LibraryEntry -> Library -> [TypeExpr]
allLibraryAtoms candidate lib =
  let candidateRef = TRef (leName candidate)
      libRefs = map (TRef . leName) lib
      all_ = [TUnit, TVoid, candidateRef] ++ libRefs
  in Set.toList (Set.fromList all_)

-- ============================================
-- Set-Based Enumeration
-- ============================================

-- | Set-based deduplication wrapper around enumWindowExact.
-- Replaces O(n^2) nub in enumWindowBounded with O(n log n) Set.fromList.
enumExactBounded :: [TypeExpr] -> Library -> Int -> [TypeExpr]
enumExactBounded atoms lib maxD =
  Set.toList $ Set.unions
    [Set.fromList (enumWindowExact atoms lib d_) | d_ <- [0..maxD]]

-- ============================================
-- Core Algorithm
-- ============================================

-- | Compute exact ν at a single depth.
-- Returns (rawCount, schemaCount, sortedGroups).
computeExactNuAtDepth :: LibraryEntry -> Library -> Int
                      -> (Int, Int, [(TypeExpr, [TypeExpr])])
computeExactNuAtDepth candidate lib depth_ =
  let candidateName = leName candidate
      atoms = allLibraryAtoms candidate lib
      fullLib = candidate : lib

      -- Step 1: enumerate all types up to depth
      allTypes = enumExactBounded atoms fullLib depth_

      -- Step 2: filter for types mentioning candidate
      relevant = typesInvolving candidateName allTypes

      -- Step 3: normalize and deduplicate via Set
      normalized = Set.toList $ Set.fromList $ map normalize relevant

      -- Step 4: filter out trivial types
      nonTrivial = filter (\t -> t /= TUnit && t /= TVoid) normalized

      -- Step 5: keep only newly inhabited
      newlyInh = filter (\t -> isNewlyInhabited t candidate lib) nonTrivial
      rawCount = length newlyInh

      -- Step 6: schemaize and group
      typeSchemas = [(t, normalize (schemaize candidateName lib t)) | t <- newlyInh]
      schemaGroups = groupBySchema typeSchemas

      -- Step 7: filter trivial schemas
      nonTrivialSchemas = filter (not . isTrivialSchema . fst) schemaGroups

      -- Sort by group size descending
      sorted = sortOn (negate . length . snd) nonTrivialSchemas
      schemaCount = length sorted

  in (rawCount, schemaCount, sorted)

-- | Compute exact ν up to maxDepth, collecting per-depth breakdowns.
computeExactNu :: LibraryEntry -> Library -> Int -> ExactNuResult
computeExactNu candidate lib maxDepth =
  let perDepth = [ let (raw, sc, _) = computeExactNuAtDepth candidate lib d_
                   in (d_, raw, sc)
                 | d_ <- [1..maxDepth] ]
      -- Use the max depth for the full result
      (rawFull, scFull, schemasFull) = computeExactNuAtDepth candidate lib maxDepth
  in ExactNuResult
    { enrDepth       = maxDepth
    , enrRawCount    = rawFull
    , enrSchemaCount = scFull
    , enrSchemas     = schemasFull
    , enrPerDepth    = perDepth
    }
