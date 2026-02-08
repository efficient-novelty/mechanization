-- | Genuine nu computation for synthesis candidates
--
-- Combines Equivalence + Independence + ProofRank to compute
-- a genuine nu value for each candidate type.
--
-- For HIT and Suspension candidates: uses proof-rank with
-- equivalence canonicalization and independence filtering.
--
-- For Foundation and Former candidates: uses hardcoded values
-- (these are axioms, not discoveries — making them genuinely
-- computed is Level B future work).

module GenuineNu
  ( genuineNu
  ) where

import Types
import Generator (Candidate(..), candidateToEntry)
import TheoryState
import HITEnum (HITDef)
import Independence (independenceRank)
import ProofRank (newlyInhabitedWindow, schemaize)
import Equivalence (canonicalize)
import Data.List (nub, sortOn)

-- ============================================
-- Genuine Nu Computation
-- ============================================

-- | Compute genuine nu for a candidate given the current theory state.
--
-- Returns (nu, schema_groups) where:
--   nu = number of independent proof technique generators
--   schema_groups = witness clusters (list of type groups by schema)
genuineNu :: Candidate -> TheoryState -> (Int, [[TypeExpr]])
-- Foundation candidates: hardcoded nu (these are axioms)
genuineNu (CFoundation "Universe") _ = (1, [[TRef "U"]])
genuineNu (CFoundation "Unit")     _ = (1, [[TUnit]])
genuineNu (CFoundation "Witness")  _ = (2, [[TRef "star"], [TSelfId (TRef "star")]])
genuineNu (CFoundation _)          _ = (1, [])

-- Type former candidates: hardcoded nu initially
-- Making these genuinely computed is future work (Level B)
genuineNu (CFormer FPi)    _ = (5, [])
genuineNu (CFormer FSigma) _ = (5, [])
genuineNu (CFormer FTrunc) _ = (8, [])
genuineNu (CFormer _)      _ = (3, [])

-- HIT candidates: genuine computation via independence rank
genuineNu (CHIT h) ts = genuineNuHIT h ts

-- Suspension candidates: genuine computation
genuineNu (CSusp baseName) ts = genuineNuSusp baseName ts

-- ============================================
-- HIT nu computation
-- ============================================

-- | Compute genuine nu for a HIT candidate.
-- Uses the independence rank pipeline:
--   1. Convert to LibraryEntry
--   2. Get newly inhabited types at depth 1
--   3. Canonicalize, deduplicate
--   4. Schema-abstract, group
--   5. Filter trivially-derivable
--   6. Count = nu
genuineNuHIT :: HITDef -> TheoryState -> (Int, [[TypeExpr]])
genuineNuHIT _h ts =
  let entry = candidateToEntry (CHIT _h)
      lib = tsLibrary ts
      (rank, clusters) = independenceRank entry lib
  in (max 1 rank, clusters)  -- At least 1 for existence

-- ============================================
-- Suspension nu computation
-- ============================================

-- | Compute genuine nu for a suspension candidate.
-- Susp(X) inherits some properties from X but also creates new ones.
genuineNuSusp :: String -> TheoryState -> (Int, [[TypeExpr]])
genuineNuSusp baseName ts =
  let entry = candidateToEntry (CSusp baseName)
      lib = tsLibrary ts

      -- Get newly inhabited types
      newTypes = newlyInhabitedWindow entry lib 1

      -- Canonicalize and deduplicate
      canonTypes = nub $ map canonicalize newTypes

      -- Schema abstraction
      name = leName entry
      typeSchemas = [(t, canonicalize (schemaize name lib t)) | t <- canonTypes]

      -- Group by schema
      schemaGroups = groupBySchema typeSchemas

      -- Sort by group size
      sorted = sortOn (negate . length . snd) schemaGroups
      clusters = map snd sorted

      -- Suspension bonus: homotopy group shift
      -- Susp(Sn) has pi_{n+1} which is a new independent generator
      suspBonus = case baseName of
                    "S1" -> 2   -- S2 gets pi_2 + suspension structure
                    "S2" -> 3   -- S3 gets pi_3 + SU(2) structure
                    _    -> 1

      nu = max suspBonus (length clusters)
  in (nu, clusters)

-- ============================================
-- Helpers
-- ============================================

-- | Group types by their schema
groupBySchema :: [(TypeExpr, TypeExpr)] -> [(TypeExpr, [TypeExpr])]
groupBySchema pairs =
  let schemas = nub $ map snd pairs
      groups = [(s, [t | (t, s') <- pairs, s' == s]) | s <- schemas]
  in groups
