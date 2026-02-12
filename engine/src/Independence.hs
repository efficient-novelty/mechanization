-- | Independence-based novelty filtering
--
-- Enhances ProofRank's schema-counting with a trivially-derivable
-- schema filter. A schema is trivial if its ONLY inhabitants come
-- from generic constructions on any inhabited type (pair, inl, id, refl).
--
-- Algorithm:
--   1. Get newly inhabited types via ProofRank.newlyInhabitedWindow at depth 1
--   2. Canonicalize via Equivalence.canonicalize, deduplicate
--   3. Abstract via ProofRank.schemaize, group by schema
--   4. Filter trivially-derivable schemas
--   5. Count remaining groups = nu

module Independence
  ( independenceRank
  , isTrivialSchema
  ) where

import Types
import ProofRank (newlyInhabitedWindow, schemaize)
import Equivalence (canonicalize)
import Data.List (nub, sortOn)

-- ============================================
-- Trivially-Derivable Schema Detection
-- ============================================

-- | A schema is trivially derivable if it uses only basic type algebra
-- (arrows, products, coproducts, identity types) over {X, L, 1, 0}.
-- These schemas are inhabited for ANY two inhabited types X and L via
-- generic constructions: const, pair, inl/inr, id, refl.
--
-- The one exception: bare X is non-trivial — it represents the core
-- existence novelty of adding a new type.
--
-- Non-trivial schemas use structural operations: Omega, Susp, Trunc,
-- Pi, Sigma, flat, sharp, Tangent, Connection, etc.
isTrivialSchema :: TypeExpr -> Bool
isTrivialSchema t
  | t == TRef "X" = False       -- bare X = existence novelty, non-trivial
  | otherwise     = trivClosure t
  where
    trivClosure TUnit              = True
    trivClosure TVoid              = True
    trivClosure (TRef "X")         = True
    trivClosure (TRef "L")         = True
    trivClosure (TArrow a b)       = trivClosure a && trivClosure b
    trivClosure (TProd a b)        = trivClosure a && trivClosure b
    trivClosure (TCoprod a b)      = trivClosure a && trivClosure b
    trivClosure (TSelfId a)        = trivClosure a
    trivClosure (TId a x y)        = trivClosure a && trivClosure x && trivClosure y
    -- Pi/Sigma with all-trivial arguments: same as Arrow/Prod
    -- (non-dependent Pi ≅ Arrow in our model)
    trivClosure (TPi _ a b)        = trivClosure a && trivClosure b
    trivClosure (TSigma _ a b)     = trivClosure a && trivClosure b
    trivClosure _                  = False

-- ============================================
-- Independence Rank
-- ============================================

-- | Compute independence-based novelty rank for a library entry.
--
-- Returns (nu, schema_groups) where:
--   nu = number of non-trivial schema groups
--   schema_groups = list of type groups by schema (non-trivial only)
independenceRank :: LibraryEntry -> Library -> (Int, [[TypeExpr]])
independenceRank newType lib =
  let -- Step 1: Get newly inhabited types at depth 1
      newTypes = newlyInhabitedWindow newType lib 1

      -- Step 2: Canonicalize and deduplicate
      canonTypes = nub $ map canonicalize newTypes

      -- Step 3: Schema abstraction
      name = leName newType
      typeSchemas = [(t, canonicalize (schemaize name lib t)) | t <- canonTypes]

      -- Group by schema
      schemaGroups = groupBySchema typeSchemas

      -- Step 4: Filter trivially-derivable schemas
      nonTrivial = filter (\(schema, _) -> not (isTrivialSchema schema)) schemaGroups

      -- Step 5: Sort by group size descending
      sorted = sortOn (negate . length . snd) nonTrivial
      clusters = map snd sorted

  in (length clusters, clusters)

-- | Group types by their schema, returning (schema, [types]) pairs.
groupBySchema :: [(TypeExpr, TypeExpr)] -> [(TypeExpr, [TypeExpr])]
groupBySchema pairs =
  let schemas = nub $ map snd pairs
      groups = [(s, [t | (t, s') <- pairs, s' == s]) | s <- schemas]
  in groups
