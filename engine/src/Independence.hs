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

-- | A schema is trivially derivable if its ONLY inhabitants come from
-- generic constructions available for any inhabited type:
--   X * X  -> pair(base, base)
--   X + X  -> inl(base)
--   X -> X -> id
--   x =_X x -> refl
isTrivialSchema :: TypeExpr -> Bool
-- pair(base, base) — trivially derivable for any inhabited X
isTrivialSchema (TProd (TRef "X") (TRef "X")) = True
-- inl(base) — trivially derivable for any inhabited X
isTrivialSchema (TCoprod (TRef "X") (TRef "X")) = True
-- id — trivially derivable for any type X
isTrivialSchema (TArrow (TRef "X") (TRef "X")) = True
-- refl — trivially derivable for any inhabited X
isTrivialSchema (TSelfId (TRef "X")) = True
-- Also trivial: L*L, L+L, L->L, SelfId(L) — but these shouldn't appear
-- in newly-inhabited types (they were already inhabited before X).
isTrivialSchema _ = False

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
