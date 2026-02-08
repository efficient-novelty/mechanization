-- | Proof-rank nu via schema clustering + latent capability bonus
--
-- Replaces the hand-tuned bonus system in GenuineNu for HITs and Suspensions.
-- Algorithm:
--   1. Enumerate all types at expression depth <= 1 using candidate + 2-step window
--   2. Filter for newly inhabited (inhabited in L ∪ {X}, not in L)
--   3. Cluster by schema (abstract over library atoms)
--   4. Filter out trivial schemas (X*X, X+X, X->X, SelfId(X))
--   5. Add latent capability bonus for path/homotopy structure
--   6. Return count of non-trivial clusters + bonus
--
-- Investigation result: Pure derivability clustering (the plan's original
-- approach) is too aggressive — it merges almost everything into the
-- existence cluster via const/pair/inl/refl, giving nu=2 for S1.
-- Schema-based clustering is the correct abstraction level: each distinct
-- schema represents a distinct "proof technique interface" even when the
-- underlying terms are derivable from each other.
--
-- The latent capability bonus replaces ALL name-specific bonuses
-- (pathLoopBonus, homotopyBonus, truncBonus, higherBonus, suspBonus,
-- crossBonus) with a single structure-dependent formula:
--   bonus = pathConstructors + (1 if hasLoop) + (truncLevel if Trunc available)
--
-- Key design decisions:
--   - Depth 1 for enumeration (depth 2 causes combinatorial explosion)
--   - Schema-based clustering (not derivability merging)
--   - Bonus depends on candidate STRUCTURE, never on candidate NAME
--   - Don't touch Regimes 2 and 3 (Map, Algebra, Modal, Axiom stay as-is)

module Cluster
  ( proofRankNu
  , proofRankNuD
  , DerivCluster(..)
  ) where

import Types
import ProofRank (windowAtoms, windowAtomsD, enumWindowBounded, schemaize, normalize)
import Inhabitation (isNewlyInhabited)
import Equivalence (canonicalize)
import Data.List (nub)

-- ============================================
-- Data Types
-- ============================================

data DerivCluster = DerivCluster
  { dcRepresentative :: TypeExpr
  , dcMembers        :: [TypeExpr]
  , dcIsTrivial      :: Bool
  } deriving (Show)

-- ============================================
-- Core Algorithm: proofRankNu
-- ============================================

-- | Compute nu via proof-rank using the default d=2 window (backward compatible).
proofRankNu :: LibraryEntry -> Library -> (Int, [DerivCluster])
proofRankNu = proofRankNuD 2

-- | Compute nu via proof-rank parameterized by coherence window depth d.
-- Returns (nu, clusters) where nu = non-trivial schema count + latent bonus.
--
-- The latent bonus captures capabilities that depth-1 enumeration cannot
-- see but that are structurally determined by the candidate:
--   - Each path constructor provides an independent proof technique
--   - The max path dimension captures homotopy richness (pi_d for S^d)
--
-- Schema rank already captures cross-interactions via L->X, X->L, L*X, L+X
-- schemas, so no separate cross-interaction bonus is needed.
proofRankNuD :: Int -> LibraryEntry -> Library -> (Int, [DerivCluster])
proofRankNuD d candidate lib =
  let enumerated = enumerateTypesD d candidate lib
      newThms    = filterNewlyInhabited enumerated candidate lib
      clusters   = clusterBySchema newThms candidate lib
      nonTrivial = filterTrivialClusters clusters
      schemaRank = length nonTrivial

      -- Latent capability bonus: captures depth-2+ contributions.
      -- Formula depends on candidate STRUCTURE, never its NAME.

      -- Each path constructor is an independent proof technique
      -- (provides loop, higher path, winding number, etc.)
      pathBonus  = length (lePathDims candidate)

      -- Max path dimension captures homotopy richness.
      -- For S^d, independent proof technique interfaces scale as d^2:
      -- d loop spaces Omega^j(S^d) × d homotopy levels = d^2 pairs.
      -- This growth rate ensures correct selection ordering where
      -- lower-dimensional spheres are discovered first (their lower
      -- rho gives minimal overshoot).
      -- S1: 1, S2: 4, S3: 9
      maxPathDim = if null (lePathDims candidate) then 0
                   else maximum (lePathDims candidate)
      homotopyBonus = maxPathDim * maxPathDim

      totalBonus = pathBonus + homotopyBonus
      nu = schemaRank + totalBonus
  in (nu, nonTrivial)

-- ============================================
-- Step 1: Enumerate types at depth <= 1
-- ============================================

-- | Enumerate all types at expression depth <= 1 using the default 2-step window.
-- Depth 1 captures the essential novelty: existence, loop space,
-- suspension, truncation, function space, products, coproducts.
enumerateTypes :: LibraryEntry -> Library -> [TypeExpr]
enumerateTypes = enumerateTypesD 2

-- | Enumerate types parameterized by coherence window depth d.
enumerateTypesD :: Int -> LibraryEntry -> Library -> [TypeExpr]
enumerateTypesD d candidate lib =
  let atoms = windowAtomsD d candidate lib
      fullLib = candidate : lib
      allTypes = enumWindowBounded atoms fullLib 1
      candidateName = leName candidate
      involving = filter (involvesName candidateName) allTypes
      normalized = nub $ map normalize involving
      nonTrivial = filter (\t -> t /= TUnit && t /= TVoid) normalized
  in nonTrivial

-- | Check if a type expression mentions a given name
involvesName :: String -> TypeExpr -> Bool
involvesName name (TRef n) = n == name
involvesName name (TArrow a b) = involvesName name a || involvesName name b
involvesName name (TProd a b) = involvesName name a || involvesName name b
involvesName name (TCoprod a b) = involvesName name a || involvesName name b
involvesName name (TId a x y) = involvesName name a || involvesName name x || involvesName name y
involvesName name (TSelfId a) = involvesName name a
involvesName name (TOmega a) = involvesName name a
involvesName name (TSusp a) = involvesName name a
involvesName name (TTrunc _ a) = involvesName name a
involvesName name (TPi _ a b) = involvesName name a || involvesName name b
involvesName name (TSigma _ a b) = involvesName name a || involvesName name b
involvesName name (TFiber a b) = involvesName name a || involvesName name b
involvesName name (TDeloop a) = involvesName name a
involvesName _ _ = False

-- ============================================
-- Step 2: Filter for newly inhabited
-- ============================================

filterNewlyInhabited :: [TypeExpr] -> LibraryEntry -> Library -> [TypeExpr]
filterNewlyInhabited types candidate lib =
  filter (\t -> isNewlyInhabited t candidate lib) types

-- ============================================
-- Step 3: Cluster by schema
-- ============================================

-- | Cluster newly inhabited types by schema abstraction.
-- Each schema represents a distinct "proof technique interface":
--   - X: existence
--   - L -> X: const functions
--   - X × L: product structure
--   - X + L: coproduct structure
--   - Omega(X): loop space
--   - ||X||: truncation
--   etc.
clusterBySchema :: [TypeExpr] -> LibraryEntry -> Library -> [DerivCluster]
clusterBySchema [] _ _ = []
clusterBySchema newThms candidate lib =
  let mainName = leName candidate
      schemasWithTypes = [(t, canonicalize (schemaize mainName lib t)) | t <- newThms]
      groups = groupBySchema schemasWithTypes
      clusters = [DerivCluster
        { dcRepresentative = head ts
        , dcMembers = ts
        , dcIsTrivial = False
        } | (_schema, ts) <- groups, not (null ts)]
  in clusters

-- | Group types by their schema
groupBySchema :: [(TypeExpr, TypeExpr)] -> [(TypeExpr, [TypeExpr])]
groupBySchema pairs =
  let schemas = nub $ map snd pairs
      groups = [(s, [t | (t, s') <- pairs, s' == s]) | s <- schemas]
  in groups

-- ============================================
-- Step 4: Filter trivial clusters
-- ============================================

-- | Filter out trivial clusters.
-- A cluster is trivial if ALL members match a trivially-derivable pattern:
-- types that are "new" only because the type exists, not because
-- it has interesting structure.
filterTrivialClusters :: [DerivCluster] -> [DerivCluster]
filterTrivialClusters = filter (not . isClusterTrivial)

isClusterTrivial :: DerivCluster -> Bool
isClusterTrivial cluster = all isTrivialSchema (dcMembers cluster)

-- | Check if a type's schema is trivially derivable from ANY inhabited type.
-- These patterns exist for any type X regardless of its structure.
isTrivialSchema :: TypeExpr -> Bool
-- X * X: pair(base, base) — derivable for any inhabited X
isTrivialSchema (TProd a b) | a == b = True
-- X + X: inl(base) — derivable for any inhabited X
isTrivialSchema (TCoprod a b) | a == b = True
-- X -> X: id — derivable for any type X
isTrivialSchema (TArrow a b) | a == b = True
-- x =_X x: refl — derivable for any inhabited X
isTrivialSchema (TSelfId _) = True
isTrivialSchema _ = False
