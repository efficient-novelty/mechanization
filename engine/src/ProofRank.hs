{-# LANGUAGE BangPatterns #-}

-- | Proof-rank computation (depth-2 enumeration + clustering)
--
-- Implements the proof-rank ν as the number of independent clusters of newly
-- inhabited types at expression depth ≤ d, using derivability via the
-- inhabitation checker on function types.

module ProofRank where

import Types
import Enumerate (typesInvolving)
import Inhabitation (checkInhab, isInhabited, isNewlyInhabited)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================
-- Expression Depth
-- ============================================

-- | Expression depth: atoms have depth 0, unary/binary add 1.
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
-- Depth-Bounded Enumeration
-- ============================================

-- | Enumerate atoms (depth 0)
atoms :: Library -> [TypeExpr]
atoms lib = TUnit : TVoid : map (TRef . leName) lib

-- | Enumerate all type expressions with depth exactly d
enumerateDepthExact :: Library -> Int -> [TypeExpr]
enumerateDepthExact lib d
  | d < 0 = []
  | d == 0 = atoms lib
  | otherwise = nub $ unaryOps ++ binaryOps
  where
    sub = enumerateDepthBounded lib (d - 1)
    unaryOps =
      [TOmega a | a <- sub, depth a == d - 1] ++
      [TSusp a | a <- sub, depth a == d - 1] ++
      [TSelfId a | a <- sub, depth a == d - 1]
    binaryOps =
      [TArrow a b | a <- sub, b <- sub, depth (TArrow a b) == d] ++
      [TProd a b | a <- sub, b <- sub, depth (TProd a b) == d] ++
      [TCoprod a b | a <- sub, b <- sub, depth (TCoprod a b) == d]

-- | Enumerate all type expressions with depth ≤ d
enumerateDepthBounded :: Library -> Int -> [TypeExpr]
enumerateDepthBounded lib d = nub $ concatMap (enumerateDepthExact lib) [0..d]

-- ============================================
-- Derivability and Clustering
-- ============================================

-- | Derivable if T₁ → T₂ is inhabited using library operations alone.
derivable :: TypeExpr -> TypeExpr -> Library -> Bool
derivable t1 t2 lib = isInhabited (checkInhab (TArrow t1 t2) lib)

-- | Build adjacency map for derivability graph.
derivabilityGraph :: [TypeExpr] -> Library -> Map.Map TypeExpr [TypeExpr]
derivabilityGraph types lib =
  Map.fromList [(t, neighbors t) | t <- types]
  where
    neighbors t = [u | u <- types, u /= t, derivable t u lib || derivable u t lib]

-- | Compute connected components of an undirected graph.
connectedComponents :: Ord a => Map.Map a [a] -> [[a]]
connectedComponents graph = go Set.empty (Map.keys graph)
  where
    go _ [] = []
    go visited (n:ns)
      | n `Set.member` visited = go visited ns
      | otherwise =
          let (component, visited') = dfs visited [n]
          in component : go visited' ns
    dfs visited [] = ([], visited)
    dfs visited (x:xs)
      | x `Set.member` visited = dfs visited xs
      | otherwise =
          let visited' = Set.insert x visited
              next = Map.findWithDefault [] x graph
              (rest, visited'') = dfs visited' (next ++ xs)
          in (x : rest, visited'')

-- ============================================
-- Proof-Rank
-- ============================================

-- | Newly inhabited types at depth ≤ d that involve the new type.
newlyInhabitedDepth :: LibraryEntry -> Library -> Int -> [TypeExpr]
newlyInhabitedDepth newType lib d =
  let types = enumerateDepthBounded (newType : lib) d
      relevant = typesInvolving (leName newType) types
  in filter (\t -> isNewlyInhabited t newType lib) relevant

-- | Compute proof-rank as the number of derivability clusters.
proofRank :: LibraryEntry -> Library -> Int -> (Int, [[TypeExpr]])
proofRank newType lib d =
  let newTypes = newlyInhabitedDepth newType lib d
      graph = derivabilityGraph newTypes lib
      clusters = connectedComponents graph
  in (length clusters, clusters)

