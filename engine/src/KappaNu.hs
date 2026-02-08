{-# LANGUAGE BangPatterns #-}

-- | Kolmogorov κ and Shannon ν computation for PEN
--
-- This module implements:
-- - κ(X | L) = Kolmogorov complexity of X given library L
-- - ν(X | L) = Shannon surprise of X (newly inhabited types)

module KappaNu where

import Types
import Inhabitation
import Enumerate
import Capability (computedNuSimple)
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

-- ============================================
-- Kolmogorov Complexity κ
-- ============================================

-- | Compute κ(X | L) = minimum program cost to define X using L
-- Returns the shortest program and its cost
kolmogorovKappa :: TypeExpr -> Library -> (Int, Maybe TypeProgram)
kolmogorovKappa target lib =
  let maxCost = 6   -- Reduced from 15: exponential blowup beyond ~6
      progs = allPrograms lib maxCost
      matches = filter (\p -> programToExpr p `typeEquiv` target) progs
  in case matches of
    [] -> (maxCost + 1, Nothing)  -- Not found within budget
    ps -> let best = minimumBy (comparing programCost) ps
          in (programCost best, Just best)

-- | Simpler κ that just returns the cost
kappa :: TypeExpr -> Library -> Int
kappa target lib = fst (kolmogorovKappa target lib)

-- | Type equivalence (structural equality for now)
-- In full version, this would handle α-equivalence and known isomorphisms
typeEquiv :: TypeExpr -> TypeExpr -> Bool
typeEquiv TUnit TUnit = True
typeEquiv TVoid TVoid = True
typeEquiv (TRef a) (TRef b) = a == b
typeEquiv (TArrow a1 b1) (TArrow a2 b2) = typeEquiv a1 a2 && typeEquiv b1 b2
typeEquiv (TProd a1 b1) (TProd a2 b2) = typeEquiv a1 a2 && typeEquiv b1 b2
typeEquiv (TCoprod a1 b1) (TCoprod a2 b2) = typeEquiv a1 a2 && typeEquiv b1 b2
typeEquiv (TSusp a) (TSusp b) = typeEquiv a b
typeEquiv (TOmega a) (TOmega b) = typeEquiv a b
typeEquiv (TTrunc n1 a) (TTrunc n2 b) = n1 == n2 && typeEquiv a b
typeEquiv (TSelfId a) (TSelfId b) = typeEquiv a b
typeEquiv (THIT p1 d1) (THIT p2 d2) = p1 == p2 && d1 == d2
typeEquiv _ _ = False

-- ============================================
-- Shannon Surprise ν
-- ============================================

-- | Weight function for surprise calculation
data WeightScheme
  = UniformWeight    -- ^ All types count as 1
  | ComplexityWeight -- ^ Weight = complexity of the type
  | RarityWeight     -- ^ Weight = 1 / (count at same level)
  deriving (Eq, Show)

-- | Compute ν(X | L, k) = total surprise of newly inhabited types up to complexity k
-- Using the specified weight scheme
nu :: LibraryEntry -> Library -> Int -> WeightScheme -> Int
nu newType lib maxK scheme =
  let -- Generate all types up to complexity k
      types = allTypes (newType : lib) maxK
      -- Filter for types that involve the new type
      relevantTypes = typesInvolving (leName newType) types
      -- Check which are newly inhabited
      newlyInhab = filter (\t -> isNewlyInhabited t newType lib) relevantTypes
  in case scheme of
    UniformWeight -> length newlyInhab
    ComplexityWeight -> sum (map complexity newlyInhab)
    RarityWeight -> length newlyInhab  -- Simplified for now

-- | Simpler version with uniform weight
nuSimple :: LibraryEntry -> Library -> Int -> Int
nuSimple newType lib maxK = nu newType lib maxK UniformWeight

-- | Get the actual newly inhabited types (for debugging)
getNewlyInhabited :: LibraryEntry -> Library -> Int -> [TypeExpr]
getNewlyInhabited newType lib maxK =
  let types = allTypes (newType : lib) maxK
      relevantTypes = typesInvolving (leName newType) types
  in filter (\t -> isNewlyInhabited t newType lib) relevantTypes

-- ============================================
-- Efficiency ρ
-- ============================================

-- | Compute ρ = ν / κ
rho :: LibraryEntry -> Library -> Int -> Double
rho newType lib maxK =
  let v = fromIntegral $ nuSimple newType lib maxK
      -- For κ, we need to convert the entry to a type expression
      typeExpr = entryToTypeExpr newType
      k = fromIntegral $ kappa typeExpr lib
  in if k > 0 then v / k else 0

-- | Convert a library entry to its type expression
entryToTypeExpr :: LibraryEntry -> TypeExpr
entryToTypeExpr entry = case leName entry of
  "1"   -> TUnit
  "0"   -> TVoid
  "S1"  -> THIT 1 [1]
  "S2"  -> TSusp (TRef "S1")
  "S3"  -> TSusp (TRef "S2")
  name  -> TRef name

-- ============================================
-- Genesis Type Entries
-- ============================================

-- | Library entries for the Genesis sequence
genesisEntry :: Int -> LibraryEntry
genesisEntry 1 = LibraryEntry "U" 0 [] False Nothing       -- Universe
genesisEntry 2 = LibraryEntry "1" 1 [] False (Just 0)      -- Unit
genesisEntry 3 = LibraryEntry "star" 1 [] False Nothing    -- Witness (★)
genesisEntry 4 = LibraryEntry "Pi" 0 [] False Nothing      -- Pi/Sigma
genesisEntry 5 = LibraryEntry "S1" 1 [1] True Nothing      -- Circle
genesisEntry 6 = LibraryEntry "Trunc" 0 [] False Nothing   -- PropTrunc
genesisEntry 7 = LibraryEntry "S2" 1 [2] True Nothing      -- S²
genesisEntry 8 = LibraryEntry "S3" 1 [3] True Nothing      -- S³
genesisEntry 9 = LibraryEntry "Hopf" 0 [] False Nothing    -- Hopf fibration
genesisEntry 10 = LibraryEntry "Lie" 0 [] False Nothing    -- Lie groups
genesisEntry 11 = LibraryEntry "Cohesion" 0 [] False Nothing
genesisEntry 12 = LibraryEntry "Connections" 0 [] False Nothing
genesisEntry 13 = LibraryEntry "Curvature" 0 [] False Nothing
genesisEntry 14 = LibraryEntry "Metric" 0 [] False Nothing
genesisEntry 15 = LibraryEntry "Hilbert" 0 [] False Nothing
genesisEntry 16 = LibraryEntry "DCT" 0 [] False Nothing
genesisEntry _ = LibraryEntry "unknown" 0 [] False Nothing

-- | Build library up to step n
buildLibrary :: Int -> Library
buildLibrary n = map genesisEntry [1..n]

-- | Paper's κ values for Genesis sequence
paperKappa :: Int -> Int
paperKappa 1 = 2   -- Universe
paperKappa 2 = 1   -- Unit
paperKappa 3 = 1   -- Witness
paperKappa 4 = 3   -- Π/Σ
paperKappa 5 = 3   -- S¹
paperKappa 6 = 3   -- PropTrunc
paperKappa 7 = 3   -- S²
paperKappa 8 = 5   -- S³
paperKappa 9 = 4   -- Hopf
paperKappa 10 = 6  -- Lie
paperKappa 11 = 4  -- Cohesion
paperKappa 12 = 5  -- Connections
paperKappa 13 = 6  -- Curvature
paperKappa 14 = 7  -- Metric
paperKappa 15 = 9  -- Hilbert
paperKappa 16 = 8  -- DCT
paperKappa _ = 1

-- | Paper's ν values for Genesis sequence
paperNu :: Int -> Int
paperNu 1 = 1    -- Universe
paperNu 2 = 1    -- Unit
paperNu 3 = 2    -- Witness
paperNu 4 = 5    -- Π/Σ
paperNu 5 = 7    -- S¹
paperNu 6 = 8    -- PropTrunc
paperNu 7 = 10   -- S²
paperNu 8 = 18   -- S³
paperNu 9 = 17   -- Hopf
paperNu 10 = 9   -- Lie
paperNu 11 = 19  -- Cohesion
paperNu 12 = 26  -- Connections
paperNu 13 = 34  -- Curvature
paperNu 14 = 43  -- Metric
paperNu 15 = 60  -- Hilbert
paperNu 16 = 150 -- DCT
paperNu _ = 0

-- | Computed ν via capability engine (should match paperNu)
computedNu :: Int -> Int
computedNu = computedNuSimple

-- ============================================
-- Comparison and Analysis
-- ============================================

-- | Compare computed vs paper values for step n
compareStep :: Int -> Int -> (Int, Int, Int, Int)
compareStep n maxK =
  let lib = buildLibrary (n - 1)
      entry = genesisEntry n
      compNu = nuSimple entry lib maxK
      compKappa = kappa (entryToTypeExpr entry) lib
      pNu = paperNu n
      pKappa = paperKappa n
  in (compKappa, compNu, pKappa, pNu)

-- | Run comparison for steps 1-10
runComparison :: Int -> [(Int, Int, Int, Int, Int)]
runComparison maxK =
  [(n, ck, cv, pk, pv) | n <- [1..10]
                       , let (ck, cv, pk, pv) = compareStep n maxK]
