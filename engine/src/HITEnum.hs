-- | Parametric HIT Enumeration
--
-- Enumerates higher inductive types (HITs) parametrically by cost.
-- A HIT is defined by:
--   - Number of point constructors (>= 0)
--   - A list of path constructor dimensions (each >= 1)
--
-- Cost = 1 + numPoints + sum(path dimensions)
-- This matches Types.hs:53 (THIT cost).
--
-- Symmetry breaking: path dimensions are non-decreasing.

module HITEnum
  ( HITDef(..)
  , PathSpec(..)
  , enumerateHITs
  , hitCost
  , hitToTypeExpr
  , hitToLibraryEntry
  , hitHasLoop
  , knownHITName
  ) where

import Types

-- ============================================
-- Types
-- ============================================

data HITDef = HITDef
  { hitNumPoints :: Int
  , hitPaths     :: [PathSpec]
  } deriving (Eq, Ord, Show)

data PathSpec = PathSpec { psDimension :: Int }
  deriving (Eq, Ord, Show)

-- ============================================
-- Cost
-- ============================================

-- | Cost of a HIT definition.
-- Matches THIT cost in Types.hs: 1 + pts + sum(path dims)
hitCost :: HITDef -> Int
hitCost h = 1 + hitNumPoints h + sum (map psDimension (hitPaths h))

-- ============================================
-- Enumeration
-- ============================================

-- | Enumerate all HITs with cost <= maxCost.
-- Uses symmetry breaking: path dimensions are non-decreasing.
enumerateHITs :: Int -> [HITDef]
enumerateHITs maxCost = concatMap hitsAtCost [2..maxCost]

-- | Enumerate HITs at exactly the given cost.
hitsAtCost :: Int -> [HITDef]
hitsAtCost cost = do
  -- cost = 1 + numPoints + sumPathDims
  -- So numPoints + sumPathDims = cost - 1
  let budget = cost - 1
  -- Partition budget between points and path dimensions
  numPts <- [0..budget]
  let pathBudget = budget - numPts
  paths <- partitionIntoNonDecreasing pathBudget
  return $ HITDef numPts (map PathSpec paths)

-- | Partition n into a non-decreasing list of positive integers.
-- E.g., partitionIntoNonDecreasing 3 = [[1,1,1], [1,2], [3]]
partitionIntoNonDecreasing :: Int -> [[Int]]
partitionIntoNonDecreasing 0 = [[]]  -- empty list of paths
partitionIntoNonDecreasing n = partHelper n 1
  where
    partHelper 0 _ = [[]]
    partHelper remaining minVal = do
      val <- [minVal..remaining]
      rest <- partHelper (remaining - val) val
      return (val : rest)

-- ============================================
-- Conversion
-- ============================================

-- | Convert a HIT definition to a TypeExpr (THIT pts dims).
hitToTypeExpr :: HITDef -> TypeExpr
hitToTypeExpr h = THIT (hitNumPoints h) (map psDimension (hitPaths h))

-- | Convert a HIT definition to a LibraryEntry with a given name.
hitToLibraryEntry :: HITDef -> String -> LibraryEntry
hitToLibraryEntry h name = (mkLibraryEntry name (hitNumPoints h) (map psDimension (hitPaths h)) (hitHasLoop h) Nothing)

-- ============================================
-- Properties
-- ============================================

-- | Does this HIT have a non-trivial loop?
-- A HIT has a loop if it has at least one point constructor
-- and at least one path constructor.
hitHasLoop :: HITDef -> Bool
hitHasLoop h = hitNumPoints h > 0 && not (null (hitPaths h))

-- ============================================
-- Known HIT identification
-- ============================================

-- | Identify a HIT by its structure if it matches a known space.
knownHITName :: HITDef -> Maybe String
knownHITName (HITDef 1 []) = Just "Unit"   -- 1 point, no paths ~ contractible
knownHITName (HITDef 1 [PathSpec 1]) = Just "S1"
knownHITName (HITDef 2 []) = Just "Bool"
knownHITName (HITDef 1 [PathSpec 2]) = Just "S2"
knownHITName (HITDef 1 [PathSpec 1, PathSpec 1]) = Just "Torus"  -- figure-eight / torus
knownHITName (HITDef 2 [PathSpec 1]) = Just "Interval"
knownHITName (HITDef 1 [PathSpec 3]) = Just "S3"
knownHITName _ = Nothing
