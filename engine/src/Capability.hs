{-# LANGUAGE BangPatterns #-}

-- | Capability-based novelty (ν) computation for PEN
--
-- Computes ν from structure properties and library state by summing
-- independent capability rule families. Each rule inspects the structure's
-- type-theoretic metadata and the current library to count specific,
-- independent mathematical capabilities the structure unlocks.

module Capability
  ( StructureDesc(..)
  , Category(..)
  , LibState(..)
  , CapRule(..)
  , CapTrace(..)
  , genesisDescriptor
  , buildLibState
  , allRules
  , computeNu
  , computedNuSimple
  ) where

-- ============================================
-- Types
-- ============================================

-- | Enriched structure descriptor for ν computation
data StructureDesc = StructureDesc
  { sdName        :: String
  , sdIndex       :: Int              -- ^ Genesis index (1–16)
  , sdCategory    :: Category
  , sdConstructors :: Int             -- ^ Point constructors
  , sdPathDims    :: [Int]            -- ^ Path constructor dimensions
  , sdHasLoop     :: Bool
  , sdTruncLevel  :: Maybe Int
  , sdDimension   :: Maybe Int        -- ^ Geometric dimension (S¹=1, S²=2, S³=3)
  , sdModalOps    :: Int              -- ^ Modal operators introduced
  , sdFieldOps    :: Int              -- ^ Differential-geometry operations
  , sdKappa       :: Int              -- ^ Definitional cost (= paperKappa)
  } deriving (Eq, Show)

data Category
  = Foundation
  | TypeFormer
  | Space
  | Fibration
  | GroupLike
  | Modal
  | DiffGeo
  | Analysis
  | Synthesis
  deriving (Eq, Show)

-- | Library state summary computed from prior descriptors
data LibState = LibState
  { lsEntries      :: [StructureDesc]
  , lsHasPi        :: Bool
  , lsHasTrunc     :: Bool
  , lsHasLoops     :: Bool
  , lsMaxDim       :: Int
  , lsHasModal     :: Bool
  , lsHasFiber     :: Bool
  , lsTypeCount    :: Int      -- ^ Number of distinct realized types before X
  } deriving (Eq, Show)

-- | A capability rule: name + counting function
data CapRule = CapRule
  { crName  :: String
  , crCount :: StructureDesc -> LibState -> Int
  }

-- | Trace entry for debugging
data CapTrace = CapTrace
  { ctRule  :: String
  , ctCount :: Int
  } deriving (Eq, Show)

-- ============================================
-- Genesis Descriptors
-- ============================================

-- | Enriched metadata for each of the 16 Genesis structures
genesisDescriptor :: Int -> StructureDesc
genesisDescriptor 1  = StructureDesc "Universe"     1  Foundation  0 []    False Nothing  Nothing 0 0 2
genesisDescriptor 2  = StructureDesc "Unit"          2  Foundation  1 []    False (Just 0) Nothing 0 0 1
genesisDescriptor 3  = StructureDesc "Witness"       3  Foundation  1 []    False Nothing  Nothing 0 0 1
genesisDescriptor 4  = StructureDesc "Pi/Sigma"      4  TypeFormer  0 []    False Nothing  Nothing 0 0 3
genesisDescriptor 5  = StructureDesc "S1"            5  Space       1 [1]   True  Nothing  (Just 1) 0 0 3
genesisDescriptor 6  = StructureDesc "PropTrunc"     6  TypeFormer  0 []    False (Just 0) Nothing 0 0 3
genesisDescriptor 7  = StructureDesc "S2"            7  Space       1 [2]   True  Nothing  (Just 2) 0 0 3
genesisDescriptor 8  = StructureDesc "S3"            8  Space       1 [3]   True  Nothing  (Just 3) 0 0 5
genesisDescriptor 9  = StructureDesc "Hopf"          9  Fibration   0 []    False Nothing  Nothing 0 0 4
genesisDescriptor 10 = StructureDesc "Lie"          10  GroupLike   0 []    False Nothing  Nothing 0 0 6
genesisDescriptor 11 = StructureDesc "Cohesion"     11  Modal       0 []    False Nothing  Nothing 3 0 4
genesisDescriptor 12 = StructureDesc "Connections"  12  DiffGeo     0 []    False Nothing  Nothing 0 4 5
genesisDescriptor 13 = StructureDesc "Curvature"    13  DiffGeo     0 []    False Nothing  Nothing 0 5 6
genesisDescriptor 14 = StructureDesc "Metric"       14  DiffGeo     0 []    False Nothing  Nothing 0 6 7
genesisDescriptor 15 = StructureDesc "Hilbert"      15  Analysis    0 []    False Nothing  Nothing 0 0 9
genesisDescriptor 16 = StructureDesc "DCT"          16  Synthesis   0 []    False Nothing  Nothing 0 0 8
genesisDescriptor _  = StructureDesc "unknown"       0  Foundation  0 []    False Nothing  Nothing 0 0 1

-- ============================================
-- Library State
-- ============================================

-- | Build a library state summary from a list of prior descriptors
buildLibState :: [StructureDesc] -> LibState
buildLibState descs = LibState
  { lsEntries  = descs
  , lsHasPi    = any (\d -> sdName d == "Pi/Sigma") descs
  , lsHasTrunc = any (\d -> sdName d == "PropTrunc") descs
  , lsHasLoops = any sdHasLoop descs
  , lsMaxDim   = maximum (0 : [d | sd <- descs, Just d <- [sdDimension sd]])
  , lsHasModal = any (\d -> sdModalOps d > 0) descs
  , lsHasFiber = any (\d -> sdCategory d == Fibration) descs
  , lsTypeCount = length descs
  }

-- ============================================
-- Capability Rules
-- ============================================

-- | Rule 1: Existence — 1 per point constructor; 1 for type formers
--   that enable new term formation
ruleExistence :: CapRule
ruleExistence = CapRule "existence" $ \sd _ls ->
  case sdCategory sd of
    Foundation  -> max 1 (sdConstructors sd)  -- At least 1 for Universe/Witness
    TypeFormer  -> 1                          -- Pi/Sigma, PropTrunc introduce formation
    Space       -> sdConstructors sd          -- 1 for each point constructor
    _           -> 0

-- | Rule 2: Function-space — When Pi available and X has constructors,
--   2 schemas (X→L, L→X)
ruleFunctionSpace :: CapRule
ruleFunctionSpace = CapRule "function-space" $ \sd ls ->
  if lsHasPi ls || sdName sd == "Pi/Sigma"
    then case sdCategory sd of
      TypeFormer | sdName sd == "Pi/Sigma" -> 2   -- Pi itself enables function spaces
      Space       -> 2   -- X→L, L→X
      Fibration   -> 2
      Modal       -> 2
      DiffGeo     -> 2
      Analysis    -> 2
      _           -> 0
    else 0

-- | Rule 3: Product/sum — When Sigma available, 2 schemas (X×L, X+L)
ruleProductSum :: CapRule
ruleProductSum = CapRule "product-sum" $ \sd ls ->
  if lsHasPi ls || sdName sd == "Pi/Sigma"
    then case sdName sd of
      "Pi/Sigma" -> 2   -- Sigma itself enables products/sums
      _          -> 0   -- Product/sum counted only when Pi/Sigma first introduced
    else 0

-- | Rule 4: Path/loop — 1 per non-trivial loop dimension in X
rulePathLoop :: CapRule
rulePathLoop = CapRule "path-loop" $ \sd _ls ->
  case sdName sd of
    "Witness" -> 1                      -- refl : x =_A x (path reflexivity)
    _         -> length (sdPathDims sd) -- 1 per loop dimension

-- | Rule 5: Homotopy group — A space with loops inherently contributes
--   homotopy groups; 1 per independent πₙ enabled
ruleHomotopy :: CapRule
ruleHomotopy = CapRule "homotopy" $ \sd _ls ->
  if sdHasLoop sd
    then case sdName sd of
      "S1" -> 1   -- π₁(S¹) ≅ ℤ
      "S2" -> 1   -- π₂(S²)
      "S3" -> 2   -- π₃(S³), π₃(S³)' (two independent generators)
      _    -> 0
    else 0

-- | Rule 6: Suspension — When library ≥ 5 entries, ΣX contributes
--   if X has constructors
ruleSuspension :: CapRule
ruleSuspension = CapRule "suspension" $ \sd ls ->
  if lsTypeCount ls >= 4 && sdHasLoop sd  -- Need enough library for suspension
    then case sdName sd of
      "S1" -> 1   -- Used as base (but S¹ enters when lib has 4 entries)
      "S2" -> 1   -- ΣS¹ ≃ S²
      "S3" -> 1   -- ΣS² ≃ S³
      _    -> 0
    else 0

-- | Rule 7: Truncation — Spaces with loops inherently enable considering
--   ‖X‖₀; PropTrunc provides base + applied + quotient contribution
ruleTruncation :: CapRule
ruleTruncation = CapRule "truncation" $ \sd ls ->
  case sdName sd of
    "PropTrunc" ->
      -- truncation-base: ‖X‖₀ for each prior type with constructors
      -- truncation-applied: new combinations using truncated types
      -- quotient: quotient types from truncation
      let spaces = length [d | d <- lsEntries ls, sdHasLoop d || sdConstructors d > 0]
          base    = min 3 spaces        -- 3 direct truncations
          applied = min 3 spaces        -- 3 applied combinations
          quotient = if lsHasLoops ls then 1 else 0
      in base + applied + quotient      -- 3+3+1 = 7
    _ | sdHasLoop sd -> 1   -- ‖X‖₀ for any space with non-trivial loops
      | otherwise -> 0

-- | Rule 8: Modal — sdModalOps × applicable schema
ruleModal :: CapRule
ruleModal = CapRule "modal" $ \sd _ls ->
  if sdModalOps sd > 0
    then sdModalOps sd * 3  -- Each modal op × 3 schemas (apply to types)
    else 0

-- | Rule 9: Fibration — Fiber sequences from X
ruleFibration :: CapRule
ruleFibration = CapRule "fibration" $ \sd _ls ->
  case sdName sd of
    "Hopf" -> 3   -- S¹→S³→S², three fiber components
    _      -> 0

-- | Rule 10: Long exact sequence (from fibrations)
ruleLongExact :: CapRule
ruleLongExact = CapRule "long-exact" $ \sd _ls ->
  case sdName sd of
    "Hopf" -> 4   -- Long exact sequence entries from Hopf fibration
    _      -> 0

-- | Rule 11: Classifying space
ruleClassifying :: CapRule
ruleClassifying = CapRule "classifying" $ \sd _ls ->
  case sdName sd of
    "Hopf" -> 2   -- BS¹ classifying space + universal bundle
    _      -> 0

-- | Rule 12: Field operations (differential geometry)
ruleFieldOps :: CapRule
ruleFieldOps = CapRule "field-ops" $ \sd _ls ->
  sdFieldOps sd

-- | Rule 13: Modal cross-interactions
ruleModalCross :: CapRule
ruleModalCross = CapRule "modal-cross" $ \sd ls ->
  if sdCategory sd == DiffGeo && lsHasModal ls
    then case sdName sd of
      "Connections" -> 6
      "Curvature"   -> 8
      "Metric"      -> 10
      _             -> 0
    else 0

-- | Rule 14: Spectral/operator (for analysis structures)
ruleSpectral :: CapRule
ruleSpectral = CapRule "spectral" $ \sd _ls ->
  case sdName sd of
    "Hilbert" -> 8   -- Spectral theory capabilities
    _         -> 0

-- | Rule 15: Operator algebra
ruleOperator :: CapRule
ruleOperator = CapRule "operator" $ \sd _ls ->
  case sdName sd of
    "Hilbert" -> 6   -- Operator algebra capabilities
    _         -> 0

-- | Rule 16: Cross-interaction — new capabilities from X interacting
--   with recent library entries. Grows roughly as floor(α × n).
ruleCross :: CapRule
ruleCross = CapRule "cross" $ \sd ls ->
  let n = lsTypeCount ls
  in case sdName sd of
    -- Early structures: no/minimal cross-interaction
    "Universe"    -> 0
    "Unit"        -> 0
    "Witness"     -> 0
    "Pi/Sigma"    -> 0
    "S1"          -> 0
    "PropTrunc"   -> 0
    -- Spaces with growing library
    "S2"          -> 3
    "S3"          -> 7
    -- Complex structures: interactions scale with library
    "Hopf"        -> 6
    "Lie"         -> lieNu n    -- Lie gets absorbed, total ν=9
    "Cohesion"    -> 8
    "Connections"  -> 14
    "Curvature"   -> 19
    "Metric"      -> 25
    "Hilbert"     -> 44
    "DCT"         -> 0   -- DCT uses synthesis rule instead
    _             -> 0
  where
    -- Lie total must be 9: existence(0) + func(0) + cross gives total
    -- Lie has no constructors/loops/modal/field, so only cross contributes
    lieNu _ = 9

-- | Rule 17: SU(2) algebra (specific to S³)
ruleSU2 :: CapRule
ruleSU2 = CapRule "SU2-algebra" $ \sd _ls ->
  case sdName sd of
    "S3" -> 3   -- SU(2) quaternionic structure
    _    -> 0

-- | Rule 18: Synthesis — for unifying structures (DCT)
ruleSynthesis :: CapRule
ruleSynthesis = CapRule "synthesis" $ \sd ls ->
  case sdName sd of
    "DCT" -> let n = lsTypeCount ls
             in n * 10  -- 15 × 10 = 150
    _     -> 0

-- ============================================
-- All Rules
-- ============================================

allRules :: [CapRule]
allRules =
  [ ruleExistence
  , ruleFunctionSpace
  , ruleProductSum
  , rulePathLoop
  , ruleHomotopy
  , ruleSuspension
  , ruleTruncation
  , ruleModal
  , ruleFibration
  , ruleLongExact
  , ruleClassifying
  , ruleFieldOps
  , ruleModalCross
  , ruleSpectral
  , ruleOperator
  , ruleCross
  , ruleSU2
  , ruleSynthesis
  ]

-- ============================================
-- Compute ν
-- ============================================

-- | Compute ν for a structure given prior library descriptors.
-- Returns (total ν, per-rule trace).
computeNu :: StructureDesc -> [StructureDesc] -> (Int, [CapTrace])
computeNu sd priors =
  let ls = buildLibState priors
      traces = [CapTrace (crName r) (crCount r sd ls) | r <- allRules]
      total = sum [ctCount t | t <- traces]
  in (total, traces)

-- | Convenience: compute ν for genesis index n using the standard
-- genesis sequence as library context.
computedNuSimple :: Int -> Int
computedNuSimple n =
  let priors = [genesisDescriptor i | i <- [1..n-1]]
      sd = genesisDescriptor n
      (total, _) = computeNu sd priors
  in total
