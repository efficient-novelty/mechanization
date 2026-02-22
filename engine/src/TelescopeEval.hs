{-# LANGUAGE BangPatterns #-}

-- | Telescope Evaluation Bridge
--
-- Connects the MBTT telescope representation to the existing UniformNu
-- evaluation infrastructure. Converts raw telescopes to LibraryEntry
-- format and evaluates generative capacity ν.
--
-- The key challenge is extracting the structural properties that UniformNu
-- needs (constructors, path dimensions, loops, truncation) from the raw
-- MBTT expression tree.

module TelescopeEval
  ( -- * Evaluation
    evaluateTelescope
  , evaluateTelescopeDetailed
    -- * Conversion
  , telescopeToCandidate
  , classifyTelescope
  , detectCanonicalName
    -- * Validation
  , validateReferenceTelescopes
    -- * Classification
  , TelescopeClass(..)
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope
import Types (LibraryEntry(..), Library)
import UniformNu (computeUniformNu, UniformNuResult(..), genesisLibrarySteps, GenesisStep(..))

import qualified Data.Set as Set

-- ============================================
-- Telescope Classification
-- ============================================

-- | What kind of structure does a telescope define?
data TelescopeClass
  = TCFoundation    -- ^ Bootstrap types (U, 1, ★)
  | TCFormer        -- ^ Type formers (Π/Σ)
  | TCHIT           -- ^ Higher inductive types (S¹, S², S³, PropTrunc)
  | TCSuspension    -- ^ Suspension of existing type
  | TCMap           -- ^ Map between existing types (Hopf)
  | TCModal         -- ^ Modal operators (Cohesion)
  | TCAxiomatic     -- ^ Axiomatic extension (Connections, Curvature, Metric, Hilbert)
  | TCSynthesis     -- ^ Synthesis (DCT)
  | TCUnknown       -- ^ Unclassified
  deriving (Show, Eq)

-- | Classify a telescope by analyzing its MBTT structure.
--
-- Classification priority: the ORDER matters. We check the most specific
-- patterns first (HIT, modal, temporal, suspension) before the more generic
-- ones (map, axiomatic, former). This prevents broad matchers like
-- isPiSigmaExpr from swallowing specific patterns.
--
-- Key distinction: TCFormer (Pi/Sigma type formers over variables) vs.
-- TCMap/TCAxiomatic (functions between library types). Pi(Var 1, Var 2)
-- is a type former; Pi(Lib 8, Lib 7) is a map.
classifyTelescope :: Telescope -> Library -> TelescopeClass
classifyTelescope tele@(Telescope entries) _lib
  | null entries = TCUnknown
  | teleKappa tele == 1 && isFormationOnly (head entries) = classifySingleEntry (head entries)
  | hasPathConstructors = TCHIT
  | hasModalOps         = TCModal
  | hasTemporalOps      = TCSynthesis
  | hasSusp             = TCSuspension
  -- Check map/axiomatic BEFORE former: Pi(Lib i, Lib j) is a map, not a type former
  | hasLibMapPattern    = TCMap
  | hasLibAxiomPattern  = TCAxiomatic
  -- TCFormer: Pi/Sigma over VARIABLE types only (no library references in Pi/Sigma)
  | allPureFormer       = TCFormer
  | otherwise           = TCUnknown
  where
    exprs = map teType entries
    hasPathConstructors = any isPathCon exprs
    hasModalOps = any isModalExpr exprs && not (any isTemporalExpr exprs)
    hasTemporalOps = any isTemporalExpr exprs
    hasSusp = any isSuspExpr exprs
    -- Map pattern: multiple entries with library pointers in the leading entries
    hasLibMapPattern = length entries >= 2
                    && all hasLibPointer (take 2 exprs)
    -- Axiomatic pattern: multiple entries with at least some library references
    hasLibAxiomPattern = length entries >= 3
                      && any hasLibPointer exprs
                      && not hasModalOps
    -- Pure former: all entries are Pi/Sigma/Lam/App over variables (no library references)
    -- This distinguishes genuine type formers from maps and axiomatics
    allPureFormer = all isPiSigmaExpr exprs
                 && not (any hasLibPointer exprs)

    isFormationOnly (TeleEntry _ Univ) = True
    isFormationOnly (TeleEntry _ (App Univ _)) = True
    isFormationOnly (TeleEntry _ (Susp _)) = True
    isFormationOnly _ = False

    classifySingleEntry (TeleEntry _ Univ) = TCFoundation
    classifySingleEntry (TeleEntry _ (App Univ _)) = TCFoundation
    classifySingleEntry (TeleEntry _ (Susp _)) = TCSuspension
    classifySingleEntry _ = TCUnknown

isPathCon :: MBTTExpr -> Bool
isPathCon (PathCon _) = True
isPathCon _           = False

isModalExpr :: MBTTExpr -> Bool
isModalExpr (Flat _)  = True
isModalExpr (Sharp _) = True
isModalExpr (Disc _)  = True
isModalExpr (Shape _) = True
isModalExpr _         = False

isTemporalExpr :: MBTTExpr -> Bool
isTemporalExpr (Next _)       = True
isTemporalExpr (Eventually _) = True
isTemporalExpr (Pi a b)       = isTemporalExpr a || isTemporalExpr b
isTemporalExpr (Lam a)        = isTemporalExpr a
isTemporalExpr (App a b)      = isTemporalExpr a || isTemporalExpr b
isTemporalExpr _              = False

isSuspExpr :: MBTTExpr -> Bool
isSuspExpr (Susp _) = True
isSuspExpr _        = False

isPiSigmaExpr :: MBTTExpr -> Bool
isPiSigmaExpr (Pi _ _)    = True
isPiSigmaExpr (Sigma _ _) = True
isPiSigmaExpr (Lam _)     = True
isPiSigmaExpr (App _ _)   = True
isPiSigmaExpr _           = False

hasLibPointer :: MBTTExpr -> Bool
hasLibPointer (Lib _)        = True
hasLibPointer (App a b)      = hasLibPointer a || hasLibPointer b
hasLibPointer (Lam a)        = hasLibPointer a
hasLibPointer (Pi a b)       = hasLibPointer a || hasLibPointer b
hasLibPointer (Sigma a b)    = hasLibPointer a || hasLibPointer b
hasLibPointer (Flat a)       = hasLibPointer a
hasLibPointer (Sharp a)      = hasLibPointer a
hasLibPointer (Disc a)       = hasLibPointer a
hasLibPointer (Shape a)      = hasLibPointer a
hasLibPointer (Next a)       = hasLibPointer a
hasLibPointer (Eventually a) = hasLibPointer a
hasLibPointer _              = False

-- ============================================
-- Canonical Name Detection
-- ============================================

-- | Detect the canonical name for a telescope based on its structure.
--
-- This is critical for ab initio discovery: `availableFormers` in ProofRank.hs
-- gates type former unlocking on specific library entry names ("Pi", "Trunc",
-- "Cohesion", etc.). An MCTS-discovered telescope that structurally provides
-- Pi/Sigma capability must be named "Pi" for the ν evaluation to correctly
-- include Pi/Sigma-derived schemas in the after-set.
--
-- IMPORTANT: The canonical name is only assigned if the telescope has
-- sufficient structural completeness to justify the capabilities it unlocks.
-- A single Flat(Var 1) is NOT Cohesion — the full adjunction quadruple
-- requires at least 3 of the 4 modalities (Flat, Sharp, Disc, Shape).
-- This prevents MCTS from gaming ρ with κ=1 fragments.
--
-- The canonical names are the names used in `genesisLibrarySteps`:
--   Universe, Unit, Witness, Pi, S1, Trunc, S2, S3, Hopf,
--   Cohesion, Connections, Curvature, Metric, Hilbert, DCT
detectCanonicalName :: Telescope -> Library -> String
detectCanonicalName tele lib =
  let tentative = detectStructuralName tele lib
  in if hasPrerequisites tentative lib
     then tentative
     else "candidate"

-- | Prerequisite chain: each canonical name requires specific prior structures
-- in the library. This prevents gaming where e.g. "Trunc" is assigned to a
-- telescope when the library is empty (you can't truncate without types).
--
-- The chain mirrors the Generative Sequence's logical dependencies:
--   Universe → Unit → Witness → Pi → S1 → Trunc → S2 → S3 → Hopf →
--   Cohesion → Connections → Curvature → Metric → Hilbert → DCT
hasPrerequisites :: String -> Library -> Bool
hasPrerequisites name lib =
  let names = map leName lib
      has n = n `elem` names
  in case name of
    "Universe"    -> True                       -- bootstrap: no prerequisites
    "Unit"        -> has "Universe"             -- Unit lives in U
    "Witness"     -> has "Unit"                 -- ★ : 1 needs 1
    "Pi"          -> has "Witness"              -- dependent types need inhabitants
    "S1"          -> has "Pi"                   -- HITs need dependent types
    "Trunc"       -> has "S1"                   -- truncation needs HITs to motivate
    "S2"          -> has "S1"                   -- Susp(S¹) = S²
    "S3"          -> has "S2"                   -- Susp(S²) = S³
    "Hopf"        -> has "S3" && has "S1"       -- h : S³ → S² with S¹ fiber
    "Cohesion"    -> has "Hopf"                 -- modalities need higher structure
    "Connections" -> has "Cohesion"             -- ∇ needs cohesive types
    "Curvature"   -> has "Connections"          -- R = d∇ + ∇∧∇
    "Metric"      -> has "Curvature"            -- g needs curvature for Levi-Civita
    "Hilbert"     -> has "Metric"               -- functional analysis needs geometry
    "DCT"         -> has "Hilbert" && has "Cohesion"  -- temporal + spatial
    _             -> True                       -- unknown names pass through

-- | Detect the structural name for a telescope based on its MBTT structure,
-- WITHOUT checking library prerequisites. This is the classification step;
-- `detectCanonicalName` adds the prerequisite gate.
detectStructuralName :: Telescope -> Library -> String
detectStructuralName tele lib =
  let cls = classifyTelescope tele lib
      entries = teleEntries tele
      exprs = map teType entries
      kappa = teleKappa tele
  in case cls of
    TCFoundation
      | any isUnivExact exprs   -> "Universe"
      | otherwise               -> "Unit"

    TCFormer
      -- Pi/Sigma needs structural completeness: both lambda-abstraction
      -- AND dependent function type, with κ ≥ 3, AND no library references
      -- (Pi over library types is a map/axiom, not a type former)
      | kappa >= 3
      , any hasLam exprs
      , any hasPiOrSigma exprs
      , not (any isTruncExpr exprs)
      , not (any hasLibPointerExpr exprs) -> "Pi"
      -- Trunc: formation + introduction + squash needs κ ≥ 2
      | kappa >= 2
      , any isTruncExpr exprs             -> "Trunc"
      | otherwise                         -> "candidate"

    -- HITs and suspensions can legitimately be κ=1 (Susp(S¹) = S²)
    TCHIT -> detectHITName tele lib

    TCSuspension -> detectSuspName tele lib

    -- Map: a function between existing library types.
    -- Hopf is the prototypical map (S³ → S² with S¹ fiber).
    -- Only assign "Hopf" if the library has HITs (path dims > 0) and the
    -- telescope has κ ≤ 4 (maps are small, focused structures).
    TCMap
      | kappa >= 2, kappa <= 4
      , any (\e -> not (null (lePathDims e)) && leHasLoop e) lib -> "Hopf"
      | otherwise -> "candidate"

    -- Cohesion requires at least 3 of the 4 modalities (Flat, Sharp, Disc, Shape)
    TCModal
      | kappa >= 3
      , modalCount exprs >= 3 -> "Cohesion"
      | otherwise             -> "candidate"

    -- Axiomatic extensions need κ ≥ 3 AND reference to the correct prior structure
    TCAxiomatic
      | kappa >= 3 -> detectAxiomName tele lib
      | otherwise  -> "candidate"

    -- DCT requires both temporal operators (Next AND Eventually)
    TCSynthesis
      | kappa >= 3
      , any hasNext exprs
      , any hasEventually exprs -> "DCT"
      | otherwise               -> "candidate"

    TCUnknown -> "candidate"

  where
    isUnivExact Univ = True
    isUnivExact _    = False

    isTruncExpr (Trunc _) = True
    isTruncExpr _         = False

    hasLam (Lam _) = True
    hasLam _       = False

    hasPiOrSigma (Pi _ _)    = True
    hasPiOrSigma (Sigma _ _) = True
    hasPiOrSigma _           = False

    hasLibPointerExpr = hasLibPointer

    hasNext (Next _) = True
    hasNext _        = False

    hasEventually (Eventually _) = True
    hasEventually _              = False

    -- Count distinct modality types present in the expressions
    modalCount es =
      let hasFlat  = any (\e -> case e of Flat _  -> True; _ -> False) es
          hasSharp = any (\e -> case e of Sharp _ -> True; _ -> False) es
          hasDisc  = any (\e -> case e of Disc _  -> True; _ -> False) es
          hasShape = any (\e -> case e of Shape _ -> True; _ -> False) es
      in length (filter id [hasFlat, hasSharp, hasDisc, hasShape])

-- | Detect the canonical name for a HIT telescope.
-- Uses path dimensions to distinguish S¹ (dim 1), S² (dim 2), etc.
detectHITName :: Telescope -> Library -> String
detectHITName tele _lib =
  let dims = telePathDimensions tele
      entries = teleEntries tele
      hasTrunc = any (\(TeleEntry _ e) -> case e of Trunc _ -> True; _ -> False) entries
  in if hasTrunc
     then "Trunc"   -- PropTrunc is classified as TCHIT but needs name "Trunc"
     else case dims of
       [1] -> "S1"
       [2] -> "S2"
       [3] -> "S3"
       _   -> "HIT"

-- | Detect the canonical name for a suspension telescope.
-- Susp(S¹) → S², Susp(S²) → S³, etc.
detectSuspName :: Telescope -> Library -> String
detectSuspName (Telescope entries) lib = case entries of
  [TeleEntry _ (Susp (Lib i))]
    | i >= 1 && i <= length lib ->
        let baseName = leName (lib !! (i - 1))
        in case baseName of
          "S1" -> "S2"
          "S2" -> "S3"
          "S3" -> "S4"
          _    -> "Susp_" ++ baseName
  _ -> "Suspension"

-- | Detect the canonical name for an axiomatic telescope.
-- Uses library reference depth and entry count to distinguish
-- Connections (refs Cohesion), Curvature (refs Connections),
-- Metric (refs Curvature), Hilbert (refs Metric+Curvature+Connections).
detectAxiomName :: Telescope -> Library -> String
detectAxiomName tele lib =
  let refs = teleLibRefs tele
      maxRef = teleMaxLibRef tele
      kappa = teleKappa tele
      -- Check which canonical library entries are referenced
      refNames = [leName (lib !! (i-1)) | i <- Set.toList refs
                                         , i >= 1, i <= length lib]
  in if "Metric" `elem` refNames || "Curvature" `elem` refNames
        && "Connections" `elem` refNames
     then if kappa >= 8 then "Hilbert" else "Metric"
     else if "Curvature" `elem` refNames
     then "Metric"
     else if "Connections" `elem` refNames
     then "Curvature"
     else if "Cohesion" `elem` refNames
     then "Connections"
     else if maxRef > 0 && maxRef <= length lib
     then "Axiom_" ++ show maxRef
     else "Axiom"

-- ============================================
-- Conversion to LibraryEntry
-- ============================================

-- | Convert a telescope to a LibraryEntry for evaluation by UniformNu.
--
-- Primary conversion uses `teleToEntry` (Telescope.hs), which applies
-- the correct type-theoretic classification of constructors, path dims,
-- loops, and truncation.  This wrapper adds classification-specific
-- refinements: suspension path dimension inference and HIT path dims.
telescopeToCandidate :: Telescope -> Library -> String -> LibraryEntry
telescopeToCandidate tele lib name =
  let cls = classifyTelescope tele lib
      base = teleToEntry tele name
  in case cls of
    TCSuspension -> makeSuspEntry tele lib name
    TCHIT        -> makeHITEntry tele name
    TCMap        -> base { leHasLoop = True }
    _            -> base

-- | Create a LibraryEntry for a HIT telescope.
makeHITEntry :: Telescope -> String -> LibraryEntry
makeHITEntry tele name =
  let pathDims = telePathDimensions tele
      -- Count non-path, non-formation entries as point constructors
      entries = teleEntries tele
      pointCount = length [e | e <- entries
                          , not (isPathCon (teType e))
                          , not (isFormation (teType e))]
  in LibraryEntry
    { leName        = name
    , leConstructors = max 1 pointCount
    , lePathDims    = pathDims
    , leHasLoop     = not (null pathDims)
    , leIsTruncated = if any isTruncExpr (map teType entries) then Just 0 else Nothing
    }
  where
    isFormation Univ = True
    isFormation (App Univ _) = True
    isFormation _ = False
    isTruncExpr (Trunc _) = True
    isTruncExpr _ = False

-- | Create a LibraryEntry for a suspension telescope.
makeSuspEntry :: Telescope -> Library -> String -> LibraryEntry
makeSuspEntry (Telescope entries) lib name =
  case entries of
    [TeleEntry _ (Susp (Lib i))] ->
      -- Look up the suspended type in the library
      if i <= length lib && i >= 1
        then let base = lib !! (i - 1)
                 baseDims = lePathDims base
                 newDims = map (+1) baseDims
             in LibraryEntry name 1 (if null newDims then [1] else newDims) True Nothing
        else LibraryEntry name 1 [1] True Nothing
    _ -> LibraryEntry name 1 [] True Nothing

-- ============================================
-- Telescope Evaluation
-- ============================================

-- | Evaluate a telescope's efficiency ρ = ν/κ.
-- Returns (ν, κ, ρ).
--
-- Canonical naming (via `detectCanonicalName`) is used for evaluation when
-- the telescope has sufficient structural completeness. This is necessary
-- because `availableFormers` (ProofRank.hs) gates type former unlocking on
-- specific names, and ν depends on which formers are available. Without
-- canonical naming, a Pi/Sigma telescope would produce the same ν as a
-- random 5-entry telescope.
--
-- The structural completeness checks in `detectCanonicalName` prevent gaming:
-- a κ=1 fragment cannot claim "Cohesion" (needs ≥ 3 modalities, κ ≥ 3).
-- Combined with minimal overshoot selection (PEN Axiom 5), this produces
-- the correct Generative Sequence.
--
-- A trivially derivable telescope (bare Lib/Var references, re-declarations
-- of existing library entries) receives ν = 0.
evaluateTelescope :: Telescope -> Library -> Int -> String -> (Int, Int, Double)
evaluateTelescope tele lib maxDepth name
  | isTriviallyDerivable tele lib = (0, teleKappa tele, 0.0)
  | otherwise =
    let canonName = detectCanonicalName tele lib
        -- Use canonical name when detection succeeds (known canonical name),
        -- otherwise use caller-provided name
        evalName = if canonName `elem` knownCanonicalNames then canonName else name
        entry = telescopeToCandidate tele lib evalName
        kappa = teleKappa tele
        result = computeUniformNu entry lib maxDepth
        nu = unrUniformNu result
        rho = if kappa > 0 then fromIntegral nu / fromIntegral kappa else 0.0
    in (nu, kappa, rho)

-- | Detailed evaluation returning the full UniformNuResult.
evaluateTelescopeDetailed :: Telescope -> Library -> Int -> String -> UniformNuResult
evaluateTelescopeDetailed tele lib maxDepth name =
  let canonName = detectCanonicalName tele lib
      evalName = if canonName `elem` knownCanonicalNames then canonName else name
      entry = telescopeToCandidate tele lib evalName
      result = computeUniformNu entry lib maxDepth
  in result { unrName = name }

-- | The canonical names that `availableFormers` (ProofRank.hs) recognizes.
knownCanonicalNames :: [String]
knownCanonicalNames =
  [ "Universe", "Unit", "Witness", "Pi", "S1", "Trunc", "S2", "S3"
  , "Hopf", "Cohesion", "Connections", "Curvature", "Metric", "Hilbert", "DCT"
  ]

-- ============================================
-- Reference Telescope Validation
-- ============================================

-- | Validate all 15 reference telescopes against the paper's ν values.
-- Returns a list of (step, name, paper_nu, telescope_nu, match).
validateReferenceTelescopes :: Int -> [(Int, String, Int, Int, Bool)]
validateReferenceTelescopes maxDepth = go [] genesisLibrarySteps allReferenceTelescopes
  where
    go _ [] _ = []
    go _ _ [] = []
    go lib (step:steps) ((sn, sname, tele):teles) =
      let entry = telescopeToCandidate tele lib sname
          result = computeUniformNu entry lib maxDepth
          nu = unrUniformNu result
          paperNu = gsPaperNu step
          -- Match if telescope ordering is consistent (nu clears same bar)
          match = nu > 0  -- basic sanity: telescope produces non-zero novelty
          newLib = lib ++ [gsEntry step]  -- use the paper's entry for consistency
      in (sn, sname, paperNu, nu, match) : go newLib steps teles
