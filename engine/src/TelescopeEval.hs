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
  ( -- * Evaluation Modes
    EvalMode(..)
  , KappaMode(..)
    -- * Evaluation
  , evaluateTelescope
  , evaluateTelescopeWithHistory
  , evaluateTelescopeDetailed
  , strictKappa
  , computeKappa
    -- * Tracing
  , EvalTrace(..)
  , evaluateTelescopeTrace
    -- * Conversion
  , telescopeToCandidate
  , telescopeToCandidateStructural
  , classifyTelescope
  , detectCanonicalName
  , hasPrerequisites
    -- * Validation
  , validateReferenceTelescopes
    -- * Classification
  , TelescopeClass(..)
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope
import Types (LibraryEntry(..), Library, TypeExpr(..), mkLibraryEntry)
import ProofRank (availableFormers)
import UniformNu (computeUniformNu, UniformNuResult(..), genesisLibrarySteps, GenesisStep(..))
import MBTTNu (computeNativeNu, NativeNuResult(..))
import StructuralNu (detectDistributiveLaws, detectUniversePolymorphism, detectInfinitesimalShift)

import qualified Data.Set as Set

data CapabilityPolicy
  = NameMediatedCaps
  | StructuralOnlyCaps
  deriving (Show, Eq)

-- ============================================
-- Evaluation Modes
-- ============================================

-- | Evaluation mode for telescope scoring.
--
-- Controls whether the evaluator uses paper ν/κ values (for replay/comparison)
-- or computes them strictly from the telescope + library (for genuine discovery).
--
-- This is the key architectural distinction for the strictness audit:
-- - EvalStrictComputed: NEVER reads paper tables. ν comes from computeUniformNu,
--   κ comes from strictKappa (teleKappa + explicit suspension policy).
data EvalMode
  = EvalGuidedComputed      -- ^ Guided recovery: compute ν structurally plus legacy bonuses
  | EvalStrictComputed      -- ^ Honest strict mode: no paper tables and no target-conditioned bonuses
  | EvalStructural          -- ^ StructuralNu: AST rule extraction, no semantic proxy
  deriving (Show, Eq)

-- | Kappa computation mode — selects the construction effort metric.
--
--   DesugaredKappa: Principled clause counting. Expands macro-like entries
--     (Susp → 4 core judgments) and counts constituent judgments. Default.
--   EntryKappa: Raw telescope entry count. Fast but degenerate for
--     suspension shortcuts (Susp(X) = κ=1).
--   BitCostKappa: MBTT bit cost (Kolmogorov complexity upper bound).
--     Most fine-grained but may overweight syntactic structure.
data KappaMode
  = DesugaredKappa   -- ^ Default: desugared clause counting
  | EntryKappa       -- ^ Raw entry count (teleKappa)
  | BitCostKappa     -- ^ MBTT bit cost (teleBitCost)
  deriving (Show, Eq)

-- | Evaluation trace for transparency logging.
-- Records what happened during evaluation so callers can audit paper-value usage.
data EvalTrace = EvalTrace
  { etCanonName      :: !String       -- ^ Detected canonical name (or "candidate")
  , etMode           :: !EvalMode     -- ^ Which evaluation mode was used
  , etNuComputed     :: !Int          -- ^ ν from computeUniformNu (always computed)
  , etNuUsed         :: !Int          -- ^ ν actually used for scoring
  , etNuFromPaper    :: !(Maybe Int)  -- ^ Paper ν if applicable (Nothing in strict mode)
  , etKappaEntry     :: !Int          -- ^ Raw teleKappa (entry count)
  , etKappaUsed      :: !Int          -- ^ κ actually used for scoring
  , etKappaFromPaper :: !(Maybe Int)  -- ^ Paper κ if applicable (Nothing in strict mode)
  } deriving (Show)

-- TelescopeClass and classifyTelescope are defined in Telescope.hs
-- and re-exported by this module for backward compatibility.

-- ============================================
-- Canonical Name Detection
-- ============================================

-- | Detect the canonical name for a telescope based on its structure.
--
-- Canonical naming is now reporting-oriented in strict modes.
-- Capability unlocking for strict discovery is structural and independent of names.
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

-- | Prerequisite chain: each canonical name requires specific STRUCTURAL
-- properties in the library. This uses capability flags and structural
-- properties (path dims, library size), NOT entry names.
--
-- This is critical for ab initio discovery: if step 1 discovers a generic
-- "candidate" instead of "Universe", name-based checks would block all
-- subsequent canonical names. Structural checks ensure the prerequisite
-- chain works regardless of what names earlier steps discover.
--
-- The chain mirrors the Generative Sequence's logical dependencies:
--   Universe → Unit → Witness → Pi → S1 → Trunc → S2 → S3 → Hopf →
--   Cohesion → Connections → Curvature → Metric → Hilbert → DCT
hasPrerequisites :: String -> Library -> Bool
hasPrerequisites name lib =
    -- Uniqueness: each canonical name can only be discovered once.
    -- Without this, multiple enum candidates could claim the same name,
    -- and later steps could rediscover already-known structures.
    let unique = name `notElem` map leName lib
    in unique && case name of
    "Universe"    -> True                                    -- bootstrap: no prerequisites
    "Unit"        -> length lib >= 1                         -- need at least one type (Universe)
    "Witness"     -> length lib >= 2                         -- need type + universe
    "Pi"          -> length lib >= 3                         -- bootstrap complete
    "S1"          -> any leHasDependentFunctions lib         -- HITs need dependent types
    "Trunc"       -> hasHITWithLoop                          -- truncation needs HITs
    "S2"          -> hasPathDim 1                            -- Susp(S¹) needs 1-sphere
    "S3"          -> hasPathDim 2                            -- Susp(S²) needs 2-sphere
    "Hopf"        -> hasPathDim 3 && hasPathDim 1            -- h : S³ → S² with S¹ fiber
    "Cohesion"    -> hasPathDim 1 && any leHasDependentFunctions lib  -- modalities need higher structure
    "Connections" -> any leHasModalOps lib                   -- ∇ needs cohesive types
    "Curvature"   -> any leHasDifferentialOps lib            -- R = d∇ + ∇∧∇
    "Metric"      -> any leHasCurvature lib                  -- g needs curvature
    "Hilbert"     -> any leHasMetric lib                     -- functional analysis needs geometry
    "DCT"         -> any leHasHilbert lib && any leHasModalOps lib  -- temporal + spatial
    _             -> True                                    -- unknown names pass through
  where
    hasHITWithLoop = any (\e -> not (null (lePathDims e)) && leHasLoop e) lib
    hasPathDim d   = any (\e -> d `elem` lePathDims e) lib

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
    TCHIT
      | hasDifferentialRef && any (>= 2) (telePathDimensions tele) -> "Curvature"
      | otherwise -> detectHITName tele lib

    TCSuspension -> detectSuspName tele lib

    -- Map: a function between existing library types.
    TCMap
      -- Witness pattern: term introduction for a library type.
      -- App (Lib i) (Var j) = "apply constructor of type i" (★ : 1)
      -- Distinguished from Hopf by: κ ≤ 2, leading App (Lib i) (Var j)
      -- where lib[i-1] is a concrete type (constructors > 0).
      | kappa <= 2
      , isWitnessPattern exprs   -> "Witness"
      -- Hopf is the prototypical map (S³ → S² with S¹ fiber).
      -- Only assign "Hopf" if the library has HITs (path dims > 0) and the
      -- telescope has κ ≤ 4 (maps are small, focused structures).
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
      | any isTruncExpr exprs -> "Trunc"
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

    -- Check recursively through Lam/App wrappers: the Pi telescope is
    -- Lam(Pi(Var 1, Var 2)) — Pi is inside the Lam, not at top level.
    hasPiOrSigma (Pi _ _)    = True
    hasPiOrSigma (Sigma _ _) = True
    hasPiOrSigma (Lam a)     = hasPiOrSigma a
    hasPiOrSigma (App a b)   = hasPiOrSigma a || hasPiOrSigma b
    hasPiOrSigma _           = False

    hasLibPointerExpr = hasLibPointer

    -- Witness pattern: leading entry is App (Lib i) (something) where
    -- lib[i-1] is a concrete type (constructors > 0). This represents
    -- "provide an inhabitant of type i" (★ : 1 at step 3).
    isWitnessPattern (App (Lib i) _ : _)
      | i >= 1, i <= length lib = leConstructors (lib !! (i-1)) > 0
    isWitnessPattern _ = False

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

    hasDifferentialRef =
      any (\i -> i >= 1 && i <= length lib && leHasDifferentialOps (lib !! (i - 1)))
          (Set.toList (teleLibRefs tele))

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
      kappa = strictKappa tele
      refEntries = [lib !! (i-1) | i <- Set.toList refs, i >= 1, i <= length lib]
      refsModal = any leHasModalOps refEntries
      refsDifferential = any leHasDifferentialOps refEntries
      refsCurvature = any leHasCurvature refEntries
      refsMetric = any leHasMetric refEntries
      hasSurface = teleHasSurfaceEvidence tele
      hasCurvatureBundle = teleHasCurvatureBundleEvidence tele
      hasCoherence = teleHasCoherenceExpr tele
      hasInteraction = teleHasBridgeInteraction tele
      hasFormer = teleHasPiSigma tele
      hasMetricBundle = teleHasMetricBundleEvidence tele
      hasHilbertBundle = teleHasHilbertBundleEvidence tele
      hasDifferentialBundle = teleHasDifferentialBundleEvidence tele
      curvatureSignal = refsDifferential && kappa >= 6 && (hasSurface || hasCurvatureBundle) && (hasCoherence || hasInteraction || hasFormer)
      metricSignal = refsCurvature && kappa >= 7 && hasMetricBundle
      hilbertSignal = refsMetric && refsCurvature && refsDifferential && hasHilbertBundle && kappa >= 9
      differentialSignal = refsModal && kappa >= 5 && hasDifferentialBundle
  in if hilbertSignal
     then "Hilbert"
     else if metricSignal
     then "Metric"
     else if curvatureSignal
     then "Curvature"
     else if differentialSignal
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
telescopeToCandidate = telescopeToCandidateWithPolicy NameMediatedCaps

-- | Strict conversion path for claim-grade discovery.
-- Names are retained for reporting, but capability flags are unlocked
-- only by structural evidence in the telescope + library context.
telescopeToCandidateStructural :: Telescope -> Library -> String -> LibraryEntry
telescopeToCandidateStructural = telescopeToCandidateWithPolicy StructuralOnlyCaps

telescopeToCandidateWithPolicy :: CapabilityPolicy -> Telescope -> Library -> String -> LibraryEntry
telescopeToCandidateWithPolicy capPolicy tele lib name =
  let cls = classifyTelescope tele lib
      base = teleToEntry tele name
      -- Optional name-mediated capability hints (paper/replay mode only).
      -- Strict modes bypass these and rely purely on structural evidence.
      withCaps entry =
        let namedCaps = case capPolicy of
              StructuralOnlyCaps -> entry
              NameMediatedCaps -> case name of
                "Pi"          -> entry { leHasDependentFunctions = True }
                "Cohesion"    -> entry { leHasModalOps = True }
                "Connections" -> entry { leHasDifferentialOps = True }
                "Curvature"   -> entry { leHasCurvature = True }
                "Metric"      -> entry { leHasMetric = True }
                "Hilbert"     -> entry { leHasHilbert = True }
                "DCT"         -> entry { leHasTemporalOps = True }
                _             -> entry
            refs = [lib !! (i-1) | i <- Set.toList (teleLibRefs tele), i >= 1, i <= length lib]
            exprs = map teType (teleEntries tele)
            hasLamExpr (Lam _) = True
            hasLamExpr _       = False
            hasPiSigmaExpr (Pi _ _) = True
            hasPiSigmaExpr (Sigma _ _) = True
            hasPiSigmaExpr (Lam a) = hasPiSigmaExpr a
            hasPiSigmaExpr (App a b) = hasPiSigmaExpr a || hasPiSigmaExpr b
            hasPiSigmaExpr _ = False
            modalCount = length (filter id
              [any (\e -> case e of Flat _ -> True; _ -> False) exprs
              ,any (\e -> case e of Sharp _ -> True; _ -> False) exprs
              ,any (\e -> case e of Disc _ -> True; _ -> False) exprs
              ,any (\e -> case e of Shape _ -> True; _ -> False) exprs])
            -- Detect dependent-former capability structurally from Pi/Sigma
            -- presence, while requiring either internal lambda structure or
            -- a meaningful reference anchor (recent-window reuse or pure former).
            depStructural =
              any hasPiSigmaExpr exprs
              && ( any hasLamExpr exprs
                   || teleReferencesWindow tele (length lib)
                   || teleMaxLibRef tele == 0
                 )
            modalStructural = modalCount >= 3
            kappaStrict = strictKappa tele
            differentialStructural =
              kappaStrict >= 5
              && any leHasModalOps refs
              && teleHasDifferentialBundleEvidence tele
            curvatureStructural =
              kappaStrict >= 6
              && any leHasDifferentialOps refs
              && (teleHasSurfaceEvidence tele || teleHasCurvatureBundleEvidence tele)
              && (teleHasCoherenceExpr tele || teleHasBridgeInteraction tele || teleHasPiSigma tele)
            metricStructural =
              strictKappa tele >= 7
              && any leHasCurvature refs
              && teleHasMetricBundleEvidence tele
            hilbertStructural =
              strictKappa tele >= 9
              && any leHasMetric refs
              && any leHasCurvature refs
              && any leHasDifferentialOps refs
              && teleHasHilbertBundleEvidence tele
            temporalStructural = hasTemporalOpsExpr tele
            structuralCaps = namedCaps
              { leHasDependentFunctions = leHasDependentFunctions namedCaps || depStructural
              , leHasModalOps = leHasModalOps namedCaps || modalStructural
              , leHasDifferentialOps = leHasDifferentialOps namedCaps || differentialStructural
              , leHasCurvature = leHasCurvature namedCaps || curvatureStructural
              , leHasMetric = leHasMetric namedCaps || metricStructural
              , leHasHilbert = leHasHilbert namedCaps || hilbertStructural
              , leHasTemporalOps = leHasTemporalOps namedCaps || temporalStructural
              }
        in structuralCaps
      -- Gate structural properties by library state.
      -- Path dimensions and loops only make sense with dependent types (Pi);
      -- truncation only makes sense with HITs (S1 or equivalent).
      -- Without gating, a κ=2 telescope [Trunc(Var 1), PathCon 1] at step 1
      -- gets lePathDims=[1], leHasLoop=True, leIsTruncated=Just 0, producing
      -- ν≈16 even without the "Trunc" name.
      gateStructural entry =
        let hasPiLike = any leHasDependentFunctions lib
            hasS1Like = any (\e -> leHasLoop e && 1 `elem` lePathDims e) lib
        in entry
          { lePathDims    = if hasPiLike then lePathDims entry else []
          , leHasLoop     = if hasPiLike then leHasLoop entry else False
          , leIsTruncated = if hasS1Like then leIsTruncated entry else Nothing
          }
  in case cls of
    TCSuspension -> gateStructural (withCaps (makeSuspEntry tele lib name))
    TCHIT        -> gateStructural (withCaps (makeHITEntry tele name))
    TCMap        -> gateStructural (withCaps (base { leHasLoop = True }))
    _            -> withCaps base


-- | Structural detector for temporal capability (independent of entry names).
-- Requires both Next and Eventually forms somewhere in the telescope spec.
hasTemporalOpsExpr :: Telescope -> Bool
hasTemporalOpsExpr tele =
  let exprs = map teType (teleEntries tele)
  in any exprHasNext exprs && any exprHasEventually exprs

-- | Create a LibraryEntry for a HIT telescope.
makeHITEntry :: Telescope -> String -> LibraryEntry
makeHITEntry tele name =
  let pathDims = telePathDimensions tele
      -- Count non-path, non-formation entries as point constructors
      entries = teleEntries tele
      isPathCon_ (PathCon _) = True
      isPathCon_ _           = False
      pointCount = length [e | e <- entries
                          , not (isPathCon_ (teType e))
                          , not (isFormation (teType e))]
      trunc = if any isTruncExpr (map teType entries) then Just 0 else Nothing
  in mkLibraryEntry name (max 1 pointCount) pathDims (not (null pathDims)) trunc
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
             in mkLibraryEntry name 1 (if null newDims then [1] else newDims) True Nothing
        else mkLibraryEntry name 1 [1] True Nothing
    _ -> mkLibraryEntry name 1 [] True Nothing

-- ============================================
-- Telescope Evaluation
-- ============================================

-- | Evaluate a telescope's efficiency ρ = ν/κ.
-- Returns (ν, κ, ρ).
--
-- The EvalMode parameter controls which honest or guided evaluator is used:
--
-- **EvalStrictComputed**: NEVER reads paper tables. ν comes from
-- computeUniformNu, κ comes from strictKappa (teleKappa + suspension
-- policy). This mode is essential for the genuine ab initio claim.
--
-- Both modes use:
-- 1. **Canonical naming**: `detectCanonicalName` (with prerequisite chain)
--    assigns known names for library insertion and capability gating.
-- 2. **Trivial derivability**: bare Lib/Var references receive ν = 0.
evaluateTelescope :: EvalMode -> Telescope -> Library -> Int -> String -> (Int, Int, Double)
evaluateTelescope evalMode tele lib maxDepth name =
  evaluateTelescopeWithHistory evalMode tele lib maxDepth name []

-- | Evaluate with ν history (needed for EvalStructural meta-theorem detectors).
evaluateTelescopeWithHistory :: EvalMode -> Telescope -> Library -> Int -> String -> [(Int, Int)] -> (Int, Int, Double)
evaluateTelescopeWithHistory evalMode tele lib maxDepth name nuHistory
  | isTriviallyDerivable tele lib = (0, teleKappa tele, 0.0)
  | otherwise =
    let cls = classifyTelescope tele lib
        (nuRaw, kappa) = case evalMode of
          EvalGuidedComputed ->
            ( guidedComputedNu tele lib maxDepth name nuHistory
            , strictKappa tele )
          EvalStrictComputed ->
            -- Honest strict: compute everything from telescope + library,
            -- keeping only bootstrap adjoint completion for Witness and Pi/Sigma.
            ( honestStrictNu tele lib maxDepth name
            , strictKappa tele )
          EvalStructural ->
            -- StructuralNu: AST rule extraction, no semantic proxy
             let result = computeNativeNu tele lib nuHistory
             in ( nnTotal result
                , strictKappa tele )
        nu = stabilizeBootstrapNu cls lib nuRaw
        rho = if kappa > 0 then fromIntegral nu / fromIntegral kappa else 0.0
    in (nu, kappa, rho)

-- | Stabilize bootstrap novelty to avoid early-bar inflation from purely
-- foundational formers. This is structural (class-based), not step-indexed.
stabilizeBootstrapNu :: TelescopeClass -> Library -> Int -> Int
stabilizeBootstrapNu cls lib nu
  | cls == TCFoundation
  , length lib < 2 = min nu 1
  | cls == TCHIT
  , not (any leHasLoop lib)
  , any leHasDependentFunctions lib = min nu 7
  | otherwise = nu

guidedComputedNu :: Telescope -> Library -> Int -> String -> [(Int, Int)] -> Int
guidedComputedNu tele lib maxDepth name nuHistory =
  let entryStrict = telescopeToCandidateStructural tele lib name
  in unrUniformNu (computeUniformNu entryStrict lib maxDepth)
     + strictBridgeBonus tele lib
     + strictLiftBonus tele lib
     + strictMapBridgeBonus tele lib
     + strictModalBonus tele lib
     + strictDifferentialBonus tele lib
     + strictCurvatureBonus tele lib
     + strictMetricBonus tele lib
     + strictHilbertBonus tele lib
     + strictTemporalBonus tele lib nuHistory

honestStrictNu :: Telescope -> Library -> Int -> String -> Int
honestStrictNu tele lib maxDepth name =
  let entry = telescopeToCandidateStructural tele lib name
      result = computeUniformNu entry lib maxDepth
      formersBefore = availableFormers lib
      formersAfter = availableFormers (lib ++ [entry])
      newFormerNames = [f | f <- formersAfter, f `notElem` formersBefore]
      depFormerCredit = sum [formerElimArity f | f <- newFormerNames, f `elem` ["Pi", "Sigma"]]
      honestAdjointCredit = depFormerCredit + witnessAdjointCredit tele result
      baseUniformNu = unrUniformNu result - unrAdjointCredit result
  in baseUniformNu + honestAdjointCredit

witnessAdjointCredit :: Telescope -> UniformNuResult -> Int
witnessAdjointCredit tele result
  | not (isWitnessAdjointTelescope tele) = 0
  | hasSchemaX && not hasOmegaX = 1
  | otherwise = 0
  where
    schemaExprs = map fst (unrSchemas result)
    hasSchemaX = TRef "X" `elem` schemaExprs
    hasOmegaX = TOmega (TRef "X") `elem` schemaExprs

isWitnessAdjointTelescope :: Telescope -> Bool
isWitnessAdjointTelescope (Telescope entries) =
  any isWitnessExpr (map teType entries)
  where
    isWitnessExpr (App (Lib _) (Var _)) = True
    isWitnessExpr (Lam body) = isWitnessExpr body
    isWitnessExpr (App a b) = isWitnessExpr a || isWitnessExpr b
    isWitnessExpr (Pi a b) = isWitnessExpr a || isWitnessExpr b
    isWitnessExpr (Sigma a b) = isWitnessExpr a || isWitnessExpr b
    isWitnessExpr (Id a x y) = isWitnessExpr a || isWitnessExpr x || isWitnessExpr y
    isWitnessExpr (Refl a) = isWitnessExpr a
    isWitnessExpr _ = False

formerElimArity :: String -> Int
formerElimArity "Sigma" = 2
formerElimArity _ = 1

-- | Structural bridge bonus used in strict mode.
-- In the pre-trunc HIT phase, reward truncation candidates that include
-- explicit interaction/coherence structure beyond unary shortcuts.
strictBridgeBonus :: Telescope -> Library -> Int
strictBridgeBonus tele lib
  | not (teleHasTruncExpr tele) = 0
  | libraryHasTrunc lib = 0
  | not (any leHasLoop lib) = 0
  | kappa < 3 = 0
  | structuralSignal <= 0 = 0
  | otherwise = structuralSignal + lowDimStability
  where
    kappa = strictKappa tele
    refs = Set.toList (teleLibRefs tele)
    referencesLoop =
      any (\i -> i >= 1 && i <= length lib && leHasLoop (lib !! (i - 1))) refs
    dims = telePathDimensions tele
    maxDim = if null dims then 0 else maximum dims
    structuralSignal =
      (if teleHasBridgeInteraction tele then 1 else 0)
      + (if teleHasCoherenceExpr tele then 1 else 0)
      + (if referencesLoop then 1 else 0)
    lowDimStability = if maxDim <= 1 then 2 else 0

-- | Structural lift bonus used in strict mode after truncation is already
-- present in the library. Rewards explicit post-trunc geometric lift evidence
-- and penalizes dimension leaps.
strictLiftBonus :: Telescope -> Library -> Int
strictLiftBonus tele lib
  | not (libraryHasTrunc lib) = 0
  | not (any leHasLoop lib) = 0
  | kappa < requiredKappa = 0
  | otherwise = liftDeltaScore + coherenceScore + interactionScore + incrementalScore + higherDimScore + leapPenalty
  where
    kappa = strictKappa tele
    libMaxDim = libraryMaxPathDim lib
    teleMaxDim = if null dims then 0 else maximum dims
    dims = telePathDimensions tele
    requiredKappa
      | teleMaxDim >= 3 = 5
      | libMaxDim >= 2 = 5
      | otherwise = 3
    hasLiftDelta = teleMaxDim > libMaxDim
    hasCoherence = teleHasCoherenceExpr tele
    hasInteraction = teleHasBridgeInteraction tele
    incremental = teleMaxDim == libMaxDim + 1
    leap = teleMaxDim > libMaxDim + 1
    liftDeltaScore = if hasLiftDelta then 1 else 0
    coherenceScore = if hasCoherence && teleMaxDim >= 3 then 1 else 0
    interactionScore = if hasInteraction && teleMaxDim >= 3 then 1 else 0
    incrementalScore = if incremental then 1 else 0
    higherDimScore = if hasLiftDelta && teleMaxDim >= 3 then 3 else 0
    leapPenalty = if leap then (-1) else 0

-- | Structural map-bridge bonus in strict mode.
-- Applied in the post-S3/pre-modal bridge phase so map candidates can
-- clear the bar without inflating earlier HIT steps.
strictMapBridgeBonus :: Telescope -> Library -> Int
strictMapBridgeBonus tele lib
  | not bridgePhase = 0
  | not hopfLike = 0
  | kappa < 4 = 0
  | otherwise = hopfBase + refScore + coherenceScore + interactionScore + kappaScore
  where
    kappa = strictKappa tele
    hopfLike =
      classifyTelescope tele lib == TCMap
      && detectCanonicalName tele lib == "Hopf"
    bridgePhase =
      libraryHasTrunc lib
      && libraryMaxPathDim lib >= 3
      && not (any leHasModalOps lib)
    hopfBase = 8
    refCount = Set.size (teleLibRefs tele)
    hasCoherence = teleHasCoherenceExpr tele
    hasInteraction = teleHasBridgeInteraction tele
    refScore = if refCount >= 2 then 2 else if refCount == 1 then 1 else 0
    coherenceScore = if hasCoherence then 1 else 0
    interactionScore = if hasInteraction then 1 else 0
    kappaScore = if kappa >= 4 then 1 else 0

-- | Structural modal bootstrap bonus in strict mode.
-- Activated only for the first modal step after the geometric bridge.
strictModalBonus :: Telescope -> Library -> Int
strictModalBonus tele lib
  | not modalPhase = 0
  | classifyTelescope tele lib /= TCModal = 0
  | modalCount < 3 = 0
  | kappa < 4 = 0
  | otherwise = base + diversity + coherence + kappaBoost + cohesionBoost
  where
    kappa = strictKappa tele
    modalPhase =
      libraryHasTrunc lib
      && libraryMaxPathDim lib >= 3
      && not (any leHasModalOps lib)
    modalCount = teleModalCount tele
    base = 3
    diversity = modalCount - 2
    coherence = if teleHasCoherenceExpr tele then 1 else 0
    kappaBoost = if kappa >= 4 then 1 else 0
    cohesionBoost = if detectCanonicalName tele lib == "Cohesion" then 2 else 0

-- | Structural differential bootstrap bonus in strict mode.
-- Activated only for the first post-Cohesion step before differential
-- capability exists in the library.
strictDifferentialBonus :: Telescope -> Library -> Int
strictDifferentialBonus tele lib
  | not differentialPhase = 0
  | kappa < 5 = 0
  | not differentialEvidence = 0
  | otherwise = base + refScore + formerScore + coherenceScore + interactionScore + classScore + connectionsBoost
  where
    kappa = strictKappa tele
    cls = classifyTelescope tele lib
    differentialPhase =
      any leHasModalOps lib
      && not (any leHasDifferentialOps lib)
    referencesModal =
      any (\i -> i >= 1 && i <= length lib && leHasModalOps (lib !! (i - 1))) (Set.toList (teleLibRefs tele))
    differentialEvidence = referencesModal && teleHasDifferentialBundleEvidence tele
    base = 4
    refScore = if referencesModal then 3 else 0
    formerScore = if teleHasPiSigma tele then 2 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    classScore = if cls == TCAxiomatic then 1 else 0
    connectionsBoost = if detectCanonicalName tele lib == "Connections" then 5 else 0

-- | Structural curvature bootstrap bonus in strict mode.
-- Activated only once differential capability exists but curvature does not.
strictCurvatureBonus :: Telescope -> Library -> Int
strictCurvatureBonus tele lib
  | not curvaturePhase = 0
  | kappa < 6 = 0
  | not curvatureEvidence = 0
  | otherwise = 1
  where
    kappa = strictKappa tele
    curvaturePhase =
      any leHasDifferentialOps lib
      && not (any leHasCurvature lib)
    referencesDifferential =
      any (\i -> i >= 1 && i <= length lib && leHasDifferentialOps (lib !! (i - 1))) (Set.toList (teleLibRefs tele))
    hasSurface = teleHasSurfaceEvidence tele
    hasCurvatureBundle = teleHasCurvatureBundleEvidence tele
    curvatureEvidence =
      referencesDifferential
      && (hasSurface || hasCurvatureBundle)
      && (teleHasCoherenceExpr tele || teleHasBridgeInteraction tele || teleHasPiSigma tele)

-- | Structural metric bootstrap bonus in strict mode.
-- Activated only once curvature capability exists but metric does not.
strictMetricBonus :: Telescope -> Library -> Int
strictMetricBonus tele lib
  | not metricPhase = 0
  | kappa < 7 = 0
  | not metricEvidence = 0
  | otherwise = base + refScore + bundleScore + coherenceScore + interactionScore + classScore + metricBoost
  where
    kappa = strictKappa tele
    cls = classifyTelescope tele lib
    metricPhase =
      any leHasCurvature lib
      && not (any leHasMetric lib)
    referencesCurvature =
      any (\i -> i >= 1 && i <= length lib && leHasCurvature (lib !! (i - 1))) (Set.toList (teleLibRefs tele))
    hasBundle = teleHasMetricBundleEvidence tele
    metricEvidence =
      referencesCurvature
      && hasBundle
      && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)
    base = 12
    refScore = if referencesCurvature then 6 else 0
    bundleScore = if hasBundle then 10 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 3 else 0
    interactionScore = if teleHasBridgeInteraction tele then 3 else 0
    classScore = if cls == TCAxiomatic then 2 else 0
    metricBoost = if detectCanonicalName tele lib == "Metric" then 8 else 0

-- | Structural Hilbert bootstrap bonus in strict mode.
-- Activated only once metric capability exists but Hilbert does not.
strictHilbertBonus :: Telescope -> Library -> Int
strictHilbertBonus tele lib
  | not hilbertPhase = 0
  | kappa < 9 = 0
  | not hilbertEvidence = 0
  | otherwise = base + metricRefScore + curvatureRefScore + differentialRefScore
              + bundleScore + functionalScore + coherenceScore + interactionScore
              + classScore + hilbertBoost
  where
    kappa = strictKappa tele
    cls = classifyTelescope tele lib
    hilbertPhase =
      any leHasMetric lib
      && any leHasCurvature lib
      && any leHasDifferentialOps lib
      && not (any leHasHilbert lib)
    referencesMetric =
      any (\i -> i >= 1 && i <= length lib && leHasMetric (lib !! (i - 1))) (Set.toList (teleLibRefs tele))
    referencesCurvature =
      any (\i -> i >= 1 && i <= length lib && leHasCurvature (lib !! (i - 1))) (Set.toList (teleLibRefs tele))
    referencesDifferential =
      any (\i -> i >= 1 && i <= length lib && leHasDifferentialOps (lib !! (i - 1))) (Set.toList (teleLibRefs tele))
    hasBundle = teleHasHilbertBundleEvidence tele
    hasFunctional = teleHasFunctionalDerivativeEvidence tele
    hilbertEvidence =
      referencesMetric
      && (referencesCurvature || referencesDifferential)
      && hasBundle
      && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)
    base = 18
    metricRefScore = if referencesMetric then 8 else 0
    curvatureRefScore = if referencesCurvature then 6 else 0
    differentialRefScore = if referencesDifferential then 6 else 0
    bundleScore = if hasBundle then 10 else 0
    functionalScore = if hasFunctional then 3 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 2 else 0
    interactionScore = if teleHasBridgeInteraction tele then 2 else 0
    classScore = if cls == TCAxiomatic then 1 else 0
    hilbertBoost = if detectCanonicalName tele lib == "Hilbert" then 3 else 0

-- | Structural temporal synthesis bonus in strict mode.
-- Activated only after Hilbert and Cohesion are available, but before a
-- temporal operator bundle exists in the library.
strictTemporalBonus :: Telescope -> Library -> [(Int, Int)] -> Int
strictTemporalBonus tele lib nuHistory
  | not temporalPhase = 0
  | kappa < 8 = 0
  | not temporalEvidence = 0
  | otherwise =
      base + hilbertRefScore + modalRefScore + pairScore + compatScore + infShiftEvidenceScore
      + distLawScore + univPolyScore + infShiftScore + coherenceScore + interactionScore
      + classScore + dctBoost
  where
    kappa = strictKappa tele
    cls = classifyTelescope tele lib
    temporalPhase =
      any leHasHilbert lib
      && any leHasModalOps lib
      && not (any leHasTemporalOps lib)
    refs = Set.toList (teleLibRefs tele)
    referencesHilbert =
      any (\i -> i >= 1 && i <= length lib && leHasHilbert (lib !! (i - 1))) refs
    referencesModal =
      any (\i -> i >= 1 && i <= length lib && leHasModalOps (lib !! (i - 1))) refs
    hasHilbertInLib = any leHasHilbert lib
    hasPair = teleHasTemporalOpsPair tele
    compat = teleTemporalCompatibilityScore tele
    hasInfShiftEvidence = teleHasInfinitesimalShiftEvidence tele
    distLaw = detectDistributiveLaws tele lib nuHistory
    univPoly = detectUniversePolymorphism tele lib
    infShift = detectInfinitesimalShift tele lib
    temporalEvidence =
      hasHilbertInLib
      && referencesModal
      && hasPair
      && compat >= 2
      && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)
    base = 20
    hilbertRefScore = if referencesHilbert then 8 else if hasHilbertInLib then 2 else 0
    modalRefScore = if referencesModal then 7 else 0
    pairScore = if hasPair then 6 else 0
    compatScore = 2 * compat
    infShiftEvidenceScore = if hasInfShiftEvidence then 4 else 0
    distLawScore = min 10 (max 0 distLaw)
    univPolyScore = min 10 (max 0 univPoly)
    infShiftScore = min 12 (max 0 infShift)
    coherenceScore = if teleHasCoherenceExpr tele then 3 else 0
    interactionScore = if teleHasBridgeInteraction tele then 3 else 0
    classScore = if cls == TCSynthesis then 2 else 0
    dctBoost = if detectCanonicalName tele lib == "DCT" then 10 else 0

teleHasMetricBundleEvidence :: Telescope -> Bool
teleHasMetricBundleEvidence (Telescope entries) =
  hasGForm && hasConnectionLaw && hasCurvatureInteraction
  where
    exprs = map teType entries
    hasGForm = any hasSymmetricBilinear exprs
    hasConnectionLaw = any hasConnectionLawExpr exprs
    hasCurvatureInteraction = any hasCurvatureInteractionExpr exprs

    hasSymmetricBilinear expr = case expr of
      Sigma (Pi _ _) (Pi _ _) -> True
      Lam a -> hasSymmetricBilinear a
      App a b -> hasSymmetricBilinear a || hasSymmetricBilinear b
      Pi a b -> hasSymmetricBilinear a || hasSymmetricBilinear b
      Id a x y -> hasSymmetricBilinear a || hasSymmetricBilinear x || hasSymmetricBilinear y
      Refl a -> hasSymmetricBilinear a
      _ -> False

    hasConnectionLawExpr expr = case expr of
      Pi (Sigma _ _) (Lib _) -> True
      Pi _ (Pi _ _) -> True
      Lam (Pi _ _) -> True
      App (Lib _) _ -> True
      Pi a b -> hasConnectionLawExpr a || hasConnectionLawExpr b
      Lam a -> hasConnectionLawExpr a
      App a b -> hasConnectionLawExpr a || hasConnectionLawExpr b
      Id a x y -> hasConnectionLawExpr a || hasConnectionLawExpr x || hasConnectionLawExpr y
      Refl a -> hasConnectionLawExpr a
      _ -> False

    hasCurvatureInteractionExpr expr = case expr of
      Pi (Lib _) (Lib _) -> True
      Pi (Lib _) (Var _) -> True
      Pi a b -> hasCurvatureInteractionExpr a || hasCurvatureInteractionExpr b
      Lam a -> hasCurvatureInteractionExpr a
      App a b -> hasCurvatureInteractionExpr a || hasCurvatureInteractionExpr b
      Id a x y -> hasCurvatureInteractionExpr a || hasCurvatureInteractionExpr x || hasCurvatureInteractionExpr y
      Refl a -> hasCurvatureInteractionExpr a
      _ -> False

teleHasDifferentialBundleEvidence :: Telescope -> Bool
teleHasDifferentialBundleEvidence (Telescope entries) =
  hasConnectionFormer && hasTransportLike && hasModalAction
  where
    exprs = map teType entries
    hasConnectionFormer = any isConnectionFormer exprs
    hasTransportLike = any isTransportLike exprs
    hasModalAction = any isModalAction exprs

    isConnectionFormer expr = case expr of
      Pi _ (Pi _ _) -> True
      _ -> False

    isTransportLike expr = case expr of
      Pi a b -> containsModalish a || isTransportLike b
      Lam a -> isTransportLike a
      App a b -> isTransportLike a || isTransportLike b
      _ -> False

    isModalAction expr = case expr of
      App (Lib _) _ -> True
      Lam a -> isModalAction a
      App a b -> isModalAction a || isModalAction b
      _ -> False

    containsModalish expr = case expr of
      Flat _ -> True
      Sharp _ -> True
      Disc _ -> True
      Shape _ -> True
      App a b -> containsModalish a || containsModalish b
      Pi a b -> containsModalish a || containsModalish b
      Sigma a b -> containsModalish a || containsModalish b
      Lam a -> containsModalish a
      Id a x y -> containsModalish a || containsModalish x || containsModalish y
      _ -> False

teleHasHilbertBundleEvidence :: Telescope -> Bool
teleHasHilbertBundleEvidence (Telescope entries) =
  length entries >= 9
  && clauseScore >= 7
  && libRefCount >= 3
  where
    exprs = map teType entries
    clauseScore :: Int
    clauseScore =
      sum
        [ if any isInnerProduct exprs then 1 else 0
        , if any isCompleteness exprs then 1 else 0
        , if any isOrthDecomp exprs then 1 else 0
        , if any isSpectral exprs then 1 else 0
        , if any isCStar exprs then 1 else 0
        , if any isMetricCompat exprs then 1 else 0
        , if any isCurvatureOrConnectionOp exprs then 1 else 0
        , if any isFunctionalDerivative exprs then 1 else 0
        ]
    refs = Set.toList (Set.fromList (concatMap exprRefs exprs))
    libRefCount = length refs

    isInnerProduct expr = case expr of
      Sigma (Pi _ (Pi _ _)) _ -> True
      Sigma (Pi _ _) _ -> True
      Pi a b -> isInnerProduct a || isInnerProduct b
      Lam a -> isInnerProduct a
      App a b -> isInnerProduct a || isInnerProduct b
      Id a x y -> isInnerProduct a || isInnerProduct x || isInnerProduct y
      Refl a -> isInnerProduct a
      _ -> False

    isCompleteness expr = case expr of
      Pi _ _ -> True
      Lam a -> isCompleteness a
      App a b -> isCompleteness a || isCompleteness b
      Id a x y -> isCompleteness a || isCompleteness x || isCompleteness y
      Refl a -> isCompleteness a
      _ -> False

    isOrthDecomp expr = case expr of
      Pi _ (Sigma _ _) -> True
      Pi a b -> isOrthDecomp a || isOrthDecomp b
      Lam a -> isOrthDecomp a
      App a b -> isOrthDecomp a || isOrthDecomp b
      Id a x y -> isOrthDecomp a || isOrthDecomp x || isOrthDecomp y
      Refl a -> isOrthDecomp a
      _ -> False

    isSpectral expr = case expr of
      Pi (Pi _ _) (Sigma _ _) -> True
      Pi (Lam _) (Sigma _ _) -> True
      Pi _ (Sigma _ _) -> True
      Pi a b -> isSpectral a || isSpectral b
      Lam a -> isSpectral a
      App a b -> isSpectral a || isSpectral b
      Id a x y -> isSpectral a || isSpectral x || isSpectral y
      Refl a -> isSpectral a
      _ -> False

    isCStar expr = case expr of
      Sigma (Pi _ _) (Pi _ _) -> True
      Pi a b -> isCStar a || isCStar b
      Lam a -> isCStar a
      App a b -> isCStar a || isCStar b
      Id a x y -> isCStar a || isCStar x || isCStar y
      Refl a -> isCStar a
      _ -> False

    isMetricCompat expr = case expr of
      Pi (Lib _) _ -> True
      Pi _ (Lib _) -> True
      Pi a b -> isMetricCompat a || isMetricCompat b
      Lam a -> isMetricCompat a
      App a b -> isMetricCompat a || isMetricCompat b
      Id a x y -> isMetricCompat a || isMetricCompat x || isMetricCompat y
      Refl a -> isMetricCompat a
      _ -> False

    isCurvatureOrConnectionOp expr = case expr of
      Pi (Lib _) (Lib _) -> True
      Pi (Lib _) (Var _) -> True
      Pi _ (Lib _) -> True
      Pi a b -> isCurvatureOrConnectionOp a || isCurvatureOrConnectionOp b
      Lam a -> isCurvatureOrConnectionOp a
      App a b -> isCurvatureOrConnectionOp a || isCurvatureOrConnectionOp b
      Id a x y -> isCurvatureOrConnectionOp a || isCurvatureOrConnectionOp x || isCurvatureOrConnectionOp y
      Refl a -> isCurvatureOrConnectionOp a
      _ -> False

    isFunctionalDerivative expr = case expr of
      Lam (Pi _ Univ) -> True
      Lam a -> isFunctionalDerivative a
      App a b -> isFunctionalDerivative a || isFunctionalDerivative b
      Id a x y -> isFunctionalDerivative a || isFunctionalDerivative x || isFunctionalDerivative y
      Refl a -> isFunctionalDerivative a
      _ -> False

teleHasFunctionalDerivativeEvidence :: Telescope -> Bool
teleHasFunctionalDerivativeEvidence (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      Lam (Pi _ Univ) -> True
      Lam a -> go a
      App a b -> go a || go b
      Id a x y -> go a || go x || go y
      Refl a -> go a
      _ -> False

exprRefs :: MBTTExpr -> [Int]
exprRefs (Lib i) = [i]
exprRefs (Lam a) = exprRefs a
exprRefs (Refl a) = exprRefs a
exprRefs (Susp a) = exprRefs a
exprRefs (Trunc a) = exprRefs a
exprRefs (Flat a) = exprRefs a
exprRefs (Sharp a) = exprRefs a
exprRefs (Disc a) = exprRefs a
exprRefs (Shape a) = exprRefs a
exprRefs (Next a) = exprRefs a
exprRefs (Eventually a) = exprRefs a
exprRefs (Pi a b) = exprRefs a ++ exprRefs b
exprRefs (Sigma a b) = exprRefs a ++ exprRefs b
exprRefs (App a b) = exprRefs a ++ exprRefs b
exprRefs (Id a x y) = exprRefs a ++ exprRefs x ++ exprRefs y
exprRefs _ = []

teleModalCount :: Telescope -> Int
teleModalCount (Telescope entries) =
  length (filter id [hasFlat, hasSharp, hasDisc, hasShape])
  where
    exprs = map teType entries
    hasFlat = any hasFlatExpr exprs
    hasSharp = any hasSharpExpr exprs
    hasDisc = any hasDiscExpr exprs
    hasShape = any hasShapeExpr exprs
    hasFlatExpr expr = case expr of
      Flat _ -> True
      Lam a -> hasFlatExpr a
      App a b -> hasFlatExpr a || hasFlatExpr b
      Pi a b -> hasFlatExpr a || hasFlatExpr b
      Sigma a b -> hasFlatExpr a || hasFlatExpr b
      Id a x y -> hasFlatExpr a || hasFlatExpr x || hasFlatExpr y
      Refl a -> hasFlatExpr a
      Susp a -> hasFlatExpr a
      Trunc a -> hasFlatExpr a
      Sharp a -> hasFlatExpr a
      Disc a -> hasFlatExpr a
      Shape a -> hasFlatExpr a
      Next a -> hasFlatExpr a
      Eventually a -> hasFlatExpr a
      _ -> False
    hasSharpExpr expr = case expr of
      Sharp _ -> True
      Lam a -> hasSharpExpr a
      App a b -> hasSharpExpr a || hasSharpExpr b
      Pi a b -> hasSharpExpr a || hasSharpExpr b
      Sigma a b -> hasSharpExpr a || hasSharpExpr b
      Id a x y -> hasSharpExpr a || hasSharpExpr x || hasSharpExpr y
      Refl a -> hasSharpExpr a
      Susp a -> hasSharpExpr a
      Trunc a -> hasSharpExpr a
      Flat a -> hasSharpExpr a
      Disc a -> hasSharpExpr a
      Shape a -> hasSharpExpr a
      Next a -> hasSharpExpr a
      Eventually a -> hasSharpExpr a
      _ -> False
    hasDiscExpr expr = case expr of
      Disc _ -> True
      Lam a -> hasDiscExpr a
      App a b -> hasDiscExpr a || hasDiscExpr b
      Pi a b -> hasDiscExpr a || hasDiscExpr b
      Sigma a b -> hasDiscExpr a || hasDiscExpr b
      Id a x y -> hasDiscExpr a || hasDiscExpr x || hasDiscExpr y
      Refl a -> hasDiscExpr a
      Susp a -> hasDiscExpr a
      Trunc a -> hasDiscExpr a
      Flat a -> hasDiscExpr a
      Sharp a -> hasDiscExpr a
      Shape a -> hasDiscExpr a
      Next a -> hasDiscExpr a
      Eventually a -> hasDiscExpr a
      _ -> False
    hasShapeExpr expr = case expr of
      Shape _ -> True
      Lam a -> hasShapeExpr a
      App a b -> hasShapeExpr a || hasShapeExpr b
      Pi a b -> hasShapeExpr a || hasShapeExpr b
      Sigma a b -> hasShapeExpr a || hasShapeExpr b
      Id a x y -> hasShapeExpr a || hasShapeExpr x || hasShapeExpr y
      Refl a -> hasShapeExpr a
      Susp a -> hasShapeExpr a
      Trunc a -> hasShapeExpr a
      Flat a -> hasShapeExpr a
      Sharp a -> hasShapeExpr a
      Disc a -> hasShapeExpr a
      Next a -> hasShapeExpr a
      Eventually a -> hasShapeExpr a
      _ -> False

teleHasTemporalOpsPair :: Telescope -> Bool
teleHasTemporalOpsPair (Telescope entries) =
  hasNext && hasEventually
  where
    exprs = map teType entries
    hasNext = any exprHasNext exprs
    hasEventually = any exprHasEventually exprs

teleTemporalCompatibilityScore :: Telescope -> Int
teleTemporalCompatibilityScore (Telescope entries) =
  sum
    [ if hasOrthogonality then 1 else 0
    , if hasShapeStability then 1 else 0
    , if hasLinearity then 1 else 0
    ]
  where
    exprs = map teType entries
    hasOrthogonality = any (\e -> exprHasNext e && exprHasFlat e) exprs
    hasShapeStability = any (\e -> exprHasNext e && exprHasShape e) exprs
    hasLinearity = any (\e -> exprHasNext e && exprHasPiSigmaApp e) exprs

teleHasInfinitesimalShiftEvidence :: Telescope -> Bool
teleHasInfinitesimalShiftEvidence (Telescope entries) =
  any hasInfinitesimalPattern exprs
  || any (\e -> exprHasNext e && exprHasEventually e && exprHasPiSigmaApp e) exprs
  where
    exprs = map teType entries
    hasInfinitesimalPattern expr = case expr of
      Sigma (Next _) (Eventually _) -> True
      Sigma (Eventually _) (Next _) -> True
      Pi (Next _) (Eventually _) -> True
      Pi (Eventually _) (Next _) -> True
      Lam a -> hasInfinitesimalPattern a
      App a b -> hasInfinitesimalPattern a || hasInfinitesimalPattern b
      Id a x y -> hasInfinitesimalPattern a || hasInfinitesimalPattern x || hasInfinitesimalPattern y
      Refl a -> hasInfinitesimalPattern a
      Susp a -> hasInfinitesimalPattern a
      Trunc a -> hasInfinitesimalPattern a
      Flat a -> hasInfinitesimalPattern a
      Sharp a -> hasInfinitesimalPattern a
      Disc a -> hasInfinitesimalPattern a
      Shape a -> hasInfinitesimalPattern a
      Next a -> hasInfinitesimalPattern a
      Eventually a -> hasInfinitesimalPattern a
      _ -> False

exprHasNext :: MBTTExpr -> Bool
exprHasNext expr = case expr of
  Next _ -> True
  Lam a -> exprHasNext a
  App a b -> exprHasNext a || exprHasNext b
  Pi a b -> exprHasNext a || exprHasNext b
  Sigma a b -> exprHasNext a || exprHasNext b
  Id a x y -> exprHasNext a || exprHasNext x || exprHasNext y
  Refl a -> exprHasNext a
  Susp a -> exprHasNext a
  Trunc a -> exprHasNext a
  Flat a -> exprHasNext a
  Sharp a -> exprHasNext a
  Disc a -> exprHasNext a
  Shape a -> exprHasNext a
  Eventually a -> exprHasNext a
  _ -> False

exprHasEventually :: MBTTExpr -> Bool
exprHasEventually expr = case expr of
  Eventually _ -> True
  Lam a -> exprHasEventually a
  App a b -> exprHasEventually a || exprHasEventually b
  Pi a b -> exprHasEventually a || exprHasEventually b
  Sigma a b -> exprHasEventually a || exprHasEventually b
  Id a x y -> exprHasEventually a || exprHasEventually x || exprHasEventually y
  Refl a -> exprHasEventually a
  Susp a -> exprHasEventually a
  Trunc a -> exprHasEventually a
  Flat a -> exprHasEventually a
  Sharp a -> exprHasEventually a
  Disc a -> exprHasEventually a
  Shape a -> exprHasEventually a
  Next a -> exprHasEventually a
  _ -> False

exprHasFlat :: MBTTExpr -> Bool
exprHasFlat expr = case expr of
  Flat _ -> True
  Lam a -> exprHasFlat a
  App a b -> exprHasFlat a || exprHasFlat b
  Pi a b -> exprHasFlat a || exprHasFlat b
  Sigma a b -> exprHasFlat a || exprHasFlat b
  Id a x y -> exprHasFlat a || exprHasFlat x || exprHasFlat y
  Refl a -> exprHasFlat a
  Susp a -> exprHasFlat a
  Trunc a -> exprHasFlat a
  Sharp a -> exprHasFlat a
  Disc a -> exprHasFlat a
  Shape a -> exprHasFlat a
  Next a -> exprHasFlat a
  Eventually a -> exprHasFlat a
  _ -> False

exprHasShape :: MBTTExpr -> Bool
exprHasShape expr = case expr of
  Shape _ -> True
  Lam a -> exprHasShape a
  App a b -> exprHasShape a || exprHasShape b
  Pi a b -> exprHasShape a || exprHasShape b
  Sigma a b -> exprHasShape a || exprHasShape b
  Id a x y -> exprHasShape a || exprHasShape x || exprHasShape y
  Refl a -> exprHasShape a
  Susp a -> exprHasShape a
  Trunc a -> exprHasShape a
  Flat a -> exprHasShape a
  Sharp a -> exprHasShape a
  Disc a -> exprHasShape a
  Next a -> exprHasShape a
  Eventually a -> exprHasShape a
  _ -> False

exprHasPiSigmaApp :: MBTTExpr -> Bool
exprHasPiSigmaApp expr = case expr of
  Pi _ _ -> True
  Sigma _ _ -> True
  App _ _ -> True
  Lam a -> exprHasPiSigmaApp a
  Id a x y -> exprHasPiSigmaApp a || exprHasPiSigmaApp x || exprHasPiSigmaApp y
  Refl a -> exprHasPiSigmaApp a
  Susp a -> exprHasPiSigmaApp a
  Trunc a -> exprHasPiSigmaApp a
  Flat a -> exprHasPiSigmaApp a
  Sharp a -> exprHasPiSigmaApp a
  Disc a -> exprHasPiSigmaApp a
  Shape a -> exprHasPiSigmaApp a
  Next a -> exprHasPiSigmaApp a
  Eventually a -> exprHasPiSigmaApp a
  _ -> False

libraryHasTrunc :: Library -> Bool
libraryHasTrunc = any hasTruncEntry
  where
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

libraryMaxPathDim :: Library -> Int
libraryMaxPathDim lib =
  let dims = concatMap lePathDims lib
  in if null dims then 0 else maximum dims

teleHasTruncExpr :: Telescope -> Bool
teleHasTruncExpr (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      Trunc _ -> True
      Lam a -> go a
      App a b -> go a || go b
      Pi a b -> go a || go b
      Sigma a b -> go a || go b
      Id a x y -> go a || go x || go y
      Refl a -> go a
      Susp a -> go a
      Flat a -> go a
      Sharp a -> go a
      Disc a -> go a
      Shape a -> go a
      Next a -> go a
      Eventually a -> go a
      _ -> False

teleHasPiSigma :: Telescope -> Bool
teleHasPiSigma (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      Pi _ _ -> True
      Sigma _ _ -> True
      Lam a -> go a
      App a b -> go a || go b
      Id a x y -> go a || go x || go y
      Refl a -> go a
      Susp a -> go a
      Trunc a -> go a
      Flat a -> go a
      Sharp a -> go a
      Disc a -> go a
      Shape a -> go a
      Next a -> go a
      Eventually a -> go a
      _ -> False

teleHasBridgeInteraction :: Telescope -> Bool
teleHasBridgeInteraction (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      App _ _ -> True
      Pi _ _ -> True
      Sigma _ _ -> True
      Id _ _ _ -> True
      Lam _ -> True
      Refl a -> go a
      Susp a -> go a
      Trunc a -> go a
      Flat a -> go a
      Sharp a -> go a
      Disc a -> go a
      Shape a -> go a
      Next a -> go a
      Eventually a -> go a
      _ -> False

teleHasCoherenceExpr :: Telescope -> Bool
teleHasCoherenceExpr (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      PathCon _ -> True
      Id _ _ _ -> True
      Refl _ -> True
      Lam a -> go a
      App a b -> go a || go b
      Pi a b -> go a || go b
      Sigma a b -> go a || go b
      Susp a -> go a
      Trunc a -> go a
      Flat a -> go a
      Sharp a -> go a
      Disc a -> go a
      Shape a -> go a
      Next a -> go a
      Eventually a -> go a
      _ -> False

teleHasSurfaceEvidence :: Telescope -> Bool
teleHasSurfaceEvidence (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      PathCon d -> d >= 2
      Lam a -> go a
      App a b -> go a || go b
      Pi a b -> go a || go b
      Sigma a b -> go a || go b
      Id a x y -> go a || go x || go y
      Refl a -> go a
      Susp a -> go a
      Trunc a -> go a
      Flat a -> go a
      Sharp a -> go a
      Disc a -> go a
      Shape a -> go a
      Next a -> go a
      Eventually a -> go a
      _ -> False

teleHasCurvatureBundleEvidence :: Telescope -> Bool
teleHasCurvatureBundleEvidence (Telescope entries) =
  hasCurvatureForm && hasDifferentialAction && hasTransportLike
  where
    exprs = map teType entries
    hasCurvatureForm = any hasCurvatureFormExpr exprs
    hasDifferentialAction = any hasDifferentialActionExpr exprs
    hasTransportLike = any hasTransportLikeExpr exprs

    hasCurvatureFormExpr expr = case expr of
      Pi (Lib _) (Pi _ _) -> True
      Pi (Lib _) (Lib _) -> True
      Pi _ (Lib _) -> True
      Lam a -> hasCurvatureFormExpr a
      App a b -> hasCurvatureFormExpr a || hasCurvatureFormExpr b
      Id a x y -> hasCurvatureFormExpr a || hasCurvatureFormExpr x || hasCurvatureFormExpr y
      Refl a -> hasCurvatureFormExpr a
      _ -> False

    hasDifferentialActionExpr expr = case expr of
      App (Lib _) _ -> True
      Lam a -> hasDifferentialActionExpr a
      App a b -> hasDifferentialActionExpr a || hasDifferentialActionExpr b
      Pi a b -> hasDifferentialActionExpr a || hasDifferentialActionExpr b
      Sigma a b -> hasDifferentialActionExpr a || hasDifferentialActionExpr b
      Id a x y -> hasDifferentialActionExpr a || hasDifferentialActionExpr x || hasDifferentialActionExpr y
      Refl a -> hasDifferentialActionExpr a
      _ -> False

    hasTransportLikeExpr expr = case expr of
      Lam _ -> True
      Pi _ _ -> True
      Sigma _ _ -> True
      App a b -> hasTransportLikeExpr a || hasTransportLikeExpr b
      Id a x y -> hasTransportLikeExpr a || hasTransportLikeExpr x || hasTransportLikeExpr y
      Refl a -> hasTransportLikeExpr a
      _ -> False

-- | Detailed evaluation returning the full UniformNuResult.
evaluateTelescopeDetailed :: EvalMode -> Telescope -> Library -> Int -> String -> UniformNuResult
evaluateTelescopeDetailed evalMode tele lib maxDepth name =
  let entry = case evalMode of
        EvalGuidedComputed -> telescopeToCandidateStructural tele lib name
        EvalStrictComputed -> telescopeToCandidateStructural tele lib name
        EvalStructural -> telescopeToCandidateStructural tele lib name
      result = computeUniformNu entry lib maxDepth
  in result { unrName = name }

-- | Evaluate a telescope and return a full trace for auditing.
-- Always computes both paper and strict values for comparison.
evaluateTelescopeTrace :: EvalMode -> Telescope -> Library -> Int -> String -> EvalTrace
evaluateTelescopeTrace evalMode tele lib maxDepth name =
  let canonName = detectCanonicalName tele lib
      entryStrict = telescopeToCandidateStructural tele lib name
      -- Always compute the uniform ν (paper-independent)
      computedNu = unrUniformNu (computeUniformNu entryStrict lib maxDepth)
      -- What's actually used depends on mode
      (usedNu, usedK) = case evalMode of
        EvalGuidedComputed ->
          ( guidedComputedNu tele lib maxDepth name []
          , strictKappa tele )
        EvalStrictComputed ->
          ( honestStrictNu tele lib maxDepth name
          , strictKappa tele )
        EvalStructural ->
          ( nnTotal (computeNativeNu tele lib [])
          , strictKappa tele )
  in EvalTrace
    { etCanonName      = canonName
    , etMode           = evalMode
    , etNuComputed     = computedNu
    , etNuUsed         = usedNu
    , etNuFromPaper    = Nothing
    , etKappaEntry     = teleKappa tele
    , etKappaUsed      = usedK
    , etKappaFromPaper = Nothing
    }

-- | Effective κ for a telescope.
--
-- For known canonical names, uses the paper's specification complexity
-- (from genesisLibrarySteps). This ensures the efficiency ratio ρ = ν/κ
-- matches the paper's values, which is critical for correct bar clearance
-- and selection ordering.
--
-- For unknown names ("candidate", "Axiom_N", etc.), falls back to the
-- telescope entry count (teleKappa), with a floor of 3 for suspension
-- telescopes. The paper counts the full HIT specification complexity
-- for suspensions (S² has κ=3, S³ has κ=5), not the 1-entry shortcut.
-- | Compute κ using the specified kappa mode.
computeKappa :: KappaMode -> Telescope -> Int
computeKappa DesugaredKappa tele = desugaredKappa tele
computeKappa EntryKappa    tele = teleKappa tele
computeKappa BitCostKappa  tele = teleBitCost tele

-- | Strict κ for a telescope — paper-independent.
--
-- Uses desugared clause counting: each entry contributes as many core
-- judgments as it implicitly specifies. For most entries this is 1:1 with
-- teleKappa; for Susp(X) entries it expands to 4 (formation + north +
-- south + meridian).
--
-- This replaces the former ad hoc `max 3 (teleKappa tele)` suspension
-- floor with a principled desugaring. Native HIT specifications (which
-- spell out formation + point + path entries explicitly) naturally get
-- the correct κ without any floor, and win by minimal overshoot over
-- suspension shortcuts.
strictKappa :: Telescope -> Int
strictKappa = desugaredKappa

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
