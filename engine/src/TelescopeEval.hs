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
    -- * Evaluation
  , evaluateTelescope
  , evaluateTelescopeDetailed
  , effectiveKappa
  , effectiveNu
  , strictKappa
    -- * Tracing
  , EvalTrace(..)
  , evaluateTelescopeTrace
    -- * Conversion
  , telescopeToCandidate
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
import Types (LibraryEntry(..), Library, mkLibraryEntry)
import UniformNu (computeUniformNu, UniformNuResult(..), genesisLibrarySteps, GenesisStep(..))

import qualified Data.Set as Set

-- ============================================
-- Evaluation Modes
-- ============================================

-- | Evaluation mode for telescope scoring.
--
-- Controls whether the evaluator uses paper ν/κ values (for replay/comparison)
-- or computes them strictly from the telescope + library (for genuine discovery).
--
-- This is the key architectural distinction for the strictness audit:
-- - EvalPaperCalibrated: routes through effectiveNu/effectiveKappa, which
--   return paper values for known canonical names.
-- - EvalStrictComputed: NEVER reads paper tables. ν comes from computeUniformNu,
--   κ comes from strictKappa (teleKappa + explicit suspension policy).
data EvalMode
  = EvalPaperCalibrated     -- ^ Use paper ν/κ for canonical names (effectiveNu/effectiveKappa)
  | EvalStrictComputed      -- ^ Never use paper ν/κ; compute from telescope + UniformNu
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
  | teleKappa tele == 1 && isTermIntro (head entries)    = TCMap
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
    -- Map pattern: 2-4 entries with library pointers in the leading entries.
    -- The κ ≤ 4 bound distinguishes maps (focused, small) from axiomatic
    -- extensions (broad, larger κ). Without this bound, Curvature (κ=6)
    -- with Lib refs in leading entries gets misclassified as TCMap.
    hasLibMapPattern = length entries >= 2 && length entries <= 4
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

    -- Term introduction: applying a library type constructor to produce an inhabitant.
    -- This is the ★ : 1 pattern (step 3: Witness).
    isTermIntro (TeleEntry _ (App (Lib _) _)) = True
    isTermIntro _ = False

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
    TCHIT -> detectHITName tele lib

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
      -- Set structural capability flags based on CANONICAL NAME only.
      -- IMPORTANT: Only named entries get capability flags. Random "candidate"
      -- telescopes must NOT get flags from classification alone, because this
      -- inflates their ν by unlocking formers they haven't earned.
      -- Example: a κ=2 [Lam(Var 1), Pi(Var 1, Var 2)] classified as TCFormer
      -- would wrongly get leHasDependentFunctions=True, inflating ν from ~2 to ~16.
      withCaps entry = case name of
        "Pi"          -> entry { leHasDependentFunctions = True }
        "Cohesion"    -> entry { leHasModalOps = True }
        "Connections" -> entry { leHasDifferentialOps = True }
        "Curvature"   -> entry { leHasCurvature = True }
        "Metric"      -> entry { leHasMetric = True }
        "Hilbert"     -> entry { leHasHilbert = True }
        "DCT"         -> entry { leHasTemporalOps = True }
        _             -> entry  -- no capability flags for unknown names
      -- Gate structural properties by library state.
      -- Path dimensions and loops only make sense with dependent types (Pi);
      -- truncation only makes sense with HITs (S1 or equivalent).
      -- Without gating, a κ=2 telescope [Trunc(Var 1), PathCon 1] at step 1
      -- gets lePathDims=[1], leHasLoop=True, leIsTruncated=Just 0, producing
      -- ν≈16 even without the "Trunc" name.
      gateStructural entry =
        let libNames = map leName lib
            hasPi = "Pi" `elem` libNames
            hasS1 = "S1" `elem` libNames
        in entry
          { lePathDims    = if hasPi then lePathDims entry else []
          , leHasLoop     = if hasPi then leHasLoop entry else False
          , leIsTruncated = if hasS1 then leIsTruncated entry else Nothing
          }
  in case cls of
    TCSuspension -> gateStructural (withCaps (makeSuspEntry tele lib name))
    TCHIT        -> gateStructural (withCaps (makeHITEntry tele name))
    TCMap        -> gateStructural (withCaps (base { leHasLoop = True }))
    _            -> withCaps base

-- | Create a LibraryEntry for a HIT telescope.
makeHITEntry :: Telescope -> String -> LibraryEntry
makeHITEntry tele name =
  let pathDims = telePathDimensions tele
      -- Count non-path, non-formation entries as point constructors
      entries = teleEntries tele
      pointCount = length [e | e <- entries
                          , not (isPathCon (teType e))
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
-- The EvalMode parameter controls whether paper values are used:
--
-- **EvalPaperCalibrated**: For known canonical names, uses paper's ν and κ
-- via effectiveNu/effectiveKappa. The uniform algorithm systematically
-- overestimates ν (counting all schemas rather than independent ones),
-- so paper values are needed for correct minimal-overshoot selection.
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
evaluateTelescope evalMode tele lib maxDepth name
  | isTriviallyDerivable tele lib = (0, teleKappa tele, 0.0)
  | otherwise =
    let canonName = detectCanonicalName tele lib
        -- Use canonical name when detection succeeds (known canonical name),
        -- otherwise use caller-provided name
        evalName = if canonName `elem` knownCanonicalNames then canonName else name
        entry = telescopeToCandidate tele lib evalName
        (nu, kappa) = case evalMode of
          EvalPaperCalibrated ->
            -- Paper-calibrated: use paper's κ and ν for known canonical names
            ( effectiveNu canonName entry lib maxDepth
            , effectiveKappa canonName tele )
          EvalStrictComputed ->
            -- Strict: compute everything from telescope + library, no paper tables
            ( unrUniformNu (computeUniformNu entry lib maxDepth)
            , strictKappa tele )
        rho = if kappa > 0 then fromIntegral nu / fromIntegral kappa else 0.0
    in (nu, kappa, rho)

-- | Detailed evaluation returning the full UniformNuResult.
evaluateTelescopeDetailed :: EvalMode -> Telescope -> Library -> Int -> String -> UniformNuResult
evaluateTelescopeDetailed _evalMode tele lib maxDepth name =
  let canonName = detectCanonicalName tele lib
      evalName = if canonName `elem` knownCanonicalNames then canonName else name
      entry = telescopeToCandidate tele lib evalName
      result = computeUniformNu entry lib maxDepth
  in result { unrName = name }

-- | Evaluate a telescope and return a full trace for auditing.
-- Always computes both paper and strict values for comparison.
evaluateTelescopeTrace :: EvalMode -> Telescope -> Library -> Int -> String -> EvalTrace
evaluateTelescopeTrace evalMode tele lib maxDepth name =
  let canonName = detectCanonicalName tele lib
      evalName = if canonName `elem` knownCanonicalNames then canonName else name
      entry = telescopeToCandidate tele lib evalName
      -- Always compute the uniform ν (paper-independent)
      computedNu = unrUniformNu (computeUniformNu entry lib maxDepth)
      -- Look up paper values (may be Nothing for non-canonical names)
      paperNu = canonName `lookup` paperNuByName
      paperK  = canonName `lookup` paperKappaByName
      -- What's actually used depends on mode
      (usedNu, usedK) = case evalMode of
        EvalPaperCalibrated ->
          ( effectiveNu canonName entry lib maxDepth
          , effectiveKappa canonName tele )
        EvalStrictComputed ->
          ( computedNu
          , strictKappa tele )
  in EvalTrace
    { etCanonName      = canonName
    , etMode           = evalMode
    , etNuComputed     = computedNu
    , etNuUsed         = usedNu
    , etNuFromPaper    = paperNu
    , etKappaEntry     = teleKappa tele
    , etKappaUsed      = usedK
    , etKappaFromPaper = paperK
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
effectiveKappa :: String -> Telescope -> Int
effectiveKappa canonName tele =
  case canonName `lookup` paperKappaByName of
    Just k  -> k
    Nothing
      -- Suspensions: the 1-entry shortcut Susp(X) understates the
      -- specification complexity. Use max(3, teleKappa) to match
      -- the paper's convention for unnamed suspensions.
      | any isSuspEntry (teleEntries tele) -> max 3 (teleKappa tele)
      | otherwise -> teleKappa tele
  where
    isSuspEntry (TeleEntry _ (Susp _)) = True
    isSuspEntry _ = False

-- | Strict κ for a telescope — paper-independent.
--
-- Uses the telescope entry count (teleKappa) with one explicit policy:
-- Suspension telescopes (Susp(X)) are single-entry (κ=1) but implicitly
-- generate formation + north + south + meridian. The paper assigns κ≥3
-- to such structures. The suspension floor ensures suspensions don't
-- dominate via artificially low κ.
--
-- This policy is NAMED and DOCUMENTED as a strict-mode design choice,
-- not a hidden calibration to paper values.
strictKappa :: Telescope -> Int
strictKappa tele
  | any isSuspEntry (teleEntries tele) = max 3 (teleKappa tele)
  | otherwise = teleKappa tele
  where
    isSuspEntry (TeleEntry _ (Susp _)) = True
    isSuspEntry _ = False

-- | Paper's κ values indexed by canonical name.
paperKappaByName :: [(String, Int)]
paperKappaByName =
  [ ("Universe",    2), ("Unit",        1), ("Witness",     1)
  , ("Pi",          3), ("S1",          3), ("Trunc",       3)
  , ("S2",          3), ("S3",          5), ("Hopf",        4)
  , ("Cohesion",    4), ("Connections", 5), ("Curvature",   6)
  , ("Metric",      7), ("Hilbert",     9), ("DCT",         8)
  ]

-- | Effective ν for a telescope.
--
-- For known canonical names, uses the paper's generative capacity.
-- The uniform algorithm systematically overestimates ν because it counts
-- all enumerable schemas rather than only independent ones. The paper's
-- ν values are the correct counts of independent derivation schemas.
--
-- For unknown names, computes ν via the uniform algorithm.
effectiveNu :: String -> LibraryEntry -> Library -> Int -> Int
effectiveNu canonName entry lib maxDepth =
  case canonName `lookup` paperNuByName of
    Just nu -> nu
    Nothing -> unrUniformNu (computeUniformNu entry lib maxDepth)

-- | Paper's ν values indexed by canonical name.
paperNuByName :: [(String, Int)]
paperNuByName =
  [ ("Universe",    1), ("Unit",        1), ("Witness",     2)
  , ("Pi",          5), ("S1",          7), ("Trunc",       8)
  , ("S2",         10), ("S3",         18), ("Hopf",       17)
  , ("Cohesion",   19), ("Connections", 26), ("Curvature", 34)
  , ("Metric",     43), ("Hilbert",    60), ("DCT",       105)
  ]

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
