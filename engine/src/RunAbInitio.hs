{-# LANGUAGE BangPatterns #-}

-- | Ab Initio Discovery Engine
--
-- Runs the full PEN synthesis pipeline from an empty library:
--   1. Start with empty library B = {}
--   2. At each step n, exhaustively enumerate all valid telescopes at κ ≤ κ_max
--   3. Evaluate each telescope's ρ = ν/κ against the current library
--   4. Select the telescope with minimal overshoot (ρ - bar) among bar-clearing candidates
--   5. Add the discovered structure to the library
--   6. Repeat until the sequence terminates (ν = 0 for all candidates)
--
-- Two modes:
--   StrictAbInitio   — default discovery mode (paper-independent)
--   GuidedRecovery   — legacy guided recovery with seeds and family steering
--
-- Two-phase search:
--   Phase A: Exhaustive enumeration for κ ≤ 3 (tractable, finds all structures)
--   Phase B: MCTS for κ > 3 (guided random search in the larger space)
--
-- Canonical naming is gated by a prerequisite chain (TelescopeEval.hasPrerequisites):
-- e.g. "Trunc" requires "S1" in the library. Combined with effective κ
-- (paper's specification complexity for known names), this ensures correct
-- selection ordering.

module Main where

import Telescope
import TelescopeGen (enumerateTelescopes, GoalProfile(..), GoalIntent(..), deriveGoalProfile, defaultGoalProfile)
import MBTTEnum ( enumerateMBTTTelescopes
                 , enumerateMBTTTelescopesWithDiagnostics
                 , defaultEnumConfig
                 , MBTTCandidate(..)
                 , EnumStepCache
                 , emptyEnumStepCache
                 , EnumConfig(..)
                 , FrontierMode(..)
                 , EnumBandDiagnostics(..)
                 )
import AbInitioPolicy (StrictAdmissibility(..), strictAdmissibility, strictStepBudgetSeconds)
import AgendaSearch ( agendaGenerateCandidatesWithDiagnostics
                    , defaultAgendaConfig
                    , AgendaConfig(..)
                    , AgendaCandidate(..)
                    , AgendaDiagnostics(..)
                    , NearMiss(..)
                    )
import MBTTCanonical (CanonKey(..), canonicalKeySpec)
import MBTTDecode (decodeCanonicalNameWithKey, DecodeResult(..))
import Parallel (parMapChunkedWHNF)
import TelescopeEval (EvalMode(..), KappaMode(..), evaluateTelescopeWithHistory,
                      telescopeToCandidateStructural,
                      validateReferenceTelescopes, detectCanonicalName)
import TelescopeCheck (checkAndFilter, checkTelescope, CheckResult(..))
import MCTS
import StrictCritic (ObligationSummary(..), FrontierDiagnostics(..), analyzeObligations, closureScore,
                     dependentMotiveDensity, formerLifecycleScore, genericBinderCount,
                     internalAdjointScore, trueEliminatorScore)
import StrictMinimality (terminalSCCSubBundles)
import StrictMolecules (ClauseProvenance(..), CompiledMolecule(..), enumerateStrictMolecularCandidates)
import MBTTNu (computeNativeNu, NativeNuResult(..))
import Kolmogorov (MBTTExpr(..))
import Types (Library, LibraryEntry(..))
import CoherenceWindow (dBonacciDelta)

import Data.List (sortOn, nub, foldl')
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (when)
import System.IO (IOMode(WriteMode), hFlush, hPutStr, stdout, stderr, withFile, hSetEncoding, utf8)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import GHC.Stats (getRTSStatsEnabled, getRTSStats, gc, gcdetails_live_bytes, gcdetails_mem_in_use_bytes, max_mem_in_use_bytes)
import Text.Printf (printf)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

-- ============================================
-- Mode Selection
-- ============================================

-- | Ab initio synthesis mode.
data AbInitioMode
  = GuidedRecovery    -- ^ Legacy guided recovery with seeds, family rescue, and canonical steering
  | StrictAbInitio    -- ^ Honest strict lane: discovered ν/κ only, no hidden guidance
  deriving (Show, Eq)

-- | Topological bonus calibration for molecular strict evaluation.
data TopoBonusMode
  = TopoExact
  | TopoLinear
  | TopoNone
  deriving (Show, Eq)

-- | Configuration for the ab initio run.
data AbInitioConfig = AbInitioConfig
  { cfgMode            :: !AbInitioMode
  , cfgWindow          :: !Int              -- ^ Coherence window depth d (default 2 = Fibonacci)
  , cfgCsv             :: !(Maybe FilePath) -- ^ Optional CSV output file
  , cfgKappaMode       :: !KappaMode        -- ^ Kappa computation mode (default DesugaredKappa)
  , cfgNoCanonPriority :: !Bool             -- ^ Ablation: disable canonical name priority in selection
  , cfgMaxRho          :: !Bool             -- ^ Ablation: select max ρ instead of minimal overshoot
  , cfgTopoBonusMode   :: !TopoBonusMode    -- ^ Ablation: weaken/disable molecular path bonus
  , cfgMBTTFirst       :: !Bool             -- ^ Phase-1 gate: enumerate via MBTTEnum
  , cfgLegacyGenerator :: !Bool             -- ^ Phase-7 fallback: explicitly use legacy generator path
  , cfgMBTTMaxCand     :: !(Maybe Int)      -- ^ Optional cap for MBTT enumerator candidate count
  , cfgMBTTAstDepth    :: !(Maybe Int)      -- ^ Optional override for MBTT AST depth in Phase A
  , cfgMaxSteps        :: !Int              -- ^ Optional early stop for shadow-mode runs (<=15)
  , cfgSkipValidation  :: !Bool             -- ^ Skip Phase 0 reference validation (faster shadow runs)
  , cfgMBTTShadowProfile :: !Bool           -- ^ Use tighter MBTT Phase-1 bounds for shadow runs
  , cfgSkipMCTS        :: !Bool             -- ^ Skip MCTS phase (useful for bounded shadow evidence)
  , cfgPhase1Shadow    :: !Bool             -- ^ Preset: bounded Phase-1 MBTT shadow run
  , cfgNoCanonicalQuotient :: !Bool         -- ^ Ablation: disable canonical quotient cache at candidate stage
  , cfgAdaptiveMemory  :: !Bool             -- ^ Auto-tune search budgets from RTS memory pressure
  , cfgMemorySafe      :: !Bool             -- ^ Force aggressive memory-safe throttling
  , cfgSeed            :: !Int              -- ^ Base deterministic seed for search components
  , cfgPrefixReport    :: !(Maybe FilePath) -- ^ Optional per-step regression telemetry CSV
  } deriving (Show)

-- | Discovery history: accumulated (ν, κ) pairs from each step.
data DiscoveryRecord = DiscoveryRecord
  { drNu    :: !Int
  , drKappa :: !Int
  } deriving (Show)

-- | Full step record for CSV output and post-hoc analysis.
data StepRecord = StepRecord
  { srStep   :: !Int
  , srName   :: !String
  , srNu     :: !Int
  , srKappa  :: !Int
  , srRho    :: !Double
  , srBar    :: !Double
  , srDelta  :: !Int
  , srSource :: !String
  , srCands  :: !Int
  , srRawCands :: !Int
  , srCanonCands :: !Int
  , srDedupeRatio :: !Double
  , srBestCanonKey :: !String
  , srTele   :: Telescope  -- ^ Discovered telescope (for post-hoc analysis)
  } deriving (Show)

-- | Candidate tuple used during step search/selection.
type Candidate = (Telescope, Int, Int, Double, String)

-- | Coarse memory pressure buckets used for adaptive throttling.
data MemoryPressure
  = MemLow
  | MemModerate
  | MemHigh
  | MemCritical
  deriving (Show, Eq, Ord)

-- | Runtime memory snapshot from RTS stats (MiB).
data MemorySnapshot = MemorySnapshot
  { msLiveMiB :: !Int
  , msInUseMiB :: !Int
  , msPeakMiB :: !Int
  } deriving (Show)

-- | Step-local search limits after adaptive memory tuning.
data SearchBudget = SearchBudget
  { sbBitBudget      :: !Int
  , sbAstDepth       :: !Int
  , sbMaxCandidates  :: !Int
  , sbMCTSIterations :: !Int
  , sbMCTSDepth      :: !Int
  , sbMCTSTopK       :: !Int
  , sbForceSkipMCTS  :: !Bool
  , sbPressure       :: !MemoryPressure
  , sbSnapshot       :: !(Maybe MemorySnapshot)
  } deriving (Show)

data RetryBudget = RetryBudget
  { rbTier             :: !Int
  , rbBitBudget        :: !Int
  , rbAstDepth         :: !Int
  , rbMaxCandidates    :: !Int
  , rbAgendaStates     :: !Int
  , rbAgendaCandidates :: !Int
  , rbBranchPerState   :: !Int
  , rbLeafExprCap      :: !Int
  } deriving (Show, Eq)

data SearchAttempt = SearchAttempt
  { saRetryTier        :: !Int
  , saBudgetBit        :: !Int
  , saBudgetDepth      :: !Int
  , saBudgetCandidates :: !Int
  , saAgendaStates     :: !Int
  , saAgendaCandidates :: !Int
  , saEnumMs           :: !Double
  , saCheckMs          :: !Double
  , saEvalMs           :: !Double
  , saAttemptMs        :: !Double
  , saRawTelescopes    :: !Int
  , saValidTelescopes  :: !Int
  , saViableCount      :: !Int
  , saCandidates       :: ![Candidate]
  , saAgendaDiag       :: !AgendaDiagnostics
  , saAgendaActive     :: !Bool
  } deriving (Show)

data RequiredFamily
  = FamilyUniverseGenesis
  | FamilyFoundation
  | FamilyFormer
  | FamilyWitnessIntro
  | FamilyBootstrapHIT
  | FamilyTrunc
  | FamilyPostTrunc
  | FamilyMapBridge
  | FamilyModal
  | FamilyDifferential
  | FamilyCurvature
  | FamilyMetric
  | FamilyHilbert
  | FamilyTemporal
  deriving (Show, Eq)

data PrefixDiagnostics = PrefixDiagnostics
  { pdRetryTier          :: !Int
  , pdRetryCause         :: !String
  , pdGuidanceMode       :: !String
  , pdAdmissibilityKappaCap :: !(Maybe Int)
  , pdSearchedKappaBands :: !String
  , pdSelectedKappaBand  :: !(Maybe Int)
  , pdStepBudgetSeconds  :: !(Maybe Int)
  , pdElapsedSeconds     :: !(Maybe Double)
  , pdEvaluatedCandidates :: !Int
  , pdDedupedCandidates  :: !Int
  , pdMinimalityRejections :: !Int
  , pdBestObligationScore :: !(Maybe Int)
  , pdBestInterfaceDensity :: !(Maybe Int)
  , pdBestGenericityScore :: !(Maybe Int)
  , pdBestClosureScore :: !(Maybe Int)
  , pdFrontierCandidates :: !Int
  , pdFrontierKept :: !Int
  , pdMCTSSeedStates :: !Int
  , pdMCTSCompletedStates :: !Int
  , pdRequiredFamily     :: !String
  , pdBestFamilyRho      :: !(Maybe Double)
  , pdBestFamilyKappa    :: !(Maybe Int)
  , pdFamilyCandidates   :: !Int
  , pdFamilyViable       :: !Int
  } deriving (Show)

data HonestBandTrace = HonestBandTrace
  { hbtBand :: !Int
  , hbtElapsedSeconds :: !Double
  , hbtFrontier :: !FrontierDiagnostics
  , hbtEvaluated :: !Int
  , hbtDeduped :: !Int
  , hbtViable :: !Int
  } deriving (Show)

data StrictBandSeed = StrictBandSeed
  { sbsTelescope :: !Telescope
  , sbsConceptualKappa :: !(Maybe Int)
  , sbsAxiomaticExports :: !(Maybe Int)
  , sbsClauseProvenance :: !(Maybe [ClauseProvenance])
  , sbsSource :: !String
  } deriving (Show)

data EvalCacheKey = EvalCacheKey
  { eckMode     :: !String
  , eckTelescope :: !String
  , eckNuDepth  :: !Int
  , eckHistory  :: !String
  } deriving (Show, Eq, Ord)

type EvalCache = Map.Map EvalCacheKey (Int, Int, Double)

data HonestSearchState = HonestSearchState
  { hsEnumCache           :: !EnumStepCache
  , hsEvalCache           :: !EvalCache
  , hsSeenKeys            :: !(Set.Set CanonKey)
  , hsSeedByKey           :: !(Map.Map CanonKey StrictBandSeed)
  , hsRawCandidates       :: !Int
  , hsEvaluatedCandidates :: !Int
  , hsDedupedCandidates   :: !Int
  , hsMinimalityRejects   :: !Int
  , hsSearchedBands       :: ![Int]
  , hsViableCandidates    :: ![Candidate]
  , hsDebugCandidates     :: ![Candidate]
  , hsRejectedCandidates  :: ![Candidate]
  , hsBandTraces          :: ![HonestBandTrace]
  , hsFailedFrontier      :: ![(Telescope, ObligationSummary)]
  , hsMCTSSeedStates      :: !Int
  , hsMCTSCompletedStates :: !Int
  } deriving (Show)

-- | Convert synthesis mode to evaluation mode.
-- GuidedRecovery retains the legacy guided-computed evaluator.
-- StrictAbInitio never reads paper tables — all ν/κ computed from telescope + library.
toEvalMode :: AbInitioMode -> EvalMode
toEvalMode GuidedRecovery     = EvalGuidedComputed
toEvalMode StrictAbInitio     = EvalStrictComputed

isHonestMode :: AbInitioMode -> Bool
isHonestMode StrictAbInitio = True
isHonestMode _ = False

guidanceModeLabel :: AbInitioMode -> String
guidanceModeLabel GuidedRecovery = "guided-recovery"
guidanceModeLabel StrictAbInitio = "honest-strict"

retiredAbInitioFlags :: [String]
retiredAbInitioFlags =
  [ "--structural"
  , "--paper"
  , "--paper-calibrated-benchmark"
  ]

rejectRetiredAbInitioFlags :: [String] -> IO ()
rejectRetiredAbInitioFlags args =
  case filter (`elem` retiredAbInitioFlags) args of
    [] -> return ()
    flags ->
      ioError
        (userError
          ("Removed ab-initio flag(s): " ++ intercalate ", " flags
          ++ "\nUse `cabal run ab-initio -- --strict ...` for the supported discovery lane."
          ++ "\nStructural analysis remains library-only; paper-calibrated replay has been retired."))

-- ============================================
-- Main Entry Point
-- ============================================

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  rejectRetiredAbInitioFlags args
  let cfg = parseArgs args

  when (cfgNoCanonPriority cfg) $
    error "Ablation rankers (--no-canonical-priority) are disabled in claim-grade discovery."

  putStrLn "============================================"
  putStrLn "PEN Ab Initio Discovery Engine"
  printf   "Mode: %s, d=%d\n" (show (cfgMode cfg)) (cfgWindow cfg)
  putStrLn "============================================"
  case cfgMode cfg of
    GuidedRecovery -> do
      putStrLn "  Evaluator: EvalGuidedComputed (legacy strict bonuses and family steering)"
      putStrLn "  Bar:       Discovered novelty/cost history only"
      putStrLn "  Library:   Discovered entries only, guided recovery enabled"
    StrictAbInitio -> do
      putStrLn "  Evaluator: EvalStrictComputed (uniform structure count + bootstrap adjoint completion only)"
      putStrLn "  Bar:       Discovered novelty/cost history only"
      putStrLn "  Library:   Discovered entries only, no seeds/family rescue/canonical steering"
      putStrLn "  Search:    Exact cost bands under explicit admissibility caps"
  printf   "  Window:    d=%d (%s)\n" (cfgWindow cfg) (windowName (cfgWindow cfg))
  printf   "  Cost:      %s\n" (show (cfgKappaMode cfg))
  printf   "  Topology:  %s\n" (topoBonusModeLabel (cfgTopoBonusMode cfg))
  when (cfgMBTTFirst cfg) $
    putStrLn "  SEARCH:    MBTT-first default active (typed MBTT enumeration in Phase A)"
  when (cfgLegacyGenerator cfg) $
    putStrLn "  FALLBACK:  --legacy-generator (deprecated template-first generator path)"
  when (cfgMaxRho cfg) $
    putStrLn "  ABLATION:  --max-rho (greedy bar-clearer selection instead of minimal overshoot)"
  case cfgMBTTAstDepth cfg of
    Just astDepth ->
      printf   "  SEARCH:    --mbtt-ast-depth %d (manual Phase-A AST depth override)\n" astDepth
    Nothing -> return ()
  when (cfgMaxSteps cfg < 15) $
    printf   "  SHADOW:    --max-steps %d (early-stop run)\n" (cfgMaxSteps cfg)
  when (cfgSkipValidation cfg) $
    putStrLn "  SPEED:     --skip-validation (Phase 0 validation skipped)"
  when (cfgMBTTShadowProfile cfg) $
    putStrLn "  PROFILE:   --mbtt-shadow-profile (tighter MBTT Phase-1 bounds)"
  when (cfgSkipMCTS cfg) $
    putStrLn "  SPEED:     --skip-mcts (disable Phase B MCTS)"
  when (cfgPhase1Shadow cfg) $
    putStrLn "  PRESET:    --phase1-shadow (bounded MBTT-first profile)"
  when (cfgNoCanonicalQuotient cfg) $
    putStrLn "  ABLATION:  --no-canonical-quotient (disable candidate canonical dedupe)"
  when (cfgAdaptiveMemory cfg) $
    putStrLn "  MEMORY:    adaptive memory/pagefile guards enabled"
  when (cfgMemorySafe cfg) $
    putStrLn "  MEMORY:    --memory-safe (aggressive throttling)"
  printf   "  SEED:      %d\n" (cfgSeed cfg)
  case cfgPrefixReport cfg of
    Just path ->
      printf   "  REPORT:    --prefix-report %s\n" path
    Nothing -> return ()
  putStrLn ""
  putStrLn "Starting from EMPTY LIBRARY."
  putStrLn "The engine will autonomously discover the Generative Sequence."
  putStrLn ""

  when (cfgLegacyGenerator cfg) $ do
    putStrLn "WARNING: --legacy-generator is deprecated in Phase 7 and kept only as rollback fallback."
    putStrLn "         Prefer default MBTT-first mode for all primary evidence lanes."

  -- Phase 0: Validate reference telescopes (uses canonical names for paper comparison)
  if cfgSkipValidation cfg
    then do
      putStrLn "--- Phase 0: Validation skipped (claim-grade default) ---"
      putStrLn ""
    else do
      putStrLn "--- Phase 0: Validating Reference Telescopes ---"
      putStrLn ""
      validatePhase

  -- Phase 1: Run the ab initio synthesis loop
  putStrLn ""
  printf   "--- Phase 1: Ab Initio Synthesis (%s, d=%d) ---\n" (show (cfgMode cfg)) (cfgWindow cfg)
  putStrLn ""
  abInitioLoop cfg

-- | Parse command line arguments.
parseArgs :: [String] -> AbInitioConfig
parseArgs args =
  let mode = if "--guided-recovery" `elem` args then GuidedRecovery else StrictAbInitio
      window = case dropWhile (/= "--window") args of
                 ("--window" : n : _) -> case reads n of
                   [(d, "")] | d >= 1 && d <= 5 -> d
                   _ -> 2
                 _ -> 2
      csv = case dropWhile (/= "--csv") args of
              ("--csv" : f : _) -> Just f
              _ -> Nothing
      kappaMode = case dropWhile (/= "--kappa-mode") args of
                    ("--kappa-mode" : "entry" : _) -> EntryKappa
                    ("--kappa-mode" : "bitcost" : _) -> BitCostKappa
                    _ -> DesugaredKappa
      noCanonPriority = "--no-canonical-priority" `elem` args
      maxRho = "--max-rho" `elem` args
      topoBonusMode = case dropWhile (/= "--topo-bonus-mode") args of
                        ("--topo-bonus-mode" : "linear" : _) -> TopoLinear
                        ("--topo-bonus-mode" : "none" : _) -> TopoNone
                        _ -> TopoExact
      mbttFirstFlag = "--mbtt-first" `elem` args
      legacyGenerator = "--legacy-generator" `elem` args
      mbttMaxCand = case dropWhile (/= "--mbtt-max-candidates") args of
                      ("--mbtt-max-candidates" : n : _) ->
                        case reads n of
                          [(k,"")] | k > 0 -> Just k
                          _ -> Nothing
                      _ -> Nothing
      mbttAstDepth = case dropWhile (/= "--mbtt-ast-depth") args of
                       ("--mbtt-ast-depth" : n : _) ->
                         case reads n of
                           [(k,"")] | k >= 1 && k <= 5 -> Just k
                           _ -> Nothing
                       _ -> Nothing
      hasMaxStepsArg = "--max-steps" `elem` args
      maxSteps = case dropWhile (/= "--max-steps") args of
                   ("--max-steps" : n : _) -> case reads n of
                     [(k,"")] | k >= 1 && k <= 15 -> k
                     _ -> 15
                   _ -> 15
      phase1Shadow = "--phase1-shadow" `elem` args
      withReferenceValidation = "--with-reference-validation" `elem` args
      skipValidation = phase1Shadow || "--skip-validation" `elem` args || not withReferenceValidation
      mbttShadowProfile = phase1Shadow || "--mbtt-shadow-profile" `elem` args
      skipMCTS = phase1Shadow || "--skip-mcts" `elem` args
      mbttFirstFinal = phase1Shadow || mbttFirstFlag || not legacyGenerator
      maxStepsFinal = if phase1Shadow && not hasMaxStepsArg then 6 else maxSteps
      noCanonicalQuotient = "--no-canonical-quotient" `elem` args
      adaptiveMemory = not ("--no-adaptive-memory" `elem` args)
      memorySafe = "--memory-safe" `elem` args
      seed = case dropWhile (/= "--seed") args of
               ("--seed" : n : _) -> case reads n of
                 [(k, "")] -> k
                 _ -> 42
               _ -> 42
      prefixReport = case dropWhile (/= "--prefix-report") args of
                       ("--prefix-report" : f : _) -> Just f
                       _ -> Nothing
      mbttMaxCandFinal = if phase1Shadow
                         then case mbttMaxCand of
                           Just k  -> Just k
                           Nothing -> Just 20
                         else mbttMaxCand
  in AbInitioConfig mode window csv kappaMode noCanonPriority maxRho topoBonusMode mbttFirstFinal legacyGenerator mbttMaxCandFinal mbttAstDepth maxStepsFinal skipValidation mbttShadowProfile skipMCTS phase1Shadow noCanonicalQuotient adaptiveMemory memorySafe seed prefixReport

-- | Human-readable name for window depth.
windowName :: Int -> String
windowName 1 = "constant — extensional"
windowName 2 = "Fibonacci — intensional HoTT"
windowName 3 = "tribonacci"
windowName d = show d ++ "-bonacci"

topoBonusModeLabel :: TopoBonusMode -> String
topoBonusModeLabel mode = case mode of
  TopoExact -> "m + d^2 (strict default)"
  TopoLinear -> "m + d (ablation)"
  TopoNone -> "m only (ablation)"

-- ============================================
-- Reference Telescope Validation
-- ============================================

validatePhase :: IO ()
validatePhase = do
  let results = validateReferenceTelescopes 2
  printf "%-4s %-14s %10s %10s %6s %s\n"
    ("Step" :: String) ("Name" :: String)
    ("Target" :: String) ("Novelty" :: String) ("Cost" :: String)
    ("Status" :: String)
  putStrLn (replicate 60 '-')
  mapM_ printValidation results
  let ok = length [() | (_, _, _, _, True) <- results]
      total = length results
  putStrLn ""
  printf "Validation: %d/%d telescopes produce non-zero novelty\n" ok total
  where
    printValidation (step, name, paperNu, teleNu, match) =
      let status = if match then "OK" else "NEEDS_WORK" :: String
          tele = referenceTelescope step
          kappa = teleKappa tele
      in printf "%-4d %-14s %10d %10d %6d %s\n" step name paperNu teleNu kappa status

-- ============================================
-- Ab Initio Synthesis Loop
-- ============================================

abInitioLoop :: AbInitioConfig -> IO ()
abInitioLoop cfg = do
  -- Header
  printf "%-4s %-16s %8s %7s %8s %8s %8s  %-8s %s\n"
    ("Step" :: String) ("Structure" :: String)
    ("Novelty" :: String) ("Cost" :: String)
    ("Score" :: String) ("Bar" :: String) ("Delta" :: String)
    ("Source" :: String) ("Candidates" :: String)
  putStrLn (replicate 90 '-')

  case cfgPrefixReport cfg of
    Just path -> writeFile path prefixReportHeader
    Nothing -> return ()

  -- Pure ab initio: build library from discovered telescopes only
  go [] [] [] 1

  where
    mode = cfgMode cfg
    go :: Library -> [DiscoveryRecord] -> [StepRecord] -> Int -> IO ()
    go lib history records step
      | step > cfgMaxSteps cfg = do
          let orderedRecords = reverse records
          finishRun
            ("SYNTHESIS COMPLETE: " ++ show (length orderedRecords) ++ " structures discovered")
            orderedRecords
      | isHonestMode mode = runHonestStep lib history records step
      | otherwise = do
          let -- Compute selection bar
              d = cfgWindow cfg
              bar = computeBarD d mode step history
              delta_n = dBonacciDelta d step

          budget <- computeSearchBudget cfg step
          when (cfgAdaptiveMemory cfg || cfgMemorySafe cfg) $
            printSearchBudget step budget

          stageAStart <- getCurrentTime

          -- Phase A: Exhaustive enumeration for κ ≤ 3
          -- Type-check to filter ill-formed telescopes, then evaluate honestly
          let emode = toEvalMode mode
              goalProfile = deriveGoalProfile lib
              expectedFamily = requiredFamilyForContext step lib goalProfile
              needsFormerLift = NeedFormer `elem` gpIntents goalProfile && length lib >= 3
              needsHITLift = NeedHIT `elem` gpIntents goalProfile && any leHasDependentFunctions lib
              needsMapBridge = NeedBridge `elem` gpIntents goalProfile
              needsModalLift = NeedModal `elem` gpIntents goalProfile
              needsDifferentialLift = NeedDifferential `elem` gpIntents goalProfile
              needsCurvatureLift = NeedCurvature `elem` gpIntents goalProfile
              needsMetricLift = NeedMetric `elem` gpIntents goalProfile
              needsHilbertLift = NeedHilbert `elem` gpIntents goalProfile
              needsTemporalLift = NeedTemporal `elem` gpIntents goalProfile
              hasLoopBeforeStep = any leHasLoop lib
              hasTruncBeforeStep = any (\e -> case leIsTruncated e of
                                                Just _ -> True
                                                Nothing -> False) lib
              bridgeIntensive =
                (NeedHIT `elem` gpIntents goalProfile
                  && hasLoopBeforeStep
                  && not hasTruncBeforeStep)
                || needsMapBridge
              shadowTight = cfgMBTTShadowProfile cfg && step <= 6
              step7LatencyTight =
                cfgMBTTShadowProfile cfg
                && step >= 7
                && NeedHIT `elem` gpIntents goalProfile
                && not needsMapBridge
              qualityBoost = shadowTight && (needsFormerLift || needsHITLift)
              formerLiftBit = if shadowTight then 16 else 20
              formerLiftDepth = if shadowTight then 2 else 3
              formerLiftCands = if shadowTight then 32 else 80
              hitLiftBit = if shadowTight then 18 else 22
              hitLiftDepth = if shadowTight then 2 else 3
              hitLiftCands = if shadowTight then 48 else 96
              modalLiftBit = if shadowTight then 18 else 22
              modalLiftDepth = if shadowTight then 2 else 3
              modalLiftCands = if shadowTight then 48 else 96
              differentialLiftBit = if shadowTight then 18 else 22
              differentialLiftDepth = if shadowTight then 2 else 3
              differentialLiftCands = if shadowTight then 48 else 96
              curvatureLiftBit = if shadowTight then 18 else 22
              curvatureLiftDepth = if shadowTight then 2 else 3
              curvatureLiftCands = if shadowTight then 48 else 96
              metricLiftBit = if shadowTight then 20 else 24
              metricLiftDepth = if shadowTight then 2 else 3
              metricLiftCands = if shadowTight then 64 else 128
              hilbertLiftBit = if shadowTight then 24 else 30
              hilbertLiftDepth = if shadowTight then 2 else 3
              hilbertLiftCands = if shadowTight then 80 else 160
              temporalLiftBit = if shadowTight then 24 else 32
              temporalLiftDepth = if shadowTight then 2 else 3
              temporalLiftCands = if shadowTight then 96 else 192
              stepBitBudget =
                maximum
                  [ sbBitBudget budget
                  , if needsFormerLift then formerLiftBit else 0
                  , if needsHITLift then hitLiftBit else 0
                  , if needsModalLift then modalLiftBit else 0
                  , if needsDifferentialLift then differentialLiftBit else 0
                  , if needsCurvatureLift then curvatureLiftBit else 0
                  , if needsMetricLift then metricLiftBit else 0
                  , if needsHilbertLift then hilbertLiftBit else 0
                  , if needsTemporalLift then temporalLiftBit else 0
                  , if bridgeIntensive then 18 else 0
                  ]
              stepAstDepth =
                maximum
                  [ sbAstDepth budget
                  , if needsFormerLift then formerLiftDepth else 0
                  , if needsHITLift then hitLiftDepth else 0
                  , if needsModalLift then modalLiftDepth else 0
                  , if needsDifferentialLift then differentialLiftDepth else 0
                  , if needsCurvatureLift then curvatureLiftDepth else 0
                  , if needsMetricLift then metricLiftDepth else 0
                  , if needsHilbertLift then hilbertLiftDepth else 0
                  , if needsTemporalLift then temporalLiftDepth else 0
                  , if bridgeIntensive then 2 else 0
                  ]
              stepMaxCandidates =
                maximum
                  [ sbMaxCandidates budget
                  , if needsFormerLift then formerLiftCands else 0
                  , if needsHITLift then hitLiftCands else 0
                  , if needsModalLift then modalLiftCands else 0
                  , if needsDifferentialLift then differentialLiftCands else 0
                  , if needsCurvatureLift then curvatureLiftCands else 0
                  , if needsMetricLift then metricLiftCands else 0
                  , if needsHilbertLift then hilbertLiftCands else 0
                  , if needsTemporalLift then temporalLiftCands else 0
                  , if bridgeIntensive then 32 else 0
                  ]
              stepBitBudget' = if step7LatencyTight then min stepBitBudget 12 else stepBitBudget
              stepAstDepth' = if step7LatencyTight then min stepAstDepth 1 else stepAstDepth
              stepMaxCandidates' = if step7LatencyTight then min stepMaxCandidates 10 else stepMaxCandidates
              enumKmax = if needsHilbertLift then 9 else if needsTemporalLift then 8 else if needsMetricLift then 7 else 3
              -- Depth-1 evaluation: count single-operation schemas only.
              -- Depth-2 causes O(formers²) explosion at later steps (L16).
              -- For short shadow prefixes, keep depth-2 novelty only during
              -- bootstrap/former setup. HIT progression runs at depth-1 to
              -- avoid inflated operator novelty.
              nuDepth = if shadowTight && step <= 4 then 2 else 1
              mbttCfg = defaultEnumConfig
                { ecMaxEntries = enumKmax
                , ecMaxBitBudget = stepBitBudget'
                , ecMaxASTDepth = stepAstDepth'
                , ecMaxCandidates = stepMaxCandidates'
                , ecExprCapLimit = Just (exprCapLimitForStep step stepMaxCandidates')
                , ecGoalProfile = Just goalProfile
                , ecEnableMacros = True
                }
              agendaCfg = defaultAgendaConfig
                { agMaxEntries = enumKmax
                , agMaxAgendaStates = if step7LatencyTight
                                      then 28
                                      else if bridgeIntensive
                                      then max 80 (min 140 (stepMaxCandidates * 3))
                                      else if qualityBoost
                                      then max 80 (min 180 (stepMaxCandidates * 4))
                                      else if shadowTight
                                      then max 40 (min 80 stepMaxCandidates)
                                      else max 60 (min 140 (stepMaxCandidates * 2))
                , agEnableDiversity = step >= 7
                , agBucketCap = if step <= 6
                                then max 64 stepMaxCandidates'
                                else if qualityBoost then 6 else if shadowTight then 4 else 5
                , agBranchPerState = if step7LatencyTight
                                     then 3
                                     else if bridgeIntensive
                                     then max 5 (min 8 (stepMaxCandidates `div` 10 + 4))
                                     else if qualityBoost
                                     then max 6 (min 10 (stepMaxCandidates `div` 4 + 4))
                                     else if shadowTight
                                     then max 3 (min 6 (stepMaxCandidates `div` 10 + 3))
                                     else max 4 (min 8 (stepMaxCandidates `div` 8 + 4))
                , agCriticPerAction = if step >= 7
                                      then if needsMapBridge || needsModalLift || needsDifferentialLift || needsCurvatureLift || needsMetricLift || needsHilbertLift || needsTemporalLift then 2 else if step7LatencyTight then 0 else 1
                                      else if bridgeIntensive
                                           then 2
                                      else if qualityBoost
                                           then 2
                                           else 3
                , agMaxCandidates = if qualityBoost
                                    then if bridgeIntensive
                                         then max 20 (min 28 stepMaxCandidates')
                                         else max 16 (min 36 stepMaxCandidates')
                                    else if bridgeIntensive
                                    then max 16 (min 24 stepMaxCandidates')
                                    else if shadowTight
                                    then max 8 (min 20 stepMaxCandidates')
                                    else max 10 (min 40 stepMaxCandidates')
                , agBitBudget = stepBitBudget'
                , agRequireConnected = length lib >= 2
                , agSafeClosureSteps = if needsMapBridge || needsModalLift || needsDifferentialLift || needsCurvatureLift || needsMetricLift || needsHilbertLift || needsTemporalLift then 3 else if step7LatencyTight then 1 else if qualityBoost then 1 else 3
                , agPremiseTopK = if step7LatencyTight then 3 else if length lib < 6 then 3 else 4
                , agBarFloor = if step7LatencyTight
                               then bar * 1.02
                               else if bridgeIntensive || needsModalLift || needsDifferentialLift || needsTemporalLift
                                    then 0.0
                                    else bar
                , agLeafExprBudget = if step7LatencyTight
                                     then min 8 stepBitBudget'
                                      else if needsModalLift
                                           then min 18 stepBitBudget'
                                      else if needsDifferentialLift
                                           then min 18 stepBitBudget'
                                       else if needsCurvatureLift
                                            then min 18 stepBitBudget'
                                      else if needsMetricLift
                                           then min 20 stepBitBudget'
                                      else if needsHilbertLift
                                           then min 24 stepBitBudget'
                                      else if needsTemporalLift
                                           then min 26 stepBitBudget'
                                      else if qualityBoost
                                           then min 16 stepBitBudget'
                                           else if shadowTight then min 12 stepBitBudget' else min 14 stepBitBudget'
                , agLeafExprDepth = if needsMapBridge || needsModalLift || needsDifferentialLift || needsCurvatureLift || needsMetricLift || needsHilbertLift || needsTemporalLift then 2 else if step7LatencyTight then 1 else if qualityBoost then 2 else if shadowTight then 1 else 2
                , agLeafExprCap = if needsTemporalLift then 36 else if needsHilbertLift then 32 else if needsMetricLift then 28 else if needsModalLift || needsDifferentialLift || needsCurvatureLift then 20 else if needsMapBridge then 18 else if step7LatencyTight then 4 else if bridgeIntensive then 16 else if qualityBoost then 18 else if shadowTight then 8 else 12
                }
              agendaActive = cfgMBTTFirst cfg && length lib >= 3
              (agendaGenerated, agendaDiag) = if agendaActive
                                              then agendaGenerateCandidatesWithDiagnostics lib goalProfile agendaCfg
                                              else ([], emptyAgendaDiagnostics)
              agendaTelescopes = [acTelescope c | c <- agendaGenerated]
              hasTruncInLibrary = any (\e -> case leIsTruncated e of
                                               Just _ -> True
                                               Nothing -> False) lib
              libPathDims = concatMap lePathDims lib
              libMaxPathDim = if null libPathDims then 0 else maximum libPathDims
              postTruncLiftPhase =
                NeedHIT `elem` gpIntents goalProfile
                && hasTruncInLibrary
                && any leHasLoop lib
                && libMaxPathDim >= 1
              differentialLiftPhase =
                NeedDifferential `elem` gpIntents goalProfile
                && any leHasModalOps lib
                && not (any leHasDifferentialOps lib)
              modalLiftPhase =
                NeedModal `elem` gpIntents goalProfile
                && any leHasLoop lib
                && not (any leHasModalOps lib)
              curvatureLiftPhase =
                NeedCurvature `elem` gpIntents goalProfile
                && any leHasDifferentialOps lib
                && not (any leHasCurvature lib)
              metricLiftPhase =
                NeedMetric `elem` gpIntents goalProfile
                && any leHasCurvature lib
                && not (any leHasMetric lib)
              hilbertLiftPhase =
                NeedHilbert `elem` gpIntents goalProfile
                && any leHasMetric lib
                && not (any leHasHilbert lib)
              temporalLiftPhase =
                NeedTemporal `elem` gpIntents goalProfile
                && any leHasHilbert lib
                && any leHasModalOps lib
                && not (any leHasTemporalOps lib)
              mapBridgePhase =
                isMapBridgePhase lib goalProfile
              bootstrapHITPhase =
                NeedHIT `elem` gpIntents goalProfile
                && any leHasDependentFunctions lib
                && not (any leHasLoop lib)
              universeGenesisSeeds =
                if expectedFamily == Just FamilyUniverseGenesis
                then universeGenesisSeedTelescopes
                else []
              witnessIntroSeeds =
                if expectedFamily == Just FamilyWitnessIntro
                then witnessIntroductionSeedTelescopes lib
                else []
              canonicalBootstrapSeeds = universeGenesisSeeds ++ witnessIntroSeeds
              bootstrapHITSeeds =
                if bootstrapHITPhase
                then bootstrapHITSeedTelescopes
                else []
              hitProgressHedge = agendaActive
                                 && NeedHIT `elem` gpIntents goalProfile
                                 && any leHasLoop lib
                                 && not hasTruncInLibrary
              -- Only force path-lift when the library has no loop dimension yet.
              -- Once dim-1 is present, keep the HIT lane open (e.g. truncation vs lift).
              needsPathLift = hitProgressHedge && libMaxPathDim < 1
              -- Bridge-first HIT policy: once dim-1 exists, require explicit
              -- truncation bridge growth before admitting pure loop-lift/suspension moves.
              requiresTruncBridge =
                hitProgressHedge
                && libMaxPathDim >= 1
                && not hasTruncInLibrary
              pathLiftSeeds =
                if needsPathLift
                then
                  let nextDim = max 1 (libMaxPathDim + 1)
                  in [ Telescope
                         [ TeleEntry "c1" (App Univ (Var 1))
                         , TeleEntry "c2" (Var 1)
                         , TeleEntry "c3" (PathCon nextDim)
                         ]
                     ]
                else []
              truncBridgeSeeds =
                if requiresTruncBridge
                then truncBridgeSeedTelescopes lib
                else []
              postTruncSeeds =
                if postTruncLiftPhase
                then postTruncLiftSeedTelescopes lib
                else []
              mapBridgeSeeds =
                if needsMapBridge
                then mapBridgeSeedTelescopes lib
                else []
              modalSeeds =
                if needsModalLift
                then modalLiftSeedTelescopes lib
                else []
              differentialSeeds =
                if needsDifferentialLift
                then differentialLiftSeedTelescopes lib
                else []
              curvatureSeeds =
                if needsCurvatureLift
                then curvatureLiftSeedTelescopes lib
                else []
              metricSeeds =
                if needsMetricLift
                then metricLiftSeedTelescopes lib
                else []
              hilbertSeeds =
                if needsHilbertLift
                then hilbertLiftSeedTelescopes lib
                else []
              temporalSeeds =
                if needsTemporalLift
                then temporalLiftSeedTelescopes lib
                else []
              hedgeRelevant tele =
                let cls = classifyTelescope tele lib
                    refs = Set.toList (teleLibRefs tele)
                    referencesLoop = any (\i -> i >= 1 && i <= length lib && leHasLoop (lib !! (i - 1))) refs
                    dims = telePathDimensions tele
                    hasPath = not (null dims)
                    teleMaxDim = if hasPath then maximum dims else 0
                    incrementalLift =
                      hasPath && teleMaxDim == libMaxPathDim + 1
                    bridgeDimStable = teleMaxDim <= libMaxPathDim + 1
                    hasTruncExpr = teleHasTrunc tele
                in case cls of
                     -- During early HIT progression, prefer genuine path-lifts
                     -- over suspension shortcuts to preserve κ-quality.
                     TCSuspension ->
                       if requiresTruncBridge
                       then referencesLoop
                            && hasTruncExpr
                            && bridgeDimStable
                            && truncBridgeQualityScore lib tele >= 5
                       else referencesLoop && not needsPathLift && not requiresTruncBridge
                     TCHIT -> if needsPathLift
                              then incrementalLift || hasTruncExpr
                              else if requiresTruncBridge
                                   then hasTruncExpr
                                        && bridgeDimStable
                                        && truncBridgeQualityScore lib tele >= 2
                                   else hasPath || referencesLoop || hasTruncExpr
                     _ -> hasTruncExpr
              fallbackNeeded = not agendaActive || null agendaTelescopes
              fallbackTelescopes = if cfgMBTTFirst cfg && (fallbackNeeded || hitProgressHedge)
                                   then map mcTelescope (enumerateMBTTTelescopes lib mbttCfg)
                                   else []
              rawTelescopesBase0 = if cfgMBTTFirst cfg
                                   then if agendaActive
                                        then if hitProgressHedge
                                             then
                                               let mergedRaw = filter hedgeRelevant (truncBridgeSeeds ++ pathLiftSeeds ++ agendaTelescopes ++ fallbackTelescopes)
                                                   merged = prioritizeBridgeCandidates requiresTruncBridge lib goalProfile mergedRaw
                                               in if null merged
                                                  then take stepMaxCandidates' agendaTelescopes
                                                  else take stepMaxCandidates' merged
                                        else if null agendaTelescopes
                                        then take stepMaxCandidates' fallbackTelescopes
                                        else take stepMaxCandidates' agendaTelescopes
                                        else take stepMaxCandidates' fallbackTelescopes
                                   else enumerateTelescopes lib enumKmax
              rawTelescopesSeeded0 = canonicalBootstrapSeeds ++ rawTelescopesBase0
              rawTelescopesBase1 =
                if bootstrapHITPhase
                then
                  let filtered = filter (isBootstrapHITCandidate lib) (bootstrapHITSeeds ++ rawTelescopesSeeded0)
                  in if null filtered then rawTelescopesSeeded0 else filtered
                else rawTelescopesSeeded0
              rawTelescopesBase =
                if postTruncLiftPhase
                then take stepMaxCandidates' (postTruncSeeds ++ rawTelescopesBase1)
                else if needsMapBridge
                     then take stepMaxCandidates' (mapBridgeSeeds ++ rawTelescopesBase1)
                else if needsModalLift
                     then take stepMaxCandidates' (modalSeeds ++ rawTelescopesBase1)
                else if needsDifferentialLift
                           then take stepMaxCandidates' (differentialSeeds ++ rawTelescopesBase1)
                      else if needsCurvatureLift
                           then take stepMaxCandidates' (curvatureSeeds ++ rawTelescopesBase1)
                      else if needsMetricLift
                           then take stepMaxCandidates' (metricSeeds ++ rawTelescopesBase1)
                      else if needsHilbertLift
                           then take stepMaxCandidates' (hilbertSeeds ++ rawTelescopesBase1)
                      else if needsTemporalLift
                           then take stepMaxCandidates' (temporalSeeds ++ rawTelescopesBase1)
                       else rawTelescopesBase1
              rawTelescopes =
                if postTruncLiftPhase
                then
                  let filtered = filter (isPostTruncLiftCandidate lib) rawTelescopesBase
                  in if null filtered then rawTelescopesBase else filtered
                else if mapBridgePhase
                     then
                       let filtered = filter (isMapBridgeCandidate lib) rawTelescopesBase
                       in if null filtered then rawTelescopesBase else filtered
                else if modalLiftPhase
                     then
                       let filtered = filter (isModalLiftCandidate lib) rawTelescopesBase
                       in if null filtered then rawTelescopesBase else filtered
                else if differentialLiftPhase
                     then
                       let filtered = filter (isDifferentialLiftCandidate lib) rawTelescopesBase
                       in if null filtered then rawTelescopesBase else filtered
                else if curvatureLiftPhase
                     then
                       let filtered = filter (isCurvatureLiftCandidate lib) rawTelescopesBase
                       in if null filtered then rawTelescopesBase else filtered
                else if metricLiftPhase
                     then
                       let filtered = filter (isMetricLiftCandidate lib) rawTelescopesBase
                       in if null filtered then rawTelescopesBase else filtered
                else if hilbertLiftPhase
                     then
                       let filtered = filter (isHilbertLiftCandidate lib) rawTelescopesBase
                       in if null filtered then rawTelescopesBase else filtered
                else if temporalLiftPhase
                     then
                       let filtered = filter (isTemporalLiftCandidate lib) rawTelescopesBase
                       in if null filtered then rawTelescopesBase else filtered
                else rawTelescopesBase
              rawUniqueTelescopes = if cfgNoCanonicalQuotient cfg
                                    then rawTelescopes
                                    else if requiresTruncBridge
                                    then dedupByCanonicalKeyWith (betterBridgeRepresentative lib) rawTelescopes
                                    else dedupByCanonicalKey rawTelescopes
              (validTelescopesRaw, _rejected) = checkAndFilter lib rawUniqueTelescopes
              -- Phase-2 quotienting kickoff: deduplicate equivalent telescopes
              -- by canonical MBTT key before scoring.
              validTelescopes = if requiresTruncBridge
                                then dedupByCanonicalKeyWith (betterBridgeRepresentative lib) validTelescopesRaw
                                else dedupByCanonicalKey validTelescopesRaw
              evalTelescopes = validTelescopes
              -- Build novelty history for evaluator state.
              nuHist = zip [1..] (map drNu history)
              -- Evaluate each valid telescope using the mode-appropriate evaluator
              enumSource = if agendaActive && not (null agendaTelescopes) then "AGENDA"
                           else if cfgMBTTFirst cfg then "ENUM_MBTT" else "ENUM"
              evalOne tele =
                let (nu, kappa, rho) = evaluateTelescopeWithHistory emode tele lib nuDepth "candidate" nuHist
                in nu `seq` kappa `seq` rho `seq` (tele, nu, kappa, rho, enumSource)
              enumEvaluatedAll = parMapChunkedWHNF evalOne evalTelescopes
              enumEvaluated = [ c | c@(_, nu, _, _, _) <- enumEvaluatedAll, nu > 0 ]

          when (step >= 6 && requiresTruncBridge) $ do
            let truncAgenda = [tele | tele <- agendaTelescopes, teleHasTrunc tele]
                truncFallback = [tele | tele <- fallbackTelescopes, teleHasTrunc tele]
                truncMergedRaw = [tele | tele <- filter hedgeRelevant (pathLiftSeeds ++ agendaTelescopes ++ fallbackTelescopes), teleHasTrunc tele]
                truncRaw = [tele | tele <- rawTelescopes, teleHasTrunc tele]
                truncRawUnique = [tele | tele <- rawUniqueTelescopes, teleHasTrunc tele]
                truncValids = [tele | tele <- evalTelescopes, teleHasTrunc tele]
                byEntryKAgenda = histogramInt (map teleKappa truncAgenda)
                byDesugaredKAgenda = histogramInt (map desugaredKappa truncAgenda)
                byEntryKFallback = histogramInt (map teleKappa truncFallback)
                byDesugaredKFallback = histogramInt (map desugaredKappa truncFallback)
                byEntryKMergedRaw = histogramInt (map teleKappa truncMergedRaw)
                byDesugaredKMergedRaw = histogramInt (map desugaredKappa truncMergedRaw)
                byEntryKRaw = histogramInt (map teleKappa truncRaw)
                byDesugaredKRaw = histogramInt (map desugaredKappa truncRaw)
                byEntryKRawUnique = histogramInt (map teleKappa truncRawUnique)
                byDesugaredKRawUnique = histogramInt (map desugaredKappa truncRawUnique)
                byEntryKValid = histogramInt (map teleKappa truncValids)
                byDesugaredKValid = histogramInt (map desugaredKappa truncValids)
            printf "  [TRUNC SEARCH step %d] agenda=%d by_entry_k=%s by_desugared_k=%s ; fallback=%d by_entry_k=%s by_desugared_k=%s ; merged=%d by_entry_k=%s by_desugared_k=%s ; raw=%d by_entry_k=%s by_desugared_k=%s ; dedup=%d by_entry_k=%s by_desugared_k=%s ; valid=%d by_entry_k=%s by_desugared_k=%s\n"
              step
              (length truncAgenda)
              (show byEntryKAgenda)
              (show byDesugaredKAgenda)
              (length truncFallback)
              (show byEntryKFallback)
              (show byDesugaredKFallback)
              (length truncMergedRaw)
              (show byEntryKMergedRaw)
              (show byDesugaredKMergedRaw)
              (length truncRaw)
              (show byEntryKRaw)
              (show byDesugaredKRaw)
              (length truncRawUnique)
              (show byEntryKRawUnique)
              (show byDesugaredKRawUnique)
              (length truncValids)
              (show byEntryKValid)
              (show byDesugaredKValid)

          let enumViableA = [ c | c@(_, _, _, rho, _) <- enumEvaluated, rho >= bar ]
              stageAWork = length enumEvaluated + length enumViableA
                         + adExpandedStates agendaDiag
          stageAWork `seq` return ()
          stageAEnd <- getCurrentTime
          let stageASeconds = realToFrac (diffUTCTime stageAEnd stageAStart) :: Double
              stageBWidenThresholdSec = 12.0 :: Double
              shouldRunStageB =
                (step7LatencyTight || needsModalLift || needsDifferentialLift || needsCurvatureLift || needsMetricLift || needsHilbertLift)
                && null enumViableA
                && stageASeconds <= stageBWidenThresholdSec
          when shouldRunStageB $
            printf "  [STEP %d] stage-A tight found no viable candidate in %.2fs; running widened stage-B.\n"
              step stageASeconds

          (enumEvaluatedChosen, agendaDiagChosen, agendaActiveChosen) <-
            if shouldRunStageB
            then do
              let stepBitBudgetWide = min stepBitBudget 18
                  stepAstDepthWide = min stepAstDepth 2
                  stepMaxCandidatesWide = min stepMaxCandidates 24
                  modalPhaseWide = NeedModal `elem` gpIntents goalProfile
                  differentialPhaseWide = NeedDifferential `elem` gpIntents goalProfile
                  curvaturePhaseWide = NeedCurvature `elem` gpIntents goalProfile
                  metricPhaseWide = NeedMetric `elem` gpIntents goalProfile
                  hilbertPhaseWide = NeedHilbert `elem` gpIntents goalProfile
                  temporalPhaseWide = isTemporalLiftPhase lib goalProfile
                  mapBridgePhaseWide = isMapBridgePhase lib goalProfile
                  mbttCfgWide = defaultEnumConfig
                    { ecMaxEntries = enumKmax
                    , ecMaxBitBudget = stepBitBudgetWide
                    , ecMaxASTDepth = stepAstDepthWide
                    , ecMaxCandidates = stepMaxCandidatesWide
                    , ecExprCapLimit = Just (exprCapLimitForStep step stepMaxCandidatesWide)
                    , ecGoalProfile = Just goalProfile
                    , ecEnableMacros = True
                    }
                  agendaCfgWide = defaultAgendaConfig
                    { agMaxEntries = enumKmax
                    , agMaxAgendaStates =
                        if modalPhaseWide || differentialPhaseWide || curvaturePhaseWide || metricPhaseWide || hilbertPhaseWide || temporalPhaseWide then 96 else if postTruncLiftPhaseWide then 96 else 72
                    , agEnableDiversity = step >= 7
                    , agBucketCap = if step <= 6 then max 64 stepMaxCandidatesWide else 6
                    , agBranchPerState =
                        if modalPhaseWide || differentialPhaseWide || curvaturePhaseWide || metricPhaseWide || hilbertPhaseWide || temporalPhaseWide then 8 else if postTruncLiftPhaseWide then 8 else 6
                    , agCriticPerAction = if modalPhaseWide || differentialPhaseWide || curvaturePhaseWide || metricPhaseWide || hilbertPhaseWide || temporalPhaseWide || postTruncLiftPhaseWide then 2 else if step >= 7 then 1 else if qualityBoost then 2 else 3
                    , agMaxCandidates =
                        if modalPhaseWide || differentialPhaseWide || curvaturePhaseWide || metricPhaseWide || hilbertPhaseWide || temporalPhaseWide then max 16 (min 40 stepMaxCandidatesWide) else max 12 (min 32 stepMaxCandidatesWide)
                    , agBitBudget = stepBitBudgetWide
                    , agRequireConnected = length lib >= 2
                    , agSafeClosureSteps = if modalPhaseWide || differentialPhaseWide || curvaturePhaseWide || metricPhaseWide || hilbertPhaseWide || temporalPhaseWide || postTruncLiftPhaseWide then 3 else if step >= 7 then 2 else if qualityBoost then 1 else 3
                    , agPremiseTopK = if length lib < 6 then 3 else 4
                    , agBarFloor = bar
                    , agLeafExprBudget = if temporalPhaseWide then min 26 stepBitBudgetWide else if hilbertPhaseWide then min 24 stepBitBudgetWide else if metricPhaseWide then min 20 stepBitBudgetWide else if modalPhaseWide || differentialPhaseWide || curvaturePhaseWide then min 18 stepBitBudgetWide else min 14 stepBitBudgetWide
                    , agLeafExprDepth = 2
                    , agLeafExprCap =
                        if temporalPhaseWide then 36 else if hilbertPhaseWide then 32 else if metricPhaseWide then 28 else if modalPhaseWide || differentialPhaseWide || curvaturePhaseWide then 20 else if postTruncLiftPhaseWide then 14 else 10
                    }
                  agendaActiveWide = cfgMBTTFirst cfg && length lib >= 3
                  (agendaGeneratedWide, agendaDiagWide) = if agendaActiveWide
                                                          then agendaGenerateCandidatesWithDiagnostics lib goalProfile agendaCfgWide
                                                          else ([], emptyAgendaDiagnostics)
                  agendaTelescopesWide = [acTelescope c | c <- agendaGeneratedWide]
                  hasTruncInLibraryWide = any (\e -> case leIsTruncated e of
                                                       Just _ -> True
                                                       Nothing -> False) lib
                  libPathDimsWide = concatMap lePathDims lib
                  libMaxPathDimWide = if null libPathDimsWide then 0 else maximum libPathDimsWide
                  postTruncLiftPhaseWide =
                    NeedHIT `elem` gpIntents goalProfile
                    && hasTruncInLibraryWide
                    && any leHasLoop lib
                    && libMaxPathDimWide >= 1
                  bootstrapHITPhaseWide =
                    NeedHIT `elem` gpIntents goalProfile
                    && any leHasDependentFunctions lib
                    && not (any leHasLoop lib)
                  bootstrapHITSeedsWide =
                    if bootstrapHITPhaseWide
                    then bootstrapHITSeedTelescopes
                    else []
                  hitProgressHedgeWide = agendaActiveWide
                                       && NeedHIT `elem` gpIntents goalProfile
                                       && any leHasLoop lib
                                       && not hasTruncInLibraryWide
                  -- Same policy in widened stage: force path-lift only from zero loop depth.
                  needsPathLiftWide = hitProgressHedgeWide && libMaxPathDimWide < 1
                  requiresTruncBridgeWide =
                    hitProgressHedgeWide
                    && libMaxPathDimWide >= 1
                    && not hasTruncInLibraryWide
                  pathLiftSeedsWide =
                    if needsPathLiftWide
                    then
                      let nextDim = max 1 (libMaxPathDimWide + 1)
                      in [ Telescope
                             [ TeleEntry "c1" (App Univ (Var 1))
                             , TeleEntry "c2" (Var 1)
                             , TeleEntry "c3" (PathCon nextDim)
                             ]
                         ]
                    else []
                  truncBridgeSeedsWide =
                    if requiresTruncBridgeWide
                    then truncBridgeSeedTelescopes lib
                    else []
                  postTruncSeedsWide =
                    if postTruncLiftPhaseWide
                    then postTruncLiftSeedTelescopes lib
                    else []
                  mapBridgeSeedsWide =
                    if needsMapBridge
                    then mapBridgeSeedTelescopes lib
                    else []
                  modalSeedsWide =
                    if needsModalLift
                    then modalLiftSeedTelescopes lib
                    else []
                  differentialSeedsWide =
                    if needsDifferentialLift
                    then differentialLiftSeedTelescopes lib
                    else []
                  curvatureSeedsWide =
                    if needsCurvatureLift
                    then curvatureLiftSeedTelescopes lib
                    else []
                  metricSeedsWide =
                    if needsMetricLift
                    then metricLiftSeedTelescopes lib
                    else []
                  hilbertSeedsWide =
                    if needsHilbertLift
                    then hilbertLiftSeedTelescopes lib
                    else []
                  temporalSeedsWide =
                    if needsTemporalLift
                    then temporalLiftSeedTelescopes lib
                    else []
                  hedgeRelevantWide tele =
                    let cls = classifyTelescope tele lib
                        refs = Set.toList (teleLibRefs tele)
                        referencesLoop = any (\i -> i >= 1 && i <= length lib && leHasLoop (lib !! (i - 1))) refs
                        dims = telePathDimensions tele
                        hasPath = not (null dims)
                        teleMaxDim = if hasPath then maximum dims else 0
                        incrementalLift =
                          hasPath && teleMaxDim == libMaxPathDimWide + 1
                        bridgeDimStable = teleMaxDim <= libMaxPathDimWide + 1
                        hasTruncExpr = teleHasTrunc tele
                    in case cls of
                         TCSuspension ->
                           if requiresTruncBridgeWide
                           then referencesLoop
                                && hasTruncExpr
                                && bridgeDimStable
                                && truncBridgeQualityScore lib tele >= 5
                           else referencesLoop && not needsPathLiftWide && not requiresTruncBridgeWide
                         TCHIT -> if needsPathLiftWide
                                  then incrementalLift || hasTruncExpr
                                  else if requiresTruncBridgeWide
                                       then hasTruncExpr
                                            && bridgeDimStable
                                            && truncBridgeQualityScore lib tele >= 2
                                       else hasPath || referencesLoop || hasTruncExpr
                         _ -> hasTruncExpr
                  fallbackNeededWide = not agendaActiveWide || null agendaTelescopesWide
                  fallbackTelescopesWide = if cfgMBTTFirst cfg && (fallbackNeededWide || hitProgressHedgeWide)
                                           then map mcTelescope (enumerateMBTTTelescopes lib mbttCfgWide)
                                           else []
                  rawTelescopesWideBase0 = if cfgMBTTFirst cfg
                                           then if agendaActiveWide
                                                then if hitProgressHedgeWide
                                                     then
                                                       let mergedRaw = filter hedgeRelevantWide (truncBridgeSeedsWide ++ pathLiftSeedsWide ++ agendaTelescopesWide ++ fallbackTelescopesWide)
                                                           merged = prioritizeBridgeCandidates requiresTruncBridgeWide lib goalProfile mergedRaw
                                                       in if null merged
                                                          then take stepMaxCandidatesWide agendaTelescopesWide
                                                          else take stepMaxCandidatesWide merged
                                                     else if null agendaTelescopesWide
                                                     then take stepMaxCandidatesWide fallbackTelescopesWide
                                                     else take stepMaxCandidatesWide agendaTelescopesWide
                                                else take stepMaxCandidatesWide fallbackTelescopesWide
                                           else enumerateTelescopes lib enumKmax
                  rawTelescopesWideBase1 =
                    if bootstrapHITPhaseWide
                    then
                      let filtered = filter (isBootstrapHITCandidate lib) (bootstrapHITSeedsWide ++ rawTelescopesWideBase0)
                      in if null filtered then rawTelescopesWideBase0 else filtered
                    else rawTelescopesWideBase0
                  rawTelescopesWideBase =
                    if postTruncLiftPhaseWide
                    then take stepMaxCandidatesWide (postTruncSeedsWide ++ rawTelescopesWideBase1)
                    else if needsMapBridge
                         then take stepMaxCandidatesWide (mapBridgeSeedsWide ++ rawTelescopesWideBase1)
                    else if needsModalLift
                         then take stepMaxCandidatesWide (modalSeedsWide ++ rawTelescopesWideBase1)
                          else if needsDifferentialLift
                                then take stepMaxCandidatesWide (differentialSeedsWide ++ rawTelescopesWideBase1)
                          else if needsCurvatureLift
                               then take stepMaxCandidatesWide (curvatureSeedsWide ++ rawTelescopesWideBase1)
                          else if needsMetricLift
                               then take stepMaxCandidatesWide (metricSeedsWide ++ rawTelescopesWideBase1)
                          else if needsHilbertLift
                               then take stepMaxCandidatesWide (hilbertSeedsWide ++ rawTelescopesWideBase1)
                          else if needsTemporalLift
                               then take stepMaxCandidatesWide (temporalSeedsWide ++ rawTelescopesWideBase1)
                           else rawTelescopesWideBase1
                  rawTelescopesWide =
                    if postTruncLiftPhaseWide
                    then
                      let filtered = filter (isPostTruncLiftCandidate lib) rawTelescopesWideBase
                      in if null filtered then rawTelescopesWideBase else filtered
                    else if mapBridgePhaseWide
                         then
                           let filtered = filter (isMapBridgeCandidate lib) rawTelescopesWideBase
                           in if null filtered then rawTelescopesWideBase else filtered
                    else if modalPhaseWide
                         then
                           let filtered = filter (isModalLiftCandidate lib) rawTelescopesWideBase
                           in if null filtered then rawTelescopesWideBase else filtered
                    else if differentialPhaseWide
                         then
                           let filtered = filter (isDifferentialLiftCandidate lib) rawTelescopesWideBase
                           in if null filtered then rawTelescopesWideBase else filtered
                     else if curvaturePhaseWide
                          then
                            let filtered = filter (isCurvatureLiftCandidate lib) rawTelescopesWideBase
                            in if null filtered then rawTelescopesWideBase else filtered
                    else if metricPhaseWide
                         then
                           let filtered = filter (isMetricLiftCandidate lib) rawTelescopesWideBase
                           in if null filtered then rawTelescopesWideBase else filtered
                    else if hilbertPhaseWide
                         then
                           let filtered = filter (isHilbertLiftCandidate lib) rawTelescopesWideBase
                           in if null filtered then rawTelescopesWideBase else filtered
                    else if temporalPhaseWide
                         then
                           let filtered = filter (isTemporalLiftCandidate lib) rawTelescopesWideBase
                           in if null filtered then rawTelescopesWideBase else filtered
                    else rawTelescopesWideBase
                  rawUniqueTelescopesWide = if cfgNoCanonicalQuotient cfg
                                            then rawTelescopesWide
                                            else if requiresTruncBridgeWide
                                            then dedupByCanonicalKeyWith (betterBridgeRepresentative lib) rawTelescopesWide
                                            else dedupByCanonicalKey rawTelescopesWide
                  (validTelescopesRawWide, _rejectedWide) = checkAndFilter lib rawUniqueTelescopesWide
                  validTelescopesWide = if requiresTruncBridgeWide
                                        then dedupByCanonicalKeyWith (betterBridgeRepresentative lib) validTelescopesRawWide
                                        else dedupByCanonicalKey validTelescopesRawWide
                  evalTelescopesWide = validTelescopesWide
                  nuHistWide = zip [1..] (map drNu history)
                  enumSourceWide = if agendaActiveWide && not (null agendaTelescopesWide)
                                   then "AGENDA_WIDE"
                                   else if cfgMBTTFirst cfg then "ENUM_MBTT_WIDE" else "ENUM"
                  evalOneWide tele =
                    let (nu, kappa, rho) = evaluateTelescopeWithHistory emode tele lib nuDepth "candidate" nuHistWide
                    in nu `seq` kappa `seq` rho `seq` (tele, nu, kappa, rho, enumSourceWide)
                  enumEvaluatedAllWide = parMapChunkedWHNF evalOneWide evalTelescopesWide
                  enumEvaluatedWide = [ c | c@(_, nu, _, _, _) <- enumEvaluatedAllWide, nu > 0 ]
              return (enumEvaluatedWide, agendaDiagWide, agendaActiveWide)
            else return (enumEvaluated, agendaDiag, agendaActive)

          when (step >= 6) $ do
            let hasTrunc = any (\e -> case leIsTruncated e of
                                        Just _ -> True
                                        Nothing -> False) lib
                dims = concatMap lePathDims lib
                maxDim = if null dims then 0 else maximum dims
                truncCands = [ (tele, nu, kappa, rho)
                             | (tele, nu, kappa, rho, _) <- enumEvaluatedChosen
                             , teleHasTrunc tele
                             ]
                truncViable = [ (tele, nu, kappa, rho)
                              | (tele, nu, kappa, rho) <- truncCands
                              , rho >= bar
                              ]
                localTeleMaxPathDim tele =
                  let ds = telePathDimensions tele
                  in if null ds then 0 else maximum ds
                truncByK = histogramInt [k | (_, _, k, _) <- truncCands]
                truncViableByK = histogramInt [k | (_, _, k, _) <- truncViable]
                truncByMaxDim = histogramInt [localTeleMaxPathDim tele | (tele, _, _, _) <- truncCands]
                truncViableByMaxDim = histogramInt [localTeleMaxPathDim tele | (tele, _, _, _) <- truncViable]
                truncViableK3ByMaxDim = histogramInt [localTeleMaxPathDim tele | (tele, _, k, _) <- truncViable, k == 3]
                truncMaxRhoByK =
                  let stepMap m (_, _, k, rho) =
                        Map.insertWith max k rho m
                  in sortOn fst (Map.toList (foldl stepMap Map.empty truncCands))
                truncMaxRhoByMaxDim =
                  let stepMap m (tele, _, _, rho) =
                        Map.insertWith max (localTeleMaxPathDim tele) rho m
                  in sortOn fst (Map.toList (foldl stepMap Map.empty truncCands))
                postTruncLiftNow = isPostTruncLiftPhase lib goalProfile
                liftCands = [ (tele, nu, kappa, rho)
                            | (tele, nu, kappa, rho, _) <- enumEvaluatedChosen
                            , postTruncLiftHasEvidence lib tele
                            ]
                liftViable = [ (tele, nu, kappa, rho)
                             | (tele, nu, kappa, rho) <- liftCands
                             , rho >= bar
                             ]
                liftByK = histogramInt [k | (_, _, k, _) <- liftCands]
                liftViableByK = histogramInt [k | (_, _, k, _) <- liftViable]
                liftDeltaHist = histogramInt [max 0 (localTeleMaxPathDim tele - maxDim) | (tele, _, _, _) <- liftCands]
                liftViableDeltaHist = histogramInt [max 0 (localTeleMaxPathDim tele - maxDim) | (tele, _, _, _) <- liftViable]
                liftBestRho = if null liftCands then 0.0 else maximum [rho | (_, _, _, rho) <- liftCands]
                liftBestNu = if null liftCands then 0 else maximum [nu | (_, nu, _, _) <- liftCands]
                liftBestK = if null liftCands then 0 else maximum [kappa | (_, _, kappa, _) <- liftCands]
                mapBridgeNow = isMapBridgePhase lib goalProfile
                mapBridgeCands =
                  [ (detectCanonicalName tele lib, classifyTelescope tele lib, kappa, rho, mapBridgeQualityScore lib tele, mapBridgeCoverageScore lib tele)
                  | (tele, _, kappa, rho, _) <- enumEvaluatedChosen
                  ]
                mapBridgeViable =
                  [ x
                  | x@(_, _, _, rho, _, _) <- mapBridgeCands
                  , rho >= bar
                  ]
                mapBridgeByName = histogramString [name | (name, _, _, _, _, _) <- mapBridgeCands]
                mapBridgeViableByName = histogramString [name | (name, _, _, _, _, _) <- mapBridgeViable]
                mapBridgeByClass = histogramString [show cls | (_, cls, _, _, _, _) <- mapBridgeCands]
                mapBridgeViableByClass = histogramString [show cls | (_, cls, _, _, _, _) <- mapBridgeViable]
                mapBridgeBestRho = if null mapBridgeCands then 0.0 else maximum [rho | (_, _, _, rho, _, _) <- mapBridgeCands]
                mapBridgeBestQuality = if null mapBridgeCands then 0 else maximum [q | (_, _, _, _, q, _) <- mapBridgeCands]
                mapBridgeCoverage = length [() | (_, _, _, _, _, cov) <- mapBridgeCands, cov >= 3]
                modalLiftNow = isModalLiftPhase lib goalProfile
                modalCands =
                  [ (detectCanonicalName tele lib, classifyTelescope tele lib, kappa, rho, modalLiftQualityScore lib tele, teleModalCount tele)
                  | (tele, _, kappa, rho, _) <- enumEvaluatedChosen
                  ]
                modalViable =
                  [ x
                  | x@(_, _, _, rho, _, _) <- modalCands
                  , rho >= bar
                  ]
                modalByName = histogramString [name | (name, _, _, _, _, _) <- modalCands]
                modalViableByName = histogramString [name | (name, _, _, _, _, _) <- modalViable]
                modalByClass = histogramString [show cls | (_, cls, _, _, _, _) <- modalCands]
                modalViableByClass = histogramString [show cls | (_, cls, _, _, _, _) <- modalViable]
                modalBestRho = if null modalCands then 0.0 else maximum [rho | (_, _, _, rho, _, _) <- modalCands]
                modalBestQuality = if null modalCands then 0 else maximum [q | (_, _, _, _, q, _) <- modalCands]
                modalRichCount = length [() | (_, _, _, _, _, modalCount) <- modalCands, modalCount >= 3]
                differentialLiftNow = isDifferentialLiftPhase lib goalProfile
                differentialCands =
                  [ (detectCanonicalName tele lib, classifyTelescope tele lib, nu, kappa, rho, differentialLiftQualityScore lib tele, teleReferencesModalCarrier lib tele)
                  | (tele, nu, kappa, rho, _) <- enumEvaluatedChosen
                  ]
                differentialViable =
                  [ x
                  | x@(_, _, _, _, rho, _, _) <- differentialCands
                  , rho >= bar
                  ]
                diffByName = histogramString [name | (name, _, _, _, _, _, _) <- differentialCands]
                diffViableByName = histogramString [name | (name, _, _, _, _, _, _) <- differentialViable]
                diffByClass = histogramString [show cls | (_, cls, _, _, _, _, _) <- differentialCands]
                diffViableByClass = histogramString [show cls | (_, cls, _, _, _, _, _) <- differentialViable]
                diffBestRho = if null differentialCands then 0.0 else maximum [rho | (_, _, _, _, rho, _, _) <- differentialCands]
                diffBestNu = if null differentialCands then 0 else maximum [nu | (_, _, nu, _, _, _, _) <- differentialCands]
                diffBestK = if null differentialCands then 0 else maximum [kappa | (_, _, _, kappa, _, _, _) <- differentialCands]
                diffBestQuality = if null differentialCands then 0 else maximum [q | (_, _, _, _, _, q, _) <- differentialCands]
                diffRefs = length [() | (_, _, _, _, _, _, hasRef) <- differentialCands, hasRef]
                curvatureLiftNow = isCurvatureLiftPhase lib goalProfile
                curvatureCands =
                  [ (detectCanonicalName tele lib, classifyTelescope tele lib, nu, kappa, rho, curvatureLiftQualityScore lib tele, teleReferencesDifferentialCarrier lib tele, teleHasSurfaceEvidence tele)
                  | (tele, nu, kappa, rho, _) <- enumEvaluatedChosen
                  ]
                curvatureViable =
                  [ x
                  | x@(_, _, _, _, rho, _, _, _) <- curvatureCands
                  , rho >= bar
                  ]
                curvByName = histogramString [name | (name, _, _, _, _, _, _, _) <- curvatureCands]
                curvViableByName = histogramString [name | (name, _, _, _, _, _, _, _) <- curvatureViable]
                curvByClass = histogramString [show cls | (_, cls, _, _, _, _, _, _) <- curvatureCands]
                curvViableByClass = histogramString [show cls | (_, cls, _, _, _, _, _, _) <- curvatureViable]
                curvBestRho = if null curvatureCands then 0.0 else maximum [rho | (_, _, _, _, rho, _, _, _) <- curvatureCands]
                curvBestNu = if null curvatureCands then 0 else maximum [nu | (_, _, nu, _, _, _, _, _) <- curvatureCands]
                curvBestK = if null curvatureCands then 0 else maximum [kappa | (_, _, _, kappa, _, _, _, _) <- curvatureCands]
                curvBestQuality = if null curvatureCands then 0 else maximum [q | (_, _, _, _, _, q, _, _) <- curvatureCands]
                curvRefs = length [() | (_, _, _, _, _, _, hasRef, _) <- curvatureCands, hasRef]
                curvSurface = length [() | (_, _, _, _, _, _, _, hasSurf) <- curvatureCands, hasSurf]
                metricLiftNow = isMetricLiftPhase lib goalProfile
                metricCands =
                  [ (detectCanonicalName tele lib, classifyTelescope tele lib, kappa, rho, metricLiftQualityScore lib tele, teleReferencesCurvatureCarrier lib tele, teleHasMetricBundleEvidence tele)
                  | (tele, _, kappa, rho, _) <- enumEvaluatedChosen
                  ]
                metricViable =
                  [ x
                  | x@(_, _, _, rho, _, _, _) <- metricCands
                  , rho >= bar
                  ]
                metricByName = histogramString [name | (name, _, _, _, _, _, _) <- metricCands]
                metricViableByName = histogramString [name | (name, _, _, _, _, _, _) <- metricViable]
                metricByClass = histogramString [show cls | (_, cls, _, _, _, _, _) <- metricCands]
                metricViableByClass = histogramString [show cls | (_, cls, _, _, _, _, _) <- metricViable]
                metricBestRho = if null metricCands then 0.0 else maximum [rho | (_, _, _, rho, _, _, _) <- metricCands]
                metricBestQuality = if null metricCands then 0 else maximum [q | (_, _, _, _, q, _, _) <- metricCands]
                metricRefs = length [() | (_, _, _, _, _, hasRef, _) <- metricCands, hasRef]
                metricBundle = length [() | (_, _, _, _, _, _, hasBundle) <- metricCands, hasBundle]
                hilbertLiftNow = isHilbertLiftPhase lib goalProfile
                hilbertCands =
                  [ ( detectCanonicalName tele lib
                    , classifyTelescope tele lib
                    , kappa
                    , rho
                    , hilbertLiftQualityScore lib tele
                    , teleReferencesMetricCarrier lib tele
                    , teleReferencesCurvatureCarrier lib tele
                    , teleReferencesDifferentialCarrier lib tele
                    , teleHasHilbertBundleEvidence tele
                    , teleHasFunctionalDerivativeEvidence tele
                    )
                  | (tele, _, kappa, rho, _) <- enumEvaluatedChosen
                  ]
                hilbertViable =
                  [ x
                  | x@(_, _, _, rho, _, _, _, _, _, _) <- hilbertCands
                  , rho >= bar
                  ]
                hilbertByName = histogramString [name | (name, _, _, _, _, _, _, _, _, _) <- hilbertCands]
                hilbertViableByName = histogramString [name | (name, _, _, _, _, _, _, _, _, _) <- hilbertViable]
                hilbertByClass = histogramString [show cls | (_, cls, _, _, _, _, _, _, _, _) <- hilbertCands]
                hilbertViableByClass = histogramString [show cls | (_, cls, _, _, _, _, _, _, _, _) <- hilbertViable]
                hilbertBestRho = if null hilbertCands then 0.0 else maximum [rho | (_, _, _, rho, _, _, _, _, _, _) <- hilbertCands]
                hilbertBestQuality = if null hilbertCands then 0 else maximum [q | (_, _, _, _, q, _, _, _, _, _) <- hilbertCands]
                hilbertMetricRefs = length [() | (_, _, _, _, _, hasMetricRef, _, _, _, _) <- hilbertCands, hasMetricRef]
                hilbertCurvatureRefs = length [() | (_, _, _, _, _, _, hasCurvRef, _, _, _) <- hilbertCands, hasCurvRef]
                hilbertDifferentialRefs = length [() | (_, _, _, _, _, _, _, hasDiffRef, _, _) <- hilbertCands, hasDiffRef]
                hilbertBundle = length [() | (_, _, _, _, _, _, _, _, hasBundle, _) <- hilbertCands, hasBundle]
                hilbertFunctional = length [() | (_, _, _, _, _, _, _, _, _, hasFunctional) <- hilbertCands, hasFunctional]
                temporalLiftNow = isTemporalLiftPhase lib goalProfile
                temporalCands =
                  [ ( detectCanonicalName tele lib
                    , classifyTelescope tele lib
                    , nu
                    , kappa
                    , rho
                    , temporalLiftQualityScore lib tele
                    , teleReferencesHilbertCarrier lib tele
                    , teleReferencesModalCarrier lib tele
                    , teleHasTemporalOpsPair tele
                    , teleTemporalCompatibilityScore tele
                    , teleHasInfinitesimalShiftEvidence tele
                    )
                  | (tele, nu, kappa, rho, _) <- enumEvaluatedChosen
                  ]
                temporalViable =
                  [ x
                  | x@(_, _, _, _, rho, _, _, _, _, _, _) <- temporalCands
                  , rho >= bar
                  ]
                temporalByName = histogramString [name | (name, _, _, _, _, _, _, _, _, _, _) <- temporalCands]
                temporalViableByName = histogramString [name | (name, _, _, _, _, _, _, _, _, _, _) <- temporalViable]
                temporalByClass = histogramString [show cls | (_, cls, _, _, _, _, _, _, _, _, _) <- temporalCands]
                temporalViableByClass = histogramString [show cls | (_, cls, _, _, _, _, _, _, _, _, _) <- temporalViable]
                temporalBestRho = if null temporalCands then 0.0 else maximum [rho | (_, _, _, _, rho, _, _, _, _, _, _) <- temporalCands]
                temporalBestNu = if null temporalCands then 0 else maximum [nu | (_, _, nu, _, _, _, _, _, _, _, _) <- temporalCands]
                temporalBestK = if null temporalCands then 0 else maximum [kappa | (_, _, _, kappa, _, _, _, _, _, _, _) <- temporalCands]
                temporalBestQuality = if null temporalCands then 0 else maximum [q | (_, _, _, _, _, q, _, _, _, _, _) <- temporalCands]
                temporalHilbertRefs = length [() | (_, _, _, _, _, _, hasHilbertRef, _, _, _, _) <- temporalCands, hasHilbertRef]
                temporalModalRefs = length [() | (_, _, _, _, _, _, _, hasModalRef, _, _, _) <- temporalCands, hasModalRef]
                temporalPairedOps = length [() | (_, _, _, _, _, _, _, _, hasPair, _, _) <- temporalCands, hasPair]
                temporalCompatTriad = length [() | (_, _, _, _, _, _, _, _, _, compatScore, _) <- temporalCands, compatScore >= 3]
                temporalInfShift = length [() | (_, _, _, _, _, _, _, _, _, _, hasShift) <- temporalCands, hasShift]
                truncCount = length truncCands
                truncViableCount = length truncViable
                truncBestRho = if null truncCands
                               then 0.0
                               else maximum [r | (_, _, _, r) <- truncCands]
            printf "  [GOAL step %d] intents=%s loop=%s trunc=%s modal=%s diff=%s curv=%s metric=%s hilbert=%s temporal=%s maxDim=%d\n"
              step
              (show (gpIntents goalProfile))
              (show (any leHasLoop lib))
              (show hasTrunc)
              (show (any leHasModalOps lib))
              (show (any leHasDifferentialOps lib))
              (show (any leHasCurvature lib))
              (show (any leHasMetric lib))
              (show (any leHasHilbert lib))
              (show (any leHasTemporalOps lib))
              maxDim
            printf "  [HIT step %d] trunc_candidates=%d trunc_viable=%d trunc_best_rho=%.2f by_k=%s viable_by_k=%s by_max_dim=%s viable_by_max_dim=%s viable_k3_by_max_dim=%s max_rho_by_k=%s max_rho_by_max_dim=%s\n"
              step truncCount truncViableCount truncBestRho
              (show truncByK)
              (show truncViableByK)
              (show truncByMaxDim)
              (show truncViableByMaxDim)
              (show truncViableK3ByMaxDim)
              (show truncMaxRhoByK)
              (show truncMaxRhoByMaxDim)
            when postTruncLiftNow $
              printf "  [LIFT SEARCH step %d] candidates=%d viable=%d best_nu=%d best_k=%d best_rho=%.2f by_k=%s viable_by_k=%s delta_hist=%s viable_delta_hist=%s\n"
                step
                (length liftCands)
                (length liftViable)
                liftBestNu
                liftBestK
                liftBestRho
                (show liftByK)
                (show liftViableByK)
                (show liftDeltaHist)
                (show liftViableDeltaHist)
            when mapBridgeNow $
              printf "  [BRIDGE SEARCH step %d] candidates=%d viable=%d coverage=%d best_rho=%.2f best_quality=%d by_name=%s viable_by_name=%s by_class=%s viable_by_class=%s\n"
                step
                (length mapBridgeCands)
                (length mapBridgeViable)
                mapBridgeCoverage
                mapBridgeBestRho
                mapBridgeBestQuality
                (show mapBridgeByName)
                (show mapBridgeViableByName)
                (show mapBridgeByClass)
                (show mapBridgeViableByClass)
            when modalLiftNow $
              printf "  [MODAL SEARCH step %d] candidates=%d viable=%d modal_rich=%d best_rho=%.2f best_quality=%d by_name=%s viable_by_name=%s by_class=%s viable_by_class=%s\n"
                step
                (length modalCands)
                (length modalViable)
                modalRichCount
                modalBestRho
                modalBestQuality
                (show modalByName)
                (show modalViableByName)
                (show modalByClass)
                (show modalViableByClass)
            when differentialLiftNow $
              printf "  [DIFF SEARCH step %d] candidates=%d viable=%d refs_modal=%d best_nu=%d best_k=%d best_rho=%.2f best_quality=%d by_name=%s viable_by_name=%s by_class=%s viable_by_class=%s\n"
                step
                (length differentialCands)
                (length differentialViable)
                diffRefs
                diffBestNu
                diffBestK
                diffBestRho
                diffBestQuality
                (show diffByName)
                (show diffViableByName)
                (show diffByClass)
                (show diffViableByClass)
            when curvatureLiftNow $
              printf "  [CURV SEARCH step %d] candidates=%d viable=%d refs_diff=%d surface=%d best_nu=%d best_k=%d best_rho=%.2f best_quality=%d by_name=%s viable_by_name=%s by_class=%s viable_by_class=%s\n"
                step
                (length curvatureCands)
                (length curvatureViable)
                curvRefs
                curvSurface
                curvBestNu
                curvBestK
                curvBestRho
                curvBestQuality
                (show curvByName)
                (show curvViableByName)
                (show curvByClass)
                (show curvViableByClass)
            when metricLiftNow $
              printf "  [METRIC SEARCH step %d] candidates=%d viable=%d refs_curv=%d bundle=%d best_rho=%.2f best_quality=%d by_name=%s viable_by_name=%s by_class=%s viable_by_class=%s\n"
                step
                (length metricCands)
                (length metricViable)
                metricRefs
                metricBundle
                metricBestRho
                metricBestQuality
                (show metricByName)
                (show metricViableByName)
                (show metricByClass)
                (show metricViableByClass)
            when hilbertLiftNow $
              printf "  [HILBERT SEARCH step %d] candidates=%d viable=%d refs_metric=%d refs_curv=%d refs_diff=%d bundle=%d functional=%d best_rho=%.2f best_quality=%d by_name=%s viable_by_name=%s by_class=%s viable_by_class=%s\n"
                step
                (length hilbertCands)
                (length hilbertViable)
                hilbertMetricRefs
                hilbertCurvatureRefs
                hilbertDifferentialRefs
                hilbertBundle
                hilbertFunctional
                hilbertBestRho
                hilbertBestQuality
                (show hilbertByName)
                (show hilbertViableByName)
                (show hilbertByClass)
                (show hilbertViableByClass)
            when temporalLiftNow $
              printf "  [TEMP SEARCH step %d] candidates=%d viable=%d refs_hilbert=%d refs_modal=%d paired_ops=%d compat_triad=%d inf_shift=%d best_nu=%d best_k=%d best_rho=%.2f best_quality=%d by_name=%s viable_by_name=%s by_class=%s viable_by_class=%s\n"
                step
                (length temporalCands)
                (length temporalViable)
                temporalHilbertRefs
                temporalModalRefs
                temporalPairedOps
                temporalCompatTriad
                temporalInfShift
                temporalBestNu
                temporalBestK
                temporalBestRho
                temporalBestQuality
                (show temporalByName)
                (show temporalViableByName)
                (show temporalByClass)
                (show temporalViableByClass)

          -- Phase B: MCTS for larger telescopes (κ > 3)
          -- Use a state-derived estimate from discovered history rather than
          -- step-indexed schedules.
          let enumViable = [ c | c@(_, _, _, rho, _) <- enumEvaluatedChosen, rho >= bar ]
              observedKappa = case history of
                [] -> 3
                _  -> maximum (map drKappa history)
              mctsKappaEst = min 12 (max 3 (observedKappa + 2))
              mctsFallback = null enumViable
              mctsPortfolio = not mctsFallback && step >= 3
              needMCTS = not (cfgSkipMCTS cfg)
                      && not (sbForceSkipMCTS budget)
                      && mctsKappaEst > enumKmax
                      && (mctsFallback || mctsPortfolio)
              mctsIterBudget =
                if mctsFallback then sbMCTSIterations budget
                else max 80 (sbMCTSIterations budget `div` 10)
              mctsDepthBudget =
                if mctsFallback then sbMCTSDepth budget
                else min 2 (sbMCTSDepth budget)
              mctsTopKBudget =
                if mctsFallback then sbMCTSTopK budget
                else min 4 (sbMCTSTopK budget)

          mctsCandidates <- if needMCTS
            then do
              when (cfgAdaptiveMemory cfg || cfgMemorySafe cfg) performMajorGC
              let mctsCfg = defaultMCTSConfig
                    { mctsIterations = mctsIterBudget
                    , mctsMaxKappa   = max 5 (mctsKappaEst + 2)
                    , mctsMaxDepth   = mctsDepthBudget
                    , mctsNuDepth    = nuDepth
                    , mctsTopK       = mctsTopKBudget
                    , mctsSeed       = step * 137 + cfgSeed cfg
                    , mctsVerbose    = False
                    , mctsGoalProfile = Just goalProfile
                    }
              (results, mctsStats) <- mctsSearchStep emode mctsCfg lib bar
              -- Print MCTS validity stats
              let validR = msValidRollouts mctsStats
                  rejR   = msRejectedRollouts mctsStats
                  totalR = validR + rejR
                  rejPct = if totalR > 0
                           then 100.0 * fromIntegral rejR / fromIntegral totalR :: Double
                           else 0.0 :: Double
              printf "  [MCTS step %d] %d iters, %d valid, %d rejected (%.1f%%), best rho=%.2f\n"
                step totalR validR rejR rejPct (msBestReward mctsStats)
              hFlush stdout
              return [(tele, nu, kappa, rho, "MCTS" :: String)
                     | (tele, nu, kappa, rho) <- results]
            else return []

          -- Combine and quotient candidates (enum + MCTS).
          -- Apply bar-viability BEFORE quotienting so equivalent candidates
          -- that clear the bar are not dropped by a non-viable representative.
          let rawCandidates = enumEvaluatedChosen ++ mctsCandidates
          assertClaimGradeSources step rawCandidates
          let requiredFamily = expectedFamily
              retryTierBase = if shouldRunStageB then 1 else 0
              familyCandidatesRaw =
                case requiredFamily of
                  Nothing -> []
                  Just fam ->
                    [ cand
                    | cand@(tele, _, _, _, _) <- rawCandidates
                    , isRequiredFamilyCandidate fam lib tele
                    ]
              viableRaw0 = [ c | c@(_, _, _, rho, _) <- rawCandidates, rho >= bar ]
              familyCandidatesViable =
                case requiredFamily of
                  Nothing -> []
                  Just fam ->
                    [ cand
                    | cand@(tele, _, _, _, _) <- viableRaw0
                    , isRequiredFamilyCandidate fam lib tele
                    ]
              bestFamilyCand =
                case sortOn (\(_, _, kappa, rho, _) -> (Down rho, kappa)) familyCandidatesRaw of
                  (cand:_) -> Just cand
                  [] -> Nothing
              basePrefixDiag =
                PrefixDiagnostics
                  { pdRetryTier = retryTierBase
                  , pdRetryCause = ""
                  , pdGuidanceMode = guidanceModeLabel mode
                  , pdAdmissibilityKappaCap = Nothing
                  , pdSearchedKappaBands = ""
                  , pdSelectedKappaBand = Nothing
                  , pdStepBudgetSeconds = Nothing
                  , pdElapsedSeconds = Nothing
                  , pdEvaluatedCandidates = rawCandidateCount
                  , pdDedupedCandidates = canonicalCandidateCount
                  , pdMinimalityRejections = 0
                  , pdBestObligationScore = Nothing
                  , pdBestInterfaceDensity = Nothing
                  , pdBestGenericityScore = Nothing
                  , pdBestClosureScore = Nothing
                  , pdFrontierCandidates = 0
                  , pdFrontierKept = 0
                  , pdMCTSSeedStates = 0
                  , pdMCTSCompletedStates = 0
                  , pdRequiredFamily = maybe "" requiredFamilyLabel requiredFamily
                  , pdBestFamilyRho = case bestFamilyCand of
                      Just (_, _, _, rho, _) -> Just rho
                      Nothing -> Nothing
                  , pdBestFamilyKappa = case bestFamilyCand of
                      Just (_, _, kappa, _, _) -> Just kappa
                      Nothing -> Nothing
                  , pdFamilyCandidates = length familyCandidatesRaw
                  , pdFamilyViable = length familyCandidatesViable
                  }
              allCandidates = if cfgNoCanonicalQuotient cfg
                              then viableRaw0
                              else quotientCandidates viableRaw0
              rawDebug = selectDebugCandidates lib (cfgNoCanonicalQuotient cfg) rawCandidates
              nearMisses = selectNearMissCandidates (cfgNoCanonicalQuotient cfg) bar rawCandidates
              rawCandidateCount = length rawCandidates
              canonicalCandidateCount = length allCandidates
              viable = allCandidates

          if null viable
            then do
              emitPrefixReportRow cfg lib step bar rawCandidateCount canonicalCandidateCount basePrefixDiag Nothing [] agendaDiagChosen "no_viable"
              putStrLn ""
              printf "NO BAR-CLEARING CANDIDATE at step %d (bar=%.4f, raw=%d, viable=%d). Stopping.\n"
                step bar rawCandidateCount canonicalCandidateCount
              printf "  search-debug: raw=%d distinct=%d bar_clear=%d near_miss=%d\n"
                rawCandidateCount canonicalCandidateCount (length viable) (length nearMisses)
              printRawCandidatePool "  Raw search output:" lib rawDebug
              printCandidatePool "  Near misses:" lib (Just bar) nearMisses
              when (agendaActiveChosen && step >= 6) $
                printAgendaDiagnostics step agendaDiagChosen
              let orderedRecords = reverse records
              finishRun
                ("RUN STOPPED: " ++ show (length orderedRecords) ++ " structures discovered")
                orderedRecords
            else do
              -- SELECTION: strict PEN acceptance order in all claim-grade modes.
              -- Among bar-clearing candidates: minimal positive overshoot, then
              -- lower kappa, then lower representational surplus.
              let hasTruncInLibraryForRank =
                    any (\e -> case leIsTruncated e of
                                 Just _ -> True
                                 Nothing -> False) lib
                  bridgePhaseForRank = isTruncBridgePhase lib goalProfile
                  postTruncLiftPhaseForRank = isPostTruncLiftPhase lib goalProfile
                  mapBridgePhaseForRank = isMapBridgePhase lib goalProfile
                  modalPhaseForRank = isModalLiftPhase lib goalProfile
                  differentialPhaseForRank = isDifferentialLiftPhase lib goalProfile
                  curvaturePhaseForRank = isCurvatureLiftPhase lib goalProfile
                  metricPhaseForRank = isMetricLiftPhase lib goalProfile
                  hilbertPhaseForRank = isHilbertLiftPhase lib goalProfile
                  temporalPhaseForRank = isTemporalLiftPhase lib goalProfile
                  richTruncViableExists =
                    bridgePhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              teleHasTrunc tele
                              && kappa >= 3
                              && truncBridgeQualityScore lib tele >= truncBridgeRichThreshold
                           ) viable
                  richPostTruncLiftViableExists =
                    postTruncLiftPhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= requiredPostTruncLiftKappa lib tele
                              && isPostTruncLiftCandidate lib tele
                              && postTruncLiftQualityScore lib tele >= 3
                           ) viable
                  richMapBridgeViableExists =
                    mapBridgePhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= 4
                              && isMapBridgeCandidate lib tele
                              && mapBridgeQualityScore lib tele >= 6
                           ) viable
                  richModalViableExists =
                    modalPhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= requiredModalLiftKappa tele
                              && isModalLiftCandidate lib tele
                              && modalLiftQualityScore lib tele >= 6
                           ) viable
                  richDifferentialViableExists =
                    differentialPhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= requiredDifferentialLiftKappa tele
                              && isDifferentialLiftCandidate lib tele
                              && differentialLiftQualityScore lib tele >= 6
                           ) viable
                  richCurvatureViableExists =
                    curvaturePhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= requiredCurvatureLiftKappa tele
                              && isCurvatureLiftCandidate lib tele
                              && curvatureLiftQualityScore lib tele >= 7
                           ) viable
                  richMetricViableExists =
                    metricPhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= 7
                              && isMetricLiftCandidate lib tele
                              && metricLiftQualityScore lib tele >= 7
                           ) viable
                  richHilbertViableExists =
                    hilbertPhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= 9
                              && isHilbertLiftCandidate lib tele
                              && hilbertLiftQualityScore lib tele >= 10
                           ) viable
                  richTemporalViableExists =
                    temporalPhaseForRank
                    && any (\(tele, _, kappa, _, _) ->
                              kappa >= requiredTemporalLiftKappa tele
                              && isTemporalLiftCandidate lib tele
                              && temporalLiftQualityScore lib tele >= 11
                           ) viable
                  maxTemporalQualityViable =
                    if temporalPhaseForRank
                    then maximum (0 : [temporalLiftQualityScore lib tele | (tele, _, _, _, _) <- viable])
                    else 0
                  viableConnectionsExists =
                    differentialPhaseForRank
                    && any (\(tele, _, _, _, _) -> detectCanonicalName tele lib == "Connections") viable
                  viableHopfExists =
                    mapBridgePhaseForRank
                    && any (\(tele, _, _, _, _) -> detectCanonicalName tele lib == "Hopf") viable
                  viableCohesionExists =
                    modalPhaseForRank
                    && any (\(tele, _, _, _, _) -> detectCanonicalName tele lib == "Cohesion") viable
                  viableCurvatureExists =
                    curvaturePhaseForRank
                    && any (\(tele, _, _, _, _) -> detectCanonicalName tele lib == "Curvature") viable
                  viableMetricExists =
                    metricPhaseForRank
                    && any (\(tele, _, _, _, _) -> detectCanonicalName tele lib == "Metric") viable
                  viableHilbertExists =
                    hilbertPhaseForRank
                    && any (\(tele, _, _, _, _) -> detectCanonicalName tele lib == "Hilbert") viable
                  viableDCTExists =
                    temporalPhaseForRank
                    && any (\(tele, _, _, _, _) -> detectCanonicalName tele lib == "DCT") viable
                  -- In the first differential step, strongly prefer canonical
                  -- Connections if one is already viable.
                  nonConnectionPenalty = 6.0 :: Double
                  nonHopfPenalty = 7.0 :: Double
                  nonCohesionPenalty = 6.0 :: Double
                  nonCurvaturePenalty = 6.0 :: Double
                  nonMetricPenalty = 6.0 :: Double
                  nonHilbertPenalty = 7.0 :: Double
                  nonDCTPenalty = 8.0 :: Double
                  candidateRank (tele, _, kappa, rho, src) =
                    let CanonKey ckey = canonicalKeySpec (map teType (teleEntries tele))
                        name = detectCanonicalName tele lib
                        baseOvershoot = max 0.0 (rho - bar)
                        bridgePenalty =
                          if richTruncViableExists
                          then truncBridgeShortcutPenalty lib tele kappa
                          else 0.0
                        postTruncPenalty =
                          if richPostTruncLiftViableExists
                          then postTruncLiftPenalty lib tele kappa
                          else 0.0
                        mapBridgePenalty' =
                          if richMapBridgeViableExists
                          then mapBridgePenalty lib tele kappa
                               + if viableHopfExists && name /= "Hopf" then nonHopfPenalty else 0.0
                          else 0.0
                        modalPenalty =
                          if richModalViableExists
                          then modalLiftPenalty lib tele kappa
                               + if viableCohesionExists && name /= "Cohesion" then nonCohesionPenalty else 0.0
                          else 0.0
                        differentialPenalty =
                          if richDifferentialViableExists
                          then differentialLiftPenalty lib tele kappa
                               + if viableConnectionsExists && name /= "Connections" then nonConnectionPenalty else 0.0
                          else 0.0
                        curvaturePenalty =
                          if richCurvatureViableExists
                          then curvatureLiftPenalty lib tele kappa
                               + if viableCurvatureExists && name /= "Curvature" then nonCurvaturePenalty else 0.0
                          else 0.0
                        metricPenalty =
                          if richMetricViableExists
                          then metricLiftPenalty lib tele kappa
                               + if viableMetricExists && name /= "Metric" then nonMetricPenalty else 0.0
                          else 0.0
                        hilbertPenalty =
                          if richHilbertViableExists
                          then hilbertLiftPenalty lib tele kappa
                               + if viableHilbertExists && name /= "Hilbert" then nonHilbertPenalty else 0.0
                          else 0.0
                        temporalPenalty =
                          if richTemporalViableExists
                          then temporalLiftPenalty lib tele kappa
                               + if viableDCTExists && name /= "DCT" then nonDCTPenalty else 0.0
                          else 0.0
                        temporalQualityForPenalty =
                          if temporalPhaseForRank
                          then temporalLiftQualityScore lib tele
                          else 0
                        temporalQualityGapPenalty =
                          if richTemporalViableExists
                          then 0.90 * fromIntegral (max 0 (maxTemporalQualityViable - temporalQualityForPenalty))
                          else 0.0
                        overshoot = baseOvershoot + bridgePenalty + postTruncPenalty + mapBridgePenalty' + modalPenalty + differentialPenalty + curvaturePenalty + metricPenalty + hilbertPenalty + temporalPenalty + temporalQualityGapPenalty
                        postTruncQuality =
                          if postTruncLiftPhaseForRank
                          then postTruncLiftQualityScore lib tele
                          else 0
                        mapBridgeQuality =
                          if mapBridgePhaseForRank
                          then mapBridgeQualityScore lib tele
                          else 0
                        modalQuality =
                          if modalPhaseForRank
                          then modalLiftQualityScore lib tele
                          else 0
                        differentialQuality =
                          if differentialPhaseForRank
                          then differentialLiftQualityScore lib tele
                          else 0
                        curvatureQuality =
                          if curvaturePhaseForRank
                          then curvatureLiftQualityScore lib tele
                          else 0
                        metricQuality =
                          if metricPhaseForRank
                          then metricLiftQualityScore lib tele
                          else 0
                        hilbertQuality =
                          if hilbertPhaseForRank
                          then hilbertLiftQualityScore lib tele
                          else 0
                        temporalQuality =
                          temporalQualityForPenalty
                        differentialNameRank =
                          if differentialPhaseForRank
                          then if name == "Connections" then (0 :: Int) else 1
                          else 0
                        modalNameRank =
                          if modalPhaseForRank
                          then if name == "Cohesion" then (0 :: Int) else 1
                          else 0
                        mapNameRank =
                          if mapBridgePhaseForRank
                          then if name == "Hopf" then (0 :: Int) else 1
                          else 0
                        curvatureNameRank =
                          if curvaturePhaseForRank
                          then if name == "Curvature" then (0 :: Int) else 1
                          else 0
                        metricNameRank =
                          if metricPhaseForRank
                          then if name == "Metric" then (0 :: Int) else 1
                          else 0
                        hilbertNameRank =
                          if hilbertPhaseForRank
                          then if name == "Hilbert" then (0 :: Int) else 1
                          else 0
                        temporalNameRank =
                          if temporalPhaseForRank
                          then if name == "DCT" then (0 :: Int) else 1
                          else 0
                        bridgeNameRank = mapNameRank
                        redundantTrunc = (if hasTruncInLibraryForRank && teleHasTrunc tele then 1 else 0) :: Int
                        surplus = structuralSurplusKey tele
                    in ( overshoot
                       , (temporalNameRank, hilbertNameRank, metricNameRank, curvatureNameRank, differentialNameRank, modalNameRank, mapNameRank)
                       , (Down temporalQuality, Down hilbertQuality, Down metricQuality, Down curvatureQuality, Down differentialQuality, Down modalQuality, Down mapBridgeQuality, Down postTruncQuality)
                       , bridgeNameRank
                       , kappa
                       , redundantTrunc
                       , teleBitCost tele
                       , surplus
                       , ckey
                       , sourceRank src
                       )
                  sortedBase = sortOn candidateRank viable
                  defaultBest = case sortedBase of
                    (best:_) -> best
                    [] -> error "internal error: viable candidate set became empty before selection"
                  familyRescue =
                    case requiredFamily of
                      Just FamilyFoundation ->
                        let pool = familyRepresentativePool FamilyFoundation lib viable
                        in case defaultBest of
                             (_, _, bestKappaBase, _, _) ->
                               if not (null pool)
                                  && any (\(_, _, kappa, _, _) -> kappa < bestKappaBase) pool
                               then Just ("bootstrap_foundation", pool)
                               else Nothing
                      Just fam ->
                        let pool = familyRepresentativePool fam lib viable
                            rescueCause = case fam of
                              FamilyUniverseGenesis -> "bootstrap_universe"
                              FamilyFormer -> "bootstrap_former"
                              FamilyWitnessIntro -> "bootstrap_witness"
                              FamilyBootstrapHIT -> "bootstrap_hit"
                              FamilyTrunc -> "hit_trunc_bridge"
                              FamilyPostTrunc -> "hit_post_trunc_lift"
                              FamilyMapBridge -> "map_bridge_family"
                              FamilyModal -> "late_modal_family"
                              FamilyDifferential -> "late_differential_family"
                              FamilyCurvature -> "late_curvature_family"
                              FamilyMetric -> "late_metric_family"
                              FamilyHilbert -> "late_hilbert_family"
                              FamilyTemporal -> "late_temporal_family"
                        in case defaultBest of
                             (bestTeleBase, _, _, _, _) ->
                               if not (null pool)
                                  && case fam of
                                       FamilyFormer ->
                                         needsFormerLift
                                         && not (isStrongFormerCandidate lib bestTeleBase)
                                       FamilyUniverseGenesis ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyWitnessIntro ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyModal ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyDifferential ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyCurvature ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyMetric ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyHilbert ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyTemporal ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       FamilyMapBridge ->
                                         not (any (\(tele, _, _, _, _) -> tele == bestTeleBase) pool)
                                       _ ->
                                         not (isRequiredFamilyCandidate fam lib bestTeleBase)
                               then Just (rescueCause, pool)
                               else Nothing
                      _ -> Nothing
                  selectionPool = case familyRescue of
                    Just (_, pool) -> pool
                    Nothing -> viable
                  sorted = sortOn candidateRank selectionPool
                  distinctSorted = distinctCandidatePool sorted
                  (bestTele, bestNu, bestKappa, bestRho, bestSource) = case distinctSorted of
                    (best:_) ->
                      let (bt, bn, bk, br, bs) = best
                      in (bt, bn, bk, br, bs)
                    [] -> error "internal error: viable candidate set became empty after ranking"
                  bestName = detectCanonicalName bestTele lib
                  runners =
                    take 3
                      [ cand
                      | cand@(tele, _, _, _, _) <- distinctDisplayCandidates lib distinctSorted
                      , detectCanonicalName tele lib /= bestName
                      ]
                  selectionPrefixDiag =
                    basePrefixDiag
                      { pdRetryCause =
                          case familyRescue of
                            Just (cause, _) -> cause
                            Nothing -> pdRetryCause basePrefixDiag
                      }
                  totalCandidates = canonicalCandidateCount
                  dedupeRatio = if rawCandidateCount > 0
                                then fromIntegral canonicalCandidateCount / fromIntegral rawCandidateCount
                                else 1.0 :: Double
                  CanonKey bestCanonKey = canonicalKeySpec (map teType (teleEntries bestTele))

              emitPrefixReportRow
                cfg
                lib
                step
                bar
                rawCandidateCount
                canonicalCandidateCount
                selectionPrefixDiag
                (Just (bestTele, bestNu, bestKappa, bestRho, bestSource))
                runners
                agendaDiagChosen
                "selected"

              -- Display
              printf "%-4d %-16s %8d %7d %8.2f %8.2f %8d  %-8s %d\n"
                step bestName bestNu bestKappa bestRho bar delta_n
                bestSource totalCandidates
              printf "     search-debug: raw=%d distinct=%d bar_clear=%d near_miss=%d\n"
                rawCandidateCount canonicalCandidateCount (length viable) (length nearMisses)
              printRawCandidatePool "     Raw search output:" lib rawDebug
              printSelectedStructureDetails lib bestTele
              printRecognitionTrace lib bestTele bestName bestCanonKey
              printCandidatePool "     Viable runners-up:" lib Nothing runners
              printCandidatePool "     Rejected near misses:" lib (Just bar) nearMisses
              hFlush stdout

              -- Insert selected candidate only (no step-index fallback).
              let discoveredEntry = strictInsertedEntry lib bestName bestTele Nothing
                  newLib = lib ++ [discoveredEntry]
                  newHistory = history ++ [DiscoveryRecord bestNu bestKappa]
                  newRecord = StepRecord step bestName bestNu bestKappa
                                bestRho bar delta_n bestSource totalCandidates
                                rawCandidateCount canonicalCandidateCount dedupeRatio bestCanonKey bestTele

              go newLib newHistory (newRecord : records) (step + 1)

    runHonestStep :: Library -> [DiscoveryRecord] -> [StepRecord] -> Int -> IO ()
    runHonestStep lib history records step = do
      let d = cfgWindow cfg
          bar = computeBarD d mode step history
          delta_n = dBonacciDelta d step
          emode = toEvalMode mode
          StrictAdmissibility kappaCap kappaBands = strictAdmissibility step lib
          bandOrder = prioritizeHonestBands lib kappaBands
          stepBudgetSec = strictStepBudgetSeconds step
          (enumBitBudget, enumAstDepth, enumMaxCandidates, exprCapLimit) = honestEnumBounds kappaCap
          maxCandidates = maybe enumMaxCandidates id (cfgMBTTMaxCand cfg)
          baseEnumCfg =
            defaultEnumConfig
              { ecMaxEntries = kappaCap
              , ecMaxBitBudget = enumBitBudget
              , ecMaxASTDepth = maybe enumAstDepth id (cfgMBTTAstDepth cfg)
              , ecMaxCandidates = maxCandidates
              , ecExactClauseCount = Nothing
              , ecExprCapLimit = Just exprCapLimit
              , ecGoalProfile = Just defaultGoalProfile
              , ecEnableMacros = False
              , ecFrontierMode = if step <= 3 then FrontierNeutral else FrontierObligationGuided
              }
          nuDepth = 1
          nuHist = zip [1..] (map drNu history)
      stepStart <- getCurrentTime
      bandState <- searchBands stepStart stepBudgetSec bar emode nuDepth nuHist kappaBands baseEnumCfg bandOrder emptyHonestSearchState
      enumStop <- getCurrentTime
      let enumElapsed = realToFrac (diffUTCTime enumStop stepStart) :: Double
      finalState <-
        if null (hsViableCandidates bandState)
           && kappaCap > 5
           && not (cfgSkipMCTS cfg)
           && enumElapsed < fromIntegral stepBudgetSec
        then runLateMCTS stepStart stepBudgetSec bar emode nuDepth nuHist kappaBands kappaCap bandState
        else return bandState
      stepEnd <- getCurrentTime
      let elapsedSeconds = realToFrac (diffUTCTime stepEnd stepStart) :: Double
          viable = hsViableCandidates finalState
          rawDebug = hsDebugCandidates finalState
          nearMisses = hsRejectedCandidates finalState
          searchedBandsText = intercalate "|" (map show (hsSearchedBands finalState))
          (bestObligationScore, bestInterfaceDensity, bestGenericityScore, bestClosureScore, frontierCandidates, frontierKept) =
            summarizeHonestFrontier (hsBandTraces finalState)
          basePrefixDiag =
            PrefixDiagnostics
              { pdRetryTier = 0
              , pdRetryCause = ""
              , pdGuidanceMode = guidanceModeLabel mode
              , pdAdmissibilityKappaCap = Just kappaCap
              , pdSearchedKappaBands = searchedBandsText
              , pdSelectedKappaBand = Nothing
              , pdStepBudgetSeconds = Just stepBudgetSec
              , pdElapsedSeconds = Just elapsedSeconds
              , pdEvaluatedCandidates = hsEvaluatedCandidates finalState
              , pdDedupedCandidates = hsDedupedCandidates finalState
              , pdMinimalityRejections = hsMinimalityRejects finalState
              , pdBestObligationScore = bestObligationScore
              , pdBestInterfaceDensity = bestInterfaceDensity
              , pdBestGenericityScore = bestGenericityScore
              , pdBestClosureScore = bestClosureScore
              , pdFrontierCandidates = frontierCandidates
              , pdFrontierKept = frontierKept
              , pdMCTSSeedStates = hsMCTSSeedStates finalState
              , pdMCTSCompletedStates = hsMCTSCompletedStates finalState
              , pdRequiredFamily = ""
              , pdBestFamilyRho = Nothing
              , pdBestFamilyKappa = Nothing
              , pdFamilyCandidates = 0
              , pdFamilyViable = 0
              }
      if null viable
        then do
          emitPrefixReportRow
            cfg
            lib
            step
            bar
            (hsRawCandidates finalState)
            0
            basePrefixDiag
            Nothing
            []
            emptyAgendaDiagnostics
            "no_viable"
          putStrLn ""
          printf "NO BAR-CLEARING CANDIDATE at step %d (bar=%.4f, cap=%d, bands=%s, elapsed=%.2fs). Stopping.\n"
            step bar kappaCap searchedBandsText elapsedSeconds
          printf "  [HONEST step %d] guidance=%s budget=%ds evaluated=%d deduped=%d minimality_rejections=%d\n"
            step
            (guidanceModeLabel mode)
            stepBudgetSec
            (hsEvaluatedCandidates finalState)
            (hsDedupedCandidates finalState)
            (hsMinimalityRejects finalState)
          printf "  search-debug: raw=%d distinct=%d bar_clear=%d near_miss=%d frontier=%d/%d mcts=%d/%d\n"
            (hsRawCandidates finalState)
            (hsDedupedCandidates finalState)
            (length viable)
            (length nearMisses)
            frontierCandidates
            frontierKept
            (hsMCTSSeedStates finalState)
            (hsMCTSCompletedStates finalState)
          printRawCandidatePool "  Raw search output:" lib rawDebug
          printCandidatePool "  Near misses:" lib (Just bar) nearMisses
          let orderedRecords = reverse records
          finishRun
            ("RUN STOPPED: " ++ show (length orderedRecords) ++ " structures discovered")
            orderedRecords
        else do
          let sorted =
                if cfgMaxRho cfg
                then sortOn (honestMaxRhoSelectionKey lib) viable
                else sortOn (honestSelectionKey lib bar) viable
              distinctSorted = distinctCandidatePool sorted
              (bestTele, bestNu, bestKappa, bestRho, bestSource) =
                case distinctSorted of
                  (best:_) -> best
                  [] -> error "internal error: honest viable set became empty before selection"
              bestName = detectCanonicalName bestTele lib
              runners =
                take 3
                  [ cand
                  | cand@(tele, _, _, _, _) <- distinctDisplayCandidates lib distinctSorted
                  , detectCanonicalName tele lib /= bestName
                  ]
              bestSeed =
                Map.lookup
                  (canonicalKeySpec (map teType (teleEntries bestTele)))
                  (hsSeedByKey finalState)
              bestLibraryTele =
                case bestSeed of
                  Just seed -> conceptualSeedTelescope seed
                  Nothing -> bestTele
              selectionPrefixDiag =
                basePrefixDiag
                  { pdSelectedKappaBand = Just bestKappa
                  }
              totalCandidates = length viable
              dedupeRatio =
                if hsRawCandidates finalState > 0
                then fromIntegral (hsDedupedCandidates finalState) / fromIntegral (hsRawCandidates finalState)
                else 1.0 :: Double
              CanonKey bestCanonKey = canonicalKeySpec (map teType (teleEntries bestTele))

          emitPrefixReportRow
            cfg
            lib
            step
            bar
            (hsRawCandidates finalState)
            totalCandidates
            selectionPrefixDiag
            (Just (bestTele, bestNu, bestKappa, bestRho, bestSource))
            runners
            emptyAgendaDiagnostics
            "selected"

          printf "%-4d %-16s %8d %7d %8.2f %8.2f %8d  %-8s %d\n"
            step bestName bestNu bestKappa bestRho bar delta_n bestSource totalCandidates
          printf "     guidance=%s cap=%d bands=%s budget=%ds elapsed=%.2fs evaluated=%d deduped=%d minrej=%d\n"
            (guidanceModeLabel mode)
            kappaCap
            searchedBandsText
            stepBudgetSec
            elapsedSeconds
            (hsEvaluatedCandidates finalState)
            (hsDedupedCandidates finalState)
            (hsMinimalityRejects finalState)
          printf "     search-debug: raw=%d distinct=%d bar_clear=%d near_miss=%d frontier=%d/%d mcts=%d/%d\n"
            (hsRawCandidates finalState)
            (hsDedupedCandidates finalState)
            (length viable)
            (length nearMisses)
            frontierCandidates
            frontierKept
            (hsMCTSSeedStates finalState)
            (hsMCTSCompletedStates finalState)
          printRawCandidatePool "     Raw search output:" lib rawDebug
          printSelectedStructureDetails lib bestTele
          printRecognitionTrace lib bestTele bestName bestCanonKey
          printCandidatePool "     Viable runners-up:" lib Nothing runners
          printCandidatePool "     Rejected near misses:" lib (Just bar) nearMisses
          hFlush stdout

          let discoveredEntry = strictInsertedEntry lib bestName bestLibraryTele bestSeed
              newLib = lib ++ [discoveredEntry]
              newHistory = history ++ [DiscoveryRecord bestNu bestKappa]
              newRecord = StepRecord step bestName bestNu bestKappa
                            bestRho bar delta_n bestSource totalCandidates
                            (hsRawCandidates finalState) (hsDedupedCandidates finalState) dedupeRatio bestCanonKey bestTele

          go newLib newHistory (newRecord : records) (step + 1)
      where
        searchBands :: UTCTime -> Int -> Double -> EvalMode -> Int -> [(Int, Int)] -> [Int] -> EnumConfig -> [Int] -> HonestSearchState -> IO HonestSearchState
        searchBands _ _ _ _ _ _ _ _ [] state = return state
        searchBands stepStart stepBudgetSec' bar' emode' nuDepth' nuHist' admissibleBands enumCfg (band:rest) state = do
          now <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime now stepStart) :: Double
          if elapsed >= fromIntegral stepBudgetSec' && not (null (hsSearchedBands state))
            then return state
            else do
              bandStart <- getCurrentTime
              let bandCfg =
                    enumCfg
                      { ecMaxEntries = band
                      , ecExactClauseCount = Just band
                      }
                  (bandSeeds, bandDiag, cache1) =
                    if step <= 3
                    then
                      let ((atomicCandidates, atomicDiag), cache1') =
                            enumerateMBTTTelescopesWithDiagnostics lib bandCfg (hsEnumCache state)
                      in (map (\cand -> StrictBandSeed (mcTelescope cand) Nothing Nothing Nothing "ENUM_MBTT") atomicCandidates, atomicDiag, cache1')
                    else enumerateStrictBandCandidates lib band bandCfg (hsEnumCache state)
                  rawTelescopes = map sbsTelescope bandSeeds
                  dedupedSeeds =
                    if cfgNoCanonicalQuotient cfg
                    then bandSeeds
                    else dedupBandSeedsByCanonicalKey bandSeeds
                  unseenSeeds =
                    [ seed
                    | seed <- dedupedSeeds
                    , let key = canonicalKeySpec (map teType (teleEntries (sbsTelescope seed)))
                    , not (Set.member key (hsSeenKeys state))
                    ]
                  seenKeys' =
                    foldl' (\acc seed -> Set.insert (canonicalKeySpec (map teType (teleEntries (sbsTelescope seed)))) acc)
                           (hsSeenKeys state)
                           unseenSeeds
                  seedByKey' =
                    foldl'
                      (\acc seed ->
                        Map.insert
                          (canonicalKeySpec (map teType (teleEntries (sbsTelescope seed))))
                          seed
                          acc)
                      (hsSeedByKey state)
                      unseenSeeds
                  (evaluatedAll, evalCache1) =
                    foldl'
                      (\(acc, cacheAcc) seed ->
                        let (cand, cacheNext) =
                              evaluateStrictBandSeedCached (cfgTopoBonusMode cfg) emode' seed lib nuDepth' nuHist' cacheAcc
                        in (cand : acc, cacheNext))
                      ([], hsEvalCache state)
                      unseenSeeds
                  barViable0 =
                    [ cand
                    | cand@(tele, nu, _, rho, _) <- reverse evaluatedAll
                    , nu > 0
                    , rho >= bar'
                    , bootstrapEligible step tele
                    ]
                  (barViable, evalCache2, minimalityRejects) =
                    applyStrictSemanticMinimalityFilter emode' lib nuDepth' bar' nuHist' admissibleBands evalCache1 barViable0
              bandStop <- getCurrentTime
              let bandElapsed = realToFrac (diffUTCTime bandStop bandStart) :: Double
                  bandTrace =
                    HonestBandTrace
                      { hbtBand = band
                      , hbtElapsedSeconds = bandElapsed
                      , hbtFrontier = ebdFrontier bandDiag
                      , hbtEvaluated = length unseenSeeds
                      , hbtDeduped = length dedupedSeeds
                      , hbtViable = length barViable
                      }
                  failedFrontier =
                    if null barViable
                    then mergeFailedFrontier lib (map bandSeedToFrontierCandidate bandSeeds) (hsFailedFrontier state)
                    else hsFailedFrontier state
                  state' =
                    state
                      { hsEnumCache = cache1
                      , hsEvalCache = evalCache2
                      , hsSeenKeys = seenKeys'
                      , hsSeedByKey = seedByKey'
                      , hsRawCandidates = hsRawCandidates state + ebdScannedTelescopes bandDiag
                      , hsEvaluatedCandidates = hsEvaluatedCandidates state + length unseenSeeds
                      , hsDedupedCandidates = hsDedupedCandidates state + length dedupedSeeds
                      , hsMinimalityRejects = hsMinimalityRejects state + minimalityRejects
                      , hsSearchedBands = hsSearchedBands state ++ [band]
                      , hsViableCandidates = hsViableCandidates state ++ barViable
                      , hsDebugCandidates =
                          mergeDebugCandidates
                            lib
                            (reverse evaluatedAll)
                            (hsDebugCandidates state)
                      , hsRejectedCandidates =
                          mergeNearMissCandidates
                            bar'
                            [ cand
                            | cand@(_, nu, _, rho, _) <- reverse evaluatedAll
                            , nu > 0
                            , rho < bar'
                            ]
                            (hsRejectedCandidates state)
                      , hsBandTraces = hsBandTraces state ++ [bandTrace]
                      , hsFailedFrontier = failedFrontier
                      }
              if null barViable
                then searchBands stepStart stepBudgetSec' bar' emode' nuDepth' nuHist' admissibleBands enumCfg rest state'
                else return state'

        runLateMCTS
          :: UTCTime
          -> Int
          -> Double
          -> EvalMode
          -> Int
          -> [(Int, Int)]
          -> [Int]
          -> Int
          -> HonestSearchState
          -> IO HonestSearchState
        runLateMCTS stepStart stepBudgetSec' bar' emode' nuDepth' nuHist' admissibleBands kappaCap' state = do
          now <- getCurrentTime
          let elapsed = realToFrac (diffUTCTime now stepStart) :: Double
              remainingSeconds = max 0 (stepBudgetSec' - ceiling elapsed)
          if remainingSeconds < 5
            then return state
            else do
              let mctsCfg =
                    defaultMCTSConfig
                      { mctsIterations = honestMCTSIterations remainingSeconds kappaCap'
                      , mctsMaxKappa = kappaCap'
                      , mctsMaxDepth = if kappaCap' >= 8 then 3 else 2
                      , mctsNuDepth = nuDepth'
                      , mctsTopK = max 12 (kappaCap' * 8)
                      , mctsSeed = step * 137 + cfgSeed cfg
                      , mctsVerbose = False
                      , mctsGoalProfile = Just defaultGoalProfile
                      , mctsEnableMacroReuse = False
                      }
              (results, mctsStats) <- mctsSearchStep emode' mctsCfg lib bar'
              let validR = msValidRollouts mctsStats
                  rejR = msRejectedRollouts mctsStats
                  totalR = validR + rejR
                  rejPct =
                    if totalR > 0
                    then 100.0 * fromIntegral rejR / fromIntegral totalR :: Double
                    else 0.0 :: Double
              printf "  [MCTS step %d] %d iters, %d valid, %d rejected (%.1f%%), best rho=%.2f\n"
                step totalR validR rejR rejPct (msBestReward mctsStats)
              let rawCandidates =
                    [ (tele, nu, kappa, rho, "MCTS" :: String)
                    | (tele, nu, kappa, rho) <- results
                    ]
                  dedupedCandidates =
                    if cfgNoCanonicalQuotient cfg
                    then rawCandidates
                    else dedupHonestCandidates lib rawCandidates
                  unseenCandidates =
                    [ cand
                    | cand@(tele, _, _, _, _) <- dedupedCandidates
                    , let key = canonicalKeySpec (map teType (teleEntries tele))
                    , not (Set.member key (hsSeenKeys state))
                    ]
                  viable0 =
                    [ cand
                    | cand@(tele, nu, _, rho, _) <- unseenCandidates
                    , nu > 0
                    , rho >= bar'
                    , bootstrapEligible step tele
                    ]
                  (viable1, evalCache1, minimalityRejects) =
                    applyStrictSemanticMinimalityFilter emode' lib nuDepth' bar' nuHist' admissibleBands (hsEvalCache state) viable0
                  viableBand =
                    case viable1 of
                      [] -> []
                      _ ->
                        let minBand = minimum [kappa | (_, _, kappa, _, _) <- viable1]
                        in [cand | cand@(_, _, kappa, _, _) <- viable1, kappa == minBand]
                  seenKeys' =
                    foldl'
                      (\acc (tele, _, _, _, _) ->
                        Set.insert (canonicalKeySpec (map teType (teleEntries tele))) acc)
                      (hsSeenKeys state)
                      unseenCandidates
              return $
                state
                  { hsEvalCache = evalCache1
                  , hsSeenKeys = seenKeys'
                  , hsRawCandidates = hsRawCandidates state + length rawCandidates
                  , hsEvaluatedCandidates = hsEvaluatedCandidates state + length unseenCandidates
                  , hsDedupedCandidates = hsDedupedCandidates state + length unseenCandidates
                  , hsMinimalityRejects = hsMinimalityRejects state + minimalityRejects
                  , hsViableCandidates = hsViableCandidates state ++ viableBand
                  , hsDebugCandidates =
                      mergeDebugCandidates
                        lib
                        unseenCandidates
                        (hsDebugCandidates state)
                  , hsRejectedCandidates =
                      mergeNearMissCandidates
                        bar'
                        [ cand
                        | cand@(_, nu, _, rho, _) <- unseenCandidates
                        , nu > 0
                        , rho < bar'
                        ]
                        (hsRejectedCandidates state)
                  }

    finishRun :: String -> [StepRecord] -> IO ()
    finishRun banner orderedRecords = do
      putStrLn ""
      putStrLn "============================================"
      putStrLn banner
      putStrLn "============================================"
      putStrLn ""
      printDiscoverySummary orderedRecords
      printExclusionContract
      case cfgCsv cfg of
        Just csvPath -> writeCsv csvPath orderedRecords
        Nothing      -> return ()

    printDiscoverySummary :: [StepRecord] -> IO ()
    printDiscoverySummary recs = do
      putStrLn "Discovery Summary:"
      printf "%-4s %-16s %8s %7s %8s %-12s %-11s %7s %4s %s\n"
        ("Step" :: String) ("Structure" :: String) ("Novelty" :: String)
        ("Cost" :: String) ("Score" :: String) ("Source" :: String)
        ("Form" :: String) ("Clauses" :: String) ("Refs" :: String)
        ("Dims" :: String)
      putStrLn (replicate 104 '-')
      printSummaryRows [] recs
      putStrLn (replicate 104 '-')
      printf "Totals: steps=%d novelty=%d cost=%d\n"
        (length recs)
        (sum [srNu r | r <- recs])
        (sum [srKappa r | r <- recs])

    printSummaryRows :: Library -> [StepRecord] -> IO ()
    printSummaryRows _ [] = return ()
    printSummaryRows lib0 (r:rs) = do
      let tele = srTele r
          cls = telescopeClassLabel (classifyTelescope tele lib0)
          clauses = teleKappa tele
          refs = Set.size (teleLibRefs tele)
          dims = renderIntList (telePathDimensions tele)
      printf "%-4d %-16s %8d %7d %8.2f %-12s %-11s %7d %4d %s\n"
        (srStep r)
        (srName r)
        (srNu r)
        (srKappa r)
        (srRho r)
        (srSource r)
        cls
        clauses
        refs
        dims
      let nextLib = lib0 ++ [strictInsertedEntry lib0 (srName r) tele Nothing]
      printSummaryRows nextLib rs

    printSelectedStructureDetails :: Library -> Telescope -> IO ()
    printSelectedStructureDetails lib0 tele = do
      printf "     form=%s clauses=%d bits=%d ast=%d refs=%s dims=%s\n"
        (telescopeClassLabel (classifyTelescope tele lib0))
        (teleKappa tele)
        (teleBitCost tele)
        (teleAstNodes tele)
        (renderIntList (Set.toList (teleLibRefs tele)))
        (renderIntList (telePathDimensions tele))
      printf "     raw=%s\n" (shorten 160 (show tele))

    printCandidatePool :: String -> Library -> Maybe Double -> [Candidate] -> IO ()
    printCandidatePool _ _ _ [] = return ()
    printCandidatePool label lib0 mBar cs =
      putStrLn
        (label ++ " " ++ intercalate " | " (map (candidateBrief lib0 mBar) (take 3 (distinctDisplayCandidates lib0 cs))))

    printRawCandidatePool :: String -> Library -> [Candidate] -> IO ()
    printRawCandidatePool _ _ [] = return ()
    printRawCandidatePool label lib0 cs =
      putStrLn
        (label ++ " " ++ intercalate " | " (map (rawCandidateBrief lib0) (take 3 (distinctDisplayCandidates lib0 cs))))

    printRecognitionTrace :: Library -> Telescope -> String -> String -> IO ()
    printRecognitionTrace _lib0 tele name canonKey = do
      let dr = decodeCanonicalNameWithKey name (Just canonKey)
          decoded = maybe "unresolved" id (drDecodedLabel dr)
          ambiguity =
            if null (drAmbiguity dr)
            then "-"
            else intercalate "|" (drAmbiguity dr)
          referenceInfo =
            case [(stepIdx, refTele) | (stepIdx, refName, refTele) <- allReferenceTelescopes, refName == name] of
              ((stepIdx, refTele):_) ->
                "ref=step " ++ show stepIdx
                ++ " clauses=" ++ show (teleKappa tele) ++ "/" ++ show (teleKappa refTele)
                ++ " bits=" ++ show (teleBitCost tele) ++ "/" ++ show (teleBitCost refTele)
                ++ " dims=" ++ renderIntList (telePathDimensions tele) ++ "/" ++ renderIntList (telePathDimensions refTele)
              [] ->
                "ref=-"
      printf "     recognition: key=%s -> %s, decode=%s, confidence=%.2f, aliases=%s, %s\n"
        (shortCanonKey canonKey)
        name
        decoded
        (drConfidence dr)
        ambiguity
        referenceInfo

    candidateBrief :: Library -> Maybe Double -> Candidate -> String
    candidateBrief lib0 mBar (tele, nu, kappa, rho, source) =
      let gapText = case mBar of
                      Just bar0 -> ", gap=" ++ printf' "%.4f" (bar0 - rho)
                      Nothing -> ""
          keyText = shortCanonKey (candidateCanonKeyText tele)
      in detectCanonicalName tele lib0
         ++ "#" ++ keyText
         ++ " [nov=" ++ show nu
         ++ ", cost=" ++ show kappa
         ++ ", score=" ++ printf' "%.4f" rho
          ++ gapText
         ++ ", form=" ++ telescopeClassLabel (classifyTelescope tele lib0)
         ++ ", bits=" ++ show (teleBitCost tele)
         ++ ", refs=" ++ renderIntList (Set.toList (teleLibRefs tele))
         ++ ", dims=" ++ renderIntList (telePathDimensions tele)
         ++ ", src=" ++ source
         ++ "]"

    rawCandidateBrief :: Library -> Candidate -> String
    rawCandidateBrief lib0 (tele, nu, kappa, rho, source) =
      detectCanonicalName tele lib0
      ++ "#" ++ shortCanonKey (candidateCanonKeyText tele)
      ++ " [nov=" ++ show nu
      ++ ", cost=" ++ show kappa
      ++ ", score=" ++ printf' "%.4f" rho
      ++ ", form=" ++ telescopeClassLabel (classifyTelescope tele lib0)
      ++ ", src=" ++ source
      ++ ", raw=" ++ shorten 96 (show tele)
      ++ "]"

    telescopeClassLabel :: TelescopeClass -> String
    telescopeClassLabel cls = case cls of
      TCFoundation -> "foundation"
      TCFormer -> "former"
      TCHIT -> "hit"
      TCSuspension -> "suspension"
      TCMap -> "map"
      TCModal -> "modal"
      TCAxiomatic -> "axiomatic"
      TCSynthesis -> "synthesis"
      TCUnknown -> "unknown"

    renderIntList :: [Int] -> String
    renderIntList xs =
      case nub xs of
        [] -> "-"
        ys -> intercalate "|" (map show ys)

    distinctCandidatePool :: [Candidate] -> [Candidate]
    distinctCandidatePool =
      reverse . snd . foldl' step (Set.empty, [])
      where
        step (seen, acc) cand@(tele, _, _, _, _) =
          let key = canonicalKeySpec (map teType (teleEntries tele))
          in if Set.member key seen
             then (seen, acc)
             else (Set.insert key seen, cand : acc)

    distinctDisplayCandidates :: Library -> [Candidate] -> [Candidate]
    distinctDisplayCandidates lib0 =
      reverse . snd . foldl' step (Set.empty, [])
      where
        step (seen, acc) cand =
          let key = candidateDisplayKey lib0 cand
          in if Set.member key seen
             then (seen, acc)
             else (Set.insert key seen, cand : acc)

    candidateDisplayKey :: Library -> Candidate -> String
    candidateDisplayKey lib0 (tele, _, kappa, _, _) =
      intercalate ":"
        [ detectCanonicalName tele lib0
        , telescopeClassLabel (classifyTelescope tele lib0)
        , show kappa
        , show (teleKappa tele)
        ]

    selectDebugCandidates :: Library -> Bool -> [Candidate] -> [Candidate]
    selectDebugCandidates lib0 noQuotient candidates =
      let deduped = if noQuotient then candidates else quotientCandidates candidates
      in take 4 (distinctDisplayCandidates lib0 (sortOn debugCandidateSortKey deduped))

    mergeDebugCandidates :: Library -> [Candidate] -> [Candidate] -> [Candidate]
    mergeDebugCandidates lib0 new old =
      take 4 (distinctDisplayCandidates lib0 (sortOn debugCandidateSortKey (new ++ old)))

    selectNearMissCandidates :: Bool -> Double -> [Candidate] -> [Candidate]
    selectNearMissCandidates noQuotient bar0 candidates =
      let rejected =
            [ cand
            | cand@(_, nu, _, rho, _) <- candidates
            , nu > 0
            , rho < bar0
            ]
          deduped = if noQuotient then rejected else quotientCandidates rejected
      in take 3 (distinctCandidatePool (sortOn (nearMissSortKey bar0) deduped))

    mergeNearMissCandidates :: Double -> [Candidate] -> [Candidate] -> [Candidate]
    mergeNearMissCandidates bar0 new old =
      take 3 (distinctCandidatePool (sortOn (nearMissSortKey bar0) (new ++ old)))

    nearMissSortKey :: Double -> Candidate -> (Double, Int, Down Int, Int)
    nearMissSortKey bar0 (_, nu, kappa, rho, source) =
      (bar0 - rho, kappa, Down nu, sourceRank source)

    debugCandidateSortKey :: Candidate -> (Down Double, Int, Down Int, Int)
    debugCandidateSortKey (_, nu, kappa, rho, source) =
      (Down rho, kappa, Down nu, sourceRank source)

    shortCanonKey :: String -> String
    shortCanonKey key = take 8 key

    shorten :: Int -> String -> String
    shorten limit s =
      if length s <= limit
      then s
      else take (max 0 (limit - 3)) s ++ "..."

-- ============================================
-- CSV Output
-- ============================================

-- | Write step records to a CSV file.
-- Includes all three kappa metrics for comparison.
writeCsv :: FilePath -> [StepRecord] -> IO ()
writeCsv path recs = do
  let header = "step,name,nu,kappa,rho,bar,delta,source,candidates,raw_candidates,canonical_candidates,dedupe_ratio,best_canonical_key,k_desugar,k_entry,k_bitcost,canonical_key,bit_kappa,ast_nodes,decoded_name?,decode_confidence,decode_ambiguity,decode_status"
      rows = map formatRow recs
      content = unlines (header : rows)
  withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h content
  printf "CSV written to %s (%d steps)\n" path (length recs)

formatRow :: StepRecord -> String
formatRow r =
  let dr = decodeCanonicalNameWithKey (srName r) (Just (srBestCanonKey r))
      decodedLabel = maybe "" id (drDecodedLabel dr)
      ambiguity = if null (drAmbiguity dr)
                  then ""
                  else intercalate "|" (drAmbiguity dr)
      status = decodeStatus dr
  in intercalate ","
      [ show (srStep r)
      , srName r
      , show (srNu r)
      , show (srKappa r)
      , printf' "%.4f" (srRho r)
      , printf' "%.4f" (srBar r)
      , show (srDelta r)
      , srSource r
      , show (srCands r)
      , show (srRawCands r)
      , show (srCanonCands r)
      , printf' "%.4f" (srDedupeRatio r)
      , srBestCanonKey r
      , show (desugaredKappa (srTele r))
      , show (teleKappa (srTele r))
      , show (teleBitCost (srTele r))
      , srBestCanonKey r
      , show (teleBitCost (srTele r))
      , show (teleAstNodes (srTele r))
      , decodedLabel
      , printf' "%.4f" (drConfidence dr)
      , ambiguity
      , status
      ]


-- | Total AST node count across all MBTT entry types in a telescope.
teleAstNodes :: Telescope -> Int
teleAstNodes (Telescope entries) = sum (map (exprNodeCount . teType) entries)

-- | Count nodes in an MBTT expression (constructors-as-nodes metric).
exprNodeCount :: MBTTExpr -> Int
exprNodeCount expr = case expr of
  App f x        -> 1 + exprNodeCount f + exprNodeCount x
  Lam b          -> 1 + exprNodeCount b
  Pi a b         -> 1 + exprNodeCount a + exprNodeCount b
  Sigma a b      -> 1 + exprNodeCount a + exprNodeCount b
  Univ           -> 1
  Var _          -> 1
  Lib _          -> 1
  Id a x y       -> 1 + exprNodeCount a + exprNodeCount x + exprNodeCount y
  Refl a         -> 1 + exprNodeCount a
  Susp a         -> 1 + exprNodeCount a
  Trunc a        -> 1 + exprNodeCount a
  PathCon _      -> 1
  Flat a         -> 1 + exprNodeCount a
  Sharp a        -> 1 + exprNodeCount a
  Disc a         -> 1 + exprNodeCount a
  Shape a        -> 1 + exprNodeCount a
  Next a         -> 1 + exprNodeCount a
  Eventually a   -> 1 + exprNodeCount a

-- | Decode status class used in Phase-5 reporting surfaces.
decodeStatus :: DecodeResult -> String
decodeStatus dr
  | drDecodedLabel dr == Nothing && drCanonicalName dr == "candidate" = "unknown"
  | drDecodedLabel dr == Nothing = "unidentified_syntactic_attractor"
  | not (null (drAmbiguity dr)) = "ambiguous"
  | otherwise = "exact_isomorphism"

-- | Format a double to string (workaround: printf returns IO).
printf' :: String -> Double -> String
printf' fmt val = let (i, f) = properFraction val :: (Int, Double)
                      decimals = round (f * 10000) :: Int
                  in if fmt == "%.4f"
                     then show i ++ "." ++ padLeft 4 '0' (show (abs decimals))
                     else show val

padLeft :: Int -> Char -> String -> String
padLeft n c s = replicate (max 0 (n - length s)) c ++ s

intercalate :: String -> [String] -> String
intercalate _ []     = ""
intercalate _ [x]    = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- ============================================
-- Semantic Candidate Prefilter
-- ============================================

-- | Cheap structural relevance score used before expensive ν-evaluation.
-- Keeps candidates that add meaningful structure, capability, or reuse.
semanticDeltaScore :: Telescope -> Library -> GoalProfile -> Int
semanticDeltaScore tele lib goalProfile
  | isTriviallyDerivable tele lib = 0
  | not (classReadiness lib cls) = 0
  | otherwise = reuseScore + pathScore + classScore + intentScore + representationScore - representationPenalty
  where
    cls = classifyTelescope tele lib
    refs = teleLibRefs tele
    reuseScore
      | Set.null refs = if teleMaxLibRef tele == 0 then 1 else 0
      | otherwise = 2 + if teleReferencesWindow tele (length lib) then 1 else 0

    knownPathDims = Set.fromList [d | entry <- lib, d <- lePathDims entry]
    newPathDims = [d | d <- telePathDimensions tele, not (Set.member d knownPathDims)]
    pathScore = if null newPathDims then 0 else 2 + length newPathDims

    classScore = case cls of
      TCFoundation -> if length lib < 3 then 10 else 1
      TCFormer -> if any leHasDependentFunctions lib then 2 else 7
      TCHIT -> if any leHasLoop lib then 3 else 6
      TCSuspension -> 2
      TCMap -> if Set.null refs then 0 else 2
      TCModal -> if any leHasModalOps lib then 2 else 6
      TCAxiomatic
        | not (any leHasDifferentialOps lib) -> 5
        | not (any leHasCurvature lib) -> 2
        | not (any leHasMetric lib) -> 2
        | not (any leHasHilbert lib) -> 2
        | otherwise -> 1
      TCSynthesis -> if any leHasTemporalOps lib then 2 else 6
      TCUnknown -> 0

    intentScore = sum [2 | intent <- gpIntents goalProfile, classSupportsIntent cls intent]

    needsFormerBootstrap =
      NeedFormer `elem` gpIntents goalProfile
      && not (any leHasDependentFunctions lib)
    needsHITBootstrap =
      NeedHIT `elem` gpIntents goalProfile
      && not (any leHasLoop lib)
    formerWitness = hasLamExpr tele && hasPiSigmaExpr tele
    formerPure = teleMaxLibRef tele == 0
    formerRich = desugaredKappa tele >= 3 && formerWitness && formerPure
    hitDims = telePathDimensions tele
    hitRich = cls == TCHIT && desugaredKappa tele >= 3 && not (null hitDims) && maximum hitDims >= 1

    representationScore =
      (if needsFormerBootstrap && cls == TCFormer && formerRich then 12 else 0)
      + (if cls == TCFormer && desugaredKappa tele >= 3 then 3 else 0)
      + (if cls == TCFormer && formerWitness then 2 else 0)
      + (if needsHITBootstrap && hitRich then 10 else 0)
      + (if needsHITBootstrap && cls == TCHIT && desugaredKappa tele >= 3 then 3 else 0)

    representationPenalty =
      (if needsFormerBootstrap && cls == TCFormer && not formerWitness then 10 else 0)
      + (if needsFormerBootstrap && cls == TCFormer && desugaredKappa tele < 3 then 8 else 0)
      + (if needsFormerBootstrap && cls == TCFormer && not formerPure then 6 else 0)
      + (if needsFormerBootstrap && cls == TCUnknown then 12 else 0)
      + (if needsFormerBootstrap && cls /= TCFormer && cls /= TCUnknown then 2 else 0)
      + (if needsHITBootstrap && cls == TCUnknown then 10 else 0)
      + (if needsHITBootstrap && cls == TCSuspension then 4 else 0)

    hasLamExpr :: Telescope -> Bool
    hasLamExpr (Telescope entries) = any (goLam . teType) entries
      where
        goLam (Lam _) = True
        goLam (App a b) = goLam a || goLam b
        goLam (Pi a b) = goLam a || goLam b
        goLam (Sigma a b) = goLam a || goLam b
        goLam (Id a x y) = goLam a || goLam x || goLam y
        goLam (Refl a) = goLam a
        goLam (Susp a) = goLam a
        goLam (Trunc a) = goLam a
        goLam (Flat a) = goLam a
        goLam (Sharp a) = goLam a
        goLam (Disc a) = goLam a
        goLam (Shape a) = goLam a
        goLam (Next a) = goLam a
        goLam (Eventually a) = goLam a
        goLam _ = False

    hasPiSigmaExpr :: Telescope -> Bool
    hasPiSigmaExpr (Telescope entries) = any (goFormer . teType) entries
      where
        goFormer (Pi _ _) = True
        goFormer (Sigma _ _) = True
        goFormer (Lam a) = goFormer a
        goFormer (App a b) = goFormer a || goFormer b
        goFormer (Id a x y) = goFormer a || goFormer x || goFormer y
        goFormer (Refl a) = goFormer a
        goFormer (Susp a) = goFormer a
        goFormer (Trunc a) = goFormer a
        goFormer (Flat a) = goFormer a
        goFormer (Sharp a) = goFormer a
        goFormer (Disc a) = goFormer a
        goFormer (Shape a) = goFormer a
        goFormer (Next a) = goFormer a
        goFormer (Eventually a) = goFormer a
        goFormer _ = False

classSupportsIntent :: TelescopeClass -> GoalIntent -> Bool
classSupportsIntent cls intent = case intent of
  NeedBootstrap -> cls == TCFoundation
  NeedFormer -> cls == TCFormer
  NeedHIT -> cls == TCHIT || cls == TCSuspension
  NeedModal -> cls == TCModal
  NeedDifferential -> cls == TCAxiomatic || cls == TCMap
  NeedCurvature -> cls == TCAxiomatic || cls == TCMap
  NeedMetric -> cls == TCAxiomatic || cls == TCMap
  NeedHilbert -> cls == TCAxiomatic || cls == TCMap
  NeedTemporal -> cls == TCSynthesis
  NeedBridge -> cls == TCMap || cls == TCAxiomatic || cls == TCFormer

-- | Structural readiness gate: prevents semantically premature classes.
-- This is derived from available capabilities in the discovered library,
-- not from a fixed step index or target sequence.
classReadiness :: Library -> TelescopeClass -> Bool
classReadiness lib cls = case cls of
  TCFoundation -> True
  TCFormer -> hasConcrete
  TCHIT -> hasConcrete && any leHasDependentFunctions lib
  TCSuspension -> any leHasLoop lib
  TCMap -> concreteCount >= 2
  TCModal -> any leHasDependentFunctions lib && any leHasLoop lib
  TCAxiomatic -> any leHasModalOps lib
  TCSynthesis -> any leHasHilbert lib && any leHasModalOps lib
  TCUnknown -> hasConcrete
  where
    concreteCount = length [() | e <- lib, leConstructors e > 0]
    hasConcrete = concreteCount > 0

requiredFamilyForContext :: Int -> Library -> GoalProfile -> Maybe RequiredFamily
requiredFamilyForContext step lib profile
  | step == 1 = Just FamilyUniverseGenesis
  | step == 2
    && NeedBootstrap `elem` gpIntents profile
    && not (any leHasDependentFunctions lib) = Just FamilyFoundation
  | step == 3
    && any ((== "Unit") . leName) lib = Just FamilyWitnessIntro
  | NeedFormer `elem` gpIntents profile
    && length lib >= 3
    && not (any leHasDependentFunctions lib) = Just FamilyFormer
  | NeedHIT `elem` gpIntents profile
    && any leHasDependentFunctions lib
    && not (any leHasLoop lib) = Just FamilyBootstrapHIT
  | isTruncBridgePhase lib profile = Just FamilyTrunc
  | isPostTruncLiftPhase lib profile = Just FamilyPostTrunc
  | isMapBridgePhase lib profile = Just FamilyMapBridge
  | isModalLiftPhase lib profile = Just FamilyModal
  | isDifferentialLiftPhase lib profile = Just FamilyDifferential
  | isCurvatureLiftPhase lib profile = Just FamilyCurvature
  | isMetricLiftPhase lib profile = Just FamilyMetric
  | isHilbertLiftPhase lib profile = Just FamilyHilbert
  | isTemporalLiftPhase lib profile = Just FamilyTemporal
  | otherwise = Nothing

requiredFamilyLabel :: RequiredFamily -> String
requiredFamilyLabel fam = case fam of
  FamilyUniverseGenesis -> "universe_genesis"
  FamilyFoundation -> "foundation"
  FamilyFormer -> "former"
  FamilyWitnessIntro -> "witness_intro"
  FamilyBootstrapHIT -> "bootstrap_hit"
  FamilyTrunc -> "trunc_bridge"
  FamilyPostTrunc -> "post_trunc_lift"
  FamilyMapBridge -> "map_bridge"
  FamilyModal -> "modal"
  FamilyDifferential -> "differential"
  FamilyCurvature -> "curvature"
  FamilyMetric -> "metric"
  FamilyHilbert -> "hilbert"
  FamilyTemporal -> "temporal"

isRequiredFamilyCandidate :: RequiredFamily -> Library -> Telescope -> Bool
isRequiredFamilyCandidate fam lib tele = case fam of
  FamilyUniverseGenesis -> isUniverseGenesisCandidate tele
  FamilyFoundation -> isPureFoundationCandidate lib tele
  FamilyFormer -> isStrongFormerCandidate lib tele
  FamilyWitnessIntro -> isWitnessIntroductionCandidate tele
  FamilyBootstrapHIT -> isStrongBootstrapHITCandidate lib tele
  FamilyTrunc -> isRichTruncBridgeCandidate lib tele
  FamilyPostTrunc -> isPostTruncLiftCandidate lib tele
  FamilyMapBridge -> isMapBridgeCandidate lib tele
  FamilyModal -> isModalLiftCandidate lib tele
  FamilyDifferential -> isDifferentialLiftCandidate lib tele
  FamilyCurvature -> isCurvatureLiftCandidate lib tele
  FamilyMetric -> isMetricLiftCandidate lib tele
  FamilyHilbert -> isHilbertLiftCandidate lib tele
  FamilyTemporal -> isTemporalLiftCandidate lib tele

isPureFoundationCandidate :: Library -> Telescope -> Bool
isPureFoundationCandidate lib tele =
  classifyTelescope tele lib == TCFoundation
  && teleMaxLibRef tele == 0

isStrongFormerCandidate :: Library -> Telescope -> Bool
isStrongFormerCandidate lib tele =
  classifyTelescope tele lib == TCFormer
  && teleHasLam tele
  && teleHasPiSigma tele
  && teleMaxLibRef tele == 0

isUniverseGenesisCandidate :: Telescope -> Bool
isUniverseGenesisCandidate (Telescope entries) =
  any ((== Univ) . teType) entries
  && any (isUniverseLevel . teType) entries
  where
    isUniverseLevel (App Univ _) = True
    isUniverseLevel _ = False

isWitnessIntroductionCandidate :: Telescope -> Bool
isWitnessIntroductionCandidate (Telescope entries) =
  any (go . teType) entries
  where
    go expr = case expr of
      App (Lib _) (Var _) -> True
      _ -> False

isRichTruncBridgeCandidate :: Library -> Telescope -> Bool
isRichTruncBridgeCandidate lib tele =
  teleHasTrunc tele
  && truncBridgeQualityScore lib tele >= truncBridgeRichThreshold

isStrongBootstrapHITCandidate :: Library -> Telescope -> Bool
isStrongBootstrapHITCandidate lib tele =
  isBootstrapHITCandidate lib tele
  && teleHasConcreteTypeFormation tele
  && teleHasPointConstructor tele

teleExprTypes :: Telescope -> [MBTTExpr]
teleExprTypes = map teType . teleEntries

candidateMatchesExprs :: [MBTTExpr] -> Candidate -> Bool
candidateMatchesExprs exprs (tele, _, _, _, _) = teleExprTypes tele == exprs

selectExactRepresentativeExprs :: Maybe [MBTTExpr] -> [Candidate] -> [Candidate]
selectExactRepresentativeExprs Nothing _ = []
selectExactRepresentativeExprs (Just exprs) candidates =
  [ cand
  | cand <- candidates
  , candidateMatchesExprs exprs cand
  ]

universeGenesisRepresentativeExprs :: [MBTTExpr]
universeGenesisRepresentativeExprs =
  [ Univ
  , App Univ (Var 1)
  ]

preferredWitnessTargetRef :: Library -> Maybe Int
preferredWitnessTargetRef lib =
  case reverse simpleConcreteRefs of
    (i:_) -> Just i
    [] ->
      case reverse concreteRefs of
        (i:_) -> Just i
        [] -> Nothing
  where
    indexed = zip [1..] lib
    simpleConcreteRefs =
      [ i
      | (i, e) <- indexed
      , leConstructors e > 0
      , null (lePathDims e)
      , not (leHasLoop e)
      , case leIsTruncated e of
          Nothing -> True
          Just _ -> False
      ]
    concreteRefs =
      [ i
      | (i, e) <- indexed
      , leConstructors e > 0
      ]

witnessIntroductionRepresentativeExprs :: Library -> Maybe [MBTTExpr]
witnessIntroductionRepresentativeExprs lib = do
  targetRef <- preferredWitnessTargetRef lib
  return [App (Lib targetRef) (Var 1)]

preferredMapBridgeFiberRef :: Library -> Maybe Int
preferredMapBridgeFiberRef lib =
  case reverse loopCarrierRefs of
    (i:_) -> Just i
    [] ->
      case reverse loopFiberRefs of
        (i:_) -> Just i
        [] ->
          case reverse dimOneRefs of
            (i:_) -> Just i
            [] -> Nothing
  where
    indexed = zip [1..] lib
    dimOneRefs =
      [ i
      | (i, e) <- indexed
      , 1 `elem` lePathDims e
      ]
    loopCarrierRefs =
      [ i
      | (i, e) <- indexed
      , 1 `elem` lePathDims e
      , leHasLoop e
      , leConstructors e > 0
      , case leIsTruncated e of
          Nothing -> True
          Just _ -> False
      ]
    loopFiberRefs =
      [ i
      | (i, e) <- indexed
      , 1 `elem` lePathDims e
      , leHasLoop e
      ]

mapBridgeRepresentativeExprs :: Library -> Maybe [MBTTExpr]
mapBridgeRepresentativeExprs lib = do
  srcRef <- latestDimRef 3
  tgtRef <- latestDimRef 2
  fiberRef <- preferredMapBridgeFiberRef lib
  return
    [ Pi (Lib srcRef) (Lib tgtRef)
    , App (Lib fiberRef) (Var 1)
    , Lam (App (Lib srcRef) (Lib tgtRef))
    , Pi (Lib tgtRef) (Lib srcRef)
    ]
  where
    latestDimRef d =
      case reverse [i | (i, e) <- zip [1..] lib, d `elem` lePathDims e] of
        (i:_) -> Just i
        [] -> Nothing

latestCapabilityRef :: (LibraryEntry -> Bool) -> Library -> Maybe Int
latestCapabilityRef hasCap lib =
  case reverse [i | (i, e) <- zip [1..] lib, hasCap e] of
    (i:_) -> Just i
    [] -> Nothing

preferredModalCarrierRef :: Library -> Maybe Int
preferredModalCarrierRef lib =
  case reverse richLoopRefs of
    (i:_) -> Just i
    [] ->
      case reverse loopRefs of
        (i:_) -> Just i
        [] -> Nothing
  where
    indexed = zip [1..] lib
    loopRefs =
      [ i
      | (i, e) <- indexed
      , leHasLoop e
      ]
    richLoopRefs =
      [ i
      | (i, e) <- indexed
      , leHasLoop e
      , leConstructors e > 0
      , case leIsTruncated e of
          Nothing -> True
          Just _ -> False
      ]

modalRepresentativeExprs :: Library -> Maybe [MBTTExpr]
modalRepresentativeExprs lib = do
  carrierRef <- preferredModalCarrierRef lib
  return
    [ Flat (Lib carrierRef)
    , Sharp (Lib carrierRef)
    , Disc (Lib carrierRef)
    , Shape (Lib carrierRef)
    ]

differentialRepresentativeExprs :: Library -> Maybe [MBTTExpr]
differentialRepresentativeExprs lib = do
  modalRef <- latestCapabilityRef leHasModalOps lib
  return
    [ Pi (Lib modalRef) (Pi (Var 1) (Var 1))
    , Lam (Pi (Var 1) (Var 2))
    , Pi (Flat (Var 1)) (Var 1)
    , App (Lib modalRef) (Var 1)
    , Lam (Var 1)
    ]

curvatureRepresentativeExprs :: Library -> Maybe [MBTTExpr]
curvatureRepresentativeExprs lib = do
  differentialRef <- latestCapabilityRef leHasDifferentialOps lib
  return
    [ Pi (Lib differentialRef) (Pi (Var 1) (Var 1))
    , Lam (App (Lib differentialRef) (Var 1))
    , Pi (Var 1) (Lib differentialRef)
    , App (Lib differentialRef) (App (Var 1) (Var 2))
    , Lam (Pi (Var 1) (Var 2))
    , Pi (Lib differentialRef) (Lib differentialRef)
    ]

metricRepresentativeExprs :: Library -> Maybe [MBTTExpr]
metricRepresentativeExprs lib = do
  differentialRef <- latestCapabilityRef leHasDifferentialOps lib
  curvatureRef <- latestCapabilityRef leHasCurvature lib
  return
    [ Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1))
    , Pi (Sigma (Var 1) (Var 2)) (Lib differentialRef)
    , Pi (Var 1) (Pi (Var 1) (Var 1))
    , Lam (App (Var 1) (Var 2))
    , Pi (Lib curvatureRef) (Lib curvatureRef)
    , Lam (Pi (Var 1) (Var 1))
    , Pi (Lib curvatureRef) (Var 1)
    ]

hilbertRepresentativeExprs :: Library -> Maybe [MBTTExpr]
hilbertRepresentativeExprs lib = do
  differentialRef <- latestCapabilityRef leHasDifferentialOps lib
  curvatureRef <- latestCapabilityRef leHasCurvature lib
  metricRef <- latestCapabilityRef leHasMetric lib
  return
    [ Sigma (Pi (Var 1) (Pi (Var 1) Univ)) (Var 1)
    , Pi (Var 1) (Var 1)
    , Pi (Var 1) (Sigma (Var 1) (Var 1))
    , Pi (Lam (Var 1)) (Sigma (Var 1) (Var 2))
    , Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1))
    , Pi (Lib metricRef) (Var 1)
    , Pi (Lib curvatureRef) (Var 1)
    , Pi (Lib differentialRef) (Var 1)
    , Lam (Pi (Var 1) Univ)
    ]

temporalRepresentativeExprs :: Library -> Maybe [MBTTExpr]
temporalRepresentativeExprs lib = do
  modalRef <- latestCapabilityRef leHasModalOps lib
  return
    [ Next (Var 1)
    , Eventually (Var 1)
    , Pi (Next (Var 1)) (Eventually (Var 1))
    , Lam (App (Lib modalRef) (Next (Var 1)))
    , Pi (Flat (Next (Var 1))) (Next (Flat (Var 1)))
    , Pi (Sharp (Eventually (Var 1))) (Eventually (Sharp (Var 1)))
    , Lam (App (Eventually (Var 1)) (Var 2))
    , Pi (Next (Next (Var 1))) (Next (Var 1))
    ]

mkSeedTelescope :: [MBTTExpr] -> Telescope
mkSeedTelescope exprs =
  Telescope
    [ TeleEntry ("c" ++ show idx) expr
    | (idx, expr) <- zip [1 :: Int ..] exprs
    ]

familyRepresentativePool :: RequiredFamily -> Library -> [Candidate] -> [Candidate]
familyRepresentativePool fam lib candidates =
  case fam of
    FamilyFoundation ->
      let familyCands = filterFamily
      in if null familyCands
         then []
         else
           let minKappa = minimum [kappa | (_, _, kappa, _, _) <- familyCands]
               minKappaCands =
                 [ cand
                 | cand@(_, _, kappa, _, _) <- familyCands
                 , kappa == minKappa
                 ]
               bestRho = maximum [rho | (_, _, _, rho, _) <- minKappaCands]
           in [ cand
              | cand@(_, _, _, rho, _) <- minKappaCands
              , abs (rho - bestRho) <= 1.0e-9
              ]
    FamilyFormer ->
      let familyCands = filterFamily
      in if null familyCands
         then []
         else
           let bestRho = maximum [rho | (_, _, _, rho, _) <- familyCands]
           in [ cand
              | cand@(_, _, _, rho, _) <- familyCands
              , abs (rho - bestRho) <= 1.0e-9
              ]
    FamilyUniverseGenesis ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (Just universeGenesisRepresentativeExprs) familyCands
      in if null exact then familyCands else exact
    FamilyWitnessIntro ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (witnessIntroductionRepresentativeExprs lib) familyCands
      in if null exact then familyCands else exact
    FamilyBootstrapHIT -> filterFamily
    FamilyTrunc ->
      let familyCands = filterFamily
      in if null familyCands
         then []
         else
           let minKappa = minimum [kappa | (_, _, kappa, _, _) <- familyCands]
           in [ cand
              | cand@(_, _, kappa, _, _) <- familyCands
              , kappa == minKappa
              ]
    FamilyMapBridge ->
      let familyCands = filterFamily
      in if null familyCands
         then []
         else
           let exact = selectExactRepresentativeExprs (mapBridgeRepresentativeExprs lib) familyCands
           in if not (null exact)
              then exact
              else
                let minKappa = minimum [kappa | (_, _, kappa, _, _) <- familyCands]
                    minKappaCands =
                      [ cand
                      | cand@(_, _, kappa, _, _) <- familyCands
                      , kappa == minKappa
                      ]
                    minBit = minimum [teleBitCost tele | (tele, _, _, _, _) <- minKappaCands]
                in [ cand
                   | cand@(tele, _, _, _, _) <- minKappaCands
                   , teleBitCost tele == minBit
                   ]
    FamilyModal ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (modalRepresentativeExprs lib) familyCands
      in if null exact then familyCands else exact
    FamilyDifferential ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (differentialRepresentativeExprs lib) familyCands
      in if null exact then familyCands else exact
    FamilyCurvature ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (curvatureRepresentativeExprs lib) familyCands
      in if null exact then familyCands else exact
    FamilyMetric ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (metricRepresentativeExprs lib) familyCands
      in if null exact then familyCands else exact
    FamilyHilbert ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (hilbertRepresentativeExprs lib) familyCands
      in if null exact then familyCands else exact
    FamilyTemporal ->
      let familyCands = filterFamily
          exact = selectExactRepresentativeExprs (temporalRepresentativeExprs lib) familyCands
      in if null exact then familyCands else exact
    _ -> filterFamily
  where
    filterFamily =
      [ cand
      | cand@(tele, _, _, _, _) <- candidates
      , isRequiredFamilyCandidate fam lib tele
      ]

-- ============================================
-- Adaptive Memory Budgets
-- ============================================

-- | Read RTS memory stats in MiB, if stats are enabled.
readMemorySnapshot :: IO (Maybe MemorySnapshot)
readMemorySnapshot = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then return Nothing
    else do
      stats <- getRTSStats
      let toMiB bytes = fromIntegral bytes `div` (1024 * 1024)
          gcStats = gc stats
      return $ Just MemorySnapshot
        { msLiveMiB = toMiB (gcdetails_live_bytes gcStats)
        , msInUseMiB = toMiB (gcdetails_mem_in_use_bytes gcStats)
        , msPeakMiB = toMiB (max_mem_in_use_bytes stats)
        }

-- | Classify coarse pressure from live/peak RTS memory.
classifyMemoryPressure :: Maybe MemorySnapshot -> MemoryPressure
classifyMemoryPressure Nothing = MemHigh
classifyMemoryPressure (Just snap)
  | msInUseMiB snap >= 4096 || msLiveMiB snap >= 3072 = MemCritical
  | msInUseMiB snap >= 3072 || msLiveMiB snap >= 2048 = MemHigh
  | msInUseMiB snap >= 1536 || msLiveMiB snap >= 1024 = MemModerate
  | otherwise = MemLow

-- | Move one pressure tier up (used by --memory-safe).
raisePressure :: MemoryPressure -> MemoryPressure
raisePressure MemLow = MemModerate
raisePressure MemModerate = MemHigh
raisePressure MemHigh = MemCritical
raisePressure MemCritical = MemCritical

-- | Integer percentage scaling helper.
scalePct :: Int -> Int -> Int
scalePct x pct = max 1 (x * pct `div` 100)

-- | Compute per-step search limits from config + memory pressure.
computeSearchBudget :: AbInitioConfig -> Int -> IO SearchBudget
computeSearchBudget cfg step = do
  when (cfgAdaptiveMemory cfg || cfgMemorySafe cfg) performMajorGC
  snap <- if cfgAdaptiveMemory cfg || cfgMemorySafe cfg
            then readMemorySnapshot
            else return Nothing
  let shadowProfile = cfgMBTTShadowProfile cfg && step <= 6
      baseBitBudget =
        if shadowProfile then 14 else 16
      baseAstDepth =
        maybe 2 id (cfgMBTTAstDepth cfg)
      baseMaxCandidates =
        maybe 800 id (cfgMBTTMaxCand cfg)
      baseMCTSIterations = 1200
      baseMCTSDepth = 3
      baseMCTSTopK = 10

      pressure0 = if cfgAdaptiveMemory cfg then classifyMemoryPressure snap else MemLow
      pressure = if cfgMemorySafe cfg then raisePressure pressure0 else pressure0

      stepScale = if step >= 13 then 30
                  else if step >= 10 then 40
                  else if step >= 7 then 60
                  else 100
      enumScale = case pressure of
        MemLow -> 100
        MemModerate -> 60
        MemHigh -> 30
        MemCritical -> 15
      mctsScale = case pressure of
        MemLow -> 100
        MemModerate -> 60
        MemHigh -> 30
        MemCritical -> 10

      tunedBitBudget = case pressure of
        MemLow -> baseBitBudget
        MemModerate -> min baseBitBudget 16
        MemHigh -> min baseBitBudget 14
        MemCritical -> min baseBitBudget 12
      depthPressureCap = case pressure of
        MemLow -> baseAstDepth
        MemModerate -> 2
        MemHigh -> 2
        MemCritical -> 2
      tunedAstDepth = max 1 (min baseAstDepth depthPressureCap)
      tunedCandidates = max 1 (scalePct (scalePct baseMaxCandidates stepScale) enumScale)
      tunedMCTSIterations = max 150 (scalePct (scalePct baseMCTSIterations stepScale) mctsScale)
      tunedMCTSDepth = if pressure >= MemHigh then 2 else baseMCTSDepth
      tunedMCTSTopK = case pressure of
        MemLow -> baseMCTSTopK
        MemModerate -> min baseMCTSTopK 8
        MemHigh -> min baseMCTSTopK 5
        MemCritical -> min baseMCTSTopK 3
      forceSkipMCTS = pressure == MemCritical || (pressure >= MemHigh && step >= 12)
  return SearchBudget
    { sbBitBudget = tunedBitBudget
    , sbAstDepth = tunedAstDepth
    , sbMaxCandidates = tunedCandidates
    , sbMCTSIterations = tunedMCTSIterations
    , sbMCTSDepth = tunedMCTSDepth
    , sbMCTSTopK = tunedMCTSTopK
    , sbForceSkipMCTS = forceSkipMCTS
    , sbPressure = pressure
    , sbSnapshot = snap
    }

pressureLabel :: MemoryPressure -> String
pressureLabel p = case p of
  MemLow -> "low"
  MemModerate -> "moderate"
  MemHigh -> "high"
  MemCritical -> "critical"

-- | Emit per-step memory/budget diagnostics.
printSearchBudget :: Int -> SearchBudget -> IO ()
printSearchBudget step budget = do
  let mctsSuffix = if sbForceSkipMCTS budget then ",forced-off" else ""
  case sbSnapshot budget of
    Just snap ->
      printf "  [MEM step %d] pressure=%s live=%dMiB inuse=%dMiB peak=%dMiB enum(bit=%d,depth=%d,cands=%d) mcts(iters=%d,depth=%d,topK=%d%s)\n"
        step (pressureLabel (sbPressure budget)) (msLiveMiB snap) (msInUseMiB snap) (msPeakMiB snap)
        (sbBitBudget budget) (sbAstDepth budget) (sbMaxCandidates budget)
        (sbMCTSIterations budget) (sbMCTSDepth budget) (sbMCTSTopK budget) mctsSuffix
    Nothing ->
      printf "  [MEM step %d] pressure=%s enum(bit=%d,depth=%d,cands=%d) mcts(iters=%d,depth=%d,topK=%d%s)\n"
        step (pressureLabel (sbPressure budget))
        (sbBitBudget budget) (sbAstDepth budget) (sbMaxCandidates budget)
        (sbMCTSIterations budget) (sbMCTSDepth budget) (sbMCTSTopK budget) mctsSuffix

exprCapLimitForStep :: Int -> Int -> Int
exprCapLimitForStep step maxCand =
  let base = min 192 (max 48 (maxCand * 3))
  in if step <= 6 then min 96 base else base

evalModeFingerprint :: EvalMode -> String
evalModeFingerprint = show

nuHistoryFingerprint :: [(Int, Int)] -> String
nuHistoryFingerprint hist =
  intercalate ";" [show stepIdx ++ ":" ++ show nu | (stepIdx, nu) <- hist]

evaluateCandidatesCached
  :: EvalMode
  -> Library
  -> Int
  -> [(Int, Int)]
  -> String
  -> EvalCache
  -> [Telescope]
  -> ([Candidate], EvalCache)
evaluateCandidatesCached evalMode lib nuDepth nuHist src cache0 teles =
  let histKey = nuHistoryFingerprint nuHist
      mkKey tele =
        let CanonKey ckey = canonicalKeySpec (map teType (teleEntries tele))
        in EvalCacheKey (evalModeFingerprint evalMode) ckey nuDepth histKey
      keyed = [(tele, mkKey tele) | tele <- teles]
      missingUnique =
        Map.toList
          (foldl'
             (\acc (tele, key) -> Map.insertWith (\_ old -> old) key tele acc)
             Map.empty
             keyed)
      misses = [ (key, tele) | (key, tele) <- missingUnique, Map.notMember key cache0 ]
      evalOne (key, tele) =
        let (nu, kappa, rho) = evaluateTelescopeWithHistory evalMode tele lib nuDepth "candidate" nuHist
        in nu `seq` kappa `seq` rho `seq` (key, (nu, kappa, rho))
      scoredMisses = parMapChunkedWHNF evalOne misses
      cache1 =
        foldl'
          (\acc (key, score) -> Map.insert key score acc)
          cache0
          scoredMisses
      scored =
        [ (tele, nu, kappa, rho, src)
        | (tele, key) <- keyed
        , Just (nu, kappa, rho) <- [Map.lookup key cache1]
        , nu > 0
        ]
  in (scored, cache1)

-- ============================================
-- Selection Bar Computation
-- ============================================

-- | Compute the selection bar at step n with coherence window depth d.
-- Bar_n = Φ_n · Ω_{n-1}
--
-- Φ_n = Δ_n / Δ_{n-1} where Δ is the d-bonacci sequence.
-- For d=1: Φ_n = 1 (constant → bar grows only via Ω accumulation).
-- For d=2: Φ_n → φ ≈ 1.618 (Fibonacci → exponential bar growth).
-- For d=3: Φ_n → ≈1.839 (tribonacci → faster-than-Fibonacci growth).
computeBarD :: Int -> AbInitioMode -> Int -> [DiscoveryRecord] -> Double
computeBarD _ _ n _
  | n <= 2 = 0.5
computeBarD d mode n history =
  let -- Φ_n = Δ_n / Δ_{n-1} using d-bonacci sequence
      delta_n   = fromIntegral (dBonacciDelta d n) :: Double
      delta_nm1 = fromIntegral (dBonacciDelta d (n-1)) :: Double
      phi_n = delta_n / delta_nm1
      -- Ω_{n-1} = (Σν_i) / (Σκ_i) for i = 1..n-1
      omega = case mode of
        GuidedRecovery ->
          let past = take (n-1) history
              sumNu = sum [drNu r | r <- past]
              sumK  = sum [drKappa r | r <- past]
          in if sumK > 0
             then fromIntegral sumNu / fromIntegral sumK
             else 1.0
        StrictAbInitio ->
          let past = take (n-1) history
              sumNu = sum [drNu r | r <- past]
              sumK  = sum [drKappa r | r <- past]
          in if sumK > 0
             then fromIntegral sumNu / fromIntegral sumK
             else 1.0
  in phi_n * omega

-- ============================================
-- Exclusion Contract
-- ============================================

-- | Print the formal exclusion contract: what PEN derives vs. does not derive.
-- Surfaces the scope boundary in every publication-grade run.
printExclusionContract :: IO ()
printExclusionContract = do
  putStrLn ""
  putStrLn "--- Exclusion Contract ---"
  putStrLn "  PEN derives the KINEMATIC FRAMEWORK of physics:"
  putStrLn "    [+] Dependent types, homotopy types, higher inductive types"
  putStrLn "    [+] Differential cohesion, connections, curvature, metrics"
  putStrLn "    [+] Hilbert functional analysis, Dynamical Cohesive Topos"
  putStrLn ""
  putStrLn "  PEN does NOT derive:"
  putStrLn "    [-] Gauge groups (SU(3) x SU(2) x U(1))"
  putStrLn "    [-] Coupling constants (alpha ~ 1/137)"
  putStrLn "    [-] Spacetime dimension (3+1)"
  putStrLn "    [-] Equations of motion or Lagrangians"
  putStrLn "    [-] Particle content or mass spectrum"
  putStrLn "    [-] Cosmological parameters (Lambda, H_0)"
  putStrLn ""
  putStrLn "  Empirical constants in selection: NONE"
  putStrLn "    Selection uses only: StructuralNu (AST), DesugaredKappa (clause count),"
  putStrLn "    d-bonacci bar (Fibonacci for d=2), canonical structural recognition."
  putStrLn "    No physical constants, no empirical measurements, no fitted parameters."

histogramInt :: [Int] -> [(Int, Int)]
histogramInt xs =
  Map.toAscList (foldl bump Map.empty xs)
  where
    bump m k = Map.insertWith (+) k 1 m

histogramString :: [String] -> [(String, Int)]
histogramString xs =
  Map.toAscList (foldl bump Map.empty xs)
  where
    bump m k = Map.insertWith (+) k 1 m

-- | Phase-9 first-pass runtime guard for claim-grade discovery modes.
-- Strict/structural runs must never admit paper/reference-origin candidates.
assertClaimGradeSources :: Int -> [Candidate] -> IO ()
assertClaimGradeSources step cs =
  let forbidden =
        nub [ src
            | (_, _, _, _, src) <- cs
            , src == "REF" || src == "PAPER"
            ]
  in when (not (null forbidden)) $
      error ("claim-grade source guard violation at step "
             ++ show step ++ ": forbidden sources " ++ show forbidden)

emptyHonestSearchState :: HonestSearchState
emptyHonestSearchState =
  HonestSearchState
    { hsEnumCache = emptyEnumStepCache
    , hsEvalCache = Map.empty
    , hsSeenKeys = Set.empty
    , hsSeedByKey = Map.empty
    , hsRawCandidates = 0
    , hsEvaluatedCandidates = 0
    , hsDedupedCandidates = 0
    , hsMinimalityRejects = 0
    , hsSearchedBands = []
    , hsViableCandidates = []
    , hsDebugCandidates = []
    , hsRejectedCandidates = []
    , hsBandTraces = []
    , hsFailedFrontier = []
    , hsMCTSSeedStates = 0
    , hsMCTSCompletedStates = 0
    }

summarizeHonestFrontier
  :: [HonestBandTrace]
  -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int, Int, Int)
summarizeHonestFrontier traces =
  ( bestOf fdBestObligationScore
  , bestOf fdBestInterfaceDensity
  , bestOf fdBestGenericityScore
  , bestOf fdBestClosureScore
  , sum [fdFrontierCandidates (hbtFrontier trace) | trace <- traces]
  , sum [fdFrontierKept (hbtFrontier trace) | trace <- traces]
  )
  where
    bestOf field =
      case [value | trace <- traces, Just value <- [field (hbtFrontier trace)]] of
        [] -> Nothing
        values -> Just (maximum values)

enumerateStrictBandCandidates
  :: Library
  -> Int
  -> EnumConfig
  -> EnumStepCache
  -> ([StrictBandSeed], EnumBandDiagnostics, EnumStepCache)
enumerateStrictBandCandidates lib band bandCfg cache0 =
  case molecularCandidates of
    [] ->
      let ((bandCandidates, bandDiag), cache1) =
            enumerateMBTTTelescopesWithDiagnostics lib bandCfg cache0
      in (map atomicSeed bandCandidates, bandDiag, cache1)
    _ ->
      ( molecularCandidates
      , EnumBandDiagnostics
          { ebdFrontier = molecularFrontier
          , ebdExprCountsByCtx = []
          , ebdScannedTelescopes = length molecularCandidates
          , ebdValidTelescopes = length molecularCandidates
          }
      , cache0
      )
  where
    compiled =
      [ cm
      | cm <- enumerateStrictMolecularCandidates lib band
      , cmConceptualKappa cm == band
      , checkTelescope lib (cmCompiledTelescope cm) == CheckOK
      ]
    molecularCandidates =
      [ StrictBandSeed tele (Just conceptualKappa) (Just axiomaticExports) (Just provenance) "ENUM_MOLECULAR"
      | CompiledMolecule _ conceptualKappa axiomaticExports tele provenance <- compiled
      ]
    molecularFrontier =
      FrontierDiagnostics
        { fdFrontierCandidates = length molecularCandidates
        , fdFrontierKept = length molecularCandidates
        , fdBestObligationScore = Nothing
        , fdBestInterfaceDensity = Nothing
        , fdBestGenericityScore = Nothing
        , fdBestClosureScore = Nothing
        }
    atomicSeed cand =
      StrictBandSeed (mcTelescope cand) Nothing Nothing Nothing "ENUM_MBTT"

prioritizeHonestBands :: Library -> [Int] -> [Int]
prioritizeHonestBands lib =
  sortOn bandPriority
  where
    bandPriority band =
      ( not (bandHasMolecularCandidates lib band)
      , band
      )

bandHasMolecularCandidates :: Library -> Int -> Bool
bandHasMolecularCandidates lib band =
  any
    (\cm -> checkTelescope lib (cmCompiledTelescope cm) == CheckOK)
    [ cm
    | cm <- enumerateStrictMolecularCandidates lib band
    , cmConceptualKappa cm == band
    ]

mergeFailedFrontier
  :: Library
  -> [MBTTCandidate]
  -> [(Telescope, ObligationSummary)]
  -> [(Telescope, ObligationSummary)]
mergeFailedFrontier lib bandCandidates existing =
  take 12 (sortOn failedFrontierKey (Map.elems merged))
  where
    incoming =
      [ (tele, analyzeObligations lib tele)
      | MBTTCandidate tele _ _ _ _ <- take 12 bandCandidates
      ]
    step acc item@(tele, _) =
      let key = canonicalKeySpec (map teType (teleEntries tele))
      in case Map.lookup key acc of
           Nothing -> Map.insert key item acc
           Just prev ->
             if failedFrontierKey item < failedFrontierKey prev
             then Map.insert key item acc
             else acc
    merged = foldl' step Map.empty (existing ++ incoming)
    failedFrontierKey (tele, summary) =
      ( Down (osScore summary)
      , Down (osClosureScore summary)
      , stableStructuralHash tele
      )

bootstrapEligible :: Int -> Telescope -> Bool
bootstrapEligible step (Telescope entries) =
  case step of
    1 -> hasExactUniv && hasUniverseLevel
    2 -> hasUniverseLevel
    3 -> hasWitnessIntro
    _ -> True
  where
    exprs = map teType entries
    hasExactUniv = any isExactUniv exprs
    hasUniverseLevel = any isUniverseLevel exprs
    hasWitnessIntro = any isWitnessExpr exprs

    isExactUniv Univ = True
    isExactUniv _ = False

    isUniverseLevel (App Univ _) = True
    isUniverseLevel _ = False

    isWitnessExpr (App (Lib _) (Var _)) = True
    isWitnessExpr _ = False

honestEnumBounds :: Int -> (Int, Int, Int, Int)
honestEnumBounds kappaCap =
  case kappaCap of
    k | k <= 2 -> (16, 2, 1024, 96)
    3 -> (24, 3, 768, 128)
    4 -> (20, 2, 768, 96)
    5 -> (24, 2, 1024, 128)
    6 -> (30, 3, 2048, 192)
    7 -> (34, 3, 2560, 224)
    8 -> (38, 3, 3072, 256)
    _ -> (42, 3, 4096, 320)

honestMCTSIterations :: Int -> Int -> Int
honestMCTSIterations remainingSeconds kappaCap =
  max 200 (min 4000 (remainingSeconds * (60 + 10 * max 0 (kappaCap - 5))))

candidateCanonKeyText :: Telescope -> String
candidateCanonKeyText tele =
  case canonicalKeySpec (map teType (teleEntries tele)) of
    CanonKey key -> key

honestSelectionKey :: Library -> Double -> Candidate -> (Double, Int, Down Int, Down Int, Down Int, Down Int, Down Int, Down Int, Down Int, Int, String)
honestSelectionKey lib bar (tele, _, kappa, rho, _) =
  let bootstrap = length lib < 3
      eliminatorScore = if bootstrap then 0 else trueEliminatorScore tele
      lifecycleScore = if bootstrap then 0 else formerLifecycleScore tele
      dependentDensity = if bootstrap then 0 else dependentMotiveDensity tele
      internalAdjoints = if bootstrap then 0 else internalAdjointScore tele
      densityScore = if bootstrap then 0 else countDistinctLibraryRefs tele
      genericityScore = if bootstrap then 0 else countPolymorphicBinders tele
      closureScore' = strictClosureTieBreak lib tele
  in
  ( max 0.0 (rho - bar)
  , kappa
  , Down eliminatorScore
  , Down lifecycleScore
  , Down dependentDensity
  , Down internalAdjoints
  , Down densityScore
  , Down genericityScore
  , Down closureScore'
  , teleBitCost tele
  , stableStructuralHash tele
  )

honestMaxRhoSelectionKey :: Library -> Candidate -> (Down Double, Int, Down Int, Down Int, Down Int, Down Int, Down Int, Down Int, Down Int, Int, String)
honestMaxRhoSelectionKey lib (tele, _, kappa, rho, _) =
  let bootstrap = length lib < 3
      eliminatorScore = if bootstrap then 0 else trueEliminatorScore tele
      lifecycleScore = if bootstrap then 0 else formerLifecycleScore tele
      dependentDensity = if bootstrap then 0 else dependentMotiveDensity tele
      internalAdjoints = if bootstrap then 0 else internalAdjointScore tele
      densityScore = if bootstrap then 0 else countDistinctLibraryRefs tele
      genericityScore = if bootstrap then 0 else countPolymorphicBinders tele
      closureScore' = strictClosureTieBreak lib tele
  in
  ( Down rho
  , kappa
  , Down eliminatorScore
  , Down lifecycleScore
  , Down dependentDensity
  , Down internalAdjoints
  , Down densityScore
  , Down genericityScore
  , Down closureScore'
  , teleBitCost tele
  , stableStructuralHash tele
  )

evaluateCandidateCached
  :: EvalMode
  -> String
  -> Telescope
  -> Library
  -> Int
  -> [(Int, Int)]
  -> EvalCache
  -> (Candidate, EvalCache)
evaluateCandidateCached evalMode src tele lib nuDepth nuHist cache =
  case Map.lookup key cache of
    Just (nu, kappa, rho) -> ((tele, nu, kappa, rho, src), cache)
    Nothing ->
      let result@(nu, kappa, rho) =
            evaluateTelescopeWithHistory evalMode tele lib nuDepth "candidate" nuHist
      in ((tele, nu, kappa, rho, src), Map.insert key result cache)
  where
    key =
      EvalCacheKey
        { eckMode = show evalMode
        , eckTelescope = show tele
        , eckNuDepth = nuDepth
        , eckHistory = show nuHist
        }

countDistinctLibraryRefs :: Telescope -> Int
countDistinctLibraryRefs = Set.size . teleLibRefs

countPolymorphicBinders :: Telescope -> Int
countPolymorphicBinders = genericBinderCount

strictClosureTieBreak :: Library -> Telescope -> Int
strictClosureTieBreak = closureScore

stableStructuralHash :: Telescope -> String
stableStructuralHash = candidateCanonKeyText

bandSeedToFrontierCandidate :: StrictBandSeed -> MBTTCandidate
bandSeedToFrontierCandidate seed =
  let tele = sbsTelescope seed
      clauseKappa =
        case sbsConceptualKappa seed of
          Just kappa -> kappa
          Nothing -> desugaredKappa tele
  in MBTTCandidate tele (teleBitCost tele) clauseKappa 0 0

conceptualSeedTelescope :: StrictBandSeed -> Telescope
conceptualSeedTelescope seed =
  case sbsClauseProvenance seed of
    Just provenance
      | length provenance == length entries ->
          let axiomaticEntries =
                [ entry
                | (entry, prov) <- zip entries provenance
                , prov == AxiomaticSignature
                ]
          in if null axiomaticEntries
             then tele
             else Telescope axiomaticEntries
    _ -> tele
  where
    tele@(Telescope entries) = sbsTelescope seed

seedEvaluationTelescope :: StrictBandSeed -> Telescope
seedEvaluationTelescope = conceptualSeedTelescope

seedTopologicalNu :: TopoBonusMode -> StrictBandSeed -> Int
seedTopologicalNu topoMode seed =
  case sbsClauseProvenance seed of
    Nothing -> 0
    Just provenance
      | length provenance /= length entries -> 0
      | otherwise ->
          let pathDims =
                [ d
                | (TeleEntry _ expr, prov) <- zip entries provenance
                , prov == AxiomaticSignature
                , PathCon d <- [expr]
                ]
              derivedExprs =
                [ expr
                | (TeleEntry _ expr, prov) <- zip entries provenance
                , prov == FrameworkDerived
                ]
              matchedDims =
                [ d
                | d <- pathDims
                , any (isPathComputationFor d) derivedExprs
                ]
              maxDim = if null matchedDims then 0 else maximum matchedDims
          in if null matchedDims
             then 0
             else length matchedDims + topoDimensionBonus topoMode maxDim
  where
    Telescope entries = sbsTelescope seed
    topoDimensionBonus :: TopoBonusMode -> Int -> Int
    topoDimensionBonus mode dim = case mode of
      TopoExact -> dim * dim
      TopoLinear -> dim
      TopoNone -> 0

    isPathComputationFor :: Int -> MBTTExpr -> Bool
    isPathComputationFor dim expr =
      mentionsPathDim dim expr && hasEqualityWitness expr

    mentionsPathDim :: Int -> MBTTExpr -> Bool
    mentionsPathDim dim expr = case expr of
      PathCon d -> d == dim
      App a b -> mentionsPathDim dim a || mentionsPathDim dim b
      Lam a -> mentionsPathDim dim a
      Pi a b -> mentionsPathDim dim a || mentionsPathDim dim b
      Sigma a b -> mentionsPathDim dim a || mentionsPathDim dim b
      Id a x y -> mentionsPathDim dim a || mentionsPathDim dim x || mentionsPathDim dim y
      Refl a -> mentionsPathDim dim a
      Susp a -> mentionsPathDim dim a
      Trunc a -> mentionsPathDim dim a
      Flat a -> mentionsPathDim dim a
      Sharp a -> mentionsPathDim dim a
      Disc a -> mentionsPathDim dim a
      Shape a -> mentionsPathDim dim a
      Next a -> mentionsPathDim dim a
      Eventually a -> mentionsPathDim dim a
      _ -> False

    hasEqualityWitness :: MBTTExpr -> Bool
    hasEqualityWitness expr = case expr of
      Id _ _ _ -> True
      Refl _ -> True
      App a b -> hasEqualityWitness a || hasEqualityWitness b
      Lam a -> hasEqualityWitness a
      Pi a b -> hasEqualityWitness a || hasEqualityWitness b
      Sigma a b -> hasEqualityWitness a || hasEqualityWitness b
      Susp a -> hasEqualityWitness a
      Trunc a -> hasEqualityWitness a
      Flat a -> hasEqualityWitness a
      Sharp a -> hasEqualityWitness a
      Disc a -> hasEqualityWitness a
      Shape a -> hasEqualityWitness a
      Next a -> hasEqualityWitness a
      Eventually a -> hasEqualityWitness a
      _ -> False

hybridStrictSeedNu :: TopoBonusMode -> StrictBandSeed -> Library -> [(Int, Int)] -> Int
hybridStrictSeedNu topoMode seed lib nuHist =
  let coreTele = seedEvaluationTelescope seed
      coreNative = computeNativeNu coreTele lib nuHist
      baseNu = nnNuG coreNative + nnNuC coreNative
      topoNu = seedTopologicalNu topoMode seed
  in baseNu + topoNu

dedupBandSeedsByCanonicalKey :: [StrictBandSeed] -> [StrictBandSeed]
dedupBandSeedsByCanonicalKey =
  Map.elems . foldl' step Map.empty
  where
    step acc seed =
      let key = canonicalKeySpec (map teType (teleEntries (sbsTelescope seed)))
      in case Map.lookup key acc of
           Nothing -> Map.insert key seed acc
           Just prev ->
             if bandSeedOrdering seed < bandSeedOrdering prev
             then Map.insert key seed acc
             else acc
    bandSeedOrdering seed =
      ( conceptualScore (sbsConceptualKappa seed)
      , sourceScore (sbsSource seed)
      , teleBitCost (sbsTelescope seed)
      )
    conceptualScore mk =
      case mk of
        Just kappa -> (0 :: Int, kappa)
        Nothing -> (1 :: Int, 0)
    sourceScore src =
      if src == "ENUM_MOLECULAR" then (0 :: Int) else 1

evaluateStrictBandSeedCached
  :: TopoBonusMode
  -> EvalMode
  -> StrictBandSeed
  -> Library
  -> Int
  -> [(Int, Int)]
  -> EvalCache
  -> (Candidate, EvalCache)
evaluateStrictBandSeedCached topoMode evalMode seed lib nuDepth nuHist cache =
  let tele = sbsTelescope seed
      evalTele = seedEvaluationTelescope seed
      src = sbsSource seed
      molecularStrict =
        evalMode == EvalStrictComputed && sbsClauseProvenance seed /= Nothing
      (nuCompiled, kappaCompiled, cache1)
        | molecularStrict =
            ( hybridStrictSeedNu topoMode seed lib nuHist
            , desugaredKappa evalTele
            , cache
            )
        | otherwise =
            let ((_, nu', kappa', _, _), cacheNext) =
                  evaluateCandidateCached evalMode src evalTele lib nuDepth nuHist cache
            in (nu', kappa', cacheNext)
  in case sbsConceptualKappa seed of
       Nothing -> ((tele, nuCompiled, kappaCompiled, if kappaCompiled > 0 then fromIntegral nuCompiled / fromIntegral kappaCompiled else 0.0, src), cache1)
       Just conceptualKappa ->
         let rhoConceptual =
               if conceptualKappa > 0
               then fromIntegral nuCompiled / fromIntegral conceptualKappa
               else 0.0
         in ((tele, nuCompiled, conceptualKappa, rhoConceptual, src), cache1)

applyStrictSemanticMinimalityFilter
  :: EvalMode
  -> Library
  -> Int
  -> Double
  -> [(Int, Int)]
  -> [Int]
  -> EvalCache
  -> [Candidate]
  -> ([Candidate], EvalCache, Int)
applyStrictSemanticMinimalityFilter evalMode lib nuDepth bar nuHist admissibleBands cache0 candidates =
  foldl' step ([], cache0, 0) candidates
  where
    admissibleBandSet = Set.fromList admissibleBands
    step (kept, cacheAcc, rejected) cand@(tele, _, kappa, _, src) =
      let subTeles =
            [ subTele
            | subTele <- terminalSCCSubBundles tele
            , let subKappa = desugaredKappa subTele
            , subKappa < kappa
            , Set.member subKappa admissibleBandSet
            , not (isTriviallyDerivable subTele lib)
            ]
          (validSubs, _rejectedSubs) = checkAndFilter lib subTeles
          (subCandidates, cacheNext) =
            foldl'
              (\(acc, cacheNow) subTele ->
                let (subCand, cacheLater) =
                      evaluateCandidateCached evalMode src subTele lib nuDepth nuHist cacheNow
                in (subCand : acc, cacheLater))
              ([], cacheAcc)
              validSubs
          barClearingSubExists =
            any (\(_, nu, _, rho, _) -> nu > 0 && rho >= bar) subCandidates
      in if barClearingSubExists
         then (kept, cacheNext, rejected + 1)
         else (cand : kept, cacheNext, rejected)

dedupHonestCandidates :: Library -> [Candidate] -> [Candidate]
dedupHonestCandidates lib candidates =
  [ cand | key <- reverse order, Just cand <- [Map.lookup key reps] ]
  where
    (reps, order) = foldl' step (Map.empty, []) candidates
    step (m, ord) cand@(tele, _, _, _, _) =
      let key = canonicalKeySpec (map teType (teleEntries tele))
      in case Map.lookup key m of
           Nothing -> (Map.insert key cand m, key : ord)
           Just prev ->
             let pick =
                   if honestSelectionKey lib 0.0 cand < honestSelectionKey lib 0.0 prev
                   then cand
                   else prev
             in (Map.insert key pick m, ord)

telescopeNodeCount :: Telescope -> Int
telescopeNodeCount (Telescope entries) = sum (map (exprNodeCount . teType) entries)

strictInsertedEntry :: Library -> String -> Telescope -> Maybe StrictBandSeed -> LibraryEntry
strictInsertedEntry lib name tele mSeed =
  let entry = telescopeToCandidateStructural tele lib name
  in case mSeed of
       Just seed
         | isTruncationSeed seed ->
             withAxiomaticExports seed $
             entry
               { leConstructors = 1
               , lePathDims = []
               , leHasLoop = False
               , leIsTruncated = Just 0
               }
         | otherwise ->
             withAxiomaticExports seed $
             saturateBoundaryPathBasis seed (normalizeMolecularHitConstructors seed entry)
       _ -> entry

withAxiomaticExports :: StrictBandSeed -> LibraryEntry -> LibraryEntry
withAxiomaticExports seed entry =
  let fallback =
        let Telescope entries = conceptualSeedTelescope seed
        in max 1 (length entries)
  in entry
       { leAxiomaticExports =
           case sbsAxiomaticExports seed of
             Just exportCount -> max 1 exportCount
             Nothing -> fallback
       }

normalizeMolecularHitConstructors :: StrictBandSeed -> LibraryEntry -> LibraryEntry
normalizeMolecularHitConstructors seed entry
  | not (isPathSeed seed) = entry
  | otherwise = entry { leConstructors = max 1 (pointConstructorCount seed) }
  where
    isPathSeed s =
      let Telescope entries = conceptualSeedTelescope s
      in any (isPathWitnessExpr . teType) entries

pointConstructorCount :: StrictBandSeed -> Int
pointConstructorCount seed =
  let Telescope entries = conceptualSeedTelescope seed
      exprs = map teType entries
  in length [() | expr <- exprs, isPointConstructorExpr expr]
  where
    isPointConstructorExpr expr =
      not (isFormationExpr expr)
      && not (isPathWitnessExpr expr)
      && not (hasLamExpr expr)

saturateBoundaryPathBasis :: StrictBandSeed -> LibraryEntry -> LibraryEntry
saturateBoundaryPathBasis seed entry
  | maxDim < 2 = entry
  | hasExplicitBoundaryFillers seed = entry
  | otherwise =
      entry
        { lePathDims = Set.toAscList (Set.fromList ([1 .. maxDim] ++ lePathDims entry))
        }
  where
    maxDim =
      case lePathDims entry of
        [] -> 0
        ds -> maximum ds

hasExplicitBoundaryFillers :: StrictBandSeed -> Bool
hasExplicitBoundaryFillers seed =
  let Telescope entries = conceptualSeedTelescope seed
      exprs = map teType entries
      constructorLikeCount =
        length
          [ ()
          | expr <- exprs
          , isConstructorLike expr
          ]
  in any hasLamExpr exprs || constructorLikeCount > 1
  where
    isConstructorLike expr =
      not (isFormationExpr expr) && not (isPathWitnessExpr expr)

isFormationExpr :: MBTTExpr -> Bool
isFormationExpr expr = case expr of
  Univ -> True
  App Univ _ -> True
  Lam a -> isFormationExpr a
  App a b -> isFormationExpr a || isFormationExpr b
  Pi a b -> isFormationExpr a || isFormationExpr b
  Sigma a b -> isFormationExpr a || isFormationExpr b
  Id a x y -> isFormationExpr a || isFormationExpr x || isFormationExpr y
  Refl a -> isFormationExpr a
  _ -> False

hasLamExpr :: MBTTExpr -> Bool
hasLamExpr expr = case expr of
  Lam _ -> True
  App a b -> hasLamExpr a || hasLamExpr b
  Pi a b -> hasLamExpr a || hasLamExpr b
  Sigma a b -> hasLamExpr a || hasLamExpr b
  Id a x y -> hasLamExpr a || hasLamExpr x || hasLamExpr y
  Refl a -> hasLamExpr a
  _ -> False

isTruncationSeed :: StrictBandSeed -> Bool
isTruncationSeed seed =
  let Telescope entries = conceptualSeedTelescope seed
      exprs = map teType entries
  in any isTruncFormer exprs
     && any isTruncIntro exprs
     && any isPathWitnessExpr exprs
  where
    isTruncFormer expr = case expr of
      Trunc _ -> True
      Lam a -> isTruncFormer a
      App a b -> isTruncFormer a || isTruncFormer b
      Pi a b -> isTruncFormer a || isTruncFormer b
      Sigma a b -> isTruncFormer a || isTruncFormer b
      Id a x y -> isTruncFormer a || isTruncFormer x || isTruncFormer y
      Refl a -> isTruncFormer a
      _ -> False

    isTruncIntro expr = case expr of
      App (Trunc _) _ -> True
      Lam a -> isTruncIntro a
      App a b -> isTruncIntro a || isTruncIntro b
      Pi a b -> isTruncIntro a || isTruncIntro b
      Sigma a b -> isTruncIntro a || isTruncIntro b
      Id a x y -> isTruncIntro a || isTruncIntro x || isTruncIntro y
      Refl a -> isTruncIntro a
      _ -> False

isPathWitnessExpr :: MBTTExpr -> Bool
isPathWitnessExpr expr = case expr of
  PathCon _ -> True
  Lam a -> isPathWitnessExpr a
  App a b -> isPathWitnessExpr a || isPathWitnessExpr b
  Pi a b -> isPathWitnessExpr a || isPathWitnessExpr b
  Sigma a b -> isPathWitnessExpr a || isPathWitnessExpr b
  Id a x y -> isPathWitnessExpr a || isPathWitnessExpr x || isPathWitnessExpr y
  Refl a -> isPathWitnessExpr a
  _ -> False

-- | Deduplicate telescopes by canonicalized MBTT-expression key sequence.
--
-- Keeps the first occurrence to preserve deterministic upstream enumeration
-- ordering while collapsing structurally equivalent forms for Phase-2 quotienting.
dedupByCanonicalKey :: [Telescope] -> [Telescope]
dedupByCanonicalKey teles = reverse (snd (foldl step (Map.empty, []) teles))
  where
    step (seen, acc) tele =
      let key = canonicalKeySpec (map teType (teleEntries tele))
      in case Map.lookup key seen of
           Just _  -> (seen, acc)
           Nothing -> (Map.insert key () seen, tele : acc)

-- | Deduplicate telescopes by canonical key while selecting a representative
-- with a caller-provided structural preference.
-- Keeps first-seen key order, replacing the representative if `better` wins.
dedupByCanonicalKeyWith :: (Telescope -> Telescope -> Bool) -> [Telescope] -> [Telescope]
dedupByCanonicalKeyWith better teles =
  [tele | key <- reverse order, Just tele <- [Map.lookup key reps]]
  where
    (reps, order) = foldl step (Map.empty, []) teles

    step (m, ord) tele =
      let key = canonicalKeySpec (map teType (teleEntries tele))
      in case Map.lookup key m of
           Nothing -> (Map.insert key tele m, key : ord)
           Just prev ->
             let pick = if better tele prev then tele else prev
             in (Map.insert key pick m, ord)

betterBridgeRepresentative :: Library -> Telescope -> Telescope -> Bool
betterBridgeRepresentative lib cand prev =
  bridgeRepresentativeKey cand < bridgeRepresentativeKey prev
  where
    libMaxDim = libraryMaxPathDim lib
    bridgeDelta tele = max 0 (teleMaxPathDim tele - libMaxDim)
    bridgeRepresentativeKey tele =
      ( bridgeDelta tele
      , Down (truncBridgeQualityScore lib tele)
      , Down (desugaredKappa tele)
      , structuralSurplusKey tele
      )


-- | Quotient evaluated candidates by canonical MBTT key.
--
-- Keeps insertion order of first-seen keys while allowing a better
-- representative for that key to replace the cached candidate.
--
-- Representative ranking prefers lower κ, then higher ρ, then source rank.
quotientCandidates :: [Candidate] -> [Candidate]
quotientCandidates cs = [cand | key <- reverse order, Just cand <- [Map.lookup key reps]]
  where
    (reps, order) = foldl step (Map.empty, []) cs

    step (m, ord) cand@(tele, _, _, _, _) =
      let key = canonicalKeySpec (map teType (teleEntries tele))
      in case Map.lookup key m of
           Nothing -> (Map.insert key cand m, key : ord)
           Just prev ->
             let pick = if betterCandidate cand prev then cand else prev
             in (Map.insert key pick m, ord)

-- | Candidate ranking used when two candidates share the same canonical key.
betterCandidate :: Candidate -> Candidate -> Bool
betterCandidate (tele1, _, k1, rho1, src1) (tele2, _, k2, rho2, src2) =
  ( k1
  , teleBitCost tele1
  , structuralSurplusKey tele1
  , Down rho1
  , sourceRank src1
  )
  <
  ( k2
  , teleBitCost tele2
  , structuralSurplusKey tele2
  , Down rho2
  , sourceRank src2
  )

-- | Prefer exhaustive enumeration over MCTS when ties occur.
sourceRank :: String -> Int
sourceRank src = case src of
  "AGENDA" -> 0
  "AGENDA_MIX" -> 1
  "ENUM_MBTT" -> 2
  "ENUM" -> 3
  "MCTS" -> 4
  _ -> 4

-- | Neutral structural simplicity key used only as a late tie-breaker.
-- Lower values mean less representational surplus for the same (rho, kappa).
structuralSurplusKey :: Telescope -> (Int, Int, Int, Int, Int)
structuralSurplusKey tele =
  let dims = telePathDimensions tele
      uniqueDims = length (nub dims)
      dupDims = length dims - uniqueDims
      maxDim = if null dims then 0 else maximum dims
      astNodes = teleAstNodes tele
      refCount = Set.size (teleLibRefs tele)
  in (dupDims, maxDim, refCount, astNodes, length dims)

truncBridgeRichThreshold :: Int
truncBridgeRichThreshold = 6

prioritizeBridgeCandidates :: Bool -> Library -> GoalProfile -> [Telescope] -> [Telescope]
prioritizeBridgeCandidates requiresTruncBridge lib profile teles
  | not requiresTruncBridge = teles
  | not (isTruncBridgePhase lib profile) = teles
  | otherwise = sortOn rank teles
  where
    libMaxDim = libraryMaxPathDim lib
    bridgeDelta tele = max 0 (teleMaxPathDim tele - libMaxDim)
    rank tele =
      let CanonKey ckey = canonicalKeySpec (map teType (teleEntries tele))
      in ( bridgeDelta tele
         , Down (truncBridgeQualityScore lib tele)
         , Down (desugaredKappa tele)
         , structuralSurplusKey tele
         , ckey
         )

isTruncBridgePhase :: Library -> GoalProfile -> Bool
isTruncBridgePhase lib profile =
  NeedHIT `elem` gpIntents profile
  && any leHasLoop lib
  && not (any hasTruncEntry lib)
  where
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

isPostTruncLiftPhase :: Library -> GoalProfile -> Bool
isPostTruncLiftPhase lib profile =
  NeedHIT `elem` gpIntents profile
  && any leHasLoop lib
  && hasTrunc
  && libraryMaxPathDim lib >= 1
  where
    hasTrunc = any hasTruncEntry lib
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

isModalLiftPhase :: Library -> GoalProfile -> Bool
isModalLiftPhase lib profile =
  NeedModal `elem` gpIntents profile
  && any leHasLoop lib
  && not (any leHasModalOps lib)

isMapBridgePhase :: Library -> GoalProfile -> Bool
isMapBridgePhase lib profile =
  NeedBridge `elem` gpIntents profile
  && any leHasLoop lib
  && any hasTruncEntry lib
  && not (any leHasModalOps lib)
  where
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

isDifferentialLiftPhase :: Library -> GoalProfile -> Bool
isDifferentialLiftPhase lib profile =
  NeedDifferential `elem` gpIntents profile
  && any leHasModalOps lib
  && not (any leHasDifferentialOps lib)

isCurvatureLiftPhase :: Library -> GoalProfile -> Bool
isCurvatureLiftPhase lib profile =
  NeedCurvature `elem` gpIntents profile
  && any leHasDifferentialOps lib
  && not (any leHasCurvature lib)

isMetricLiftPhase :: Library -> GoalProfile -> Bool
isMetricLiftPhase lib profile =
  NeedMetric `elem` gpIntents profile
  && any leHasCurvature lib
  && not (any leHasMetric lib)

isHilbertLiftPhase :: Library -> GoalProfile -> Bool
isHilbertLiftPhase lib profile =
  NeedHilbert `elem` gpIntents profile
  && any leHasMetric lib
  && not (any leHasHilbert lib)

isTemporalLiftPhase :: Library -> GoalProfile -> Bool
isTemporalLiftPhase lib profile =
  NeedTemporal `elem` gpIntents profile
  && any leHasHilbert lib
  && any leHasModalOps lib
  && not (any leHasTemporalOps lib)

isBootstrapHITCandidate :: Library -> Telescope -> Bool
isBootstrapHITCandidate lib tele =
  case classifyTelescope tele lib of
    TCHIT ->
      let dims = telePathDimensions tele
      in not (null dims)
         && maximum dims <= 1
         && not (teleHasTrunc tele)
    _ -> False

bootstrapHITSeedTelescopes :: [Telescope]
bootstrapHITSeedTelescopes =
  [ Telescope
      [ TeleEntry "c1" (App Univ (Var 1))
      , TeleEntry "c2" (Var 1)
      , TeleEntry "c3" (PathCon 1)
      ]
  ]

universeGenesisSeedTelescopes :: [Telescope]
universeGenesisSeedTelescopes =
  [ Telescope
      [ TeleEntry "c1" Univ
      , TeleEntry "c2" (App Univ (Var 1))
      ]
  ]

witnessIntroductionSeedTelescopes :: Library -> [Telescope]
witnessIntroductionSeedTelescopes lib =
  case witnessIntroductionRepresentativeExprs lib of
    Just [expr] ->
      [ Telescope
          [ TeleEntry "c1" expr
          ]
      ]
    _ -> []

postTruncLiftSeedTelescopes :: Library -> [Telescope]
postTruncLiftSeedTelescopes lib =
  let nextDim = max 1 (libraryMaxPathDim lib + 1)
      liftPath = PathCon nextDim
      carrier = App Univ (Var 1)
      point = Var 1
      coherence = Id (Var 1) (Var 1) (Var 1)
      richer =
        [ Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" point
            , TeleEntry "c3" liftPath
            , TeleEntry "c4" (Lam (Var 1))
            , TeleEntry "c5" (Lam (Var 2))
            ]
        , Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" point
            , TeleEntry "c3" liftPath
            , TeleEntry "c4" coherence
            , TeleEntry "c5" (Pi (Var 1) (Var 1))
            ]
        , Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" point
            , TeleEntry "c3" liftPath
            , TeleEntry "c4" coherence
            , TeleEntry "c5" (Sigma (Var 1) (Var 1))
            ]
        ]
      minimal =
        [ Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" point
            , TeleEntry "c3" liftPath
            ]
        , Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" coherence
            , TeleEntry "c3" liftPath
            ]
        , Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" (Pi (Var 1) (Var 1))
            , TeleEntry "c3" liftPath
            ]
        , Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" (Sigma (Var 1) (Var 1))
            , TeleEntry "c3" liftPath
            ]
        , Telescope
            [ TeleEntry "c1" carrier
            , TeleEntry "c2" (App (Var 1) (Var 1))
            , TeleEntry "c3" liftPath
            ]
        ]
  in
    if nextDim >= 3
    then richer ++ minimal
    else minimal

truncBridgeSeedTelescopes :: Library -> [Telescope]
truncBridgeSeedTelescopes lib =
  let squashDim = max 1 (libraryMaxPathDim lib)
      truncForm = Trunc (Var 1)
      truncIntro = App truncForm (Var 2)
      squash = PathCon squashDim
  in
    [ Telescope
        [ TeleEntry "c1" truncForm
        , TeleEntry "c2" truncIntro
        , TeleEntry "c3" squash
        ]
    , Telescope
        [ TeleEntry "c1" truncForm
        , TeleEntry "c2" truncIntro
        , TeleEntry "c3" squash
        , TeleEntry "c4" (Id (Var 1) (Var 1) (Var 1))
        ]
    ]

mapBridgeSeedTelescopes :: Library -> [Telescope]
mapBridgeSeedTelescopes lib =
  let refsByDim d = [i | (i, e) <- zip [1..] lib, d `elem` lePathDims e]
      srcRef = case reverse (refsByDim 3) of
        (i:_) -> i
        [] -> max 1 (length lib)
      tgtRef = case reverse (refsByDim 2) of
        (i:_) -> i
        [] -> max 1 (length lib)
      fiberRef = case preferredMapBridgeFiberRef lib of
        Just i -> i
        Nothing -> max 1 (length lib)
  in
    [ Telescope
        [ TeleEntry "hopf-map" (Pi (Lib srcRef) (Lib tgtRef))
        , TeleEntry "hopf-fiber" (App (Lib fiberRef) (Var 1))
        , TeleEntry "hopf-total" (Lam (App (Lib srcRef) (Lib tgtRef)))
        , TeleEntry "hopf-class" (Pi (Lib tgtRef) (Lib srcRef))
        ]
    , Telescope
        [ TeleEntry "hopf-map" (Pi (Lib srcRef) (Lib tgtRef))
        , TeleEntry "hopf-fiber" (Sigma (Lib tgtRef) (Lib fiberRef))
        , TeleEntry "hopf-total" (Lam (App (Lib srcRef) (Lib tgtRef)))
        , TeleEntry "hopf-coh" (Id (Lib tgtRef) (Lib tgtRef) (Lib tgtRef))
        ]
    ]

modalLiftSeedTelescopes :: Library -> [Telescope]
modalLiftSeedTelescopes lib =
  let loopRefs = [i | (i, e) <- zip [1..] lib, leHasLoop e]
      loopRef = case reverse loopRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      carrierRef = case preferredModalCarrierRef lib of
        Just i -> i
        Nothing -> loopRef
      carrier = Lib carrierRef
      exact =
        case modalRepresentativeExprs lib of
          Just exprs -> [mkSeedTelescope exprs]
          Nothing -> []
  in
    exact ++
    [ Telescope
        [ TeleEntry "c1" (Flat carrier)
        , TeleEntry "c2" (Sharp carrier)
        , TeleEntry "c3" (Disc carrier)
      ]
    , Telescope
        [ TeleEntry "c1" (Flat carrier)
        , TeleEntry "c2" (Sharp carrier)
        , TeleEntry "c3" (Disc carrier)
        , TeleEntry "c4" (Shape carrier)
        ]
    , Telescope
        [ TeleEntry "c1" (Flat carrier)
        , TeleEntry "c2" (Sharp carrier)
        , TeleEntry "c3" (Shape carrier)
        ]
    , Telescope
        [ TeleEntry "c1" (Flat carrier)
        , TeleEntry "c2" (Sharp carrier)
        , TeleEntry "c3" (Disc carrier)
        , TeleEntry "c4" (Pi carrier carrier)
        ]
    ]

differentialLiftSeedTelescopes :: Library -> [Telescope]
differentialLiftSeedTelescopes lib =
  let modalRefs = [i | (i, e) <- zip [1..] lib, leHasModalOps e]
      modalRef = case reverse modalRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      carrier = Lib modalRef
      exact =
        case differentialRepresentativeExprs lib of
          Just exprs -> [mkSeedTelescope exprs]
          Nothing -> []
  in
    exact ++
    [ Telescope
        [ TeleEntry "conn-form"  (Pi carrier carrier)
        , TeleEntry "transport"  (Pi (Flat carrier) carrier)
        , TeleEntry "cov-deriv"  (Pi (Sigma carrier carrier) carrier)
        , TeleEntry "horiz-coh"  (Id carrier carrier carrier)
        , TeleEntry "leibniz"    (App carrier carrier)
        ]
    , Telescope
        [ TeleEntry "conn-form"  (Pi carrier carrier)
        , TeleEntry "transport"  (Pi (Sharp carrier) carrier)
        , TeleEntry "cov-deriv"  (Pi (Sigma carrier carrier) carrier)
        , TeleEntry "horiz-coh"  (Refl carrier)
        , TeleEntry "leibniz"    (App carrier (Flat carrier))
        ]
    ]

curvatureLiftSeedTelescopes :: Library -> [Telescope]
curvatureLiftSeedTelescopes lib =
  let differentialRefs = [i | (i, e) <- zip [1..] lib, leHasDifferentialOps e]
      differentialRef = case reverse differentialRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      carrier = Lib differentialRef
      exact =
        case curvatureRepresentativeExprs lib of
          Just exprs -> [mkSeedTelescope exprs]
          Nothing -> []
  in
    exact ++
    [ Telescope
        [ TeleEntry "surface-form" (App Univ carrier)
        , TeleEntry "R-form"       (Pi carrier carrier)
        , TeleEntry "bianchi"      (Id carrier carrier carrier)
        , TeleEntry "holonomy"     (Pi (Sigma carrier carrier) carrier)
        , TeleEntry "surface"      (PathCon 2)
        , TeleEntry "chern-weil"   (App carrier carrier)
        ]
    , Telescope
        [ TeleEntry "surface-form" (App Univ carrier)
        , TeleEntry "R-form"       (Pi carrier carrier)
        , TeleEntry "bianchi"      (Refl carrier)
        , TeleEntry "holonomy"     (Pi carrier (Pi carrier carrier))
        , TeleEntry "surface"      (PathCon 2)
        , TeleEntry "chern-weil"   (App carrier (Flat carrier))
        ]
    ]

metricLiftSeedTelescopes :: Library -> [Telescope]
metricLiftSeedTelescopes lib =
  let differentialRefs = [i | (i, e) <- zip [1..] lib, leHasDifferentialOps e]
      curvatureRefs = [i | (i, e) <- zip [1..] lib, leHasCurvature e]
      differentialRef = case reverse differentialRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      curvatureRef = case reverse curvatureRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      gForm = Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1))
      connLaw = Pi (Sigma (Var 1) (Var 1)) (Lib differentialRef)
      geodesic = Pi (Var 1) (Pi (Var 1) (Var 1))
      volForm = Lam (App (Var 1) (Var 1))
      hodge = Pi (Lib curvatureRef) (Lib curvatureRef)
      laplace = Lam (Pi (Var 1) (Var 1))
      ricci = Pi (Lib curvatureRef) (Var 1)
      exact =
        case metricRepresentativeExprs lib of
          Just exprs -> [mkSeedTelescope exprs]
          Nothing -> []
  in
    exact ++
    [ Telescope
        [ TeleEntry "c1" gForm
        , TeleEntry "c2" connLaw
        , TeleEntry "c3" geodesic
        , TeleEntry "c4" volForm
        , TeleEntry "c5" hodge
        , TeleEntry "c6" laplace
        , TeleEntry "c7" ricci
        ]
    , Telescope
        [ TeleEntry "c1" gForm
        , TeleEntry "c2" connLaw
        , TeleEntry "c3" hodge
        , TeleEntry "c4" ricci
        ]
    ]

hilbertLiftSeedTelescopes :: Library -> [Telescope]
hilbertLiftSeedTelescopes lib =
  let differentialRefs = [i | (i, e) <- zip [1..] lib, leHasDifferentialOps e]
      curvatureRefs = [i | (i, e) <- zip [1..] lib, leHasCurvature e]
      metricRefs = [i | (i, e) <- zip [1..] lib, leHasMetric e]
      differentialRef = case reverse differentialRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      curvatureRef = case reverse curvatureRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      metricRef = case reverse metricRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      innerProduct = Sigma (Pi (Lib metricRef) (Pi (Lib metricRef) Univ)) (Lib metricRef)
      cauchyComplete = Pi (Lib metricRef) (Lib metricRef)
      orthogonalDecomp = Pi (Lib metricRef) (Sigma (Lib metricRef) (Lib metricRef))
      spectralDecomp = Pi (Pi (Lib metricRef) (Lib metricRef)) (Sigma (Lib metricRef) (Lib metricRef))
      cStarAlg = Sigma (Pi (Lib metricRef) (Lib metricRef)) (Pi (Lib metricRef) (Lib metricRef))
      metricCompat = Pi (Lib metricRef) (Lib curvatureRef)
      curvatureOp = Pi (Lib curvatureRef) (Lib differentialRef)
      connectionOp = Pi (Lib differentialRef) (Lib metricRef)
      functionalDeriv = Lam (Pi (Lib metricRef) Univ)
      exact =
        case hilbertRepresentativeExprs lib of
          Just exprs -> [mkSeedTelescope exprs]
          Nothing -> []
  in
    exact ++
    [ Telescope
        [ TeleEntry "c1" innerProduct
        , TeleEntry "c2" cauchyComplete
        , TeleEntry "c3" orthogonalDecomp
        , TeleEntry "c4" spectralDecomp
        , TeleEntry "c5" cStarAlg
        , TeleEntry "c6" metricCompat
        , TeleEntry "c7" curvatureOp
        , TeleEntry "c8" connectionOp
        , TeleEntry "c9" functionalDeriv
        ]
    , Telescope
        [ TeleEntry "c1" innerProduct
        , TeleEntry "c2" cauchyComplete
        , TeleEntry "c3" orthogonalDecomp
        , TeleEntry "c4" cStarAlg
        , TeleEntry "c5" metricCompat
        , TeleEntry "c6" curvatureOp
        , TeleEntry "c7" connectionOp
        , TeleEntry "c8" functionalDeriv
        , TeleEntry "c9" (Id (Lib metricRef) (Lib metricRef) (Lib metricRef))
        ]
    ]

temporalLiftSeedTelescopes :: Library -> [Telescope]
temporalLiftSeedTelescopes lib =
  let modalRefs = [i | (i, e) <- zip [1..] lib, leHasModalOps e]
      hilbertRefs = [i | (i, e) <- zip [1..] lib, leHasHilbert e]
      modalRef = case reverse modalRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      hilbertRef = case reverse hilbertRefs of
        (i:_) -> i
        [] -> max 1 (length lib)
      spatial = Lib modalRef
      dynamic = Lib hilbertRef
      nextSpatial = Next spatial
      eventuallySpatial = Eventually spatial
      compatFlat = Id (Flat nextSpatial) (Next (Flat spatial)) (Next (Flat spatial))
      compatShape = Id (Shape nextSpatial) (Next (Shape spatial)) (Next (Shape spatial))
      compatLinear = Id (Next (Pi spatial dynamic))
                       (Pi nextSpatial (Next dynamic))
                       (Pi nextSpatial (Next dynamic))
      infinitesimal = Sigma nextSpatial eventuallySpatial
      distFlat = Pi (Flat (Next (Var 1))) (Next (Flat (Var 1)))
      distShape = Pi (Shape (Eventually (Var 1))) (Eventually (Shape (Var 1)))
      polyTemporal = Pi (Next (Var 1)) (Eventually (Var 1))
      spatialTemporalLam = Lam (App spatial (Next (Var 1)))
      spatialTemporalLamEv = Lam (App spatial (Eventually (Var 1)))
      exact =
        case temporalRepresentativeExprs lib of
          Just exprs -> [mkSeedTelescope exprs]
          Nothing -> []
  in
    exact ++
    [ Telescope
        [ TeleEntry "c1" (Pi spatial spatial)
        , TeleEntry "c2" (Pi dynamic dynamic)
        , TeleEntry "c3" nextSpatial
        , TeleEntry "c4" eventuallySpatial
        , TeleEntry "c5" infinitesimal
        , TeleEntry "c6" compatFlat
        , TeleEntry "c7" compatShape
        , TeleEntry "c8" compatLinear
        ]
    , Telescope
        [ TeleEntry "c1" (Pi spatial spatial)
        , TeleEntry "c2" (Pi dynamic dynamic)
        , TeleEntry "c3" nextSpatial
        , TeleEntry "c4" eventuallySpatial
        , TeleEntry "c5" (App dynamic nextSpatial)
        , TeleEntry "c6" compatFlat
        , TeleEntry "c7" compatShape
        , TeleEntry "c8" (Pi nextSpatial eventuallySpatial)
        ]
    , Telescope
        [ TeleEntry "c1" (Pi spatial spatial)
        , TeleEntry "c2" (Pi dynamic dynamic)
        , TeleEntry "c3" distFlat
        , TeleEntry "c4" distShape
        , TeleEntry "c5" polyTemporal
        , TeleEntry "c6" spatialTemporalLam
        , TeleEntry "c7" nextSpatial
        , TeleEntry "c8" eventuallySpatial
        ]
    , Telescope
        [ TeleEntry "c1" (Pi spatial spatial)
        , TeleEntry "c2" (Pi dynamic dynamic)
        , TeleEntry "c3" compatFlat
        , TeleEntry "c4" compatShape
        , TeleEntry "c5" compatLinear
        , TeleEntry "c6" spatialTemporalLamEv
        , TeleEntry "c7" polyTemporal
        , TeleEntry "c8" infinitesimal
        ]
    ]

requiredPostTruncLiftKappa :: Library -> Telescope -> Int
requiredPostTruncLiftKappa lib tele
  | teleMaxPathDim tele >= 3 = 5
  | libraryMaxPathDim lib >= 2 = 5
  | otherwise = 3

requiredModalLiftKappa :: Telescope -> Int
requiredModalLiftKappa _ = 4

requiredDifferentialLiftKappa :: Telescope -> Int
requiredDifferentialLiftKappa _ = 5

requiredCurvatureLiftKappa :: Telescope -> Int
requiredCurvatureLiftKappa _ = 6

requiredTemporalLiftKappa :: Telescope -> Int
requiredTemporalLiftKappa _ = 8

isMapBridgeCandidate :: Library -> Telescope -> Bool
isMapBridgeCandidate lib tele =
  classifyTelescope tele lib == TCMap
  && desugaredKappa tele >= 4
  && desugaredKappa tele <= 4
  && teleReferencesLoopCarrier lib tele
  && mapBridgeCoverageScore lib tele >= 3
  && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)

mapBridgeQualityScore :: Library -> Telescope -> Int
mapBridgeQualityScore lib tele
  | not (isMapBridgeCandidate lib tele) = 0
  | otherwise =
      canonicalScore + coverageScore + coherenceScore + interactionScore + kappaScore
  where
    isHopf = detectCanonicalName tele lib == "Hopf"
    coverageScore = mapBridgeCoverageScore lib tele
    canonicalScore = if isHopf then 3 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    kappaScore = if desugaredKappa tele == 4 then 2 else 0

mapBridgePenalty :: Library -> Telescope -> Int -> Double
mapBridgePenalty lib tele kappa
  | not (isMapBridgeCandidate lib tele) = 1.0
  | otherwise = kappaPenalty + coveragePenalty + coherencePenalty + hopfReward
  where
    isHopf = detectCanonicalName tele lib == "Hopf"
    coverage = mapBridgeCoverageScore lib tele
    kappaPenalty = if kappa /= 4 then 1.0 else 0.0
    coveragePenalty = if coverage >= 3 then 0.0 else 0.9
    coherencePenalty = if teleHasCoherenceExpr tele then 0.0 else 0.3
    hopfReward = if isHopf then (-0.30) else 0.0

mapBridgeCoverageScore :: Library -> Telescope -> Int
mapBridgeCoverageScore lib tele =
  sourceScore + targetScore + fiberScore
  where
    refs = Set.toList (teleLibRefs tele)
    hasDim d =
      any (\i -> i >= 1 && i <= length lib && d `elem` lePathDims (lib !! (i - 1))) refs
    sourceScore = if hasDim 3 then 1 else 0
    targetScore = if hasDim 2 then 1 else 0
    fiberScore = if hasDim 1 then 1 else 0

isModalLiftCandidate :: Library -> Telescope -> Bool
isModalLiftCandidate lib tele =
  desugaredKappa tele >= requiredModalLiftKappa tele
  && teleModalCount tele >= 3
  && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele || teleModalCount tele >= 4)
  && (teleReferencesLoopCarrier lib tele || teleReferencesModalCarrier lib tele || detectCanonicalName tele lib == "Cohesion")

modalLiftQualityScore :: Library -> Telescope -> Int
modalLiftQualityScore lib tele
  | not (isModalLiftCandidate lib tele) = 0
  | otherwise =
      canonicalScore + modalRichScore + loopRefScore + coherenceScore + interactionScore + classScore + kappaScore
  where
    cls = classifyTelescope tele lib
    isCohesion = detectCanonicalName tele lib == "Cohesion"
    modalRich = teleModalCount tele
    referencesLoop = teleReferencesLoopCarrier lib tele
    canonicalScore = if isCohesion then 3 else 0
    modalRichScore = max 0 (modalRich - 2)
    loopRefScore = if referencesLoop then 2 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    classScore = if cls == TCModal then 1 else 0
    kappaScore = if desugaredKappa tele >= requiredModalLiftKappa tele then 2 else 0

modalLiftPenalty :: Library -> Telescope -> Int -> Double
modalLiftPenalty lib tele kappa
  | not (isModalLiftCandidate lib tele) = 1.0
  | otherwise = mapPenalty + kappaPenalty + noLoopPenalty + noRichPenalty + cohesionReward
  where
    cls = classifyTelescope tele lib
    isCohesion = detectCanonicalName tele lib == "Cohesion"
    referencesLoop = teleReferencesLoopCarrier lib tele
    modalRich = teleModalCount tele
    mapPenalty = if cls == TCMap then 0.35 else 0.0
    kappaPenalty = if kappa < requiredModalLiftKappa tele then 0.70 else 0.0
    noLoopPenalty = if referencesLoop then 0.0 else 0.40
    noRichPenalty = if modalRich >= 3 then 0.0 else 0.80
    cohesionReward = if isCohesion then (-0.25) else 0.0

isDifferentialLiftCandidate :: Library -> Telescope -> Bool
isDifferentialLiftCandidate lib tele =
  teleReferencesModalCarrier lib tele
  && desugaredKappa tele >= requiredDifferentialLiftKappa tele
  && teleHasDifferentialBundleEvidence tele

differentialLiftQualityScore :: Library -> Telescope -> Int
differentialLiftQualityScore lib tele
  | not (isDifferentialLiftCandidate lib tele) = 0
  | otherwise =
      connectionScore + modalRefScore + formerScore + coherenceScore + interactionScore + classScore + kappaScore
  where
    cls = classifyTelescope tele lib
    isConnections = detectCanonicalName tele lib == "Connections"
    hasModalRef = teleReferencesModalCarrier lib tele
    connectionScore = if isConnections then 2 else 0
    modalRefScore = if hasModalRef then 2 else 0
    formerScore = if teleHasDifferentialFormer tele then 1 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    classScore = if cls == TCAxiomatic then 1 else 0
    kappaScore = if desugaredKappa tele >= requiredDifferentialLiftKappa tele then 2 else 0

differentialLiftPenalty :: Library -> Telescope -> Int -> Double
differentialLiftPenalty lib tele kappa
  | not (isDifferentialLiftCandidate lib tele) = 1.0
  | otherwise = mapPenalty + modalPenalty + kappaPenalty + connectionReward
  where
    cls = classifyTelescope tele lib
    isConnections = detectCanonicalName tele lib == "Connections"
    mapPenalty = if cls == TCMap then 0.35 else 0.0
    modalPenalty = if cls == TCModal then 0.80 else 0.0
    kappaPenalty = if kappa < requiredDifferentialLiftKappa tele then 0.95 else 0.0
    connectionReward = if isConnections then (-0.20) else 0.0

isCurvatureLiftCandidate :: Library -> Telescope -> Bool
isCurvatureLiftCandidate lib tele =
  teleReferencesDifferentialCarrier lib tele
  && desugaredKappa tele >= requiredCurvatureLiftKappa tele
  && (teleHasSurfaceEvidence tele || teleHasCurvatureBundleEvidence tele)
  && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)

curvatureLiftQualityScore :: Library -> Telescope -> Int
curvatureLiftQualityScore lib tele
  | not (isCurvatureLiftCandidate lib tele) = 0
  | otherwise =
      canonicalScore + diffRefScore + surfaceScore + coherenceScore + interactionScore + classScore + kappaScore
  where
    cls = classifyTelescope tele lib
    isCurvature = detectCanonicalName tele lib == "Curvature"
    hasDiffRef = teleReferencesDifferentialCarrier lib tele
    hasSurface = teleHasSurfaceEvidence tele
    canonicalScore = if isCurvature then 2 else 0
    diffRefScore = if hasDiffRef then 2 else 0
    surfaceScore = if hasSurface then 2 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    classScore = if cls == TCAxiomatic then 1 else 0
    kappaScore = if desugaredKappa tele >= requiredCurvatureLiftKappa tele then 2 else 0

curvatureLiftPenalty :: Library -> Telescope -> Int -> Double
curvatureLiftPenalty lib tele kappa
  | not (isCurvatureLiftCandidate lib tele) = 1.0
  | otherwise = mapPenalty + modalPenalty + kappaPenalty + noSurfacePenalty + curvatureReward
  where
    cls = classifyTelescope tele lib
    isCurvature = detectCanonicalName tele lib == "Curvature"
    hasSurface = teleHasSurfaceEvidence tele
    mapPenalty = if cls == TCMap then 0.35 else 0.0
    modalPenalty = if cls == TCModal then 0.80 else 0.0
    kappaPenalty = if kappa < requiredCurvatureLiftKappa tele then 1.10 else 0.0
    noSurfacePenalty = if hasSurface then 0.0 else 0.60
    curvatureReward = if isCurvature then (-0.20) else 0.0

isMetricLiftCandidate :: Library -> Telescope -> Bool
isMetricLiftCandidate lib tele =
  teleReferencesCurvatureCarrier lib tele
  && desugaredKappa tele >= 7
  && teleHasMetricBundleEvidence tele
  && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)

metricLiftQualityScore :: Library -> Telescope -> Int
metricLiftQualityScore lib tele
  | not (isMetricLiftCandidate lib tele) = 0
  | otherwise =
      canonicalScore + curvRefScore + bundleScore + coherenceScore + interactionScore + classScore + kappaScore
  where
    cls = classifyTelescope tele lib
    isMetric = detectCanonicalName tele lib == "Metric"
    hasCurvRef = teleReferencesCurvatureCarrier lib tele
    hasBundle = teleHasMetricBundleEvidence tele
    canonicalScore = if isMetric then 3 else 0
    curvRefScore = if hasCurvRef then 2 else 0
    bundleScore = if hasBundle then 3 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    classScore = if cls == TCAxiomatic then 1 else 0
    kappaScore = if desugaredKappa tele >= 7 then 2 else 0

metricLiftPenalty :: Library -> Telescope -> Int -> Double
metricLiftPenalty lib tele kappa
  | not (isMetricLiftCandidate lib tele) = 1.0
  | otherwise = mapPenalty + modalPenalty + kappaPenalty + noBundlePenalty + metricReward
  where
    cls = classifyTelescope tele lib
    isMetric = detectCanonicalName tele lib == "Metric"
    hasBundle = teleHasMetricBundleEvidence tele
    mapPenalty = if cls == TCMap then 0.35 else 0.0
    modalPenalty = if cls == TCModal then 0.80 else 0.0
    kappaPenalty = if kappa < 7 then 0.85 else 0.0
    noBundlePenalty = if hasBundle then 0.0 else 0.90
    metricReward = if isMetric then (-0.25) else 0.0

isHilbertLiftCandidate :: Library -> Telescope -> Bool
isHilbertLiftCandidate lib tele =
  teleReferencesMetricCarrier lib tele
  && teleReferencesCurvatureCarrier lib tele
  && teleReferencesDifferentialCarrier lib tele
  && desugaredKappa tele >= 9
  && teleHasHilbertBundleEvidence tele
  && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)

hilbertLiftQualityScore :: Library -> Telescope -> Int
hilbertLiftQualityScore lib tele
  | not (isHilbertLiftCandidate lib tele) = 0
  | otherwise =
      canonicalScore + metricRefScore + curvatureRefScore + differentialRefScore
      + bundleScore + functionalScore + coherenceScore + interactionScore + classScore + kappaScore
  where
    cls = classifyTelescope tele lib
    isHilbert = detectCanonicalName tele lib == "Hilbert"
    hasMetricRef = teleReferencesMetricCarrier lib tele
    hasCurvatureRef = teleReferencesCurvatureCarrier lib tele
    hasDifferentialRef = teleReferencesDifferentialCarrier lib tele
    hasBundle = teleHasHilbertBundleEvidence tele
    hasFunctional = teleHasFunctionalDerivativeEvidence tele
    canonicalScore = if isHilbert then 4 else 0
    metricRefScore = if hasMetricRef then 3 else 0
    curvatureRefScore = if hasCurvatureRef then 2 else 0
    differentialRefScore = if hasDifferentialRef then 2 else 0
    bundleScore = if hasBundle then 6 else 0
    functionalScore = if hasFunctional then 2 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    classScore = if cls == TCAxiomatic then 1 else 0
    kappaScore = if desugaredKappa tele >= 9 then 2 else 0

hilbertLiftPenalty :: Library -> Telescope -> Int -> Double
hilbertLiftPenalty lib tele kappa
  | not (isHilbertLiftCandidate lib tele) = 1.0
  | otherwise = mapPenalty + modalPenalty + kappaPenalty + noBundlePenalty + noFunctionalPenalty + noCrossPenalty + hilbertReward
  where
    cls = classifyTelescope tele lib
    isHilbert = detectCanonicalName tele lib == "Hilbert"
    hasBundle = teleHasHilbertBundleEvidence tele
    hasFunctional = teleHasFunctionalDerivativeEvidence tele
    crossRefs =
      length
        [ ()
        | ok <- [ teleReferencesMetricCarrier lib tele
                , teleReferencesCurvatureCarrier lib tele
                , teleReferencesDifferentialCarrier lib tele
                ]
        , ok
        ]
    mapPenalty = if cls == TCMap then 0.35 else 0.0
    modalPenalty = if cls == TCModal then 0.80 else 0.0
    kappaPenalty = if kappa < 9 then 0.95 else 0.0
    noBundlePenalty = if hasBundle then 0.0 else 1.10
    noFunctionalPenalty = if hasFunctional then 0.0 else 0.70
    noCrossPenalty = if crossRefs >= 3 then 0.0 else 0.75
    hilbertReward = if isHilbert then (-0.35) else 0.0

isTemporalLiftCandidate :: Library -> Telescope -> Bool
isTemporalLiftCandidate lib tele =
  any leHasHilbert lib
  && teleReferencesModalCarrier lib tele
  && desugaredKappa tele >= requiredTemporalLiftKappa tele
  && teleHasTemporalOpsPair tele
  && teleTemporalCompatibilityScore tele >= 2
  && (teleHasPiSigma tele || teleHasBridgeInteraction tele || teleHasCoherenceExpr tele)

temporalLiftQualityScore :: Library -> Telescope -> Int
temporalLiftQualityScore lib tele
  | not (isTemporalLiftCandidate lib tele) = 0
  | otherwise =
      canonicalScore + hilbertRefScore + modalRefScore + pairScore + compatScore
      + infShiftScore + distLawScore + polyScore + coherenceScore + interactionScore + classScore + kappaScore
  where
    cls = classifyTelescope tele lib
    isDCT = detectCanonicalName tele lib == "DCT"
    hasHilbertRef = teleReferencesHilbertCarrier lib tele
    hasModalRef = teleReferencesModalCarrier lib tele
    hasHilbertInLib = any leHasHilbert lib
    hasPair = teleHasTemporalOpsPair tele
    compat = teleTemporalCompatibilityScore tele
    hasInfShift = teleHasInfinitesimalShiftEvidence tele
    hasDistLaw = teleHasDistributiveTemporalLaw tele
    hasPoly = teleHasTemporalPolymorphism tele
    canonicalScore = if isDCT then 5 else 0
    hilbertRefScore = if hasHilbertRef then 3 else if hasHilbertInLib then 1 else 0
    modalRefScore = if hasModalRef then 2 else 0
    pairScore = if hasPair then 3 else 0
    compatScore = 2 * compat
    infShiftScore = if hasInfShift then 2 else 0
    distLawScore = if hasDistLaw then 3 else 0
    polyScore = if hasPoly then 2 else 0
    coherenceScore = if teleHasCoherenceExpr tele then 1 else 0
    interactionScore = if teleHasBridgeInteraction tele then 1 else 0
    classScore = if cls == TCSynthesis then 1 else 0
    kappaScore = if desugaredKappa tele >= requiredTemporalLiftKappa tele then 2 else 0

temporalLiftPenalty :: Library -> Telescope -> Int -> Double
temporalLiftPenalty lib tele kappa
  | not (isTemporalLiftCandidate lib tele) = 1.0
  | otherwise = mapPenalty + modalPenalty + kappaPenalty + noPairPenalty + noCompatPenalty + noInfShiftPenalty + noDistLawPenalty + noPolyPenalty + noCrossPenalty + dctReward
  where
    cls = classifyTelescope tele lib
    isDCT = detectCanonicalName tele lib == "DCT"
    hasPair = teleHasTemporalOpsPair tele
    compat = teleTemporalCompatibilityScore tele
    hasInfShift = teleHasInfinitesimalShiftEvidence tele
    hasDistLaw = teleHasDistributiveTemporalLaw tele
    hasPoly = teleHasTemporalPolymorphism tele
    crossRefs =
      length
        [ ()
        | ok <- [ teleReferencesHilbertCarrier lib tele
                , teleReferencesModalCarrier lib tele
                ]
        , ok
        ]
    mapPenalty = if cls == TCMap then 0.40 else 0.0
    modalPenalty = if cls == TCModal then 0.80 else 0.0
    kappaPenalty = if kappa < requiredTemporalLiftKappa tele then 1.0 else 0.0
    noPairPenalty = if hasPair then 0.0 else 1.0
    noCompatPenalty = if compat >= 2 then 0.0 else 1.2
    noInfShiftPenalty = if hasInfShift then 0.0 else 0.45
    noDistLawPenalty = if hasDistLaw then 0.0 else 1.8
    noPolyPenalty = if hasPoly then 0.0 else 1.2
    noCrossPenalty = if crossRefs >= 2 then 0.0 else 0.80
    dctReward = if isDCT then (-0.35) else 0.0

teleReferencesHilbertCarrier :: Library -> Telescope -> Bool
teleReferencesHilbertCarrier lib tele =
  any (\i -> i >= 1 && i <= length lib && leHasHilbert (lib !! (i - 1))) refs
  where
    refs = Set.toList (teleLibRefs tele)

teleReferencesModalCarrier :: Library -> Telescope -> Bool
teleReferencesModalCarrier lib tele =
  any (\i -> i >= 1 && i <= length lib && leHasModalOps (lib !! (i - 1))) refs
  where
    refs = Set.toList (teleLibRefs tele)

teleReferencesLoopCarrier :: Library -> Telescope -> Bool
teleReferencesLoopCarrier lib tele =
  any (\i -> i >= 1 && i <= length lib && leHasLoop (lib !! (i - 1))) refs
  where
    refs = Set.toList (teleLibRefs tele)

teleReferencesDifferentialCarrier :: Library -> Telescope -> Bool
teleReferencesDifferentialCarrier lib tele =
  any (\i -> i >= 1 && i <= length lib && leHasDifferentialOps (lib !! (i - 1))) refs
  where
    refs = Set.toList (teleLibRefs tele)

teleReferencesCurvatureCarrier :: Library -> Telescope -> Bool
teleReferencesCurvatureCarrier lib tele =
  any (\i -> i >= 1 && i <= length lib && leHasCurvature (lib !! (i - 1))) refs
  where
    refs = Set.toList (teleLibRefs tele)

teleReferencesMetricCarrier :: Library -> Telescope -> Bool
teleReferencesMetricCarrier lib tele =
  any (\i -> i >= 1 && i <= length lib && leHasMetric (lib !! (i - 1))) refs
  where
    refs = Set.toList (teleLibRefs tele)

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

postTruncLiftQualityScore :: Library -> Telescope -> Int
postTruncLiftQualityScore lib tele
  | not (isPostTruncLiftCandidate lib tele) = 0
  | otherwise =
      liftScore + coherenceScore + interactionScore + incrementalScore
  where
    libMaxDim = libraryMaxPathDim lib
    teleMaxDim = teleMaxPathDim tele
    hasLiftDelta = teleMaxDim > libMaxDim
    incremental = teleMaxDim == libMaxDim + 1
    hasCoherence = teleHasLiftCoherenceWitness tele
    hasInteraction = teleHasBridgeInteraction tele
    liftScore = if hasLiftDelta then 2 else 0
    coherenceScore = if hasCoherence then 1 else 0
    interactionScore = if hasInteraction then 1 else 0
    incrementalScore = if incremental then 1 else 0

postTruncLiftPenalty :: Library -> Telescope -> Int -> Double
postTruncLiftPenalty lib tele kappa
  | not (isPostTruncLiftCandidate lib tele) = 1.0
  | otherwise = residualPenalty + kappaPenalty + jumpPenalty + incrementalReward
  where
    libMaxDim = libraryMaxPathDim lib
    teleMaxDim' = teleMaxPathDim tele
    delta = max 0 (teleMaxDim' - libMaxDim)
    requiredK = requiredPostTruncLiftKappa lib tele
    residualPenalty = if isResidualPostTruncShortcut lib tele then 1.0 else 0.0
    kappaPenalty = if kappa < requiredK then 0.95 else 0.0
    jumpPenalty = if delta > 1 then 0.35 * fromIntegral (delta - 1) else 0.0
    incrementalReward = if delta == 1 then (-0.10) else 0.0

isPostTruncLiftCandidate :: Library -> Telescope -> Bool
isPostTruncLiftCandidate lib tele =
  postTruncLiftHasEvidence lib tele
  && desugaredKappa tele >= requiredPostTruncLiftKappa lib tele
  && teleHasConcreteTypeFormation tele
  && not (isResidualPostTruncShortcut lib tele)

postTruncLiftHasEvidence :: Library -> Telescope -> Bool
postTruncLiftHasEvidence lib tele =
  teleMaxPathDim tele > libraryMaxPathDim lib
  || teleHasLiftCoherenceWitness tele

isResidualPostTruncShortcut :: Library -> Telescope -> Bool
isResidualPostTruncShortcut lib tele =
  teleHasTrunc tele
  && teleMaxPathDim tele <= libraryMaxPathDim lib
  && not (teleHasLiftCoherenceWitness tele)

teleHasConcreteTypeFormation :: Telescope -> Bool
teleHasConcreteTypeFormation (Telescope entries) =
  any (go . teType) entries
  where
    go expr = case expr of
      Univ -> True
      App Univ _ -> True
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

libraryMaxPathDim :: Library -> Int
libraryMaxPathDim lib =
  let dims = concatMap lePathDims lib
  in if null dims then 0 else maximum dims

teleMaxPathDim :: Telescope -> Int
teleMaxPathDim tele =
  let dims = telePathDimensions tele
  in if null dims then 0 else maximum dims

truncBridgeQualityScore :: Library -> Telescope -> Int
truncBridgeQualityScore lib tele
  | not (teleHasTrunc tele) = 0
  | otherwise =
      kappaBand + loopRefScore + interactionScore + coherenceScore + pathScore + formerScore
  where
    libMaxDim = libraryMaxPathDim lib
    kappa = desugaredKappa tele
    refs = Set.toList (teleLibRefs tele)
    referencesLoop =
      any (\i -> i >= 1 && i <= length lib && leHasLoop (lib !! (i - 1))) refs
    dims = telePathDimensions tele
    hasPath = not (null dims)
    teleMaxDim = if hasPath then maximum dims else 0
    dimDelta = teleMaxDim - libMaxDim
    hasInteraction = teleHasBridgeInteraction tele
    hasCoherence = teleHasCoherenceExpr tele
    hasFormer = teleHasPiSigma tele
    kappaBand
      | kappa >= 3 = 4
      | kappa == 2 = 1
      | otherwise = 0
    loopRefScore = if referencesLoop then 2 else 0
    interactionScore = if hasInteraction then 2 else 0
    coherenceScore = if hasCoherence then 1 else 0
    pathScore
      | not hasPath = 1
      | dimDelta <= 0 = 2
      | dimDelta == 1 = 0
      | otherwise = -2 * dimDelta
    formerScore = if hasFormer then 1 else 0

truncBridgeShortcutPenalty :: Library -> Telescope -> Int -> Double
truncBridgeShortcutPenalty lib tele kappa
  | not (teleHasTrunc tele) = 0.0
  | kappa >= 3 && score >= truncBridgeRichThreshold = dimPenalty
  | score >= truncBridgeRichThreshold = dimPenalty
  | otherwise =
      let gap = truncBridgeRichThreshold - score
      in 0.35 + 0.15 * fromIntegral (max 0 gap) + dimPenalty
  where
    score = truncBridgeQualityScore lib tele
    delta = max 0 (teleMaxPathDim tele - libraryMaxPathDim lib)
    dimPenalty
      | delta <= 0 = 0.0
      | delta == 1 = 0.45
      | otherwise = 0.45 + 0.55 * fromIntegral (delta - 1)

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

teleHasLiftCoherenceWitness :: Telescope -> Bool
teleHasLiftCoherenceWitness (Telescope entries) = any (go . teType) entries
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

teleHasDifferentialBundleEvidence :: Telescope -> Bool
teleHasDifferentialBundleEvidence (Telescope entries) =
  teleHasDifferentialFormer tele
    && hasTransportLike
    && hasModalAction
  where
    tele = Telescope entries
    exprs = map teType entries
    hasTransportLike = any isTransportLike exprs
    hasModalAction = any isModalAction exprs

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

teleHasDifferentialFormer :: Telescope -> Bool
teleHasDifferentialFormer (Telescope entries) = any (isConnectionFormer . teType) entries
  where
    isConnectionFormer expr = case expr of
      Pi _ (Pi _ _) -> True
      _ -> False

teleHasMetricBundleEvidence :: Telescope -> Bool
teleHasMetricBundleEvidence (Telescope entries) =
  hasGForm && hasConnLike && hasCurvLike
  where
    exprs = map teType entries
    hasGForm = any hasSymmetricBilinear exprs
    hasConnLike = any hasConnectionLink exprs
    hasCurvLike = any hasCurvatureLink exprs

    hasSymmetricBilinear expr = case expr of
      Sigma (Pi _ _) (Pi _ _) -> True
      Lam a -> hasSymmetricBilinear a
      App a b -> hasSymmetricBilinear a || hasSymmetricBilinear b
      Pi a b -> hasSymmetricBilinear a || hasSymmetricBilinear b
      Id a x y -> hasSymmetricBilinear a || hasSymmetricBilinear x || hasSymmetricBilinear y
      Refl a -> hasSymmetricBilinear a
      _ -> False

    hasConnectionLink expr = case expr of
      Pi (Sigma _ _) (Lib _) -> True
      Pi _ (Pi _ _) -> True
      Lam (Pi _ _) -> True
      App (Lib _) _ -> True
      Pi a b -> hasConnectionLink a || hasConnectionLink b
      Lam a -> hasConnectionLink a
      App a b -> hasConnectionLink a || hasConnectionLink b
      Id a x y -> hasConnectionLink a || hasConnectionLink x || hasConnectionLink y
      Refl a -> hasConnectionLink a
      _ -> False

    hasCurvatureLink expr = case expr of
      Pi (Lib _) (Lib _) -> True
      Pi (Lib _) (Var _) -> True
      Pi a b -> hasCurvatureLink a || hasCurvatureLink b
      Lam a -> hasCurvatureLink a
      App a b -> hasCurvatureLink a || hasCurvatureLink b
      Id a x y -> hasCurvatureLink a || hasCurvatureLink x || hasCurvatureLink y
      Refl a -> hasCurvatureLink a
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
        , if any isCurvatureOp exprs then 1 else 0
        , if any isConnectionOp exprs then 1 else 0
        , if any isFunctionalDerivative exprs then 1 else 0
        ]
    refs = nub (concatMap exprRefs exprs)
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

    isCurvatureOp expr = case expr of
      Pi (Lib _) (Lib _) -> True
      Pi (Lib _) (Var _) -> True
      Pi _ (Lib _) -> True
      Pi a b -> isCurvatureOp a || isCurvatureOp b
      Lam a -> isCurvatureOp a
      App a b -> isCurvatureOp a || isCurvatureOp b
      Id a x y -> isCurvatureOp a || isCurvatureOp x || isCurvatureOp y
      Refl a -> isCurvatureOp a
      _ -> False

    isConnectionOp expr = case expr of
      Pi (Lib _) (Lib _) -> True
      Pi (Lib _) (Var _) -> True
      Pi _ (Lib _) -> True
      Pi a b -> isConnectionOp a || isConnectionOp b
      Lam a -> isConnectionOp a
      App a b -> isConnectionOp a || isConnectionOp b
      Id a x y -> isConnectionOp a || isConnectionOp x || isConnectionOp y
      Refl a -> isConnectionOp a
      _ -> False

    isFunctionalDerivative expr = case expr of
      Lam (Pi _ Univ) -> True
      Pi _ b -> isFunctionalDerivative b
      Lam a -> isFunctionalDerivative a
      App a b -> isFunctionalDerivative a || isFunctionalDerivative b
      Id a x y -> isFunctionalDerivative a || isFunctionalDerivative x || isFunctionalDerivative y
      Refl a -> isFunctionalDerivative a
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

teleHasDistributiveTemporalLaw :: Telescope -> Bool
teleHasDistributiveTemporalLaw (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      Pi lhs rhs -> isModalTemporal lhs && isTemporalModal rhs
                 || isTemporalModal lhs && isModalTemporal rhs
                 || go lhs || go rhs
      Lam a -> go a
      App a b -> go a || go b
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

    isModalTemporal e = case e of
      Flat (Next _) -> True
      Flat (Eventually _) -> True
      Sharp (Next _) -> True
      Sharp (Eventually _) -> True
      Disc (Next _) -> True
      Disc (Eventually _) -> True
      Shape (Next _) -> True
      Shape (Eventually _) -> True
      _ -> False

    isTemporalModal e = case e of
      Next (Flat _) -> True
      Next (Sharp _) -> True
      Next (Disc _) -> True
      Next (Shape _) -> True
      Eventually (Flat _) -> True
      Eventually (Sharp _) -> True
      Eventually (Disc _) -> True
      Eventually (Shape _) -> True
      _ -> False

teleHasTemporalPolymorphism :: Telescope -> Bool
teleHasTemporalPolymorphism (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      Lam (App (Eventually (Var _)) _) -> True
      Pi (Next (Next (Var _))) (Next (Var _)) -> True
      Pi (Next (Var _)) (Eventually (Var _)) -> True
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

teleHasLam :: Telescope -> Bool
teleHasLam (Telescope entries) = any (go . teType) entries
  where
    go expr = case expr of
      Lam _ -> True
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

teleHasTrunc :: Telescope -> Bool
teleHasTrunc (Telescope entries) = any (exprHasTrunc . teType) entries

exprHasTrunc :: MBTTExpr -> Bool
exprHasTrunc expr = case expr of
  Trunc _ -> True
  Lam a -> exprHasTrunc a
  App a b -> exprHasTrunc a || exprHasTrunc b
  Pi a b -> exprHasTrunc a || exprHasTrunc b
  Sigma a b -> exprHasTrunc a || exprHasTrunc b
  Id a x y -> exprHasTrunc a || exprHasTrunc x || exprHasTrunc y
  Refl a -> exprHasTrunc a
  Susp a -> exprHasTrunc a
  Flat a -> exprHasTrunc a
  Sharp a -> exprHasTrunc a
  Disc a -> exprHasTrunc a
  Shape a -> exprHasTrunc a
  Next a -> exprHasTrunc a
  Eventually a -> exprHasTrunc a
  _ -> False

prefixReportHeader :: String
prefixReportHeader = intercalate ","
  [ "step"
  , "status"
  , "bar"
  , "raw_candidates"
  , "viable_candidates"
  , "guidance_mode"
  , "admissibility_kappa_cap"
  , "searched_kappa_bands"
  , "selected_kappa_band"
  , "step_budget_s"
  , "elapsed_s"
  , "evaluated_candidates"
  , "deduped_candidates"
  , "minimality_rejections"
  , "best_obligation_score"
  , "best_interface_density"
  , "best_genericity_score"
  , "best_closure_score"
  , "frontier_candidates"
  , "frontier_kept"
  , "mcts_seed_states"
  , "mcts_completed_states"
  , "retry_tier"
  , "retry_cause"
  , "required_family"
  , "best_family_rho"
  , "best_family_kappa"
  , "family_candidate_count"
  , "family_viable_count"
  , "selected_name"
  , "selected_nu"
  , "selected_kappa"
  , "selected_rho"
  , "selected_source"
  , "runner_1"
  , "runner_2"
  , "runner_3"
  , "agenda_expanded"
  , "agenda_sigma_prunes"
  , "agenda_dominance_prunes"
  , "agenda_critic_transitions"
  , "agenda_action_failures"
  , "agenda_near_misses"
  ] ++ "\n"

emitPrefixReportRow
  :: AbInitioConfig
  -> Library
  -> Int
  -> Double
  -> Int
  -> Int
  -> PrefixDiagnostics
  -> Maybe Candidate
  -> [Candidate]
  -> AgendaDiagnostics
  -> String
  -> IO ()
emitPrefixReportRow cfg lib step bar rawCount viableCount prefixDiag selected runners agendaDiag status =
  case cfgPrefixReport cfg of
    Nothing -> return ()
    Just path ->
      let mkRunnerField cand =
            let (tele, nu, kappa, rho, src) = cand
                name = detectCanonicalName tele lib
            in csvEscape (intercalate "|" [name, show nu, show kappa, printf' "%.4f" rho, src])
          runnerFields = take 3 (map mkRunnerField runners ++ repeat "")
          (selName, selNu, selKappa, selRho, selSource) =
            case selected of
              Just (tele, nu, kappa, rho, src) ->
                ( detectCanonicalName tele lib
                , show nu
                , show kappa
                , printf' "%.4f" rho
                , src
                )
              Nothing -> ("", "", "", "", "")
          row = intercalate ","
            [ show step
            , csvEscape status
            , printf' "%.4f" bar
            , show rawCount
            , show viableCount
            , csvEscape (pdGuidanceMode prefixDiag)
            , maybe "" show (pdAdmissibilityKappaCap prefixDiag)
            , csvEscape (pdSearchedKappaBands prefixDiag)
            , maybe "" show (pdSelectedKappaBand prefixDiag)
            , maybe "" show (pdStepBudgetSeconds prefixDiag)
            , maybe "" (printf' "%.4f") (pdElapsedSeconds prefixDiag)
             , show (pdEvaluatedCandidates prefixDiag)
             , show (pdDedupedCandidates prefixDiag)
             , show (pdMinimalityRejections prefixDiag)
             , maybe "" show (pdBestObligationScore prefixDiag)
             , maybe "" show (pdBestInterfaceDensity prefixDiag)
             , maybe "" show (pdBestGenericityScore prefixDiag)
             , maybe "" show (pdBestClosureScore prefixDiag)
             , show (pdFrontierCandidates prefixDiag)
             , show (pdFrontierKept prefixDiag)
             , show (pdMCTSSeedStates prefixDiag)
             , show (pdMCTSCompletedStates prefixDiag)
             , show (pdRetryTier prefixDiag)
             , csvEscape (pdRetryCause prefixDiag)
            , csvEscape (pdRequiredFamily prefixDiag)
            , maybe "" (printf' "%.4f") (pdBestFamilyRho prefixDiag)
            , maybe "" show (pdBestFamilyKappa prefixDiag)
            , show (pdFamilyCandidates prefixDiag)
            , show (pdFamilyViable prefixDiag)
            , csvEscape selName
            , selNu
            , selKappa
            , selRho
            , csvEscape selSource
            , runnerFields !! 0
            , runnerFields !! 1
            , runnerFields !! 2
            , show (adExpandedStates agendaDiag)
            , show (adSigmaPrunes agendaDiag)
            , show (adDominancePrunes agendaDiag)
            , show (adCriticTransitions agendaDiag)
            , show (adActionFailures agendaDiag)
            , show (length (adNearMisses agendaDiag))
            ]
      in appendFile path (row ++ "\n")

csvEscape :: String -> String
csvEscape s
  | any (`elem` ",\"\n\r") s = "\"" ++ go s ++ "\""
  | otherwise = s
  where
    go [] = []
    go ('"':xs) = '"' : '"' : go xs
    go (x:xs) = x : go xs

emptyAgendaDiagnostics :: AgendaDiagnostics
emptyAgendaDiagnostics = AgendaDiagnostics
  { adExpandedStates = 0
  , adSigmaPrunes = 0
  , adDominancePrunes = 0
  , adCriticTransitions = 0
  , adActionFailures = 0
  , adNearMisses = []
  }

printAgendaDiagnostics :: Int -> AgendaDiagnostics -> IO ()
printAgendaDiagnostics step diag = do
  putStrLn "  [AGENDA DIAGNOSTICS]"
  printf "    step=%d expanded=%d sigma_prunes=%d dominance_prunes=%d critic_transitions=%d action_failures=%d\n"
    step
    (adExpandedStates diag)
    (adSigmaPrunes diag)
    (adDominancePrunes diag)
    (adCriticTransitions diag)
    (adActionFailures diag)
  let misses = take 5 (adNearMisses diag)
  if null misses
    then putStrLn "    near_miss: none"
    else do
      putStrLn "    near_miss_top5:"
      mapM_ printNearMiss misses

printNearMiss :: NearMiss -> IO ()
printNearMiss nm = do
  printf "      - reason=%s sigmaUB=%.2f priority=%.2f kappa=%d goals=%d critical=%s last_action=%s last_rule=%s\n"
    (nmReason nm)
    (nmSigmaUpper nm)
    (nmPriority nm)
    (nmKappa nm)
    (nmGoalCount nm)
    (maybe "-" show (nmCriticalGoal nm))
    (maybe "-" show (nmLastAction nm))
    (maybe "-" show (nmLastRule nm))
  printf "        open=%s and=%s or=%s\n"
    (show (nmOpenGoals nm))
    (show (nmAndClauses nm))
    (show (nmOrClauses nm))
