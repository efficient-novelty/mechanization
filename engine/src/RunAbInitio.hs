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
-- Three modes:
--   StrictAbInitio      — default discovery mode (paper-independent)
--   StructuralAbInitio  — structural AST ν decomposition mode
--   PaperCalibrated     — explicit benchmark/replay mode only
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
import TelescopeGen (enumerateTelescopes, GoalProfile(..), GoalIntent(..), deriveGoalProfile)
import MBTTEnum (enumerateMBTTTelescopes, defaultEnumConfig, MBTTCandidate(..), EnumConfig(..))
import AgendaSearch (agendaGenerateCandidates, defaultAgendaConfig, AgendaConfig(..), AgendaCandidate(..))
import MBTTCanonical (CanonKey(..), canonicalKeySpec)
import MBTTDecode (decodeCanonicalNameWithKey, DecodeResult(..))
import TelescopeEval (EvalMode(..), KappaMode(..), evaluateTelescopeWithHistory,
                      telescopeToCandidate,
                      validateReferenceTelescopes, detectCanonicalName)
import TelescopeCheck (checkAndFilter)
import MCTS
import UniformNu (genesisLibrarySteps, GenesisStep(..), computeUniformNu, UniformNuResult(..))
import Kolmogorov (MBTTExpr(..))
import Types (Library, LibraryEntry(..))
import CoherenceWindow (dBonacciDelta)

import Data.List (sortOn)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (when)
import System.IO (hFlush, stdout, stderr, hSetEncoding, utf8)
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import GHC.Stats (getRTSStatsEnabled, getRTSStats, gc, gcdetails_live_bytes, gcdetails_mem_in_use_bytes, max_mem_in_use_bytes)
import Text.Printf (printf)

-- ============================================
-- Mode Selection
-- ============================================

-- | Ab initio synthesis mode.
data AbInitioMode
  = PaperCalibrated   -- ^ Bar from paper ν/κ (benchmark/replay only)
  | StrictAbInitio    -- ^ Bar from discovered ν/κ only, no paper fallback
  | StructuralAbInitio -- ^ StructuralNu: AST rule extraction, no semantic proxy
  deriving (Show, Eq)

-- | Configuration for the ab initio run.
data AbInitioConfig = AbInitioConfig
  { cfgMode            :: !AbInitioMode
  , cfgWindow          :: !Int              -- ^ Coherence window depth d (default 2 = Fibonacci)
  , cfgCsv             :: !(Maybe FilePath) -- ^ Optional CSV output file
  , cfgKappaMode       :: !KappaMode        -- ^ Kappa computation mode (default DesugaredKappa)
  , cfgNoCanonPriority :: !Bool             -- ^ Ablation: disable canonical name priority in selection
  , cfgMaxRho          :: !Bool             -- ^ Ablation: select max ρ instead of minimal overshoot
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

-- | Convert synthesis mode to evaluation mode.
-- PaperCalibrated uses paper ν/κ for canonical names (effectiveNu/effectiveKappa).
-- StrictAbInitio never reads paper tables — all ν/κ computed from telescope + library.
-- StructuralAbInitio uses StructuralNu AST rule extraction.
toEvalMode :: AbInitioMode -> EvalMode
toEvalMode PaperCalibrated    = EvalPaperCalibrated
toEvalMode StrictAbInitio     = EvalStrictComputed
toEvalMode StructuralAbInitio = EvalStructural

-- ============================================
-- Main Entry Point
-- ============================================

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  let cfg = parseArgs args

  when (cfgNoCanonPriority cfg || cfgMaxRho cfg) $
    error "Ablation rankers (--no-canonical-priority, --max-rho) are disabled in claim-grade discovery."

  putStrLn "============================================"
  putStrLn "PEN Ab Initio Discovery Engine"
  printf   "Mode: %s, d=%d\n" (show (cfgMode cfg)) (cfgWindow cfg)
  putStrLn "============================================"
  case cfgMode cfg of
    PaperCalibrated -> do
      putStrLn "  Evaluator: EvalPaperCalibrated (effectiveNu/effectiveKappa for canonical names)"
      putStrLn "  Bar:       Paper nu/kappa history"
      putStrLn "  Library:   Discovered entries only"
      putStrLn "  NOTE:      Benchmark mode only (not claim-grade discovery)"
    StrictAbInitio -> do
      putStrLn "  Evaluator: EvalStrictComputed (computeUniformNu + strictKappa, zero paper tables)"
      putStrLn "  Bar:       Discovered nu/kappa history only"
      putStrLn "  Library:   Discovered entries only, no fallback"
    StructuralAbInitio -> do
      putStrLn "  Evaluator: EvalStructural (StructuralNu AST rule extraction)"
      putStrLn "  Bar:       Discovered nu/kappa history only"
      putStrLn "  Library:   Discovered entries only, no fallback"
      putStrLn "  Features:  3-component decomposition (v_G + v_H + v_C)"
      putStrLn "             Meta-theorem multipliers for DCT (Big Bang)"
      putStrLn "  PAPER-INDEPENDENCE: Zero paper nu/kappa lookups in evaluation,"
      putStrLn "    bar computation, MCTS rollout guidance, or library insertion."
      putStrLn "    All scores derive from AST analysis of discovered telescopes."
  printf   "  Window:    d=%d (%s)\n" (cfgWindow cfg) (windowName (cfgWindow cfg))
  printf   "  Kappa:     %s\n" (show (cfgKappaMode cfg))
  when (cfgMBTTFirst cfg) $
    putStrLn "  SEARCH:    MBTT-first default active (typed MBTT enumeration in Phase A)"
  when (cfgLegacyGenerator cfg) $
    putStrLn "  FALLBACK:  --legacy-generator (deprecated template-first generator path)"
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
  let mode = if "--structural" `elem` args then StructuralAbInitio
             else if "--strict" `elem` args then StrictAbInitio
             else if "--paper" `elem` args || "--paper-calibrated-benchmark" `elem` args
                  then PaperCalibrated
                  else StrictAbInitio
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
      mbttMaxCandFinal = if phase1Shadow
                         then case mbttMaxCand of
                           Just k  -> Just k
                           Nothing -> Just 20
                         else mbttMaxCand
  in AbInitioConfig mode window csv kappaMode noCanonPriority maxRho mbttFirstFinal legacyGenerator mbttMaxCandFinal mbttAstDepth maxStepsFinal skipValidation mbttShadowProfile skipMCTS phase1Shadow noCanonicalQuotient adaptiveMemory memorySafe

-- | Human-readable name for window depth.
windowName :: Int -> String
windowName 1 = "constant — extensional"
windowName 2 = "Fibonacci — intensional HoTT"
windowName 3 = "tribonacci"
windowName d = show d ++ "-bonacci"

-- ============================================
-- Reference Telescope Validation
-- ============================================

validatePhase :: IO ()
validatePhase = do
  let results = validateReferenceTelescopes 2
  printf "%-4s %-14s %6s %6s %6s %s\n"
    ("Step" :: String) ("Name" :: String)
    ("ν_pap" :: String) ("ν_tel" :: String) ("κ" :: String)
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
      in printf "%-4d %-14s %6d %6d %6d %s\n" step name paperNu teleNu kappa status

-- ============================================
-- Ab Initio Synthesis Loop
-- ============================================

abInitioLoop :: AbInitioConfig -> IO ()
abInitioLoop cfg = do
  -- Header
  printf "%-4s %-16s %5s %5s %8s %8s %8s  %-8s %s\n"
    ("Step" :: String) ("Discovery" :: String)
    ("ν" :: String) ("κ" :: String)
    ("ρ" :: String) ("Bar" :: String) ("Δ_n" :: String)
    ("Source" :: String) ("Candidates" :: String)
  putStrLn (replicate 90 '-')

  -- Pure ab initio: build library from discovered telescopes only
  go [] [] [] 1

  where
    mode = cfgMode cfg
    go :: Library -> [DiscoveryRecord] -> [StepRecord] -> Int -> IO ()
    go lib history records step
      | step > cfgMaxSteps cfg = do
          let orderedRecords = reverse records
          putStrLn ""
          putStrLn "============================================"
          printf   "SYNTHESIS COMPLETE: %d structures discovered\n" (cfgMaxSteps cfg)
          putStrLn "============================================"
          putStrLn ""
          -- Print summary comparing discovered vs paper
          printSummary history
          -- Post-hoc analysis: compare StructuralNu vs UniformNu
          when (mode == StructuralAbInitio) $ do
            postHocAnalysis orderedRecords
            -- Print claim profile summary
            putStrLn ""
            putStrLn "--- Claim Profile (Publication-Grade) ---"
            putStrLn "  Mode:     EvalStructural (--structural)"
            putStrLn "  Nu:       StructuralNu — AST rule extraction (v_G + v_H + v_C)"
            putStrLn "  Kappa:    DesugaredKappa — principled clause counting"
            putStrLn "  Bar:      Phi_n * Omega_{n-1}, discovered history only"
            putStrLn "  MCTS:     EvalStructural rollout guidance (zero paper lookups)"
            putStrLn "  Library:  Discovered entries only, no paper fallback"
            let totalDiscNu = sum [drNu r | r <- history]
                totalPapNu  = sum [gsPaperNu s | s <- take (cfgMaxSteps cfg) genesisLibrarySteps]
                totalDiscK  = sum [drKappa r | r <- history]
                totalPapK   = sum [gsPaperK s | s <- take (cfgMaxSteps cfg) genesisLibrarySteps]
                exact = length [() | (dr, gs) <- zip history (take (cfgMaxSteps cfg) genesisLibrarySteps)
                                   , drNu dr == gsPaperNu gs && drKappa dr == gsPaperK gs]
            printf "  Result:   %d/%d exact match, total nu %d/%d, total kappa %d/%d\n"
              exact (cfgMaxSteps cfg) totalDiscNu totalPapNu totalDiscK totalPapK
            -- Print exclusion contract
            printExclusionContract
          -- Write CSV if requested
          case cfgCsv cfg of
            Just csvPath -> writeCsv csvPath orderedRecords
            Nothing      -> return ()
      | otherwise = do
          let -- Compute selection bar
              d = cfgWindow cfg
              bar = computeBarD d mode step history
              delta_n = dBonacciDelta d step

          budget <- computeSearchBudget cfg step
          when (cfgAdaptiveMemory cfg || cfgMemorySafe cfg) $
            printSearchBudget step budget

          -- Phase A: Exhaustive enumeration for κ ≤ 3
          -- Type-check to filter ill-formed telescopes, then evaluate honestly
          let emode = toEvalMode mode
              goalProfile = deriveGoalProfile lib
              needsFormerLift = NeedFormer `elem` gpIntents goalProfile && length lib >= 3
              stepBitBudget = if needsFormerLift then max (sbBitBudget budget) 20 else sbBitBudget budget
              stepAstDepth = if needsFormerLift then max (sbAstDepth budget) 3 else sbAstDepth budget
              stepMaxCandidates = if needsFormerLift then max (sbMaxCandidates budget) 80 else sbMaxCandidates budget
              enumKmax = 3
              -- Depth-1 evaluation: count single-operation schemas only.
              -- Depth-2 causes O(formers²) explosion at later steps (L16).
              nuDepth = 1
              mbttCfg = defaultEnumConfig
                { ecMaxEntries = enumKmax
                , ecMaxBitBudget = stepBitBudget
                , ecMaxASTDepth = stepAstDepth
                , ecMaxCandidates = stepMaxCandidates
                , ecGoalProfile = Just goalProfile
                , ecEnableMacros = True
                }
              agendaCfg = defaultAgendaConfig
                { agMaxEntries = enumKmax
                , agMaxAgendaStates = max 80 (stepMaxCandidates * 8)
                , agBranchPerState = max 6 (min 18 (stepMaxCandidates `div` 2 + 4))
                , agMaxCandidates = max 10 stepMaxCandidates
                , agBitBudget = stepBitBudget
                , agRequireConnected = length lib >= 2
                }
              agendaActive = cfgMBTTFirst cfg && length lib >= 2
              agendaGenerated = if agendaActive
                                then agendaGenerateCandidates lib goalProfile agendaCfg
                                else []
              agendaTelescopes = [acTelescope c | c <- agendaGenerated]
              fallbackTelescopes = if cfgMBTTFirst cfg
                                   then map mcTelescope (enumerateMBTTTelescopes lib mbttCfg)
                                   else []
              rawTelescopes = if cfgMBTTFirst cfg
                              then if agendaActive
                                   then take (stepMaxCandidates * 2) (agendaTelescopes ++ fallbackTelescopes)
                                   else fallbackTelescopes
                              else enumerateTelescopes lib enumKmax
              (validTelescopesRaw, _rejected) = checkAndFilter lib rawTelescopes
              -- Phase-2 quotienting kickoff: deduplicate equivalent telescopes
              -- by canonical MBTT key before scoring.
              validTelescopes = dedupByCanonicalKey validTelescopesRaw
              semanticallyScored = [ (tele, semanticDeltaScore tele lib goalProfile)
                                   | tele <- validTelescopes
                                   ]
              semanticallyRelevant = [ (tele, s) | (tele, s) <- semanticallyScored, s > 0 ]
              semanticCap = max 8 (min 40 (stepMaxCandidates `div` 2))
              selectedBySemantic =
                if null semanticallyRelevant
                then validTelescopes
                else map fst (take semanticCap (sortOn (Down . snd) semanticallyRelevant))
              -- Build nuHistory for structural mode
              nuHist = zip [1..] (map drNu history)
              -- Evaluate each valid telescope using the mode-appropriate evaluator
              enumSource = if agendaActive then "AGENDA_MIX"
                           else if cfgMBTTFirst cfg then "ENUM_MBTT" else "ENUM"
              enumEvaluated = [ (tele, nu, kappa, rho, enumSource)
                              | tele <- selectedBySemantic
                              , let (nu, kappa, rho) = evaluateTelescopeWithHistory emode tele lib nuDepth "candidate" nuHist
                              , nu > 0
                              ]

          -- Phase B: MCTS for larger telescopes (κ > 3)
          -- Use a state-derived estimate from discovered history rather than
          -- step-indexed schedules.
          let enumViable = [ c | c@(_, _, _, rho, _) <- enumEvaluated, rho >= bar ]
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
                    , mctsSeed       = step * 137 + 42
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
          let rawCandidates = enumEvaluated ++ mctsCandidates
              viableRaw = [ c | c@(_, _, _, rho, _) <- rawCandidates, rho >= bar ]
              allCandidates = if cfgNoCanonicalQuotient cfg
                              then viableRaw
                              else quotientCandidates viableRaw
              rawCandidateCount = length rawCandidates
              canonicalCandidateCount = length allCandidates
              viable = allCandidates

          if null viable
            then do
              putStrLn ""
              printf "NO BAR-CLEARING CANDIDATE at step %d (bar=%.4f, raw=%d, viable=%d). Stopping.\n"
                step bar rawCandidateCount canonicalCandidateCount
              let orderedRecords = reverse records
              case cfgCsv cfg of
                Just csvPath -> writeCsv csvPath orderedRecords
                Nothing      -> return ()
            else do
              -- SELECTION: unified objective across all discovery modes.
              -- Rank by structural utility first (deficit/readiness aware),
              -- then minimal overshoot, then lower κ, then canonical key.
              let withUtility = [ (cand, semanticDeltaScore tele lib goalProfile)
                                | cand@(tele, _, _, _, _) <- viable
                                ]
                  candidateRank ((tele, _, kappa, rho, src), util) =
                    let CanonKey ckey = canonicalKeySpec (map teType (teleEntries tele))
                    in (Down util, rho - bar, kappa, ckey, sourceRank src)
                  sorted = sortOn candidateRank withUtility
                  (bestTele, bestNu, bestKappa, bestRho, bestSource, _bestUtility) = case sorted of
                    ((best, u):_) ->
                      let (bt, bn, bk, br, bs) = best
                      in (bt, bn, bk, br, bs, u)
                    [] -> error "internal error: viable candidate set became empty after ranking"
                  bestName = detectCanonicalName bestTele lib
                  totalCandidates = canonicalCandidateCount
                  dedupeRatio = if rawCandidateCount > 0
                                then fromIntegral canonicalCandidateCount / fromIntegral rawCandidateCount
                                else 1.0 :: Double
                  CanonKey bestCanonKey = canonicalKeySpec (map teType (teleEntries bestTele))

              -- Display
              printf "%-4d %-16s %5d %5d %8.2f %8.2f %8d  %-8s %d\n"
                step bestName bestNu bestKappa bestRho bar delta_n
                bestSource totalCandidates
              hFlush stdout

              -- Insert selected candidate only (no step-index fallback).
              let discoveredEntry = telescopeToCandidate bestTele lib bestName
                  newLib = lib ++ [discoveredEntry]
                  newHistory = history ++ [DiscoveryRecord bestNu bestKappa]
                  newRecord = StepRecord step bestName bestNu bestKappa
                                bestRho bar delta_n bestSource totalCandidates
                                rawCandidateCount canonicalCandidateCount dedupeRatio bestCanonKey bestTele

              go newLib newHistory (newRecord : records) (step + 1)

    printSummary :: [DiscoveryRecord] -> IO ()
    printSummary history = do
      putStrLn "Discovery vs Paper Comparison:"
      printf "%-4s %8s %8s %8s %8s\n"
        ("Step" :: String) ("disc_ν" :: String) ("pap_ν" :: String)
        ("disc_κ" :: String) ("pap_κ" :: String)
      putStrLn (replicate 44 '-')
      let n = length history
          paperSteps = take n genesisLibrarySteps
      mapM_ (\(i, dr, gs) ->
        printf "%-4d %8d %8d %8d %8d\n"
          i (drNu dr) (gsPaperNu gs) (drKappa dr) (gsPaperK gs)
        ) (myZip3 ([1..n] :: [Int]) history paperSteps)
      let totalDiscNu = sum (map drNu history)
          totalPapNu  = sum (map gsPaperNu paperSteps)
          totalDiscK  = sum (map drKappa history)
          totalPapK   = sum (map gsPaperK paperSteps)
      putStrLn (replicate 44 '-')
      printf "%-4s %8d %8d %8d %8d\n"
        ("SUM" :: String) totalDiscNu totalPapNu totalDiscK totalPapK

    -- | Post-hoc analysis: recompute UniformNu on the discovered sequence
    -- and display a side-by-side comparison with StructuralNu.
    --
    -- This runs AFTER selection is complete — UniformNu is never called
    -- during the selection loop in structural mode.
    postHocAnalysis :: [StepRecord] -> IO ()
    postHocAnalysis recs = do
      putStrLn ""
      putStrLn "Post-Hoc Analysis: StructuralNu vs UniformNu"
      putStrLn "(UniformNu computed AFTER selection — not used for discovery)"
      printf "%-4s %-14s %6s %6s %6s %10s\n"
        ("Step" :: String) ("Name" :: String)
        ("v_str" :: String) ("v_uni" :: String) ("k" :: String)
        ("Amplif." :: String)
      putStrLn (replicate 60 '-')
      postHocGo [] recs

    postHocGo :: Library -> [StepRecord] -> IO ()
    postHocGo _ [] = return ()
    postHocGo lib (r : rest) = do
      let tele = srTele r
          name = srName r
          entry = telescopeToCandidate tele lib name
          uniResult = computeUniformNu entry lib 1
          uniNu = unrUniformNu uniResult
          strNu = srNu r
          amplif = if strNu > 0
                   then fromIntegral uniNu / fromIntegral strNu :: Double
                   else 0.0 :: Double
      printf "%-4d %-14s %6d %6d %6d %10.2fx\n"
        (srStep r) name strNu uniNu (srKappa r) amplif
      postHocGo (lib ++ [entry]) rest

    myZip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
    myZip3 (a:as) (b:bs) (c:cs) = (a,b,c) : myZip3 as bs cs
    myZip3 _ _ _ = []

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
  writeFile path content
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
  | otherwise = reuseScore + pathScore + classScore + intentScore
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

classSupportsIntent :: TelescopeClass -> GoalIntent -> Bool
classSupportsIntent cls intent = case intent of
  NeedBootstrap -> cls == TCFoundation
  NeedFormer -> cls == TCFormer
  NeedHIT -> cls == TCHIT || cls == TCSuspension
  NeedModal -> cls == TCModal
  NeedDifferential -> cls == TCAxiomatic || cls == TCMap
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
      baseBitBudget = if shadowProfile then 14 else 16
      baseAstDepth = maybe 2 id (cfgMBTTAstDepth cfg)
      baseMaxCandidates = maybe (if shadowProfile then 800 else 800) id (cfgMBTTMaxCand cfg)
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
        PaperCalibrated ->
          let steps = take (n-1) genesisLibrarySteps
              sumNu = sum [gsPaperNu s | s <- steps]
              sumK  = sum [gsPaperK s | s <- steps]
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
        StructuralAbInitio ->
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
betterCandidate (_, _, k1, rho1, src1) (_, _, k2, rho2, src2) =
  (k1, Down rho1, sourceRank src1) < (k2, Down rho2, sourceRank src2)

-- | Prefer exhaustive enumeration over MCTS when ties occur.
sourceRank :: String -> Int
sourceRank src = case src of
  "AGENDA" -> 0
  "AGENDA_MIX" -> 1
  "ENUM_MBTT" -> 2
  "ENUM" -> 3
  "MCTS" -> 4
  _ -> 5
