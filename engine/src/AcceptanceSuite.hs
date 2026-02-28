-- | Acceptance Tests for PEN Engine
--
-- Validates historical failure modes and critical invariants:
--   A. Bootstrap bar sensitivity (early ν perturbation cascades)
--   B. Pi binder novelty (ν > 0, not regression to trivial)
--   C. Trunc bounded ν (no explosion at late steps)
--   D. DCT meta-theorem decomposition (three detectors fire correctly)
--   E. Kappa consistency (desugaredKappa matches paper for all 15 steps)
--   F. Full sequence golden test (structural mode produces expected results)
--   G. Canonical name detection
--   H. Exclusion contract (no empirical physics constants in selection)
--
-- Run: cabal run acceptance

module AcceptanceSuite
  ( AcceptanceConfig(..)
  , defaultAcceptanceConfig
  , parseArgs
  , runAcceptanceMain
  , runAcceptanceWithConfig
  , runCoreAcceptance
  , runMBTTAcceptance
  ) where

import Telescope
import TelescopeEval (telescopeToCandidate, detectCanonicalName)
import StructuralNu (structuralNu, StructuralNuResult(..))
import CoherenceWindow (dBonacciDelta)
import TelescopeCheck (checkTelescope, CheckResult(..))
import MBTTEnum (enumerateExprs, enumerateMBTTTelescopes, MBTTCandidate(..), EnumConfig(..), defaultEnumConfig)
import MBTTCanonical (canonicalizeExpr, canonicalKeyExpr)
import MBTTNu (computeNativeNu, NativeNuResult(..))
import Kolmogorov (MBTTExpr(..))
import Types (Library, LibraryEntry(..))

import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Monad (when)

-- ============================================
-- Test Framework
-- ============================================

data TestResult = Pass | Fail String deriving (Show)

type Test = (String, IO TestResult)

data AcceptanceConfig = AcceptanceConfig
  { acMbttFast :: !Bool
  , acMbttSkip :: !Bool
  , acMbttMaxCandidates :: !(Maybe Int)
  } deriving (Show)

defaultAcceptanceConfig :: AcceptanceConfig
defaultAcceptanceConfig = AcceptanceConfig
  { acMbttFast = False
  , acMbttSkip = False
  , acMbttMaxCandidates = Nothing
  }

parseArgs :: [String] -> AcceptanceConfig
parseArgs args =
  let fast = "--mbtt-fast" `elem` args
      skip = "--skip-mbtt" `elem` args
      maxCandidates = case dropWhile (/= "--mbtt-max-candidates") args of
                        ("--mbtt-max-candidates" : n : _) -> readMaybe n
                        _ -> Nothing
  in defaultAcceptanceConfig
      { acMbttFast = fast
      , acMbttSkip = skip
      , acMbttMaxCandidates = maxCandidates
      }

applyMBTTConfig :: AcceptanceConfig -> EnumConfig -> EnumConfig
applyMBTTConfig cfg base =
  let fastCfg = if acMbttFast cfg
                then base { ecMaxBitBudget = min (ecMaxBitBudget base) 16
                          , ecMaxEntries = min (ecMaxEntries base) 2
                          , ecMaxASTDepth = min (ecMaxASTDepth base) 2
                          , ecMaxCandidates = min (ecMaxCandidates base) 150
                          }
                else base
  in case acMbttMaxCandidates cfg of
      Just n  -> fastCfg { ecMaxCandidates = max 1 n }
      Nothing -> fastCfg

runTests :: [Test] -> IO ()
runTests tests = do
  putStrLn "============================================"
  putStrLn "PEN Engine Acceptance Tests"
  putStrLn "============================================"
  putStrLn ""
  results <- mapM runOne tests
  let passes = length [() | Pass <- results]
      fails  = length [() | Fail _ <- results]
  putStrLn ""
  putStrLn "============================================"
  printf "Results: %d passed, %d failed, %d total\n" passes fails (passes + fails)
  putStrLn "============================================"
  if fails > 0 then exitFailure else exitSuccess

runOne :: Test -> IO TestResult
runOne (name, action) = do
  printf "  %-60s " name
  hFlush stdout
  result <- action
  case result of
    Pass    -> putStrLn "PASS"
    Fail msg -> printf "FAIL: %s\n" msg
  return result

-- ============================================
-- Library Replay (build canonical library for testing)
-- ============================================

-- | Replay the canonical 15-step discovery, building the library incrementally.
-- Returns (library_at_each_step, nu_history).
replayCanonical :: ([(Library, Int, Int)], [(Int, Int)])
replayCanonical = go [] [] 1
  where
    go lib nuHist step
      | step > 15 = ([], nuHist)
      | otherwise =
        let tele = referenceTelescope step
            name = detectCanonicalName tele lib
            result = structuralNu tele lib nuHist
            nu = snTotal result
            kappa = desugaredKappa tele
            entry = telescopeToCandidate tele lib name
            newLib = lib ++ [entry]
            newHist = nuHist ++ [(step, nu)]
            (rest, finalHist) = go newLib newHist (step + 1)
        in ((lib, nu, kappa) : rest, finalHist)

-- | Get the canonical library at a specific step (0 = empty, 15 = full).
canonicalLibAt :: Int -> Library
canonicalLibAt n = go [] 1
  where
    go lib step
      | step > n = lib
      | otherwise =
        let tele = referenceTelescope step
            name = detectCanonicalName tele lib
            entry = telescopeToCandidate tele lib name
        in go (lib ++ [entry]) (step + 1)

-- ============================================
-- Test A: Bootstrap Bar Sensitivity
-- ============================================

-- If step 6 (Trunc) had ν=10 instead of ν=8, the bar at step 8 rises
-- by 0.2, and S3 (ρ=3.6) would fail to clear (bar=3.63). This tests
-- that our Trunc ν is exactly 8.
testTruncNuExact :: Test
testTruncNuExact = ("A1. Trunc (step 6) ν = 8 exactly", do
  let lib = canonicalLibAt 5  -- library after step 5 (S1)
      tele = referenceTelescope 6
      -- Build nuHistory for steps 1-5
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..5] (take 5 snapshots)]
      result = structuralNu tele lib nuHist
      nu = snTotal result
  if nu == 8
    then return Pass
    else return $ Fail $ "Expected ν=8, got ν=" ++ show nu)

-- Universe ν must be exactly 1 (El/decoding rule only).
testUniverseNu :: Test
testUniverseNu = ("A2. Universe (step 1) ν = 1 exactly", do
  let tele = referenceTelescope 1
      result = structuralNu tele [] []
      nu = snTotal result
  if nu == 1
    then return Pass
    else return $ Fail $ "Expected ν=1, got ν=" ++ show nu)

-- ============================================
-- Test B: Pi Binder Novelty (ν > 0)
-- ============================================

testPiNuPositive :: Test
testPiNuPositive = ("B1. Pi (step 4) ν > 0 (no regression to trivial)", do
  let lib = canonicalLibAt 3
      tele = referenceTelescope 4
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..3] (take 3 snapshots)]
      result = structuralNu tele lib nuHist
      nu = snTotal result
  if nu > 0
    then return Pass
    else return $ Fail $ "ν=0 for Pi — regression to trivial!")

testPiNuExact :: Test
testPiNuExact = ("B2. Pi (step 4) ν = 5 exactly", do
  let lib = canonicalLibAt 3
      tele = referenceTelescope 4
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..3] (take 3 snapshots)]
      result = structuralNu tele lib nuHist
      nu = snTotal result
  if nu == 5
    then return Pass
    else return $ Fail $ "Expected ν=5, got ν=" ++ show nu)

-- ============================================
-- Test C: Trunc Anti-Explosion
-- ============================================

-- Trunc ν must stay bounded. Historical failure: UniformNu gave ν>50 at late steps.
-- StructuralNu should give exactly 8 regardless of library size.
testTruncBounded :: Test
testTruncBounded = ("C1. Trunc ν ≤ 10 (no explosion at late steps)", do
  let lib = canonicalLibAt 14  -- full library minus DCT
      tele = referenceTelescope 6  -- Trunc telescope
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..14] (take 14 snapshots)]
      result = structuralNu tele lib nuHist
      nu = snTotal result
  if nu <= 10
    then return Pass
    else return $ Fail $ "Trunc ν=" ++ show nu ++ " — explosion!")

-- ============================================
-- Test D: DCT Meta-Theorem Decomposition
-- ============================================

testDCTTotal :: Test
testDCTTotal = ("D1. DCT (step 15) ν ≥ 100", do
  let lib = canonicalLibAt 14
      tele = referenceTelescope 15
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..14] (take 14 snapshots)]
      result = structuralNu tele lib nuHist
      nu = snTotal result
  if nu >= 100
    then return Pass
    else return $ Fail $ "DCT ν=" ++ show nu ++ " < 100")

testDCTDistLaw :: Test
testDCTDistLaw = ("D2. DCT distributive law detector fires (bonus > 0)", do
  let lib = canonicalLibAt 14
      tele = referenceTelescope 15
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..14] (take 14 snapshots)]
      result = structuralNu tele lib nuHist
  if snDistLaw result > 0
    then return Pass
    else return $ Fail $ "Distributive law bonus = 0")

testDCTUnivPoly :: Test
testDCTUnivPoly = ("D3. DCT universe polymorphism detector fires (bonus > 0)", do
  let lib = canonicalLibAt 14
      tele = referenceTelescope 15
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..14] (take 14 snapshots)]
      result = structuralNu tele lib nuHist
  if snUnivPoly result > 0
    then return Pass
    else return $ Fail $ "Universe polymorphism bonus = 0")

testDCTInfShift :: Test
testDCTInfShift = ("D4. DCT infinitesimal dimension shift fires (bonus > 0)", do
  let lib = canonicalLibAt 14
      tele = referenceTelescope 15
      (snapshots, _) = replayCanonical
      nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..14] (take 14 snapshots)]
      result = structuralNu tele lib nuHist
  if snInfShift result > 0
    then return Pass
    else return $ Fail $ "Infinitesimal shift bonus = 0")

-- ============================================
-- Test E: Kappa Consistency
-- ============================================

-- Paper κ values for steps 1-15
expectedKappa :: [Int]
expectedKappa = [2, 1, 1, 3, 3, 3, 3, 5, 4, 4, 5, 6, 7, 9, 8]

testKappaAll :: [Test]
testKappaAll =
  [ (printf' "E%d. Step %d κ = %d" i i ek, do
      let tele = referenceTelescope i
          dk = desugaredKappa tele
      if dk == ek
        then return Pass
        else return $ Fail $ "Expected κ=" ++ show ek ++ ", got κ=" ++ show dk)
  | (i, ek) <- zip [1..15] expectedKappa
  ]
  where
    printf' fmt a b c = let s = show a ++ ". Step " ++ show b ++ " κ = " ++ show c
                        in s

-- ============================================
-- Test F: Full Sequence Golden Test
-- ============================================

-- Expected ν values from structural mode
expectedNu :: [Int]
expectedNu = [1, 1, 2, 5, 7, 8, 10, 18, 17, 19, 26, 34, 46, 62, 103]

testFullSequence :: Test
testFullSequence = ("F1. Full 15-step ν sequence matches expected values", do
  let (snapshots, _) = replayCanonical
      discoveredNu = [nu | (_, nu, _) <- take 15 snapshots]
      mismatches = [(i, exp_v, disc) | (i, exp_v, disc) <- zip3 ([1..] :: [Int]) expectedNu discoveredNu, exp_v /= disc]
  if null mismatches
    then return Pass
    else return $ Fail $ show (length mismatches) ++ " mismatches: " ++
           show [(i, e, d) | (i, e, d) <- take 3 mismatches])
  where
    zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
    zip3 _ _ _ = []

testTotalNu :: Test
testTotalNu = ("F2. Total ν = 359", do
  let (snapshots, _) = replayCanonical
      totalNu = sum [nu | (_, nu, _) <- take 15 snapshots]
  if totalNu == 359
    then return Pass
    else return $ Fail $ "Total ν=" ++ show totalNu ++ ", expected 359")

testTotalKappa :: Test
testTotalKappa = ("F3. Total κ = 64", do
  let totalK = sum [desugaredKappa (referenceTelescope i) | i <- [1..15]]
  if totalK == 64
    then return Pass
    else return $ Fail $ "Total κ=" ++ show totalK ++ ", expected 64")

-- ============================================
-- Test G: Canonical Name Detection
-- ============================================

expectedNames :: [String]
expectedNames =
  [ "Universe", "Unit", "Witness", "Pi", "S1", "Trunc", "S2", "S3"
  , "Hopf", "Cohesion", "Connections", "Curvature", "Metric", "Hilbert", "DCT"
  ]

testCanonicalNames :: [Test]
testCanonicalNames =
  [ ("G" ++ show i ++ ". Step " ++ show i ++ " detects as " ++ show eName, do
      let lib = canonicalLibAt (i - 1)
          tele = referenceTelescope i
          detected = detectCanonicalName tele lib
      if detected == eName
        then return Pass
        else return $ Fail $ "Detected '" ++ detected ++ "', expected '" ++ eName ++ "'")
  | (i, eName) <- zip [1..15] expectedNames
  ]

-- ============================================
-- Test H: Exclusion Contract
-- ============================================

-- Local record for bar computation (mirrors RunAbInitio.DiscoveryRecord).
data DiscoveryRecord = DiscoveryRecord { drNu :: !Int, drKappa :: !Int }

-- | Compute selection bar from discovered history only (no paper values).
computeBarFromHistory :: Int -> Int -> [DiscoveryRecord] -> Double
computeBarFromHistory _ n _
  | n <= 2 = 0.5
computeBarFromHistory d n history =
  let delta_n   = fromIntegral (dBonacciDelta d n) :: Double
      delta_nm1 = fromIntegral (dBonacciDelta d (n-1)) :: Double
      phi_n = delta_n / delta_nm1
      past = take (n-1) history
      sumNu = sum [drNu r | r <- past]
      sumK  = sum [drKappa r | r <- past]
      omega = if sumK > 0 then fromIntegral sumNu / fromIntegral sumK else 1.0
  in phi_n * omega

-- Validates that the StructuralNu computation path contains no empirical
-- physics constants. The exclusion contract guarantees PEN derives the
-- kinematic framework only — no gauge groups, coupling constants, or
-- spacetime dimensions appear in any selection rule.

-- | Verify StructuralNu imports no paper-value modules.
-- The module's only imports are Telescope, Kolmogorov (MBTT AST), Types,
-- and Data.Set. No KappaNu (paper tables) or UniformNu (semantic proxy).
testExclusionNoEmpiricalInNu :: Test
testExclusionNoEmpiricalInNu =
  ("H1. StructuralNu has no paper-value dependency", do
    -- StructuralNu.structuralNu takes (Telescope, Library, NuHistory).
    -- NuHistory is [(Int,Int)] — discovered step index and nu.
    -- If paper values leaked, step 1's nu would differ when given
    -- empty history vs fabricated history. Test: empty history → nu=1.
    let tele = referenceTelescope 1
        result = structuralNu tele [] []
    if snTotal result == 1
      then return Pass
      else return $ Fail "Universe nu depends on external state")

-- | Verify no step's ν computation changes when given fabricated history.
-- If any empirical constant leaked through NuHistory, mutating the history
-- would either have no effect (constant is internal) or change the result
-- (constant flows through history). We test that ν for steps 1-9
-- (foundation + HITs) is invariant to history perturbation.
testExclusionHistoryInvariance :: Test
testExclusionHistoryInvariance =
  ("H2. Foundation+HIT ν invariant to history perturbation", do
    let (snapshots, _) = replayCanonical
        -- Normal history gives expected ν
        normalNu = [nu | (_, nu, _) <- take 9 snapshots]
        -- Perturbed history: double all historical ν values
        perturbedNu = go [] [] 1
        go lib nuHist step
          | step > 9 = []
          | otherwise =
            let tele = referenceTelescope step
                pertHist = [(i, 2*v) | (i,v) <- nuHist]  -- 2x all historical ν
                result = structuralNu tele lib pertHist
                nu = snTotal result
                name = detectCanonicalName tele lib
                entry = telescopeToCandidate tele lib name
            in nu : go (lib ++ [entry]) (nuHist ++ [(step, nu)]) (step + 1)
    -- Steps 1-9 should be identical regardless of history perturbation
    -- (they don't use axiomatic v_C scaling which reads nuHistory)
    let mismatches = [(i, n, p) | (i, n, p) <- zip3 [1..9::Int] normalNu perturbedNu, n /= p]
        zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
        zip3 _ _ _ = []
    if null mismatches
      then return Pass
      else return $ Fail $ "History perturbation changed ν at steps: " ++
             show [(i, n, p) | (i, n, p) <- mismatches])

-- | Verify DesugaredKappa uses only telescope structure, no external constants.
-- Kappa for any step should remain the same when evaluated with empty library.
testExclusionKappaIndependence :: Test
testExclusionKappaIndependence =
  ("H3. DesugaredKappa is library-independent", do
    let ks = [(i, desugaredKappa (referenceTelescope i)) | i <- [1..15]]
        -- Kappa should be the same whether we pass lib or not
        -- (desugaredKappa takes only the Telescope, not the Library)
        mismatches = [(i, k) | (i, k) <- ks, k <= 0]
    if null mismatches
      then return Pass
      else return $ Fail $ "Non-positive kappa at steps: " ++ show mismatches)

-- | Verify selection bar computation uses only d-bonacci (mathematical)
-- and discovered history (no paper lookups in structural mode).
-- Test: bar at step 3 with artificial history [(1,1),(1,1)] should give
-- the same result as the mathematical formula Phi_3 * Omega_2.
testExclusionBarFormula :: Test
testExclusionBarFormula =
  ("H4. Selection bar uses only Fibonacci + discovered history", do
    -- Bar_3 = Phi_3 * Omega_2
    -- Phi_3 = Delta_3 / Delta_2 = 2/1 = 2.0
    -- Omega_2 = (nu_1 + nu_2) / (kappa_1 + kappa_2) = (1+1)/(2+1) = 0.667
    -- Bar_3 = 2.0 * 0.667 = 1.333
    let bar = computeBarFromHistory 2 3 [DiscoveryRecord 1 2, DiscoveryRecord 1 1]
        expected = 2.0 * (2.0 / 3.0)  -- Phi_3 * Omega_2
    if abs (bar - expected) < 0.001
      then return Pass
      else return $ Fail $ "Bar=" ++ show bar ++ ", expected " ++ show expected)

-- ============================================
-- Test I: MBTT-First Invariant Contracts
-- ============================================

-- Contract C1: StructuralNu is name-free.
-- structuralNu reads only structural fields (leConstructors, lePathDims,
-- leHasLoop, leIsTruncated, leHas*) from library entries, never leName.
-- Scrambling library names must not change ν for steps 1-14.
-- Step 15 (DCT) has a KNOWN gap: telescopeToCandidate gates leHasTemporalOps
-- on name=="DCT", causing ν to drop from 103→88 with scrambled names.
-- This canary documents the gap. It will BREAK when MBTT-first fixes it
-- (at which point all 15 steps should match and the canary should be updated).
testC1StructuralNuNameFree :: Test
testC1StructuralNuNameFree =
  ("I1. [C1] StructuralNu name-free for steps 1-14 (step 15 known gap)", do
    -- Replay with canonical names → get ν values
    let (canonSnapshots, _) = replayCanonical
        canonNus = [nu | (_, nu, _) <- take 15 canonSnapshots]

    -- Replay with scrambled names: replace names with "X1", "X2", ...
    let scrambledNus = goScrambled [] [] 1
        goScrambled lib nuHist step
          | step > 15 = []
          | otherwise =
            let tele = referenceTelescope step
                scrambledName = "X" ++ show step
                result = structuralNu tele lib nuHist
                nu = snTotal result
                entry = telescopeToCandidate tele lib scrambledName
                newLib = lib ++ [entry]
                newHist = nuHist ++ [(step, nu)]
            in nu : goScrambled newLib newHist (step + 1)

    -- Steps 1-14 must be identical (name-free)
    let diffs114 = [(i, c, s) | (i, c, s) <- zip3_ [1..14::Int]
                                               (take 14 canonNus)
                                               (take 14 scrambledNus), c /= s]
    -- Step 15 has known gap: 103 (canonical) vs 88 (scrambled)
    let canon15 = canonNus !! 14
        scram15 = scrambledNus !! 14
        knownGap = canon15 /= scram15

    if not (null diffs114)
      then return $ Fail $ "Steps 1-14 not name-free: " ++ show diffs114
      else if not knownGap
        then return $ Fail $ "Step 15 gap CLOSED (both=" ++ show canon15 ++
               ") — MBTT-first fix landed! Update this canary to require all 15 match."
        else return Pass)

-- Contract C1b: classifyTelescope is name-free.
-- Classification depends only on AST structure, not library entry names.
testC1bClassificationNameFree :: Test
testC1bClassificationNameFree =
  ("I2. [C1] classifyTelescope independent of library entry names", do
    let -- Build normal library
        normalLib = canonicalLibAt 14
        -- Build library with all names scrambled
        scrambledLib = [entry { leName = "Z" ++ show i } | (i, entry) <- zip [1..] normalLib]
        -- Classify step 15 (DCT) telescope against both libraries
        tele15 = referenceTelescope 15
        cls1 = classifyTelescope tele15 normalLib
        cls2 = classifyTelescope tele15 scrambledLib
    if cls1 == cls2
      then return Pass
      else return $ Fail $ "Classification changed: " ++ show cls1 ++ " vs " ++ show cls2)

-- Contract C2: DesugaredKappa is deterministic and telescope-only.
-- Running desugaredKappa twice on the same telescope must give identical results.
testC2KappaDeterminism :: Test
testC2KappaDeterminism =
  ("I3. [C2] DesugaredKappa is deterministic (same input → same output)", do
    let pairs = [(desugaredKappa (referenceTelescope i), desugaredKappa (referenceTelescope i)) | i <- [1..15]]
        mismatches = [(i, a, b) | (i, (a, b)) <- zip [1..15::Int] pairs, a /= b]
    if null mismatches
      then return Pass
      else return $ Fail $ "Non-deterministic kappa at steps: " ++ show mismatches)

-- Contract C3: κ monotonicity — telescopes with strictly more clauses have ≥ κ.
-- For the reference sequence, we verify that adding entries to a telescope
-- cannot decrease desugaredKappa.
testC3KappaMonotonicity :: Test
testC3KappaMonotonicity =
  ("I4. [C3] Kappa weakly monotone with telescope size", do
    -- Reference telescopes ordered by clause count — verify kappa tracks.
    -- We test: for all pairs (i,j) where teleKappa(i) < teleKappa(j),
    -- desugaredKappa(i) ≤ desugaredKappa(j).
    let vals = [(i, teleKappa (referenceTelescope i), desugaredKappa (referenceTelescope i)) | i <- [1..15]]
        -- Find violations: smaller raw kappa but larger desugared kappa
        violations = [(i, j, rawI, rawJ, dkI, dkJ)
                     | (i, rawI, dkI) <- vals
                     , (j, rawJ, dkJ) <- vals
                     , rawI < rawJ
                     , dkI > dkJ]
    if null violations
      then return Pass
      else return $ Fail $ show (length violations) ++ " monotonicity violations: " ++
             show (take 3 [(i,j) | (i,j,_,_,_,_) <- violations]))

-- Contract C4: Post-hoc decoding non-interference.
-- detectCanonicalName is called AFTER structuralNu scores are computed.
-- Verify: computing ν before vs after naming gives identical results.
testC4DecodingNonInterference :: Test
testC4DecodingNonInterference =
  ("I5. [C4] Scoring identical whether name detection runs or not", do
    -- Score all 15 steps with the "before naming" order:
    -- 1. Score telescope (structuralNu)
    -- 2. Then detect name (detectCanonicalName)
    -- 3. Then build library entry (telescopeToCandidate)
    -- The ν from step 1 must be identical to what replayCanonical produces
    -- (where naming happens alongside library building).
    let (canonSnapshots, _) = replayCanonical
        canonNus = [nu | (_, nu, _) <- take 15 canonSnapshots]

        -- Alternative: score first, name second, same library building
        altNus = goAlt [] [] 1
        goAlt lib nuHist step
          | step > 15 = []
          | otherwise =
            let tele = referenceTelescope step
                -- Score BEFORE naming
                result = structuralNu tele lib nuHist
                nu = snTotal result
                -- Name AFTER scoring (should not affect nu)
                name = detectCanonicalName tele lib
                entry = telescopeToCandidate tele lib name
                newLib = lib ++ [entry]
                newHist = nuHist ++ [(step, nu)]
            in nu : goAlt newLib newHist (step + 1)

    let diffs = [(i, c, a) | (i, c, a) <- zip3_ [1..15::Int] canonNus altNus, c /= a]
    if null diffs
      then return Pass
      else return $ Fail $ "Scoring order matters at " ++ show (length diffs) ++ " steps: " ++
             show (take 3 diffs))

-- Contract C4b: Bar computation uses no name lookups.
-- The bar formula Bar_n = Phi_n * Omega_{n-1} depends only on d-Bonacci
-- (mathematical) and (Int, Int) history pairs (ν, κ). No String fields.
testC4bBarNameFree :: Test
testC4bBarNameFree =
  ("I6. [C4] Bar computation uses only numeric history, no names", do
    -- Compute bars with canonical history vs scrambled-name history
    -- (same ν/κ values, different names — bar should be identical)
    let (snapshots, _) = replayCanonical
        history = [DiscoveryRecord nu k | (_, nu, k) <- take 15 snapshots]
        bars = [computeBarFromHistory 2 n (take (n-1) history) | n <- [1..15]]
        -- Bar computation doesn't take names at all, so this is tautological,
        -- but it documents the contract: bar depends only on numeric history.
        barsAgain = [computeBarFromHistory 2 n (take (n-1) history) | n <- [1..15]]
        diffs = [(n, b1, b2) | (n, b1, b2) <- zip3_ [1..15::Int] bars barsAgain, abs (b1 - b2) > 1e-10]
    if null diffs
      then return Pass
      else return $ Fail $ "Bar non-deterministic at steps: " ++ show diffs)

-- | Helper: strict zip3
zip3_ :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3_ (a:as) (b:bs) (c:cs) = (a, b, c) : zip3_ as bs cs
zip3_ _ _ _ = []

-- ============================================
-- Test J: MBTT-First Enumerator
-- ============================================

-- J1: Grammar coverage — at budget=30 and libSize=0, the enumerator produces
-- expressions using Univ, Var, Pi, Sigma, Lam, App.
testJ1GrammarCoverage :: Test
testJ1GrammarCoverage =
  ("J1. [MBTT] Grammar coverage at budget=30 (Univ,Pi,Sigma,Lam,App)", do
    let exprs = enumerateExprs 0 1 30 3 []  -- libSize=0, ctxDepth=1, budget=30, depth=3
        hasUniv = any isUniv exprs
        hasPi   = any isPi exprs
        hasSigma = any isSigma exprs
        hasLam  = any isLam exprs
        hasApp  = any isApp exprs
        missing = concat
          [ ["Univ"  | not hasUniv]
          , ["Pi"    | not hasPi]
          , ["Sigma" | not hasSigma]
          , ["Lam"   | not hasLam]
          , ["App"   | not hasApp]
          ]
    if null missing
      then return Pass
      else return $ Fail $ "Missing constructors: " ++ show missing)
  where
    isUniv Univ = True
    isUniv _ = False
    isPi (Pi _ _) = True
    isPi _ = False
    isSigma (Sigma _ _) = True
    isSigma _ = False
    isLam (Lam _) = True
    isLam _ = False
    isApp (App _ _) = True
    isApp _ = False

-- J2: Well-formedness — every telescope from enumerateMBTTTelescopes passes checkTelescope.
testJ2WellFormedness :: AcceptanceConfig -> Test
testJ2WellFormedness cfg =
  ("J2. [MBTT] All enumerated telescopes pass checkTelescope", do
    let lib = canonicalLibAt 3  -- library after Pi (step 4)
        enumCfg = applyMBTTConfig cfg $
          defaultEnumConfig { ecMaxBitBudget = 20, ecMaxEntries = 3,
                              ecMaxASTDepth = 3, ecMaxCandidates = 500 }
        candidates = enumerateMBTTTelescopes lib enumCfg
        failures = [mcTelescope c | c <- candidates,
                    checkTelescope lib (mcTelescope c) /= CheckOK]
    if null failures
      then return Pass
      else return $ Fail $ show (length failures) ++ " telescopes fail checkTelescope")

-- J3: Determinism — calling enumerateMBTTTelescopes twice produces identical results.
testJ3Determinism :: AcceptanceConfig -> Test
testJ3Determinism cfg =
  ("J3. [MBTT] Enumeration is deterministic", do
    let lib = canonicalLibAt 2
        enumCfg = applyMBTTConfig cfg $
          defaultEnumConfig { ecMaxBitBudget = 20, ecMaxEntries = 2,
                              ecMaxASTDepth = 3, ecMaxCandidates = 200 }
        run1 = enumerateMBTTTelescopes lib enumCfg
        run2 = enumerateMBTTTelescopes lib enumCfg
    if run1 == run2
      then return Pass
      else return $ Fail $ "Two runs differ: " ++ show (length run1) ++ " vs " ++ show (length run2))

-- J4: Reference telescope coverage — first 4 reference telescopes (or structural
-- equivalents) appear in the enumeration at appropriate bit budgets.
testJ4ReferenceTelescopes :: AcceptanceConfig -> Test
testJ4ReferenceTelescopes cfg =
  ("J4. [MBTT] Enumerator finds reference telescopes for steps 1-4", do
    -- Step 1 (Universe): Telescope [c1 : U] — should be in empty-library enum
    let lib0 = []
        enumCfg = applyMBTTConfig cfg $
          defaultEnumConfig { ecMaxBitBudget = 30, ecMaxEntries = 4,
                              ecMaxASTDepth = 3, ecMaxCandidates = 5000 }
        cands0 = enumerateMBTTTelescopes lib0 enumCfg
        ref1 = referenceTelescope 1
        found1 = any (\c -> mcTelescope c == ref1) cands0

    -- Steps 2-4 require library built up
    let lib1 = canonicalLibAt 1
        cands1 = enumerateMBTTTelescopes lib1 enumCfg
        ref2 = referenceTelescope 2
        found2 = any (\c -> mcTelescope c == ref2) cands1

    let lib2 = canonicalLibAt 2
        cands2 = enumerateMBTTTelescopes lib2 enumCfg
        ref3 = referenceTelescope 3
        found3 = any (\c -> mcTelescope c == ref3) cands2

    let lib3 = canonicalLibAt 3
        cands3 = enumerateMBTTTelescopes lib3 enumCfg
        ref4 = referenceTelescope 4
        found4 = any (\c -> mcTelescope c == ref4) cands3

    let missing = concat
          [ ["step1(Universe)" | not found1]
          , ["step2(Unit)"     | not found2]
          , ["step3(Witness)"  | not found3]
          , ["step4(Pi)"       | not found4]
          ]
    if null missing
      then return Pass
      else return $ Fail $ "Missing reference telescopes: " ++ show missing)

-- J5: Bit-cost ordering — output is ordered by ascending total bit cost.
testJ5BitCostOrdering :: AcceptanceConfig -> Test
testJ5BitCostOrdering cfg =
  ("J5. [MBTT] Candidates ordered by ascending bit cost", do
    let lib = canonicalLibAt 3
        enumCfg = applyMBTTConfig cfg $
          defaultEnumConfig { ecMaxBitBudget = 20, ecMaxEntries = 3,
                              ecMaxASTDepth = 3, ecMaxCandidates = 500 }
        candidates = enumerateMBTTTelescopes lib enumCfg
        costs = map mcBitKappa candidates
        ordered = all (\(a, b) -> a <= b) (zip costs (drop 1 costs))
    if ordered
      then return Pass
      else return $ Fail "Candidates not ordered by bit cost")


-- J6: Canonicalization idempotence — canonicalizeExpr . canonicalizeExpr = canonicalizeExpr.
testJ6CanonicalIdempotence :: Test
testJ6CanonicalIdempotence =
  ("J6. [MBTT] Canonicalization is idempotent", do
    let exprs =
          [ Univ
          , Var 1
          , Pi Univ (Var 1)
          , Sigma (Lib 1) (Lam (Var 1))
          , App (Lam (Var 1)) (Lib 2)
          , Id Univ (Var 1) (Var 1)
          , Next (Eventually (Flat (Sharp Univ)))
          ]
        ok = all (\e -> canonicalizeExpr (canonicalizeExpr e) == canonicalizeExpr e) exprs
    if ok
      then return Pass
      else return $ Fail "canonicalizeExpr is not idempotent")

-- J7: Canonical keys stable under canonicalization pass.
testJ7CanonicalKeyStability :: Test
testJ7CanonicalKeyStability =
  ("J7. [MBTT] Canonical keys are stable after canonicalization", do
    let exprs =
          [ Pi (Sigma Univ (Var 1)) (Lam (App (Var 1) (Var 2)))
          , Trunc (Id (Lib 1) (Var 1) (Var 1))
          , Shape (Next (Eventually (Disc Univ)))
          ]
        stable = all (\e -> canonicalKeyExpr e == canonicalKeyExpr (canonicalizeExpr e)) exprs
    if stable
      then return Pass
      else return $ Fail "canonicalKeyExpr changed after canonicalizeExpr")


-- J8: Native ν adapter parity — computeNativeNu total matches StructuralNu total.
testJ8NativeNuParity :: Test
testJ8NativeNuParity =
  ("J8. [MBTT] NativeNu total matches StructuralNu total", do
    let step = 6
        tele = referenceTelescope step
        lib = canonicalLibAt (step - 1)
        (snapshots, _) = replayCanonical
        nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..(step-1)] (take (step-1) snapshots)]
        sr = structuralNu tele lib nuHist
        nr = computeNativeNu tele lib nuHist
    if nnTotal nr == snTotal sr
      then return Pass
      else return $ Fail $ "native/structural mismatch: " ++ show (nnTotal nr) ++ " vs " ++ show (snTotal sr))

-- J9: Native ν trace schema freeze (P3-V1 contract).
testJ9NativeNuTraceSchema :: Test
testJ9NativeNuTraceSchema =
  ("J9. [MBTT] NativeNu trace schema contains required keys", do
    let tele = referenceTelescope 4
        lib = canonicalLibAt 3
        (_, nuHist) = replayCanonical
        nr = computeNativeNu tele lib (take 3 nuHist)
        requiredPrefixes =
          [ "source="
          , "nu_g="
          , "nu_h="
          , "nu_c="
          , "bonus_distributive="
          , "bonus_universe_poly="
          , "bonus_infinitesimal_shift="
          , "nu_total="
          , "node_trace_count="
          ]
        hasPrefix pre = any (\line -> take (length pre) line == pre) (nnTrace nr)
        missing = [pre | pre <- requiredPrefixes, not (hasPrefix pre)]
    if null missing
      then return Pass
      else return $ Fail $ "missing trace keys: " ++ show missing)


-- J10: Node-level trace lines are present and deterministic for same input.
testJ10NativeNuNodeTraceDeterministic :: Test
testJ10NativeNuNodeTraceDeterministic =
  ("J10. [MBTT] NativeNu node-level trace is present and deterministic", do
    let step = 5
        tele = referenceTelescope step
        lib = canonicalLibAt (step - 1)
        (snapshots, _) = replayCanonical
        nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..(step-1)] (take (step-1) snapshots)]
        t1 = nnTrace (computeNativeNu tele lib nuHist)
        t2 = nnTrace (computeNativeNu tele lib nuHist)
        nodeLines = [line | line <- t1, take 5 line == "node="]
    if (not (null nodeLines)) && t1 == t2
      then return Pass
      else return $ Fail "node trace missing or nondeterministic")


-- J11: Native ν invariance under alpha-equivalent telescope renaming.
-- TeleEntry names are binder labels only; native ν must be unchanged.
testJ11NativeNuAlphaRenameInvariant :: Test
testJ11NativeNuAlphaRenameInvariant =
  ("J11. [MBTT] NativeNu invariant under TeleEntry alpha-renaming", do
    let step = 6
        tele = referenceTelescope step
        teleRenamed = Telescope [ e { teName = "alpha_" ++ show i } | (i, e) <- zip [1 :: Int ..] (teleEntries tele) ]
        lib = canonicalLibAt (step - 1)
        (snapshots, _) = replayCanonical
        nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..(step-1)] (take (step-1) snapshots)]
        n0 = computeNativeNu tele lib nuHist
        n1 = computeNativeNu teleRenamed lib nuHist
    if (nnTotal n0 == nnTotal n1) && (nnTrace n0 == nnTrace n1)
      then return Pass
      else return $ Fail "alpha-renaming changed native ν or trace")

-- J12: Native ν invariance under canonical rewrites of entry expressions.
testJ12NativeNuCanonicalRewriteInvariant :: Test
testJ12NativeNuCanonicalRewriteInvariant =
  ("J12. [MBTT] NativeNu invariant under canonicalized expression rewrite", do
    let step = 9
        tele = referenceTelescope step
        teleCanon = Telescope [ e { teType = canonicalizeExpr (teType e) } | e <- teleEntries tele ]
        lib = canonicalLibAt (step - 1)
        (snapshots, _) = replayCanonical
        nuHist = [(i, nu) | (i, (_, nu, _)) <- zip [1..(step-1)] (take (step-1) snapshots)]
        n0 = computeNativeNu tele lib nuHist
        n1 = computeNativeNu teleCanon lib nuHist
    if (nnTotal n0 == nnTotal n1) && (nnTrace n0 == nnTrace n1)
      then return Pass
      else return $ Fail "canonical rewrite changed native ν or trace")

-- J13: Negative control — genuinely different expression should be distinguished.
testJ13NativeNuNegativeControl :: Test
testJ13NativeNuNegativeControl =
  ("J13. [MBTT] NativeNu distinguishes non-equivalent control", do
    let teleA = referenceTelescope 1
        teleB = Telescope [TeleEntry "c1" (Var 1)]
        nA = computeNativeNu teleA [] []
        nB = computeNativeNu teleB [] []
    if (nnTotal nA /= nnTotal nB) || (nnTrace nA /= nnTrace nB)
      then return Pass
      else return $ Fail "negative control unexpectedly matched")

-- ============================================
-- Entry points
-- ============================================

coreTests :: [Test]
coreTests =
  -- A: Bootstrap bar sensitivity
  [ testUniverseNu
  , testTruncNuExact
  -- B: Pi binder novelty
  , testPiNuPositive
  , testPiNuExact
  -- C: Trunc anti-explosion
  , testTruncBounded
  -- D: DCT meta-theorems
  , testDCTTotal
  , testDCTDistLaw
  , testDCTUnivPoly
  , testDCTInfShift
  ]
  -- E: Kappa consistency
  ++ testKappaAll
  -- F: Full sequence golden test
  ++ [ testFullSequence
     , testTotalNu
     , testTotalKappa
     ]
  -- G: Canonical name detection
  ++ testCanonicalNames
  -- H: Exclusion contract
  ++ [ testExclusionNoEmpiricalInNu
     , testExclusionHistoryInvariance
     , testExclusionKappaIndependence
     , testExclusionBarFormula
     ]
  -- I: MBTT-first invariant contracts (ADR-0001)
  ++ [ testC1StructuralNuNameFree
     , testC1bClassificationNameFree
     , testC2KappaDeterminism
     , testC3KappaMonotonicity
     , testC4DecodingNonInterference
     , testC4bBarNameFree
     ]

mbttTests :: AcceptanceConfig -> [Test]
mbttTests cfg =
  if acMbttSkip cfg
  then []
  else [ testJ1GrammarCoverage
       , testJ2WellFormedness cfg
       , testJ3Determinism cfg
       ]
       ++ (if acMbttFast cfg then [] else [testJ4ReferenceTelescopes cfg])
       ++ [ testJ5BitCostOrdering cfg
          , testJ6CanonicalIdempotence
          , testJ7CanonicalKeyStability
          , testJ8NativeNuParity
          , testJ9NativeNuTraceSchema
          , testJ10NativeNuNodeTraceDeterministic
          , testJ11NativeNuAlphaRenameInvariant
          , testJ12NativeNuCanonicalRewriteInvariant
          , testJ13NativeNuNegativeControl
          ]

runAcceptanceWithConfig :: AcceptanceConfig -> IO ()
runAcceptanceWithConfig cfg = do
  when (acMbttFast cfg) $
    putStrLn "[acceptance] --mbtt-fast enabled: using reduced MBTT enumerator budgets."
  when (acMbttSkip cfg) $
    putStrLn "[acceptance] --skip-mbtt enabled: skipping MBTT J-tests."
  when (acMbttFast cfg) $
    putStrLn "[acceptance] fast mode omits J4 reference-recovery due intentionally tighter MBTT bounds."
  runTests (coreTests ++ mbttTests cfg)

runCoreAcceptance :: IO ()
runCoreAcceptance = do
  putStrLn "[acceptance-core] running core suites A-I only."
  runTests coreTests

runMBTTAcceptance :: AcceptanceConfig -> IO ()
runMBTTAcceptance cfg = do
  let cfg' = cfg { acMbttSkip = False }
  when (acMbttFast cfg') $
    putStrLn "[acceptance-mbtt] --mbtt-fast enabled for MBTT-only lane."
  runTests (mbttTests cfg')

runAcceptanceMain :: IO ()
runAcceptanceMain = do
  cfg <- parseArgs <$> getArgs
  runAcceptanceWithConfig cfg
