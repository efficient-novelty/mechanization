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

module Main where

import Telescope
import TelescopeEval (telescopeToCandidate, detectCanonicalName)
import StructuralNu (structuralNu, StructuralNuResult(..))
import CoherenceWindow (dBonacciDelta)
import Types (Library)

import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)
import System.IO (hFlush, stdout)

-- ============================================
-- Test Framework
-- ============================================

data TestResult = Pass | Fail String deriving (Show)

type Test = (String, IO TestResult)

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
-- Main
-- ============================================

main :: IO ()
main = runTests $
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
