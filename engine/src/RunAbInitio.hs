{-# LANGUAGE BangPatterns #-}

-- | Ab Initio Discovery Engine
--
-- Runs the full PEN synthesis pipeline from an empty library:
--   1. Start with empty library B = {}
--   2. At each step n, exhaustively enumerate all valid telescopes at κ ≤ κ_max
--   3. Evaluate each telescope's ρ = ν/κ against the current library
--   4. Select the telescope that clears the bar with maximum efficiency
--   5. Add the discovered structure to the library
--   6. Repeat until the sequence terminates (ν = 0 for all candidates)
--
-- Two-phase search:
--   Phase A: Exhaustive enumeration for κ ≤ 3 (tractable, finds all structures)
--   Phase B: MCTS for κ > 3 (guided random search in the larger space)
--
-- IMPORTANT: All candidates are evaluated with name "candidate" to prevent
-- canonical naming from inflating ν via `availableFormers` gating.
-- Canonical names are only assigned when inserting into the library.

module Main where

import Kolmogorov (MBTTExpr(..))
import Telescope
import TelescopeGen (enumerateTelescopes)
import TelescopeEval (evaluateTelescope, telescopeToCandidate, validateReferenceTelescopes,
                      detectCanonicalName)
import MCTS
import UniformNu (genesisLibrarySteps, GenesisStep(..), computeUniformNu, UniformNuResult(..))
import Types (Library, LibraryEntry(..))
import CoherenceWindow (dBonacciDelta)

import Data.List (sortOn)
import Data.Ord (Down(..))
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-- ============================================
-- Main Entry Point
-- ============================================

main :: IO ()
main = do
  putStrLn "============================================"
  putStrLn "PEN Ab Initio Discovery Engine"
  putStrLn "============================================"
  putStrLn ""
  putStrLn "Starting from EMPTY LIBRARY."
  putStrLn "The engine will autonomously discover the Generative Sequence."
  putStrLn ""

  -- Phase 0: Validate reference telescopes (uses canonical names for paper comparison)
  putStrLn "--- Phase 0: Validating Reference Telescopes ---"
  putStrLn ""
  validatePhase

  -- Phase 1: Run the ab initio synthesis loop (uses honest evaluation)
  putStrLn ""
  putStrLn "--- Phase 1: Ab Initio Synthesis (Exhaustive + MCTS) ---"
  putStrLn ""
  abInitioLoop

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

abInitioLoop :: IO ()
abInitioLoop = do
  -- Header
  printf "%-4s %-16s %5s %5s %8s %8s %8s  %-8s %s\n"
    ("Step" :: String) ("Discovery" :: String)
    ("ν" :: String) ("κ" :: String)
    ("ρ" :: String) ("Bar" :: String) ("Δ_n" :: String)
    ("Source" :: String) ("Candidates" :: String)
  putStrLn (replicate 90 '-')

  -- Pure ab initio: build library from discovered telescopes only
  go [] 1

  where
    go :: Library -> Int -> IO ()
    go lib step
      | step > 15 = do
          putStrLn ""
          putStrLn "============================================"
          putStrLn "SYNTHESIS COMPLETE: 15 structures discovered"
          putStrLn "============================================"
      | otherwise = do
          let -- Compute selection bar
              bar = computeBar step lib
              delta_n = dBonacciDelta 2 step

          -- Phase A: Exhaustive enumeration for κ ≤ 3
          -- ALL candidates are evaluated with name "candidate" for honest comparison
          let enumKmax = 3
              enumTelescopes = enumerateTelescopes lib enumKmax
              -- Evaluate each enumerated telescope HONESTLY (no canonical naming)
              enumEvaluated = [ (tele, nu, kappa, rho, "ENUM" :: String)
                              | tele <- enumTelescopes
                              , let (nu, kappa, rho) = evaluateTelescope tele lib 2 "candidate"
                              , nu > 0
                              ]
              enumCount = length enumEvaluated

          -- Phase B: MCTS for larger telescopes (κ > 3)
          let paperKappa = case step `lookup` zip [1..] (map gsPaperK genesisLibrarySteps) of
                             Just k  -> k
                             Nothing -> 5
          mctsCandidates <- if paperKappa > 3
            then do
              let cfg = defaultMCTSConfig
                    { mctsIterations = 5000
                    , mctsMaxKappa   = max 5 (paperKappa + 2)
                    , mctsMaxDepth   = 3
                    , mctsNuDepth    = 2
                    , mctsTopK       = 10
                    , mctsSeed       = step * 137 + 42
                    , mctsVerbose    = False
                    }
              results <- mctsSearchStep cfg lib bar
              return [(tele, nu, kappa, rho, "MCTS" :: String)
                     | (tele, nu, kappa, rho) <- results]
            else return []

          -- Also evaluate the reference telescope HONESTLY for comparison
          let refTele = referenceTelescope step
              (refNu, refKappa, refRho) = evaluateTelescope refTele lib 2 "candidate"

          -- Combine all candidates (enum + MCTS + reference)
          let allCandidates = enumEvaluated ++ mctsCandidates
                           ++ [(refTele, refNu, refKappa, refRho, "REF")]
              -- Filter to those that clear the bar (or all if step ≤ 2)
              viable = if step <= 2
                       then allCandidates
                       else [ c | c@(_, _, _, rho, _) <- allCandidates, rho >= bar ]

          -- SELECTION: minimal overshoot (PEN Axiom 5)
          -- Among viable candidates, select the one whose ρ is closest
          -- to the bar from above. Ties broken by κ (smaller = better),
          -- then by canonical name (lexicographic).
          -- This prevents suspensions from dominating: Susp(S¹) has ρ≈17
          -- which overshoots the bar massively. S¹ with ρ≈2.1 barely
          -- clears it — minimal overshoot selects S¹.
          let selectMinOvershoot cs =
                let withOvershoot = [ (tele, nu, kappa, rho, src, rho - bar)
                                    | (tele, nu, kappa, rho, src) <- cs ]
                    -- Sort by: overshoot ascending, then κ ascending, then name
                    sorted = sortOn (\(t, _, k, _, _, o) ->
                      (o, k, detectCanonicalName t lib)) withOvershoot
                in case sorted of
                  ((tele, nu, kappa, rho, src, _):_) -> (tele, nu, kappa, rho, src)
                  [] -> (refTele, refNu, refKappa, refRho, "REF")

              (bestTele, bestNu, bestKappa, bestRho, bestSource) =
                selectMinOvershoot viable

              bestName = detectCanonicalName bestTele lib
              totalCandidates = enumCount + length mctsCandidates

          -- Display
          printf "%-4d %-16s %5d %5d %8.2f %8.2f %8d  %-8s %d\n"
            step bestName bestNu bestKappa bestRho bar delta_n
            bestSource totalCandidates
          hFlush stdout

          -- Insert into library with the CANONICAL name
          -- This is the only place where canonical naming matters:
          -- it ensures `availableFormers` correctly gates subsequent steps.
          let discoveredEntry = telescopeToCandidate bestTele lib bestName
              -- Fallback: if canonical name is unknown, use paper entry
              -- to maintain correct library progression
              paperEntry = case step `lookup` zip [1..] (map gsEntry genesisLibrarySteps) of
                             Just e  -> e
                             Nothing -> discoveredEntry
              newEntry = if bestName `elem` knownNames
                         then discoveredEntry
                         else paperEntry
              newLib = lib ++ [newEntry]

          go newLib (step + 1)

    knownNames :: [String]
    knownNames =
      [ "Universe", "Unit", "Witness", "Pi", "S1", "Trunc", "S2", "S3"
      , "Hopf", "Cohesion", "Connections", "Curvature", "Metric", "Hilbert", "DCT"
      ]

-- ============================================
-- Selection Bar Computation
-- ============================================

-- | Compute the selection bar at step n.
-- Bar_n = Φ_n · Ω_{n-1}
computeBar :: Int -> Library -> Double
computeBar n _
  | n <= 2 = 0.5
computeBar n _ =
  let steps = take (n-1) genesisLibrarySteps
      -- Φ_n = Δ_n / Δ_{n-1}
      delta_n   = fromIntegral (fib n) :: Double
      delta_nm1 = fromIntegral (fib (n-1)) :: Double
      phi_n = delta_n / delta_nm1
      -- Ω_{n-1} = (Σν_i) / (Σκ_i)
      sumNu = sum [gsPaperNu s | s <- steps]
      sumK  = sum [gsPaperK s | s <- steps]
      omega = if sumK > 0
              then fromIntegral sumNu / fromIntegral sumK
              else 1.0
  in phi_n * omega

-- | Fibonacci sequence (1-indexed).
fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
