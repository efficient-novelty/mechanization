{-# LANGUAGE BangPatterns #-}

-- | PEN Axiom Simulation Engine
--
-- Implements the five PEN axioms as a tick-by-tick simulation:
--   Axiom 1: Cumulative growth — Bar(τₙ) = Φₙ · Ωₙ₋₁
--   Axiom 2: Dynamic horizon — H resets to 2 after realization, +1 per idle tick
--   Axiom 3: Admissibility — candidate must have κ ≤ H
--   Axiom 4: Minimal-overshoot selection — pick ρ closest to Bar from above
--   Axiom 5: Fibonacci integration gaps — Δₙ = Fₙ, τₙ = ΣFᵢ

module Simulation
  ( SimMode(..)
  , SimConfig(..)
  , SimState(..)
  , TickResult(..)
  , defaultConfig
  , capabilityConfig
  , runSimulation
  , formatSimTable
  ) where

import Types
import KappaNu (genesisEntry, paperKappa, paperNu, computedNu)
import ProofRank (buildCostMap, kNoveltyWithBaseline)
import CoherenceWindow (dBonacciDelta, dBonacciTau, defaultWindow)
import qualified Data.Map.Strict as Map
import Data.List (minimumBy)
import Data.Ord (comparing)

-- ============================================
-- Data Types
-- ============================================

data SimMode = PaperMode | ComputedMode | CapabilityMode
  deriving (Eq, Show)

data SimConfig = SimConfig
  { cfgMode         :: SimMode
  , cfgHMax         :: Int   -- ^ Maximum horizon (cap on H)
  , cfgMaxIdleTicks :: Int   -- ^ Stop after this many consecutive idle ticks
  , cfgComputedH    :: Int   -- ^ Horizon for computed-mode cost maps
  , cfgWindow       :: Int   -- ^ Coherence window depth d (1=constant, 2=Fibonacci, 3=tribonacci)
  } deriving (Eq, Show)

defaultConfig :: SimConfig
defaultConfig = SimConfig
  { cfgMode         = PaperMode
  , cfgHMax         = 20
  , cfgMaxIdleTicks = 50
  , cfgComputedH    = 5
  , cfgWindow       = defaultWindow
  }

capabilityConfig :: SimConfig
capabilityConfig = SimConfig
  { cfgMode         = CapabilityMode
  , cfgHMax         = 20
  , cfgMaxIdleTicks = 50
  , cfgComputedH    = 5
  , cfgWindow       = defaultWindow
  }

type Candidate = (Int, LibraryEntry)  -- (genesis index, entry)

data SimState = SimState
  { ssLibrary       :: Library
  , ssCandidates    :: [Candidate]
  , ssTick          :: Int
  , ssRealizationN  :: Int   -- ^ Number of realizations so far
  , ssHorizon       :: Int   -- ^ Current admissibility horizon H
  , ssCumNu         :: Int   -- ^ Cumulative Σνᵢ
  , ssCumKappa      :: Int   -- ^ Cumulative Σκᵢ
  , ssIdleStreak    :: Int   -- ^ Consecutive idle ticks
  , ssLog           :: [TickResult]
  } deriving (Show)

data TickResult = TickResult
  { trN         :: Int       -- ^ Realization number
  , trTau       :: Int       -- ^ Cumulative time τ
  , trDelta     :: Int       -- ^ Integration gap Δ = F_n
  , trNu        :: Int       -- ^ Shannon surprise ν
  , trKappa     :: Int       -- ^ Kolmogorov complexity κ
  , trH         :: Int       -- ^ Horizon at time of admission
  , trName      :: String    -- ^ Structure name
  , trRho       :: Double    -- ^ Efficiency ρ = ν/κ
  , trPhi       :: Double    -- ^ Fibonacci ratio Φₙ = Fₙ/Fₙ₋₁
  , trOmega     :: Double    -- ^ Cumulative ratio Ωₙ₋₁ = Σν/Σκ
  , trBar       :: Double    -- ^ Bar threshold
  , trOvershoot :: Double    -- ^ ρ - Bar (how much ρ exceeds bar)
  , trCleared   :: Bool      -- ^ Did this entry clear the bar?
  } deriving (Show)

-- ============================================
-- d-Bonacci Sequence (parameterized by window depth)
-- ============================================

-- | Integration gap delta_n for window depth d (1-indexed).
-- d=2 gives the Fibonacci sequence (backward compatible).
fibDelta :: Int -> Int -> Int
fibDelta d n = dBonacciDelta d n

-- | Cumulative sum tau_n for window depth d.
fibTau :: Int -> Int -> Int
fibTau d n = dBonacciTau d n

-- ============================================
-- Bar Computation (Axiom 1)
-- ============================================

-- | Compute Bar(τₙ) = Φₙ · Ωₙ₋₁
--   where Φₙ = Fₙ / Fₙ₋₁ and Ωₙ₋₁ = Σνᵢ / Σκᵢ
computeBar :: Int -> Int -> Int -> Int -> Double
computeBar d n cumNu cumKappa
  | n <= 1    = 0.0
  | cumKappa == 0 = 0.0
  | otherwise = phi * omega
  where
    phi   = fromIntegral (fibDelta d n) / fromIntegral (fibDelta d (n - 1))
    omega = fromIntegral cumNu / fromIntegral cumKappa

-- | Get Φₙ = Fₙ / Fₙ₋₁
computePhi :: Int -> Int -> Double
computePhi d n
  | n <= 1    = 0.0
  | otherwise = fromIntegral (fibDelta d n) / fromIntegral (fibDelta d (n - 1))

-- | Get Ωₙ₋₁ = Σνᵢ / Σκᵢ
computeOmega :: Int -> Int -> Double
computeOmega cumNu cumKappa
  | cumKappa == 0 = 0.0
  | otherwise     = fromIntegral cumNu / fromIntegral cumKappa

-- ============================================
-- Candidate Evaluation
-- ============================================

data CandidateEval = CandidateEval
  { ceIndex     :: Int          -- ^ Genesis index
  , ceEntry     :: LibraryEntry
  , ceKappa     :: Int
  , ceNu        :: Int
  , ceRho       :: Double       -- ^ ν / κ
  , ceAdmissible :: Bool        -- ^ κ ≤ H
  , ceClearsBar :: Bool         -- ^ ρ ≥ Bar
  , ceOvershoot :: Double       -- ^ ρ - Bar
  } deriving (Show)

-- | Evaluate a single candidate in paper mode
evalCandidatePaper :: Int -> LibraryEntry -> Int -> Double -> CandidateEval
evalCandidatePaper idx entry horizon bar =
  let k = paperKappa idx
      v = paperNu idx
      r = if k > 0 then fromIntegral v / fromIntegral k else 0.0
      admissible = k <= horizon
      clears = r >= bar
  in CandidateEval
    { ceIndex      = idx
    , ceEntry      = entry
    , ceKappa      = k
    , ceNu         = v
    , ceRho        = r
    , ceAdmissible = admissible
    , ceClearsBar  = clears
    , ceOvershoot  = r - bar
    }

-- | Evaluate a single candidate in computed mode
evalCandidateComputed :: Int -> LibraryEntry -> Library -> Int
                      -> Map.Map TypeExpr Int -> Int -> Double -> CandidateEval
evalCandidateComputed idx entry lib horizon costBefore computedH bar =
  let k = paperKappa idx  -- Use paper κ for admissibility (same definition)
      (v, _clusters) = kNoveltyWithBaseline entry lib computedH costBefore
      r = if k > 0 then fromIntegral v / fromIntegral k else 0.0
      admissible = k <= horizon
      clears = r >= bar
  in CandidateEval
    { ceIndex      = idx
    , ceEntry      = entry
    , ceKappa      = k
    , ceNu         = v
    , ceRho        = r
    , ceAdmissible = admissible
    , ceClearsBar  = clears
    , ceOvershoot  = r - bar
    }

-- | Evaluate a single candidate in capability mode (uses computedNu)
evalCandidateCapability :: Int -> LibraryEntry -> Int -> Double -> CandidateEval
evalCandidateCapability idx entry horizon bar =
  let k = paperKappa idx
      v = computedNu idx
      r = if k > 0 then fromIntegral v / fromIntegral k else 0.0
      admissible = k <= horizon
      clears = r >= bar
  in CandidateEval
    { ceIndex      = idx
    , ceEntry      = entry
    , ceKappa      = k
    , ceNu         = v
    , ceRho        = r
    , ceAdmissible = admissible
    , ceClearsBar  = clears
    , ceOvershoot  = r - bar
    }

-- ============================================
-- Selection (Axiom 4)
-- ============================================

-- | Select the winner: minimal overshoot (ρ - bar), ties broken by
-- minimal κ, then smallest genesis index
selectWinner :: [CandidateEval] -> CandidateEval
selectWinner candidates =
  minimumBy (comparing ceOvershoot
    <> comparing ceKappa
    <> comparing ceIndex) candidates

-- ============================================
-- Simulation Loop
-- ============================================

-- | Run the full PEN axiom simulation
runSimulation :: SimConfig -> IO [TickResult]
runSimulation cfg = do
  let allCandidates = [(i, genesisEntry i) | i <- [1..16]]
      initState = SimState
        { ssLibrary      = []
        , ssCandidates   = allCandidates
        , ssTick         = 0
        , ssRealizationN = 0
        , ssHorizon      = 2
        , ssCumNu        = 0
        , ssCumKappa     = 0
        , ssIdleStreak   = 0
        , ssLog          = []
        }
  finalState <- simLoop cfg initState
  return (reverse (ssLog finalState))

-- | Main simulation loop
simLoop :: SimConfig -> SimState -> IO SimState
simLoop cfg st
  | null (ssCandidates st) = return st
  | ssIdleStreak st >= cfgMaxIdleTicks cfg = return st
  | otherwise = do
      let w       = cfgWindow cfg
          nextN   = ssRealizationN st + 1
          bar     = computeBar w nextN (ssCumNu st) (ssCumKappa st)
          horizon = ssHorizon st

          -- Evaluate all remaining candidates
          evals = case cfgMode cfg of
            PaperMode ->
              [evalCandidatePaper idx entry horizon bar
              | (idx, entry) <- ssCandidates st]
            ComputedMode ->
              let costBefore = buildCostMap (ssLibrary st) (cfgComputedH cfg)
              in [evalCandidateComputed idx entry (ssLibrary st) horizon
                    costBefore (cfgComputedH cfg) bar
                 | (idx, entry) <- ssCandidates st]
            CapabilityMode ->
              [evalCandidateCapability idx entry horizon bar
              | (idx, entry) <- ssCandidates st]

          -- Filter: admissible AND clears bar
          clearing = filter (\e -> ceAdmissible e && ceClearsBar e) evals

      if null clearing
        then do
          -- Idle tick: increment horizon, increment idle streak
          let newH = min (ssHorizon st + 1) (cfgHMax cfg)
          simLoop cfg st
            { ssTick      = ssTick st + 1
            , ssHorizon   = newH
            , ssIdleStreak = ssIdleStreak st + 1
            }
        else do
          -- Realization: select winner
          let winner = selectWinner clearing
              winIdx = ceIndex winner
              winEntry = ceEntry winner
              winNu = ceNu winner
              winKappa = ceKappa winner

              -- Compute d-bonacci integration gap
              delta = fibDelta w nextN
              tau = fibTau w nextN

              -- After realization, H resets to 2, then grows by (delta-1)
              -- because delta ticks pass: H = 2 + (delta - 1) = delta + 1
              newH = min (delta + 1) (cfgHMax cfg)

              phi = computePhi w nextN
              omega = computeOmega (ssCumNu st) (ssCumKappa st)

              result = TickResult
                { trN         = nextN
                , trTau       = tau
                , trDelta     = delta
                , trNu        = winNu
                , trKappa     = winKappa
                , trH         = horizon
                , trName      = structureName winIdx
                , trRho       = ceRho winner
                , trPhi       = phi
                , trOmega     = omega
                , trBar       = bar
                , trOvershoot = ceOvershoot winner
                , trCleared   = True
                }

              -- Remove winner from candidates
              newCandidates = filter (\(i, _) -> i /= winIdx) (ssCandidates st)

          simLoop cfg st
            { ssLibrary      = ssLibrary st ++ [winEntry]
            , ssCandidates   = newCandidates
            , ssTick         = ssTick st + 1
            , ssRealizationN = nextN
            , ssHorizon      = newH
            , ssCumNu        = ssCumNu st + winNu
            , ssCumKappa     = ssCumKappa st + winKappa
            , ssIdleStreak   = 0
            , ssLog          = result : ssLog st
            }

-- ============================================
-- Output Formatting
-- ============================================

-- | Format simulation results as a table
formatSimTable :: [TickResult] -> String
formatSimTable results = unlines $
  [ header, separator ] ++ map formatRow results ++ [separator, summary]
  where
    header = " n  | tau  | Structure      | Delta | nu  | kappa | rho    | Phi   | Omega  | Bar    | Cleared"
    separator = "----|------|----------------|-------|-----|-------|--------|-------|--------|--------|--------"

    formatRow r =
      padL 3 (show (trN r)) ++ " | "
      ++ padL 4 (show (trTau r)) ++ " | "
      ++ padR 14 (trName r) ++ " | "
      ++ padL 5 (show (trDelta r)) ++ " | "
      ++ padL 3 (show (trNu r)) ++ " | "
      ++ padL 5 (show (trKappa r)) ++ " | "
      ++ padL 6 (showF2 (trRho r)) ++ " | "
      ++ padL 5 (if trN r <= 1 then "  ---" else showF2 (trPhi r)) ++ " | "
      ++ padL 6 (if trN r <= 1 then "   ---" else showF2 (trOmega r)) ++ " | "
      ++ padL 6 (if trN r <= 1 then "   ---" else showF2 (trBar r)) ++ " | "
      ++ (if trCleared r then "YES" else " NO")

    nCleared = length (filter trCleared results)
    nTotal   = length results
    summary  = show nCleared ++ "/" ++ show nTotal ++ " structures cleared the bar."

-- | Map genesis index to human-readable name
structureName :: Int -> String
structureName 1  = "Universe"
structureName 2  = "Unit"
structureName 3  = "Witness"
structureName 4  = "Pi/Sigma"
structureName 5  = "S1"
structureName 6  = "PropTrunc"
structureName 7  = "S2"
structureName 8  = "S3"
structureName 9  = "Hopf"
structureName 10 = "Lie"
structureName 11 = "Cohesion"
structureName 12 = "Connections"
structureName 13 = "Curvature"
structureName 14 = "Metric"
structureName 15 = "Hilbert"
structureName 16 = "DCT"
structureName _  = "???"

-- | Format a Double to 2 decimal places
showF2 :: Double -> String
showF2 x = let s = show (fromIntegral (round (x * 100) :: Int) / 100.0 :: Double)
           in s

-- | Pad a string on the left to a given width
padL :: Int -> String -> String
padL n s = replicate (max 0 (n - length s)) ' ' ++ s

-- | Pad a string on the right to a given width
padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
