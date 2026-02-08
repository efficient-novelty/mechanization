{-# LANGUAGE BangPatterns #-}

-- | Synthesis engine: discovers Genesis structures from search
--
-- Mirrors Simulation.hs architecture but uses genuine candidate
-- generation instead of replaying hardcoded structures.
--
-- At each step:
--   1. generateCandidates — produce all candidates
--   2. genuineNu / candidateKappa — evaluate each
--   3. Filter: admissible (kappa <= H) AND clears bar (rho >= bar)
--   4. Select: minimal overshoot (matching Simulation.selectWinner)
--   5. Update theory state, advance Fibonacci clock

module Synthesis
  ( SynthConfig(..)
  , SynthResult(..)
  , defaultSynthConfig
  , runSynthesis
  , formatSynthTable
  , formatSynthComparison
  ) where

import Generator
import GenuineNu (genuineNu)
import TheoryState
import Data.List (minimumBy)
import Data.Ord (comparing)

-- ============================================
-- Configuration
-- ============================================

data SynthConfig = SynthConfig
  { scMaxSteps    :: Int    -- ^ Maximum structures to discover
  , scMaxIdle     :: Int    -- ^ Stop after this many consecutive idle ticks
  , scHMax        :: Int    -- ^ Maximum horizon cap
  , scInitHorizon :: Int    -- ^ Initial admissibility horizon
  } deriving (Eq, Show)

defaultSynthConfig :: SynthConfig
defaultSynthConfig = SynthConfig
  { scMaxSteps    = 15      -- Discover structures 1-15 (through DCT)
  , scMaxIdle     = 50
  , scHMax        = 20
  , scInitHorizon = 2
  }

-- ============================================
-- Results
-- ============================================

data SynthResult = SynthResult
  { srN         :: Int       -- ^ Realization number (1-indexed)
  , srTau       :: Int       -- ^ Cumulative time tau
  , srDelta     :: Int       -- ^ Integration gap delta = F_n
  , srNu        :: Int       -- ^ Genuine nu
  , srKappa     :: Int       -- ^ Kolmogorov complexity kappa
  , srH         :: Int       -- ^ Horizon at time of admission
  , srName      :: String    -- ^ Discovered structure name
  , srRho       :: Double    -- ^ Efficiency rho = nu/kappa
  , srPhi       :: Double    -- ^ Fibonacci ratio
  , srOmega     :: Double    -- ^ Cumulative ratio
  , srBar       :: Double    -- ^ Bar threshold
  , srOvershoot :: Double    -- ^ rho - bar
  , srCleared   :: Bool      -- ^ Did this entry clear the bar?
  , srCandName  :: String    -- ^ Candidate type (Foundation/HIT/etc)
  } deriving (Show)

-- ============================================
-- Fibonacci sequence (mirrors Simulation.hs)
-- ============================================

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fibDelta :: Int -> Int
fibDelta n
  | n < 1     = 1
  | otherwise = fibs !! (n - 1)

fibTau :: Int -> Int
fibTau n = sum (take n fibs)

-- ============================================
-- Bar computation (Axiom 1)
-- ============================================

computeBar :: Int -> Int -> Int -> Double
computeBar n cumNu cumKappa
  | n <= 1    = 0.0
  | cumKappa == 0 = 0.0
  | otherwise = phi * omega
  where
    phi   = fromIntegral (fibDelta n) / fromIntegral (fibDelta (n - 1))
    omega = fromIntegral cumNu / fromIntegral cumKappa

computePhi :: Int -> Double
computePhi n
  | n <= 1    = 0.0
  | otherwise = fromIntegral (fibDelta n) / fromIntegral (fibDelta (n - 1))

computeOmega :: Int -> Int -> Double
computeOmega cumNu cumKappa
  | cumKappa == 0 = 0.0
  | otherwise     = fromIntegral cumNu / fromIntegral cumKappa

-- ============================================
-- Candidate evaluation
-- ============================================

data CandEval = CandEval
  { ceCandidate  :: Candidate
  , ceKappa      :: Int
  , ceNu         :: Int
  , ceRho        :: Double
  , ceAdmissible :: Bool
  , ceClearsBar  :: Bool
  , ceOvershoot  :: Double
  , ceName       :: String
  } deriving (Show)

evalCandidate :: Candidate -> TheoryState -> Int -> Double -> CandEval
evalCandidate cand ts horizon bar =
  let k = candidateKappa cand ts
      (v, _clusters) = genuineNu cand ts
      r = if k > 0 then fromIntegral v / fromIntegral k else 0.0
      admissible = k <= horizon
      clears = r >= bar
      name = candidateName cand
  in CandEval
    { ceCandidate  = cand
    , ceKappa      = k
    , ceNu         = v
    , ceRho        = r
    , ceAdmissible = admissible
    , ceClearsBar  = clears
    , ceOvershoot  = r - bar
    , ceName       = name
    }

-- | Select winner: minimal overshoot, ties broken by kappa then name
selectWinner :: [CandEval] -> CandEval
selectWinner = minimumBy (comparing ceOvershoot
                       <> comparing ceKappa
                       <> comparing ceName)

-- ============================================
-- Synthesis state
-- ============================================

data SynthState = SynthState
  { ssTheory     :: TheoryState
  , ssTick       :: Int
  , ssRealN      :: Int       -- ^ Number of realizations
  , ssHorizon    :: Int
  , ssCumNu      :: Int
  , ssCumKappa   :: Int
  , ssIdleStreak :: Int
  , ssResults    :: [SynthResult]
  } deriving (Show)

-- ============================================
-- Main synthesis loop
-- ============================================

-- | Run the full synthesis, returning discovered structures
runSynthesis :: SynthConfig -> IO [SynthResult]
runSynthesis cfg = do
  let initState = SynthState
        { ssTheory     = initialTheoryState
        , ssTick       = 0
        , ssRealN      = 0
        , ssHorizon    = scInitHorizon cfg
        , ssCumNu      = 0
        , ssCumKappa   = 0
        , ssIdleStreak = 0
        , ssResults    = []
        }
  finalState <- synthLoop cfg initState
  return (reverse (ssResults finalState))

synthLoop :: SynthConfig -> SynthState -> IO SynthState
synthLoop cfg st
  | ssRealN st >= scMaxSteps cfg = return st
  | ssIdleStreak st >= scMaxIdle cfg = return st
  | otherwise = do
      let nextN   = ssRealN st + 1
          bar     = computeBar nextN (ssCumNu st) (ssCumKappa st)
          horizon = ssHorizon st
          ts      = ssTheory st

          -- Generate all candidates
          candidates = generateCandidates ts horizon

          -- Evaluate each candidate
          evals = map (\c -> evalCandidate c ts horizon bar) candidates

          -- Filter: admissible AND clears bar
          clearing = filter (\e -> ceAdmissible e && ceClearsBar e) evals

      if null clearing
        then do
          -- Idle tick: increment horizon
          let newH = min (ssHorizon st + 1) (scHMax cfg)
          synthLoop cfg st
            { ssTick       = ssTick st + 1
            , ssHorizon    = newH
            , ssIdleStreak = ssIdleStreak st + 1
            }
        else do
          -- Select winner
          let winner = selectWinner clearing
              winCand = ceCandidate winner
              winEntry = candidateToEntry winCand
              winNu = ceNu winner
              winKappa = ceKappa winner
              winName = ceName winner

              -- Fibonacci integration gap
              delta = fibDelta nextN
              tau = fibTau nextN

              -- After realization: H = delta + 1 (resets to 2, then grows by delta-1)
              newH = min (delta + 1) (scHMax cfg)

              phi = computePhi nextN
              omega = computeOmega (ssCumNu st) (ssCumKappa st)

              result = SynthResult
                { srN         = nextN
                , srTau       = tau
                , srDelta     = delta
                , srNu        = winNu
                , srKappa     = winKappa
                , srH         = horizon
                , srName      = winName
                , srRho       = ceRho winner
                , srPhi       = phi
                , srOmega     = omega
                , srBar       = bar
                , srOvershoot = ceOvershoot winner
                , srCleared   = True
                , srCandName  = showCandType winCand
                }

              -- Update theory state
              newTheory = addToTheory winEntry (ssTheory st)

          synthLoop cfg st
            { ssTheory     = newTheory
            , ssTick       = ssTick st + 1
            , ssRealN      = nextN
            , ssHorizon    = newH
            , ssCumNu      = ssCumNu st + winNu
            , ssCumKappa   = ssCumKappa st + winKappa
            , ssIdleStreak = 0
            , ssResults    = result : ssResults st
            }

-- | Show the type of a candidate for diagnostics
showCandType :: Candidate -> String
showCandType (CFoundation _)  = "Foundation"
showCandType (CFormer _)      = "Former"
showCandType (CHIT _)         = "HIT"
showCandType (CSusp _)        = "Suspension"
showCandType (CMap _ _ _)     = "Map"
showCandType (CAlgebra _ _)   = "Algebra"
showCandType (CModal _ _)     = "Modal"
showCandType (CAxiom _ _)     = "Axiom"
showCandType (CSynthesis _ _) = "Synthesis"

-- ============================================
-- Output formatting
-- ============================================

-- | Format synthesis results as a table
formatSynthTable :: [SynthResult] -> String
formatSynthTable results = unlines $
  [ header, separator ] ++ map formatRow results ++ [separator, summary]
  where
    header = " n  | tau  | Structure      | Type       | Delta | nu  | kappa | rho    | Phi   | Omega  | Bar    | Cleared"
    separator = "----|------|----------------|------------|-------|-----|-------|--------|-------|--------|--------|--------"

    formatRow r =
      padL 3 (show (srN r)) ++ " | "
      ++ padL 4 (show (srTau r)) ++ " | "
      ++ padR 14 (srName r) ++ " | "
      ++ padR 10 (srCandName r) ++ " | "
      ++ padL 5 (show (srDelta r)) ++ " | "
      ++ padL 3 (show (srNu r)) ++ " | "
      ++ padL 5 (show (srKappa r)) ++ " | "
      ++ padL 6 (showF2 (srRho r)) ++ " | "
      ++ padL 5 (if srN r <= 1 then "  ---" else showF2 (srPhi r)) ++ " | "
      ++ padL 6 (if srN r <= 1 then "   ---" else showF2 (srOmega r)) ++ " | "
      ++ padL 6 (if srN r <= 1 then "   ---" else showF2 (srBar r)) ++ " | "
      ++ (if srCleared r then "YES" else " NO")

    nCleared = length (filter srCleared results)
    nTotal   = length results
    summary  = show nCleared ++ "/" ++ show nTotal ++ " structures discovered via synthesis."

-- | Format a side-by-side comparison of synthesis vs Genesis
formatSynthComparison :: [SynthResult] -> String
formatSynthComparison results = unlines $
  [ "Side-by-side: Synthesis vs Genesis"
  , "-----------------------------------"
  , " n  | Synthesized    | Genesis        | nu_synth | nu_paper | Match"
  , "----|----------------|----------------|----------|----------|------"
  ] ++ map formatCompRow (zip [1..] results)
  where
    genesisNames :: [String]
    genesisNames = [ "Universe", "Unit", "Witness", "Pi/Sigma", "S1", "PropTrunc"
                   , "S2", "S3", "Hopf", "Cohesion"
                   , "Connections", "Curvature", "Metric", "Hilbert", "DCT" ]

    paperNus :: [Int]
    paperNus = [1, 1, 2, 5, 7, 8, 10, 18, 18, 20, 26, 34, 43, 60, 150]

    formatCompRow :: (Int, SynthResult) -> String
    formatCompRow (i, r) =
      let gName = if i <= length genesisNames then genesisNames !! (i-1) else "???"
          pNu   = if i <= length paperNus then paperNus !! (i-1) else 0
          nameMatch = srName r == gName
                   || (srName r == "S1" && gName == "S1")
                   || (srName r == "Universe" && gName == "Universe")
          nuMatch = abs (srNu r - pNu) <= max 1 (pNu * 30 `div` 100)  -- +-30%
          matchStr = if nameMatch && nuMatch then "YES"
                     else if nameMatch then "~nu"
                     else " NO"
      in padL 3 (show i) ++ " | "
         ++ padR 14 (srName r) ++ " | "
         ++ padR 14 gName ++ " | "
         ++ padL 8 (show (srNu r)) ++ " | "
         ++ padL 8 (show pNu) ++ " | "
         ++ matchStr

-- ============================================
-- Formatting helpers
-- ============================================

showF2 :: Double -> String
showF2 x = show (fromIntegral (round (x * 100) :: Int) / 100.0 :: Double)

padL :: Int -> String -> String
padL n s = replicate (max 0 (n - length s)) ' ' ++ s

padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '
