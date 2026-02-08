-- | Theory state tracking for synthesis
--
-- Tracks the evolving type theory: which type formers are available,
-- what's in the library, and the current simulation step.

module TheoryState
  ( TypeFormer(..)
  , TheoryState(..)
  , initialTheoryState
  , addToTheory
  , hasFormer
  , theoryLibrary
  ) where

import Types
import qualified Data.Set as Set

-- ============================================
-- Types
-- ============================================

data TypeFormer = FPi | FSigma | FId | FSusp | FTrunc | FHIT | FFibration | FModal
  deriving (Eq, Ord, Show)

data TheoryState = TheoryState
  { tsLibrary :: Library
  , tsFormers :: Set.Set TypeFormer
  , tsStep    :: Int
  } deriving (Show)

-- ============================================
-- Operations
-- ============================================

-- | Initial theory state: only HIT formation is available
initialTheoryState :: TheoryState
initialTheoryState = TheoryState
  { tsLibrary = []
  , tsFormers = Set.singleton FHIT
  , tsStep    = 0
  }

-- | Add a library entry to the theory state and update step counter.
-- Also updates available formers based on what was added.
addToTheory :: LibraryEntry -> TheoryState -> TheoryState
addToTheory entry ts = ts
  { tsLibrary = tsLibrary ts ++ [entry]
  , tsFormers = newFormers
  , tsStep    = tsStep ts + 1
  }
  where
    name = leName entry
    oldFormers = tsFormers ts
    newFormers
      -- Adding Pi/Sigma unlocks Pi, Sigma, Id, and Susp formers
      | name == "Pi" = Set.union oldFormers
          (Set.fromList [FPi, FSigma, FId, FSusp])
      -- Adding Trunc unlocks truncation former
      | name == "Trunc" = Set.insert FTrunc oldFormers
      -- Adding Hopf unlocks fibration former
      | name == "Hopf" = Set.insert FFibration oldFormers
      -- Adding Cohesion unlocks modal former
      | name == "Cohesion" = Set.insert FModal oldFormers
      | otherwise = oldFormers

-- | Check if a type former is available
hasFormer :: TypeFormer -> TheoryState -> Bool
hasFormer f ts = Set.member f (tsFormers ts)

-- | Get the current library
theoryLibrary :: TheoryState -> Library
theoryLibrary = tsLibrary
