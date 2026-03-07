{-# LANGUAGE BangPatterns #-}

-- | Lightweight proof/search state for obligation-driven exploration.
-- This module keeps search guidance structural and name-independent.
module ProofState
  ( CapabilitySig(..)
  , Obligation(..)
  , SearchState(..)
  , deriveSearchState
  , actionGoalGain
  , isSafeAction
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (TeleEntry(..))
import TelescopeGen (GoalProfile(..), GoalIntent(..), Action(..))
import Types (Library, LibraryEntry(..))

import Data.List (nub)

data CapabilitySig = CapabilitySig
  { capHasUniverse     :: !Bool
  , capHasWitness      :: !Bool
  , capHasDepFns       :: !Bool
  , capMaxPathDim      :: !Int
  , capModalCount      :: !Int
  , capHasDifferential :: !Bool
  , capHasHilbert      :: !Bool
  , capHasTemporal     :: !Bool
  } deriving (Eq, Ord, Show)

data Obligation
  = NeedTypeFormer
  | NeedWitness
  | NeedLoop !Int
  | NeedModalBundle
  | NeedDifferentialBridge
  | NeedTemporalBridge
  | NeedCoherence
  deriving (Eq, Ord, Show)

data SearchState = SearchState
  { ssEntries :: ![TeleEntry]
  , ssGoals   :: ![Obligation]
  , ssCaps    :: !CapabilitySig
  , ssPremises :: ![Int]
  , ssKappa   :: !Int
  , ssNuLower :: !Int
  , ssNuUpper :: !Int
  } deriving (Eq, Ord, Show)

deriveSearchState :: Library -> GoalProfile -> [TeleEntry] -> SearchState
deriveSearchState lib profile entries =
  let caps = mergeCaps (capsFromLibrary lib) (capsFromEntries entries)
      goals = deriveObligations profile caps
      prem = topPremises lib entries
      kappaApprox = length entries
      nuLower = capabilityScore caps
      nuUpper = nuLower + 2 * length goals
  in SearchState
      { ssEntries = entries
      , ssGoals = goals
      , ssCaps = caps
      , ssPremises = prem
      , ssKappa = kappaApprox
      , ssNuLower = nuLower
      , ssNuUpper = nuUpper
      }

deriveObligations :: GoalProfile -> CapabilitySig -> [Obligation]
deriveObligations profile caps =
  nub (concatMap fromIntent (gpIntents profile) ++ coherence)
  where
    fromIntent intent = case intent of
      NeedBootstrap ->
        [NeedWitness | not (capHasWitness caps)]
      NeedFormer ->
        [NeedTypeFormer | not (capHasDepFns caps)]
      NeedHIT ->
        [NeedLoop 1 | capMaxPathDim caps < 1]
      NeedModal ->
        [NeedModalBundle | capModalCount caps < 3]
      NeedDifferential ->
        [NeedDifferentialBridge | not (capHasDifferential caps)]
      NeedTemporal ->
        [NeedTemporalBridge | not (capHasTemporal caps)]
      NeedBridge ->
        [NeedTypeFormer | not (capHasDepFns caps)]
     ++ [NeedModalBundle | capModalCount caps < 2]
    coherence =
      [NeedCoherence | capHasDepFns caps && (capHasWitness caps || capMaxPathDim caps > 0)]

topPremises :: Library -> [TeleEntry] -> [Int]
topPremises lib entries =
  let n = length lib
      recent = [n, n - 1, n - 2]
      refs = nub [i | e <- entries, i <- exprRefs (teType e)]
  in take 4 (refs ++ [i | i <- recent, i >= 1])

capabilityScore :: CapabilitySig -> Int
capabilityScore caps =
    (if capHasUniverse caps then 1 else 0)
  + (if capHasWitness caps then 2 else 0)
  + (if capHasDepFns caps then 3 else 0)
  + capMaxPathDim caps
  + capModalCount caps
  + (if capHasDifferential caps then 2 else 0)
  + (if capHasHilbert caps then 2 else 0)
  + (if capHasTemporal caps then 2 else 0)

actionGoalGain :: SearchState -> Action -> Int
actionGoalGain st act =
  sum [gain g act | g <- ssGoals st]
  where
    gain NeedTypeFormer a = case a of
      APi -> 4
      ASigma -> 4
      ALam -> 2
      AApp -> 1
      _ -> 0
    gain NeedWitness a = case a of
      AVar _ -> 3
      ALib _ -> 3
      AApp -> 2
      _ -> 0
    gain (NeedLoop _) a = case a of
      APathCon _ -> 4
      ASusp -> 3
      ATrunc -> 3
      AId -> 2
      _ -> 0
    gain NeedModalBundle a = case a of
      AFlat -> 3
      ASharp -> 3
      ADisc -> 3
      AShape -> 3
      _ -> 0
    gain NeedDifferentialBridge a = case a of
      APi -> 2
      ASigma -> 2
      AApp -> 2
      _ -> 0
    gain NeedTemporalBridge a = case a of
      ANext -> 4
      AEventually -> 4
      _ -> 0
    gain NeedCoherence a = case a of
      ARefl -> 2
      AId -> 2
      ALam -> 1
      AApp -> 1
      _ -> 0

isSafeAction :: Action -> Bool
isSafeAction a = case a of
  AUniv -> True
  AVar _ -> True
  ALib _ -> True
  ARefl -> True
  AApp -> True
  _ -> False

capsFromLibrary :: Library -> CapabilitySig
capsFromLibrary lib =
  CapabilitySig
    { capHasUniverse = any ((== "Universe") . leName) lib
    , capHasWitness = any (\e -> leConstructors e > 0) lib
    , capHasDepFns = any leHasDependentFunctions lib
    , capMaxPathDim = maximum0 (concatMap lePathDims lib)
    , capModalCount = length (filter id
        [ any leHasModalOps lib
        , any (\e -> leHasModalOps e && leConstructors e > 0) lib
        , any (\e -> leHasModalOps e && leHasLoop e) lib
        ])
    , capHasDifferential = any leHasDifferentialOps lib
    , capHasHilbert = any leHasHilbert lib
    , capHasTemporal = any leHasTemporalOps lib
    }

capsFromEntries :: [TeleEntry] -> CapabilitySig
capsFromEntries entries =
  let exprs = map teType entries
      mods = [() | e <- exprs, hasModalExpr e]
  in CapabilitySig
      { capHasUniverse = any hasUniverseExpr exprs
      , capHasWitness = any hasWitnessExpr exprs
      , capHasDepFns = any hasDepFnExpr exprs
      , capMaxPathDim = maximum0 (concatMap pathDims exprs)
      , capModalCount = min 4 (length mods)
      , capHasDifferential = any hasDifferentialExpr exprs
      , capHasHilbert = any hasHilbertExpr exprs
      , capHasTemporal = any hasTemporalExpr exprs
      }

mergeCaps :: CapabilitySig -> CapabilitySig -> CapabilitySig
mergeCaps a b =
  CapabilitySig
    { capHasUniverse = capHasUniverse a || capHasUniverse b
    , capHasWitness = capHasWitness a || capHasWitness b
    , capHasDepFns = capHasDepFns a || capHasDepFns b
    , capMaxPathDim = max (capMaxPathDim a) (capMaxPathDim b)
    , capModalCount = max (capModalCount a) (capModalCount b)
    , capHasDifferential = capHasDifferential a || capHasDifferential b
    , capHasHilbert = capHasHilbert a || capHasHilbert b
    , capHasTemporal = capHasTemporal a || capHasTemporal b
    }

maximum0 :: [Int] -> Int
maximum0 [] = 0
maximum0 xs = maximum xs

hasUniverseExpr :: MBTTExpr -> Bool
hasUniverseExpr Univ = True
hasUniverseExpr (App Univ _) = True
hasUniverseExpr (Lam a) = hasUniverseExpr a
hasUniverseExpr (Refl a) = hasUniverseExpr a
hasUniverseExpr (Susp a) = hasUniverseExpr a
hasUniverseExpr (Trunc a) = hasUniverseExpr a
hasUniverseExpr (Flat a) = hasUniverseExpr a
hasUniverseExpr (Sharp a) = hasUniverseExpr a
hasUniverseExpr (Disc a) = hasUniverseExpr a
hasUniverseExpr (Shape a) = hasUniverseExpr a
hasUniverseExpr (Next a) = hasUniverseExpr a
hasUniverseExpr (Eventually a) = hasUniverseExpr a
hasUniverseExpr (Pi a b) = hasUniverseExpr a || hasUniverseExpr b
hasUniverseExpr (Sigma a b) = hasUniverseExpr a || hasUniverseExpr b
hasUniverseExpr (App a b) = hasUniverseExpr a || hasUniverseExpr b
hasUniverseExpr (Id a x y) = hasUniverseExpr a || hasUniverseExpr x || hasUniverseExpr y
hasUniverseExpr _ = False

hasWitnessExpr :: MBTTExpr -> Bool
hasWitnessExpr (Var _) = True
hasWitnessExpr (App (Lib _) (Var _)) = True
hasWitnessExpr (App Univ _) = True
hasWitnessExpr _ = False

hasDepFnExpr :: MBTTExpr -> Bool
hasDepFnExpr (Pi _ _) = True
hasDepFnExpr (Sigma _ _) = True
hasDepFnExpr (Lam _) = True
hasDepFnExpr _ = False

pathDims :: MBTTExpr -> [Int]
pathDims (PathCon d) = [d]
pathDims (Lam a) = pathDims a
pathDims (Refl a) = pathDims a
pathDims (Susp a) = pathDims a
pathDims (Trunc a) = pathDims a
pathDims (Flat a) = pathDims a
pathDims (Sharp a) = pathDims a
pathDims (Disc a) = pathDims a
pathDims (Shape a) = pathDims a
pathDims (Next a) = pathDims a
pathDims (Eventually a) = pathDims a
pathDims (Pi a b) = pathDims a ++ pathDims b
pathDims (Sigma a b) = pathDims a ++ pathDims b
pathDims (App a b) = pathDims a ++ pathDims b
pathDims (Id a x y) = pathDims a ++ pathDims x ++ pathDims y
pathDims _ = []

hasModalExpr :: MBTTExpr -> Bool
hasModalExpr (Flat _) = True
hasModalExpr (Sharp _) = True
hasModalExpr (Disc _) = True
hasModalExpr (Shape _) = True
hasModalExpr _ = False

hasTemporalExpr :: MBTTExpr -> Bool
hasTemporalExpr (Next _) = True
hasTemporalExpr (Eventually _) = True
hasTemporalExpr _ = False

hasDifferentialExpr :: MBTTExpr -> Bool
hasDifferentialExpr (Pi _ _) = True
hasDifferentialExpr (Sigma _ _) = True
hasDifferentialExpr _ = False

hasHilbertExpr :: MBTTExpr -> Bool
hasHilbertExpr (Sigma (Pi _ _) (Pi _ _)) = True
hasHilbertExpr _ = False

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

