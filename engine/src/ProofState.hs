{-# LANGUAGE BangPatterns #-}

-- | Lightweight proof/search state for obligation-driven exploration.
-- This module keeps search guidance structural and name-independent.
module ProofState
  ( CapabilitySig(..)
  , PremiseBuckets(..)
  , Obligation(..)
  , DebtGraph(..)
  , CriticPlan(..)
  , CriticPlanState(..)
  , SearchState(..)
  , deriveSearchState
  , advanceSearchState
  , stateOpenGoals
  , stateGoalCount
  , stateCriticalGoal
  , stateWithDebtGraph
  , criticPlanStates
  , criticRewriteStates
  , actionGoalGain
  , actionGoalGainFor
  , isSafeAction
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (TeleEntry(..))
import TelescopeGen (GoalProfile(..), GoalIntent(..), Action(..))
import Types (Library, LibraryEntry(..))

import Data.List (nub, sort, sortOn)
import Data.Ord (Down(..))

data CapabilitySig = CapabilitySig
  { capHasUniverse     :: !Bool
  , capHasWitness      :: !Bool
  , capHasDepFns       :: !Bool
  , capHasTruncation   :: !Bool
  , capMaxPathDim      :: !Int
  , capModalCount      :: !Int
  , capHasDifferential :: !Bool
  , capHasCurvature    :: !Bool
  , capHasMetric       :: !Bool
  , capHasHilbert      :: !Bool
  , capHasTemporal     :: !Bool
  } deriving (Eq, Ord, Show)

data Obligation
  = NeedTypeFormer
  | NeedWitness
  | NeedLoop !Int
  | NeedLoopLift !Int
  | NeedLoopCoherence !Int
  | NeedTruncBridge
  | NeedSourceReuse !Int !Bool
  | NeedTargetReuse !Int !(Maybe Int)
  | NeedMapHead
  | NeedFiberWitness
  | NeedTransportLaw
  | NeedCompatibilityPath !Int
  | NeedSealBridge
  | NeedModalBundle
  | NeedDifferentialBridge
  | NeedCurvatureBridge
  | NeedMetricBundle
  | NeedHilbertBundle
  | NeedTemporalBridge
  | NeedCoherence
  deriving (Eq, Ord, Show)

data PremiseBuckets = PremiseBuckets
  { pbSource :: ![Int]
  , pbTarget :: ![Int]
  , pbLaw    :: ![Int]
  , pbAux    :: ![Int]
  } deriving (Eq, Ord, Show)

-- | Hypergraph-style debt model:
-- - open atoms are unresolved obligations
-- - AND clauses require all listed atoms
-- - OR clauses require at least one listed atom
data DebtGraph = DebtGraph
  { dgOpenAtoms  :: ![Obligation]
  , dgAndClauses :: ![[Obligation]]
  , dgOrClauses  :: ![[Obligation]]
  } deriving (Eq, Ord, Show)

data CriticPlan = CriticPlan
  { cpLabel :: !String
  , cpFocus :: !(Maybe Obligation)
  , cpAddsAtoms :: ![Obligation]
  , cpAndChain :: ![[Obligation]]
  , cpOrChoices :: ![[Obligation]]
  , cpExpectedClose :: ![Obligation]
  } deriving (Eq, Ord, Show)

data CriticPlanState = CriticPlanState
  { cpsPlan :: !CriticPlan
  , cpsState :: !SearchState
  } deriving (Eq, Ord, Show)

data SearchState = SearchState
  { ssEntries   :: ![TeleEntry]
  , ssGoals     :: ![Obligation] -- compatibility mirror of open debt atoms
  , ssDebtGraph :: !DebtGraph
  , ssCaps      :: !CapabilitySig
  , ssPremiseBuckets :: !PremiseBuckets
  , ssPremises  :: ![Int]
  , ssKappa     :: !Int
  , ssNuLower   :: !Int
  , ssNuUpper   :: !Int
  } deriving (Eq, Ord, Show)

deriveSearchState :: Library -> GoalProfile -> [TeleEntry] -> SearchState
deriveSearchState lib profile entries =
  let baselineCaps = capsFromLibrary lib
      caps = mergeCaps baselineCaps (capsFromEntries entries)
      goals = deriveObligations profile caps baselineCaps
      debt = reduceDebtByCaps caps (buildDebtGraph profile goals)
      buckets = topPremiseBuckets lib (dgOpenAtoms debt) entries
      prem = flattenPremiseBuckets buckets
      kappaApprox = length entries
      nuLower = capabilityScore caps
      nuUpper = nuLower + 2 * debtGoalCount debt
  in SearchState
      { ssEntries = entries
      , ssGoals = dgOpenAtoms debt
      , ssDebtGraph = debt
      , ssCaps = caps
      , ssPremiseBuckets = buckets
      , ssPremises = prem
      , ssKappa = kappaApprox
      , ssNuLower = nuLower
      , ssNuUpper = nuUpper
      }

advanceSearchState :: Library -> GoalProfile -> SearchState -> Action -> MBTTExpr -> [TeleEntry] -> SearchState
advanceSearchState lib profile prev act expr entries =
  let baselineCaps = capsFromLibrary lib
      caps = mergeCaps baselineCaps (capsFromEntries entries)
      profileDebt = buildDebtGraph profile (deriveObligations profile caps baselineCaps)
      mergedDebt = mergeDebtGraphs (ssDebtGraph prev) profileDebt
      movedDebt = dischargeByMove caps act expr mergedDebt
      reducedDebt = reduceDebtByCaps caps movedDebt
      buckets = topPremiseBuckets lib (dgOpenAtoms reducedDebt) entries
      prem = flattenPremiseBuckets buckets
      kappaApprox = length entries
      nuLower = capabilityScore caps
      nuUpper = nuLower + 2 * debtGoalCount reducedDebt
  in SearchState
      { ssEntries = entries
      , ssGoals = dgOpenAtoms reducedDebt
      , ssDebtGraph = reducedDebt
      , ssCaps = caps
      , ssPremiseBuckets = buckets
      , ssPremises = prem
      , ssKappa = kappaApprox
      , ssNuLower = nuLower
      , ssNuUpper = nuUpper
      }

stateOpenGoals :: SearchState -> [Obligation]
stateOpenGoals = dgOpenAtoms . ssDebtGraph

stateGoalCount :: SearchState -> Int
stateGoalCount = debtGoalCount . ssDebtGraph

stateCriticalGoal :: SearchState -> Maybe Obligation
stateCriticalGoal st = case ranked of
  [] -> Nothing
  (g:_) -> Just g
  where
    dg = ssDebtGraph st
    goals = dgOpenAtoms dg
    score g =
      4 * countMembership g (dgAndClauses dg)
      + 3 * countMembership g (dgOrClauses dg)
      + obligationPriority g
    ranked = sortOn (\g -> (Down (score g), Down (obligationPriority g), g)) goals

stateWithDebtGraph :: SearchState -> DebtGraph -> SearchState
stateWithDebtGraph st debtRaw =
  let debt = reduceDebtByCaps (ssCaps st) (normalizeDebtGraph debtRaw)
      nuLower = capabilityScore (ssCaps st)
      nuUpper = nuLower + 2 * debtGoalCount debt
  in st
      { ssGoals = dgOpenAtoms debt
      , ssDebtGraph = debt
      , ssNuLower = nuLower
      , ssNuUpper = nuUpper
      }

criticPlanStates :: Action -> SearchState -> [CriticPlanState]
criticPlanStates act st =
  let plans = criticPlans act (stateCriticalGoal st)
      mkPlanState plan =
        let dg = applyCriticPlan (ssDebtGraph st) plan
            st' = stateWithDebtGraph st dg
        in CriticPlanState plan st'
      changed = [cps | cps <- map mkPlanState plans, ssDebtGraph (cpsState cps) /= ssDebtGraph st]
  in nub changed

criticRewriteStates :: Action -> SearchState -> [SearchState]
criticRewriteStates act st = [cpsState cps | cps <- criticPlanStates act st]

criticPlans :: Action -> Maybe Obligation -> [CriticPlan]
criticPlans act focus = case act of
  APi ->
    bridgePiPlans ++
    [ mk "former-from-witness" [NeedTypeFormer, NeedWitness] [[NeedTypeFormer, NeedWitness]] [[NeedTypeFormer, NeedCoherence]] [NeedTypeFormer]
    , mk "former-needs-coherence" [NeedTypeFormer, NeedCoherence] [[NeedTypeFormer, NeedCoherence]] [[NeedTypeFormer, NeedWitness]] [NeedTypeFormer]
    ]
    where
      bridgePiPlans = case focus of
        Just NeedMapHead ->
          [ mk "bridge-map-head" [NeedMapHead, NeedSourceReuse 1 True] [[NeedMapHead, NeedSourceReuse 1 True]] [[NeedMapHead, NeedTargetReuse 2 Nothing]] [NeedMapHead]
          ]
        Just NeedSealBridge ->
          [ mk "bridge-seal-head" [NeedSealBridge, NeedTransportLaw] [[NeedSealBridge, NeedTransportLaw]] [[NeedSealBridge, NeedCompatibilityPath 2]] [NeedSealBridge]
          ]
        _ -> []
  ASigma ->
    bridgeSigmaPlans ++
    [ mk "sigma-from-witness" [NeedTypeFormer, NeedWitness] [[NeedTypeFormer, NeedWitness]] [[NeedTypeFormer, NeedCoherence]] [NeedTypeFormer]
    ]
    where
      bridgeSigmaPlans = case focus of
        Just NeedFiberWitness ->
          [ mk "bridge-fiber-sigma" [NeedFiberWitness, NeedSourceReuse 1 True] [[NeedFiberWitness, NeedSourceReuse 1 True]] [[NeedFiberWitness, NeedTargetReuse 2 Nothing]] [NeedFiberWitness]
          ]
        _ -> []
  APathCon _ ->
    let d = loopDemand focus
        liftGoal = NeedLoopLift d
        cohGoal = NeedLoopCoherence (max 1 (d - 1))
    in
      if d <= 2
      then
        [ mk "loop-from-former" [NeedLoop 1, NeedTypeFormer] [[NeedLoop 1, NeedTypeFormer]] [[NeedLoop 1, NeedCoherence]] [NeedLoop 1]
        , mk "loop-from-coherence" [NeedLoop 1, NeedCoherence] [[NeedLoop 1, NeedCoherence]] [[NeedLoop 1, NeedTypeFormer]] [NeedLoop 1]
        ]
      else
        [ mk "loop-lift-needs-coherence" [liftGoal, cohGoal] [[liftGoal, cohGoal]] [[liftGoal, NeedTruncBridge]] [liftGoal]
        , mk "loop-lift-needs-trunc" [liftGoal, NeedTruncBridge] [[liftGoal, NeedTruncBridge]] [[liftGoal, cohGoal]] [liftGoal]
        ]
  ASusp ->
    let d = loopDemand focus
        liftGoal = NeedLoopLift d
        cohGoal = NeedLoopCoherence (max 1 (d - 1))
    in
      if d <= 2
      then
        [ mk "susp-loop-chain" [NeedLoop 1, NeedTypeFormer] [[NeedLoop 1, NeedTypeFormer], [NeedLoop 1, NeedCoherence]] [] [NeedLoop 1] ]
      else
        [ mk "susp-builds-trunc-bridge" [NeedTruncBridge, cohGoal] [[NeedTruncBridge, cohGoal]] [[liftGoal, NeedTruncBridge]] [NeedTruncBridge]
        , mk "susp-loop-chain" [liftGoal, NeedTypeFormer] [[liftGoal, NeedTypeFormer], [liftGoal, cohGoal]] [] [liftGoal]
        ]
  ATrunc ->
    let d = loopDemand focus
        liftGoal = NeedLoopLift d
        cohGoal = NeedLoopCoherence (max 1 d)
    in
      if d <= 2
      then
        [ mk "trunc-loop-chain" [NeedLoop 1, NeedTypeFormer] [[NeedLoop 1, NeedTypeFormer], [NeedLoop 1, NeedCoherence]] [[NeedLoop 1, NeedCoherence]] [NeedLoop 1] ]
      else
        [ mk "trunc-bridge-chain" [NeedTruncBridge, cohGoal] [[NeedTruncBridge, cohGoal]] [[liftGoal, cohGoal]] [NeedTruncBridge]
        , mk "trunc-loop-chain" [liftGoal, NeedTypeFormer] [[liftGoal, NeedTypeFormer], [liftGoal, cohGoal]] [[liftGoal, NeedTruncBridge]] [liftGoal]
        ]
  AId ->
    let d = loopDemand focus
        cohGoal = NeedLoopCoherence (max 1 d)
        bridgeIdPlans = case focus of
          Just NeedTransportLaw ->
            [ mk "bridge-transport-law" [NeedTransportLaw, NeedMapHead] [[NeedTransportLaw, NeedMapHead]] [[NeedTransportLaw, NeedCompatibilityPath 2]] [NeedTransportLaw]
            ]
          Just (NeedCompatibilityPath dim) ->
            [ mk "bridge-compatibility-path" [NeedCompatibilityPath dim, NeedTransportLaw] [[NeedCompatibilityPath dim, NeedTransportLaw]] [[NeedCompatibilityPath dim, NeedMapHead]] [NeedCompatibilityPath dim]
            ]
          _ -> []
    in
      if d <= 2
      then
        bridgeIdPlans ++
        [ mk "id-coherence-chain" [NeedCoherence, NeedTypeFormer] [[NeedCoherence, NeedTypeFormer]] [[NeedCoherence, NeedLoop 1]] [NeedCoherence] ]
      else
        bridgeIdPlans ++
        [ mk "id-loop-coherence-chain" [cohGoal, NeedTypeFormer] [[cohGoal, NeedTypeFormer]] [[cohGoal, NeedTruncBridge]] [cohGoal]
        , mk "id-coherence-chain" [NeedCoherence, NeedTypeFormer] [[NeedCoherence, NeedTypeFormer]] [[NeedCoherence, NeedLoop d]] [NeedCoherence]
        ]
  AFlat ->
    [ mk "modal-via-former" [NeedModalBundle, NeedTypeFormer] [[NeedModalBundle, NeedTypeFormer]] [[NeedModalBundle, NeedCoherence]] [NeedModalBundle] ]
  ASharp ->
    [ mk "modal-via-coherence" [NeedModalBundle, NeedCoherence] [[NeedModalBundle, NeedCoherence]] [[NeedModalBundle, NeedTypeFormer]] [NeedModalBundle] ]
  ADisc ->
    [ mk "modal-bundle-chain" [NeedModalBundle, NeedTypeFormer, NeedCoherence] [[NeedModalBundle, NeedTypeFormer], [NeedModalBundle, NeedCoherence]] [] [NeedModalBundle] ]
  AShape ->
    [ mk "modal-shape-chain" [NeedModalBundle, NeedTypeFormer] [[NeedModalBundle, NeedTypeFormer]] [[NeedModalBundle, NeedCoherence]] [NeedModalBundle] ]
  ANext ->
    [ mk "temporal-needs-modal" [NeedTemporalBridge, NeedModalBundle] [[NeedTemporalBridge, NeedModalBundle]] [[NeedTemporalBridge, NeedCoherence]] [NeedTemporalBridge] ]
  AEventually ->
    [ mk "eventual-needs-modal" [NeedTemporalBridge, NeedModalBundle] [[NeedTemporalBridge, NeedModalBundle]] [[NeedTemporalBridge, NeedCoherence]] [NeedTemporalBridge] ]
  ALib _ ->
    bridgeReusePlans ++
    [ mk "reuse-needs-witness" [NeedWitness, NeedTypeFormer] [[NeedWitness, NeedTypeFormer]] [[NeedWitness, NeedCoherence]] [NeedWitness] ]
    where
      bridgeReusePlans = case focus of
        Just (NeedSourceReuse d loopReq) ->
          [ mk "bridge-source-reuse" [NeedSourceReuse d loopReq, NeedMapHead] [[NeedSourceReuse d loopReq, NeedMapHead]] [[NeedSourceReuse d loopReq, NeedFiberWitness]] [NeedSourceReuse d loopReq]
          ]
        Just (NeedTargetReuse d maxD) ->
          [ mk "bridge-target-reuse" [NeedTargetReuse d maxD, NeedMapHead] [[NeedTargetReuse d maxD, NeedMapHead]] [[NeedTargetReuse d maxD, NeedCompatibilityPath d]] [NeedTargetReuse d maxD]
          ]
        _ -> []
  AVar _ ->
    bridgeVarPlans ++
    [ mk "var-needs-witness" [NeedWitness, NeedTypeFormer] [[NeedWitness, NeedTypeFormer]] [[NeedWitness, NeedCoherence]] [NeedWitness] ]
    where
      bridgeVarPlans = case focus of
        Just NeedFiberWitness ->
          [ mk "bridge-fiber-witness" [NeedFiberWitness, NeedSourceReuse 1 True] [[NeedFiberWitness, NeedSourceReuse 1 True]] [[NeedFiberWitness, NeedMapHead]] [NeedFiberWitness]
          ]
        _ -> []
  AApp ->
    bridgeAppPlans ++
    [ mk "app-witness-chain" [NeedWitness, NeedTypeFormer] [[NeedWitness, NeedTypeFormer]] [[NeedWitness, NeedCoherence]] [NeedWitness] ]
    where
      bridgeAppPlans = case focus of
        Just NeedMapHead ->
          [ mk "bridge-map-apply" [NeedMapHead, NeedSourceReuse 1 True] [[NeedMapHead, NeedSourceReuse 1 True]] [[NeedMapHead, NeedTargetReuse 2 Nothing]] [NeedMapHead]
          ]
        Just NeedTransportLaw ->
          [ mk "bridge-transport-apply" [NeedTransportLaw, NeedCompatibilityPath 2] [[NeedTransportLaw, NeedCompatibilityPath 2]] [[NeedTransportLaw, NeedMapHead]] [NeedTransportLaw]
          ]
        _ -> []
  ALam ->
    [ mk "lambda-coherence-chain" [NeedCoherence, NeedTypeFormer] [[NeedCoherence, NeedTypeFormer]] [[NeedCoherence, NeedWitness]] [NeedCoherence] ]
  ARefl ->
    let d = loopDemand focus
        cohGoal = NeedLoopCoherence (max 1 d)
    in
      if d <= 2
      then
        [ mk "refl-coherence-loop" [NeedCoherence, NeedLoop 1] [[NeedCoherence, NeedLoop 1]] [[NeedCoherence, NeedTypeFormer]] [NeedCoherence] ]
      else
        [ mk "refl-loop-coherence" [cohGoal] [[cohGoal]] [[cohGoal, NeedTypeFormer]] [cohGoal]
        , mk "refl-coherence-loop" [NeedCoherence, NeedLoop d] [[NeedCoherence, NeedLoop d]] [[NeedCoherence, NeedTypeFormer]] [NeedCoherence]
        ]
  AUniv ->
    [ mk "univ-bootstrap-chain" [NeedWitness, NeedTypeFormer] [[NeedWitness, NeedTypeFormer]] [[NeedWitness, NeedCoherence]] [NeedWitness] ]
  where
    mk label atoms ands ors expected =
      CriticPlan
        { cpLabel = label
        , cpFocus = focus
        , cpAddsAtoms = atoms
        , cpAndChain = ands
        , cpOrChoices = ors
        , cpExpectedClose = expected
        }
    loopDemand maybeGoal = case maybeGoal of
      Just (NeedLoop d) -> max 1 d
      Just (NeedLoopLift d) -> max 1 d
      Just (NeedLoopCoherence d) -> max 1 d
      _ -> 1

applyCriticPlan :: DebtGraph -> CriticPlan -> DebtGraph
applyCriticPlan dg plan =
  normalizeDebtGraph DebtGraph
    { dgOpenAtoms = cpAddsAtoms plan ++ dgOpenAtoms dg
    , dgAndClauses = cpAndChain plan ++ dgAndClauses dg
    , dgOrClauses = cpOrChoices plan ++ dgOrClauses dg
    }

buildDebtGraph :: GoalProfile -> [Obligation] -> DebtGraph
buildDebtGraph profile goals =
  let ands = concatMap intentAnd (gpIntents profile)
      ors = concatMap intentOr (gpIntents profile)
  in normalizeDebtGraph DebtGraph
      { dgOpenAtoms = goals ++ concat ands ++ concat ors
      , dgAndClauses = ands
      , dgOrClauses = ors
      }

intentAnd :: GoalIntent -> [[Obligation]]
intentAnd intent = case intent of
  NeedBootstrap -> [[NeedWitness]]
  NeedFormer -> [[NeedTypeFormer, NeedWitness]]
  NeedHIT -> [[NeedLoop 1, NeedCoherence]]
  NeedModal -> [[NeedModalBundle]]
  NeedDifferential -> [[NeedModalBundle, NeedDifferentialBridge]]
  NeedCurvature -> [[NeedDifferentialBridge, NeedCurvatureBridge]]
  NeedMetric -> [[NeedMetricBundle, NeedCoherence]]
  NeedHilbert -> [[NeedHilbertBundle, NeedCoherence]]
  NeedTemporal -> [[NeedModalBundle, NeedTemporalBridge]]
  NeedBridge ->
    [ [NeedMapHead, NeedSourceReuse 1 True]
    , [NeedTargetReuse 2 Nothing, NeedTransportLaw]
    ]

intentOr :: GoalIntent -> [[Obligation]]
intentOr intent = case intent of
  NeedBootstrap -> [[NeedWitness, NeedTypeFormer]]
  NeedFormer -> [[NeedTypeFormer, NeedLoop 1]]
  NeedHIT -> [[NeedLoop 1, NeedCoherence]]
  NeedModal -> [[NeedModalBundle, NeedCoherence]]
  NeedDifferential -> [[NeedDifferentialBridge, NeedCoherence]]
  NeedCurvature -> [[NeedCurvatureBridge, NeedCoherence]]
  NeedMetric -> [[NeedMetricBundle, NeedTransportLaw], [NeedMetricBundle, NeedCurvatureBridge]]
  NeedHilbert -> [[NeedHilbertBundle, NeedTransportLaw], [NeedHilbertBundle, NeedMetricBundle]]
  NeedTemporal -> [[NeedTemporalBridge, NeedCoherence]]
  NeedBridge ->
    [ [NeedFiberWitness, NeedCompatibilityPath 2]
    , [NeedSealBridge, NeedMapHead]
    ]

normalizeDebtGraph :: DebtGraph -> DebtGraph
normalizeDebtGraph dg =
  let open0 = sortNub (dgOpenAtoms dg)
      ands = normalizeAndClauses open0 (dgAndClauses dg)
      (ors, forced) = normalizeOrClauses open0 (dgOrClauses dg)
      openFinal = sortNub (open0 ++ forced ++ concat ands ++ concat ors)
  in DebtGraph
      { dgOpenAtoms = openFinal
      , dgAndClauses = ands
      , dgOrClauses = ors
      }

normalizeAndClauses :: [Obligation] -> [[Obligation]] -> [[Obligation]]
normalizeAndClauses open cls =
  let openSet = sortNub open
      project clause = sortNub [g | g <- clause, g `elem` openSet]
      cleaned = [c | c <- map project cls, not (null c)]
  in sortNub cleaned

normalizeOrClauses :: [Obligation] -> [[Obligation]] -> ([[Obligation]], [Obligation])
normalizeOrClauses open cls =
  let openSet = sortNub open
      project clause = sortNub [g | g <- clause, g `elem` openSet]
      projected = map project cls
      forced = [g | [g] <- projected]
      cleaned = [c | c <- projected, length c >= 2]
  in (sortNub cleaned, sortNub forced)

debtGoalCount :: DebtGraph -> Int
debtGoalCount dg =
  length (dgOpenAtoms dg)
  + sum [max 0 (length c - 1) | c <- dgAndClauses dg]
  + length (dgOrClauses dg)

countMembership :: Obligation -> [[Obligation]] -> Int
countMembership g clauses = length [() | c <- clauses, g `elem` c]

mergeDebtGraphs :: DebtGraph -> DebtGraph -> DebtGraph
mergeDebtGraphs a b =
  normalizeDebtGraph DebtGraph
    { dgOpenAtoms = dgOpenAtoms a ++ dgOpenAtoms b
    , dgAndClauses = dgAndClauses a ++ dgAndClauses b
    , dgOrClauses = dgOrClauses a ++ dgOrClauses b
    }

reduceDebtByCaps :: CapabilitySig -> DebtGraph -> DebtGraph
reduceDebtByCaps caps dg =
  let open0 = [g | g <- dgOpenAtoms dg, not (goalSatisfiedByCaps caps g)]
      and0 = map (filter (not . goalSatisfiedByCaps caps)) (dgAndClauses dg)
      and1 = [c | c <- and0, not (null c)]
      or0 =
        [ filter (not . goalSatisfiedByCaps caps) c
        | c <- dgOrClauses dg
        , not (any (goalSatisfiedByCaps caps) c)
        ]
  in normalizeDebtGraph DebtGraph
      { dgOpenAtoms = open0
      , dgAndClauses = and1
      , dgOrClauses = or0
      }

dischargeByMove :: CapabilitySig -> Action -> MBTTExpr -> DebtGraph -> DebtGraph
dischargeByMove caps act expr dg =
  let closable g = goalSatisfiedByCaps caps g || actionDischargesGoal act expr g
      closed = [g | g <- dgOpenAtoms dg, closable g]
      dg' = foldl dischargeGoal dg closed
  in reduceDebtByCaps caps dg'

dischargeGoal :: DebtGraph -> Obligation -> DebtGraph
dischargeGoal dg g =
  let open = [x | x <- dgOpenAtoms dg, x /= g]
      ands = [ [x | x <- clause, x /= g]
             | clause <- dgAndClauses dg
             , let rest = [x | x <- clause, x /= g]
             , not (null rest)
             ]
      ors = [ clause
            | clause <- dgOrClauses dg
            , g `notElem` clause
            ]
  in normalizeDebtGraph DebtGraph
      { dgOpenAtoms = open
      , dgAndClauses = ands
      , dgOrClauses = ors
      }

goalSatisfiedByCaps :: CapabilitySig -> Obligation -> Bool
goalSatisfiedByCaps caps goal = case goal of
  NeedTypeFormer -> capHasDepFns caps
  NeedWitness -> capHasWitness caps
  NeedLoop d -> capMaxPathDim caps >= d || capHasTruncation caps
  NeedLoopLift d -> capMaxPathDim caps >= d
  NeedLoopCoherence d ->
    capHasDepFns caps
    && capMaxPathDim caps >= max 1 (d - 1)
    && (capHasWitness caps || capHasTruncation caps)
  NeedTruncBridge -> capHasTruncation caps
  NeedSourceReuse d loopReq ->
    capHasWitness caps
    && capMaxPathDim caps >= max 1 d
    && (not loopReq || capMaxPathDim caps >= max 1 d)
  NeedTargetReuse d maxD ->
    capMaxPathDim caps >= max 1 d
    && case maxD of
         Nothing -> True
         Just u -> capMaxPathDim caps <= max 1 u
  NeedMapHead -> capHasDepFns caps
  NeedFiberWitness -> capHasWitness caps
  NeedTransportLaw ->
    capHasDifferential caps || (capHasDepFns caps && capHasWitness caps)
  NeedCompatibilityPath d ->
    capHasDepFns caps && capMaxPathDim caps >= max 1 d
  NeedSealBridge ->
    capHasHilbert caps
    || (capHasDepFns caps && capHasDifferential caps && capHasWitness caps)
  NeedModalBundle -> capModalCount caps >= 3
  NeedDifferentialBridge -> capHasDifferential caps
  NeedCurvatureBridge -> capHasCurvature caps
  NeedMetricBundle -> capHasMetric caps
  NeedHilbertBundle -> capHasHilbert caps
  NeedTemporalBridge -> capHasTemporal caps
  NeedCoherence ->
    capHasDepFns caps
    && (capHasWitness caps || capMaxPathDim caps > 0)

actionDischargesGoal :: Action -> MBTTExpr -> Obligation -> Bool
actionDischargesGoal act expr goal =
  let score = actionGoalGainFor goal act
      supports = exprSupportsGoal goal expr
  in case goal of
      -- Higher-loop lift goals require explicit structural evidence.
      NeedLoopLift _ -> score >= 3 && supports
      NeedMetricBundle -> score >= 3 && supports
      NeedHilbertBundle -> score >= 3 && supports
      _ ->
        score >= 4
        || (score >= 3 && supports)
        || (score >= 2 && supports && goal == NeedCoherence)

exprSupportsGoal :: Obligation -> MBTTExpr -> Bool
exprSupportsGoal goal expr = case goal of
  NeedTypeFormer -> hasPiSigma expr || hasLam expr
  NeedWitness -> hasWitness expr
  NeedLoop d -> hasPathAtLeast d expr || hasLoopFormer expr
  NeedLoopLift d -> hasPathAtLeast d expr || hasLoopLiftSupport d expr
  NeedLoopCoherence d -> hasCoherenceExpr expr && hasPathAtLeast (max 1 (d - 1)) expr
  NeedTruncBridge -> hasTruncExpr expr || hasLoopFormer expr
  NeedSourceReuse d loopReq ->
    hasWitness expr || (loopReq && (hasPathAtLeast d expr || hasLoopFormer expr))
  NeedTargetReuse d _ ->
    hasPathAtLeast d expr || hasPiSigma expr || hasCoherenceExpr expr
  NeedMapHead -> hasPiSigma expr || hasLam expr
  NeedFiberWitness -> hasWitness expr || hasPiSigma expr
  NeedTransportLaw -> hasCoherenceExpr expr || hasDifferentialBridge expr
  NeedCompatibilityPath d -> hasPathAtLeast d expr || hasCoherenceExpr expr
  NeedSealBridge -> hasPiSigma expr && hasCoherenceExpr expr
  NeedModalBundle -> hasModal expr
  NeedDifferentialBridge -> hasDifferentialBridge expr || hasCoherenceExpr expr
  NeedCurvatureBridge -> hasCurvatureBridge expr
  NeedMetricBundle -> hasMetricBridge expr
  NeedHilbertBundle -> hasHilbertBridge expr
  NeedTemporalBridge -> hasTemporalBridge expr
  NeedCoherence -> hasCoherenceExpr expr

deriveObligations :: GoalProfile -> CapabilitySig -> CapabilitySig -> [Obligation]
deriveObligations profile caps baselineCaps =
  nub (concatMap fromIntent (gpIntents profile) ++ coherence)
  where
    fromIntent intent = case intent of
      NeedBootstrap ->
        [NeedWitness | not (capHasWitness caps)]
      NeedFormer ->
        [NeedTypeFormer | not (capHasDepFns caps)]
      NeedHIT ->
        hitObligations caps baselineCaps
      NeedModal ->
        [NeedModalBundle | capModalCount caps < 3]
      NeedDifferential ->
        [NeedDifferentialBridge | not (capHasDifferential caps)]
      NeedCurvature ->
        [NeedCurvatureBridge | not (capHasCurvature caps)]
      NeedMetric ->
        [NeedMetricBundle | not (capHasMetric caps)]
      NeedHilbert ->
        [NeedHilbertBundle | not (capHasHilbert caps)]
      NeedTemporal ->
        [NeedTemporalBridge | not (capHasTemporal caps)]
      NeedBridge ->
        bridgeObligations caps baselineCaps
    coherence =
      [NeedCoherence | capHasDepFns caps && (capHasWitness caps || capMaxPathDim caps > 0)]

hitObligations :: CapabilitySig -> CapabilitySig -> [Obligation]
hitObligations caps baselineCaps =
  let baselineDim = max 0 (capMaxPathDim baselineCaps)
      target = max 1 (baselineDim + 1)
      needsLift = capMaxPathDim caps < target
      needsTruncBridge = baselineDim >= 1 && not (capHasTruncation caps)
  in if baselineDim < 1
     then
       [NeedLoop target | needsLift]
     else if baselineDim < 2
     then
       -- From a dim-1 baseline, require the truncation bridge before pushing
       -- to dim-2 HIT growth; this keeps the debt model bridge-first, not name-first.
       [NeedTruncBridge | needsTruncBridge]
       ++ [NeedLoop target | needsLift]
       ++ [NeedLoopCoherence baselineDim | needsLift || not (capHasDepFns caps)]
     else
       -- Keep target depth frozen to library baseline for this step.
       [NeedLoopLift target | needsLift]
       ++ [NeedLoopCoherence baselineDim | needsLift || not (capHasDepFns caps)]

bridgeObligations :: CapabilitySig -> CapabilitySig -> [Obligation]
bridgeObligations caps baselineCaps =
  let baseDim = max 1 (capMaxPathDim baselineCaps)
      targetDim = max 2 baseDim
      sourceGoal = NeedSourceReuse (max 1 (baseDim - 1)) True
      targetGoal = NeedTargetReuse targetDim Nothing
      compatGoal = NeedCompatibilityPath targetDim
      goals =
        [sourceGoal, targetGoal, NeedMapHead, NeedFiberWitness, NeedTransportLaw, compatGoal, NeedSealBridge]
  in [g | g <- goals, not (goalSatisfiedByCaps caps g)]

topPremiseBuckets :: Library -> [Obligation] -> [TeleEntry] -> PremiseBuckets
topPremiseBuckets lib goals entries =
  let n = length lib
      idxs = [1..n]
      refs = nub [i | e <- entries, i <- exprRefs (teType e), i >= 1, i <= n]
      rankBy scoreFn =
        let ranked = sortOn (\i ->
                      let e = lib !! (i - 1)
                          total = scoreFn i e
                      in (Down total, Down i)
                    ) idxs
            merged = refs ++ [i | i <- ranked, i `notElem` refs]
        in take 4 merged
      sourceScore i e = roleBaseScore goals i n refs e + sourceRoleGain goals e
      targetScore i e = roleBaseScore goals i n refs e + targetRoleGain goals e
      lawScore i e = roleBaseScore goals i n refs e + lawRoleGain goals e
      auxScore i e = roleBaseScore goals i n refs e + auxRoleGain goals e
  in PremiseBuckets
      { pbSource = rankBy sourceScore
      , pbTarget = rankBy targetScore
      , pbLaw = rankBy lawScore
      , pbAux = rankBy auxScore
      }

flattenPremiseBuckets :: PremiseBuckets -> [Int]
flattenPremiseBuckets buckets =
  take 8 (nub (pbSource buckets ++ pbTarget buckets ++ pbLaw buckets ++ pbAux buckets))

roleBaseScore :: [Obligation] -> Int -> Int -> [Int] -> LibraryEntry -> Int
roleBaseScore goals idx libSize refs e =
  let recency =
        if idx == libSize then 6
        else if idx == libSize - 1 then 4
        else if idx == libSize - 2 then 2
        else 0
      refBoost = if idx `elem` refs then 8 else 0
      witnessBoost = if leConstructors e > 0 then 2 else 0
      loopBoost = if leHasLoop e then 2 else 0
      goalBoost = sum [obligationPremiseGain g e | g <- goals]
  in recency + refBoost + witnessBoost + loopBoost + goalBoost

sourceRoleGain :: [Obligation] -> LibraryEntry -> Int
sourceRoleGain goals e =
  let dim = maximum0 (lePathDims e)
      loopGain = if leHasLoop e then 6 else 0
      dimGain = min 6 dim
      bridgeGoalGain = length [() | g <- goals, isSourceGoal g] * 2
  in loopGain + dimGain + bridgeGoalGain

targetRoleGain :: [Obligation] -> LibraryEntry -> Int
targetRoleGain goals e =
  let dim = maximum0 (lePathDims e)
      depGain = if leHasDependentFunctions e then 6 else 0
      targetDimGain = min 5 dim
      bridgeGoalGain = length [() | g <- goals, isTargetGoal g] * 2
  in depGain + targetDimGain + bridgeGoalGain

lawRoleGain :: [Obligation] -> LibraryEntry -> Int
lawRoleGain goals e =
  let depGain = if leHasDependentFunctions e then 4 else 0
      diffGain = if leHasDifferentialOps e then 5 else 0
      curvGain = if leHasCurvature e then 5 else 0
      modalGain = if leHasModalOps e then 2 else 0
      temporalGain = if leHasTemporalOps e then 2 else 0
      bridgeGoalGain = length [() | g <- goals, isLawGoal g] * 2
  in depGain + diffGain + curvGain + modalGain + temporalGain + bridgeGoalGain

auxRoleGain :: [Obligation] -> LibraryEntry -> Int
auxRoleGain goals e =
  let constructorGain = min 4 (leConstructors e)
      truncGain = case leIsTruncated e of
        Just _ -> 3
        Nothing -> 0
      bridgeGoalGain = length [() | g <- goals, isAuxGoal g]
  in constructorGain + truncGain + bridgeGoalGain

isSourceGoal :: Obligation -> Bool
isSourceGoal goal = case goal of
  NeedSourceReuse _ _ -> True
  NeedFiberWitness -> True
  _ -> False

isTargetGoal :: Obligation -> Bool
isTargetGoal goal = case goal of
  NeedTargetReuse _ _ -> True
  NeedCompatibilityPath _ -> True
  _ -> False

isLawGoal :: Obligation -> Bool
isLawGoal goal = case goal of
  NeedMapHead -> True
  NeedTransportLaw -> True
  NeedSealBridge -> True
  NeedCurvatureBridge -> True
  NeedMetricBundle -> True
  NeedHilbertBundle -> True
  _ -> False

isAuxGoal :: Obligation -> Bool
isAuxGoal goal = case goal of
  NeedTruncBridge -> True
  NeedCoherence -> True
  _ -> False

obligationPremiseGain :: Obligation -> LibraryEntry -> Int
obligationPremiseGain goal e = case goal of
  NeedTypeFormer ->
    if leHasDependentFunctions e then 8
    else if leConstructors e > 0 then 2
    else 0
  NeedWitness ->
    if leConstructors e > 0 then 8 else 0
  NeedLoop d ->
    if leHasLoop e && any (>= d) (lePathDims e) then 8
    else if any (>= d) (lePathDims e) then 5
    else 0
  NeedLoopLift d ->
    if any (>= d) (lePathDims e) then 8
    else if leHasLoop e then 4
    else 0
  NeedLoopCoherence d ->
    if leHasDependentFunctions e && any (>= max 1 (d - 1)) (lePathDims e) then 8
    else if leHasDependentFunctions e then 4
    else if leHasLoop e then 3
    else 0
  NeedTruncBridge ->
    case leIsTruncated e of
      Just _ -> 8
      Nothing -> if leHasLoop e then 3 else 0
  NeedSourceReuse d loopReq ->
    if leHasLoop e && any (>= d) (lePathDims e) then 8
    else if loopReq && leHasLoop e then 5
    else if leConstructors e > 0 then 4
    else 0
  NeedTargetReuse d _ ->
    if leHasDependentFunctions e && any (>= d) (lePathDims e) then 8
    else if leHasDependentFunctions e then 5
    else if any (>= d) (lePathDims e) then 4
    else 0
  NeedMapHead ->
    if leHasDependentFunctions e then 8
    else if leHasModalOps e then 3
    else 0
  NeedFiberWitness ->
    if leConstructors e > 0 then 7
    else if leHasLoop e then 3
    else 0
  NeedTransportLaw ->
    if leHasDifferentialOps e then 8
    else if leHasDependentFunctions e then 4
    else 0
  NeedCompatibilityPath d ->
    if any (>= d) (lePathDims e) then 8
    else if leHasDependentFunctions e then 3
    else 0
  NeedSealBridge ->
    if leHasHilbert e then 8
    else if leHasDifferentialOps e then 5
    else if leHasDependentFunctions e then 3
    else 0
  NeedModalBundle ->
    if leHasModalOps e then 8 else 0
  NeedDifferentialBridge ->
    if leHasDifferentialOps e then 8
    else if leHasModalOps e then 5
    else 0
  NeedCurvatureBridge ->
    if leHasCurvature e then 8
    else if leHasDifferentialOps e && any (>= 2) (lePathDims e) then 6
    else if leHasDifferentialOps e then 4
    else 0
  NeedMetricBundle ->
    if leHasMetric e then 8
    else if leHasCurvature e && leHasDependentFunctions e then 7
    else if leHasCurvature e then 5
    else if leHasDifferentialOps e then 3
    else 0
  NeedHilbertBundle ->
    if leHasHilbert e then 8
    else if leHasMetric e && leHasCurvature e then 7
    else if leHasMetric e then 5
    else if leHasCurvature e then 4
    else if leHasDifferentialOps e then 3
    else 0
  NeedTemporalBridge ->
    if leHasTemporalOps e then 8
    else if leHasHilbert e && leHasModalOps e then 6
    else if leHasHilbert e then 4
    else if leHasModalOps e then 3
    else 0
  NeedCoherence ->
    if leHasDependentFunctions e && (leHasLoop e || not (null (lePathDims e))) then 6
    else if leHasDependentFunctions e then 3
    else 0

capabilityScore :: CapabilitySig -> Int
capabilityScore caps =
    (if capHasUniverse caps then 1 else 0)
  + (if capHasWitness caps then 2 else 0)
  + (if capHasDepFns caps then 3 else 0)
  + (if capHasTruncation caps then 2 else 0)
  + capMaxPathDim caps
  + capModalCount caps
  + (if capHasDifferential caps then 2 else 0)
  + (if capHasCurvature caps then 2 else 0)
  + (if capHasMetric caps then 2 else 0)
  + (if capHasHilbert caps then 2 else 0)
  + (if capHasTemporal caps then 2 else 0)

actionGoalGain :: SearchState -> Action -> Int
actionGoalGain st act =
  let dg = ssDebtGraph st
      open = dgOpenAtoms dg
      weighted = sum [goalWeight g * actionGoalGainFor g act | g <- open]
      andBonus = 2 * length [() | clause <- dgAndClauses dg, all (\g -> actionGoalGainFor g act > 0) clause]
      orBonus = 2 * length [() | clause <- dgOrClauses dg, any (\g -> actionGoalGainFor g act > 0) clause]
  in weighted + andBonus + orBonus
  where
    goalWeight g =
      1 + countMembership g (dgAndClauses (ssDebtGraph st))
        + countMembership g (dgOrClauses (ssDebtGraph st))

actionGoalGainFor :: Obligation -> Action -> Int
actionGoalGainFor goal act = case goal of
  NeedTypeFormer -> case act of
    APi -> 4
    ASigma -> 4
    ALam -> 2
    AApp -> 1
    _ -> 0
  NeedWitness -> case act of
    AVar _ -> 3
    ALib _ -> 3
    AApp -> 2
    _ -> 0
  NeedLoop d -> case act of
    APathCon q
      | q == d -> if d <= 1 then 4 else 4
      | q > d -> 1
      | q == max 1 (d - 1) -> 2
      | otherwise -> 0
    ASusp -> if d <= 1 then 3 else 4
    ATrunc -> if d <= 1 then 3 else 0
    AId -> if d <= 1 then 2 else 3
    _ -> 0
  NeedLoopLift d -> case act of
    APathCon q
      | q == d -> 5
      | q == d + 1 -> 2
      | q == max 1 (d - 1) -> 3
      | otherwise -> 0
    ASusp -> if d >= 2 then 2 else 3
    ATrunc -> if d <= 1 then 2 else 0
    AId -> if d >= 2 then 5 else 3
    APi -> if d >= 2 then 3 else 2
    ASigma -> if d >= 2 then 3 else 2
    AApp -> if d >= 2 then 3 else 2
    _ -> 0
  NeedLoopCoherence d -> case act of
    AId -> if d >= 2 then 5 else 3
    ARefl -> if d >= 2 then 4 else 2
    ALam -> 2
    AApp -> 2
    APi -> 2
    ASigma -> 2
    _ -> 0
  NeedTruncBridge -> case act of
    -- Treat truncation as infrastructure: pure ATrunc remains strong,
    -- but coherence/former actions are now competitive so search keeps
    -- bridge-building context rather than collapsing to unary shortcuts.
    ATrunc -> 4
    ASusp -> 3
    AId -> 4
    APi -> 2
    ASigma -> 2
    AApp -> 2
    APathCon _ -> 1
    _ -> 0
  NeedSourceReuse d loopReq -> case act of
    ALib _ -> 5
    AVar _ -> 4
    AApp -> 3
    APathCon q -> if loopReq && q >= d then 4 else 0
    AId -> if loopReq then 3 else 2
    _ -> 0
  NeedTargetReuse d _ -> case act of
    APathCon q
      | q == d -> 5
      | q == d + 1 -> 2
      | q < d -> 2
      | otherwise -> 0
    AId -> 4
    APi -> 3
    ASigma -> 3
    ALib _ -> 2
    AApp -> 2
    _ -> 0
  NeedMapHead -> case act of
    APi -> 5
    ASigma -> 4
    ALam -> 3
    AApp -> 3
    ALib _ -> 2
    _ -> 0
  NeedFiberWitness -> case act of
    AVar _ -> 4
    ALib _ -> 4
    AApp -> 3
    ASigma -> 2
    _ -> 0
  NeedTransportLaw -> case act of
    AId -> 5
    ARefl -> 4
    APi -> 3
    AApp -> 3
    ALam -> 2
    _ -> 0
  NeedCompatibilityPath d -> case act of
    APathCon q
      | q == d -> 5
      | q == d + 1 -> 2
      | q < d -> 2
      | otherwise -> 0
    AId -> 4
    ARefl -> 3
    APi -> 2
    ASigma -> 2
    _ -> 0
  NeedSealBridge -> case act of
    APi -> 4
    ASigma -> 4
    AId -> 3
    AApp -> 2
    ALam -> 2
    _ -> 0
  NeedModalBundle -> case act of
    AFlat -> 3
    ASharp -> 3
    ADisc -> 3
    AShape -> 3
    _ -> 0
  NeedDifferentialBridge -> case act of
    APi -> 2
    ASigma -> 2
    AApp -> 2
    ALam -> 3
    ALib _ -> 3
    AId -> 2
    _ -> 0
  NeedCurvatureBridge -> case act of
    APathCon q | q >= 2 -> 1
    AId -> 4
    APi -> 3
    ASigma -> 3
    AApp -> 3
    ALam -> 2
    ALib _ -> 2
    _ -> 0
  NeedMetricBundle -> case act of
    APi -> 5
    ASigma -> 5
    AId -> 4
    AApp -> 4
    ALam -> 3
    ALib _ -> 3
    _ -> 0
  NeedHilbertBundle -> case act of
    APi -> 5
    ASigma -> 5
    AId -> 4
    AApp -> 4
    ALam -> 4
    ALib _ -> 4
    _ -> 0
  NeedTemporalBridge -> case act of
    ANext -> 4
    AEventually -> 4
    APi -> 3
    ASigma -> 2
    AApp -> 3
    ALib _ -> 2
    AFlat -> 2
    AShape -> 2
    AId -> 2
    ALam -> 2
    _ -> 0
  NeedCoherence -> case act of
    ARefl -> 2
    AId -> 2
    ALam -> 1
    AApp -> 1
    _ -> 0

obligationPriority :: Obligation -> Int
obligationPriority goal = case goal of
  NeedTypeFormer -> 8
  NeedWitness -> 7
  NeedLoop d -> 9 + d
  NeedLoopLift d -> 10 + d
  NeedLoopCoherence d -> 9 + d
  NeedTruncBridge -> 12
  NeedSourceReuse d _ -> 12 + d
  NeedTargetReuse d _ -> 12 + d
  NeedMapHead -> 13
  NeedFiberWitness -> 11
  NeedTransportLaw -> 13
  NeedCompatibilityPath d -> 12 + d
  NeedSealBridge -> 14
  NeedModalBundle -> 8
  NeedDifferentialBridge -> 10
  NeedCurvatureBridge -> 11
  NeedMetricBundle -> 12
  NeedHilbertBundle -> 13
  NeedTemporalBridge -> 9
  NeedCoherence -> 6

isSafeAction :: Action -> Bool
isSafeAction a = case a of
  AUniv -> True
  ARefl -> True
  _ -> False

capsFromLibrary :: Library -> CapabilitySig
capsFromLibrary lib =
  CapabilitySig
    { capHasUniverse = not (null lib)
    , capHasWitness = any (\e -> leConstructors e > 0) lib
    , capHasDepFns = any leHasDependentFunctions lib
    , capHasTruncation = any hasTruncEntry lib
    , capMaxPathDim = maximum0 (concatMap lePathDims lib)
    , capModalCount = if any leHasModalOps lib then 3 else 0
    , capHasDifferential = any leHasDifferentialOps lib
    , capHasCurvature = any leHasCurvature lib
    , capHasMetric = any leHasMetric lib
    , capHasHilbert = any leHasHilbert lib
    , capHasTemporal = any leHasTemporalOps lib
    }
  where
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

capsFromEntries :: [TeleEntry] -> CapabilitySig
capsFromEntries entries =
  let exprs = map teType entries
      mods = [() | e <- exprs, hasModalExpr e]
  in CapabilitySig
      { capHasUniverse = any hasUniverseExpr exprs
      , capHasWitness = any hasWitnessExpr exprs
      , capHasDepFns = any hasDepFnExpr exprs
      , capHasTruncation = any hasTruncExpr exprs
      , capMaxPathDim = maximum0 (concatMap pathDims exprs)
      , capModalCount = min 4 (length mods)
      , capHasDifferential = any hasDifferentialExpr exprs
      , capHasCurvature = any hasCurvatureExpr exprs
      , capHasMetric = any hasMetricExpr exprs
      , capHasHilbert = hasHilbertBundleEvidence entries exprs
      , capHasTemporal = any hasTemporalExpr exprs
      }

mergeCaps :: CapabilitySig -> CapabilitySig -> CapabilitySig
mergeCaps a b =
  CapabilitySig
    { capHasUniverse = capHasUniverse a || capHasUniverse b
    , capHasWitness = capHasWitness a || capHasWitness b
    , capHasDepFns = capHasDepFns a || capHasDepFns b
    , capHasTruncation = capHasTruncation a || capHasTruncation b
    , capMaxPathDim = max (capMaxPathDim a) (capMaxPathDim b)
    , capModalCount = max (capModalCount a) (capModalCount b)
    , capHasDifferential = capHasDifferential a || capHasDifferential b
    , capHasCurvature = capHasCurvature a || capHasCurvature b
    , capHasMetric = capHasMetric a || capHasMetric b
    , capHasHilbert = capHasHilbert a || capHasHilbert b
    , capHasTemporal = capHasTemporal a || capHasTemporal b
    }

maximum0 :: [Int] -> Int
maximum0 [] = 0
maximum0 xs = maximum xs

sortNub :: Ord a => [a] -> [a]
sortNub = nub . sort

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

hasTruncExpr :: MBTTExpr -> Bool
hasTruncExpr (Trunc _) = True
hasTruncExpr (Lam a) = hasTruncExpr a
hasTruncExpr (Refl a) = hasTruncExpr a
hasTruncExpr (Susp a) = hasTruncExpr a
hasTruncExpr (Flat a) = hasTruncExpr a
hasTruncExpr (Sharp a) = hasTruncExpr a
hasTruncExpr (Disc a) = hasTruncExpr a
hasTruncExpr (Shape a) = hasTruncExpr a
hasTruncExpr (Next a) = hasTruncExpr a
hasTruncExpr (Eventually a) = hasTruncExpr a
hasTruncExpr (Pi a b) = hasTruncExpr a || hasTruncExpr b
hasTruncExpr (Sigma a b) = hasTruncExpr a || hasTruncExpr b
hasTruncExpr (App a b) = hasTruncExpr a || hasTruncExpr b
hasTruncExpr (Id a x y) = hasTruncExpr a || hasTruncExpr x || hasTruncExpr y
hasTruncExpr _ = False

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
hasTemporalExpr expr = case expr of
  Next _ -> True
  Eventually _ -> True
  Lam a -> hasTemporalExpr a
  App a b -> hasTemporalExpr a || hasTemporalExpr b
  Pi a b -> hasTemporalExpr a || hasTemporalExpr b
  Sigma a b -> hasTemporalExpr a || hasTemporalExpr b
  Id a x y -> hasTemporalExpr a || hasTemporalExpr x || hasTemporalExpr y
  Refl a -> hasTemporalExpr a
  Susp a -> hasTemporalExpr a
  Trunc a -> hasTemporalExpr a
  Flat a -> hasTemporalExpr a
  Sharp a -> hasTemporalExpr a
  Disc a -> hasTemporalExpr a
  Shape a -> hasTemporalExpr a
  _ -> False

hasDifferentialExpr :: MBTTExpr -> Bool
hasDifferentialExpr (Pi _ _) = True
hasDifferentialExpr (Sigma _ _) = True
hasDifferentialExpr _ = False

hasCurvatureExpr :: MBTTExpr -> Bool
hasCurvatureExpr expr =
  any (>= 2) (pathDims expr)
  && (hasDifferentialBridge expr || hasCoherenceExpr expr)

hasMetricExpr :: MBTTExpr -> Bool
hasMetricExpr expr = case expr of
  Sigma (Pi _ _) (Pi _ _) -> True
  Pi (Lib _) (Lib _) -> True
  Pi (Lib _) (Var _) -> True
  Lam a -> hasMetricExpr a
  App a b -> hasMetricExpr a || hasMetricExpr b
  Id a x y -> hasMetricExpr a || hasMetricExpr x || hasMetricExpr y
  Refl a -> hasMetricExpr a
  _ -> hasPiSigma expr && hasCurvatureBridge expr

hasHilbertExpr :: MBTTExpr -> Bool
hasHilbertExpr expr = case expr of
  Sigma (Pi _ _) (Pi _ _) -> True
  Sigma (Pi _ _) _ -> True
  Pi (Lam _) (Sigma _ _) -> True
  Lam (Pi _ Univ) -> True
  Pi (Lib _) _ -> True
  Lam a -> hasHilbertExpr a
  App a b -> hasHilbertExpr a || hasHilbertExpr b
  Id a x y -> hasHilbertExpr a || hasHilbertExpr x || hasHilbertExpr y
  Refl a -> hasHilbertExpr a
  _ -> False

hasHilbertBundleEvidence :: [TeleEntry] -> [MBTTExpr] -> Bool
hasHilbertBundleEvidence entries exprs =
  length entries >= 9
  && clauseScore >= 7
  && libRefCount >= 2
  where
    clauseScore =
      sum
        [ if any isInnerProduct exprs then 1 else 0
        , if any isCompleteness exprs then 1 else 0
        , if any isOrthDecomp exprs then 1 else 0
        , if any isSpectral exprs then 1 else 0
        , if any isCStar exprs then 1 else 0
        , if any isMetricCompat exprs then 1 else 0
        , if any isCurvatureOrConnectionOp exprs then 1 else 0
        , if any isFunctionalDerivative exprs then 1 else 0
        ]
    refs = sortNub (concatMap exprRefs exprs)
    libRefCount = length refs

    isInnerProduct expr = case expr of
      Sigma (Pi _ _) _ -> True
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
      Lam a -> isOrthDecomp a
      App a b -> isOrthDecomp a || isOrthDecomp b
      Id a x y -> isOrthDecomp a || isOrthDecomp x || isOrthDecomp y
      Refl a -> isOrthDecomp a
      _ -> False

    isSpectral expr = case expr of
      Pi (Lam _) (Sigma _ _) -> True
      Lam a -> isSpectral a
      App a b -> isSpectral a || isSpectral b
      Id a x y -> isSpectral a || isSpectral x || isSpectral y
      Refl a -> isSpectral a
      _ -> False

    isCStar expr = case expr of
      Sigma (Pi _ _) (Pi _ _) -> True
      Lam a -> isCStar a
      App a b -> isCStar a || isCStar b
      Id a x y -> isCStar a || isCStar x || isCStar y
      Refl a -> isCStar a
      _ -> False

    isMetricCompat expr = case expr of
      Pi (Lib _) _ -> True
      Lam a -> isMetricCompat a
      App a b -> isMetricCompat a || isMetricCompat b
      Id a x y -> isMetricCompat a || isMetricCompat x || isMetricCompat y
      Refl a -> isMetricCompat a
      _ -> False

    isCurvatureOrConnectionOp expr = case expr of
      Pi (Lib _) (Var _) -> True
      Lam a -> isCurvatureOrConnectionOp a
      App a b -> isCurvatureOrConnectionOp a || isCurvatureOrConnectionOp b
      Id a x y -> isCurvatureOrConnectionOp a || isCurvatureOrConnectionOp x || isCurvatureOrConnectionOp y
      Refl a -> isCurvatureOrConnectionOp a
      _ -> False

    isFunctionalDerivative expr = case expr of
      Lam (Pi _ Univ) -> True
      Lam a -> isFunctionalDerivative a
      App a b -> isFunctionalDerivative a || isFunctionalDerivative b
      Id a x y -> isFunctionalDerivative a || isFunctionalDerivative x || isFunctionalDerivative y
      Refl a -> isFunctionalDerivative a
      _ -> False

hasPiSigma :: MBTTExpr -> Bool
hasPiSigma (Pi _ _) = True
hasPiSigma (Sigma _ _) = True
hasPiSigma (Lam a) = hasPiSigma a
hasPiSigma (App a b) = hasPiSigma a || hasPiSigma b
hasPiSigma (Id a x y) = hasPiSigma a || hasPiSigma x || hasPiSigma y
hasPiSigma (Refl a) = hasPiSigma a
hasPiSigma (Susp a) = hasPiSigma a
hasPiSigma (Trunc a) = hasPiSigma a
hasPiSigma (Flat a) = hasPiSigma a
hasPiSigma (Sharp a) = hasPiSigma a
hasPiSigma (Disc a) = hasPiSigma a
hasPiSigma (Shape a) = hasPiSigma a
hasPiSigma (Next a) = hasPiSigma a
hasPiSigma (Eventually a) = hasPiSigma a
hasPiSigma _ = False

hasLam :: MBTTExpr -> Bool
hasLam (Lam _) = True
hasLam (App a b) = hasLam a || hasLam b
hasLam (Pi a b) = hasLam a || hasLam b
hasLam (Sigma a b) = hasLam a || hasLam b
hasLam (Id a x y) = hasLam a || hasLam x || hasLam y
hasLam (Refl a) = hasLam a
hasLam (Susp a) = hasLam a
hasLam (Trunc a) = hasLam a
hasLam (Flat a) = hasLam a
hasLam (Sharp a) = hasLam a
hasLam (Disc a) = hasLam a
hasLam (Shape a) = hasLam a
hasLam (Next a) = hasLam a
hasLam (Eventually a) = hasLam a
hasLam _ = False

hasWitness :: MBTTExpr -> Bool
hasWitness (Var _) = True
hasWitness (Lib _) = True
hasWitness (App _ _) = True
hasWitness (Lam a) = hasWitness a
hasWitness (Refl a) = hasWitness a
hasWitness _ = False

hasPathAtLeast :: Int -> MBTTExpr -> Bool
hasPathAtLeast d expr = any (>= d) (pathDims expr)

hasLoopFormer :: MBTTExpr -> Bool
hasLoopFormer expr = case expr of
  PathCon _ -> True
  Id _ _ _ -> True
  Susp _ -> True
  Trunc _ -> True
  Lam a -> hasLoopFormer a
  App a b -> hasLoopFormer a || hasLoopFormer b
  Pi a b -> hasLoopFormer a || hasLoopFormer b
  Sigma a b -> hasLoopFormer a || hasLoopFormer b
  Refl a -> hasLoopFormer a
  _ -> False

hasLoopLiftSupport :: Int -> MBTTExpr -> Bool
hasLoopLiftSupport d expr
  | d <= 1 = hasLoopFormer expr
  | otherwise =
      hasPathAtLeast d expr
      || (hasCoherenceExpr expr && hasPathAtLeast (d - 1) expr)

hasModal :: MBTTExpr -> Bool
hasModal expr = case expr of
  Flat _ -> True
  Sharp _ -> True
  Disc _ -> True
  Shape _ -> True
  Lam a -> hasModal a
  App a b -> hasModal a || hasModal b
  Pi a b -> hasModal a || hasModal b
  Sigma a b -> hasModal a || hasModal b
  Id a x y -> hasModal a || hasModal x || hasModal y
  Refl a -> hasModal a
  _ -> False

hasDifferentialBridge :: MBTTExpr -> Bool
hasDifferentialBridge expr = case expr of
  Pi _ _ -> True
  Sigma _ _ -> True
  App _ _ -> True
  Lam a -> hasDifferentialBridge a
  Refl a -> hasDifferentialBridge a
  _ -> False

hasCurvatureBridge :: MBTTExpr -> Bool
hasCurvatureBridge expr = case expr of
  PathCon d -> d >= 2
  Id _ _ _ -> True
  Pi _ _ -> True
  Sigma _ _ -> True
  App _ _ -> True
  Lam a -> hasCurvatureBridge a
  Refl a -> hasCurvatureBridge a
  _ -> False

hasMetricBridge :: MBTTExpr -> Bool
hasMetricBridge expr = case expr of
  Sigma (Pi _ _) (Pi _ _) -> True
  Pi (Lib _) (Lib _) -> True
  Pi (Lib _) (Var _) -> True
  Lam a -> hasMetricBridge a
  App a b -> hasMetricBridge a || hasMetricBridge b
  Id a x y -> hasMetricBridge a || hasMetricBridge x || hasMetricBridge y
  Refl a -> hasMetricBridge a
  _ -> hasPiSigma expr && hasCurvatureBridge expr

hasHilbertBridge :: MBTTExpr -> Bool
hasHilbertBridge expr = case expr of
  Sigma (Pi _ _) (Pi _ _) -> True
  Sigma (Pi _ _) _ -> True
  Pi (Lam _) (Sigma _ _) -> True
  Pi (Lib _) _ -> True
  Lam (Pi _ Univ) -> True
  Lam a -> hasHilbertBridge a
  App a b -> hasHilbertBridge a || hasHilbertBridge b
  Id a x y -> hasHilbertBridge a || hasHilbertBridge x || hasHilbertBridge y
  Refl a -> hasHilbertBridge a
  _ -> hasMetricBridge expr || hasCurvatureBridge expr

hasTemporalBridge :: MBTTExpr -> Bool
hasTemporalBridge expr = case expr of
  Next _ -> True
  Eventually _ -> True
  Pi _ _ -> True
  Sigma _ _ -> True
  Id _ _ _ -> True
  Refl _ -> True
  Flat _ -> True
  Shape _ -> True
  App a b -> hasTemporalBridge a || hasTemporalBridge b
  Lam a -> hasTemporalBridge a
  Susp a -> hasTemporalBridge a
  Trunc a -> hasTemporalBridge a
  Sharp a -> hasTemporalBridge a
  Disc a -> hasTemporalBridge a
  _ -> False

hasCoherenceExpr :: MBTTExpr -> Bool
hasCoherenceExpr expr = case expr of
  Refl _ -> True
  Id _ _ _ -> True
  Lam _ -> True
  App _ _ -> True
  Pi _ _ -> True
  Sigma _ _ -> True
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
