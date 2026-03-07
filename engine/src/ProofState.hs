{-# LANGUAGE BangPatterns #-}

-- | Lightweight proof/search state for obligation-driven exploration.
-- This module keeps search guidance structural and name-independent.
module ProofState
  ( CapabilitySig(..)
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
  | NeedModalBundle
  | NeedDifferentialBridge
  | NeedTemporalBridge
  | NeedCoherence
  deriving (Eq, Ord, Show)

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
      prem = topPremises lib (dgOpenAtoms debt) entries
      kappaApprox = length entries
      nuLower = capabilityScore caps
      nuUpper = nuLower + 2 * debtGoalCount debt
  in SearchState
      { ssEntries = entries
      , ssGoals = dgOpenAtoms debt
      , ssDebtGraph = debt
      , ssCaps = caps
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
      prem = topPremises lib (dgOpenAtoms reducedDebt) entries
      kappaApprox = length entries
      nuLower = capabilityScore caps
      nuUpper = nuLower + 2 * debtGoalCount reducedDebt
  in SearchState
      { ssEntries = entries
      , ssGoals = dgOpenAtoms reducedDebt
      , ssDebtGraph = reducedDebt
      , ssCaps = caps
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
    [ mk "former-from-witness" [NeedTypeFormer, NeedWitness] [[NeedTypeFormer, NeedWitness]] [[NeedTypeFormer, NeedCoherence]] [NeedTypeFormer]
    , mk "former-needs-coherence" [NeedTypeFormer, NeedCoherence] [[NeedTypeFormer, NeedCoherence]] [[NeedTypeFormer, NeedWitness]] [NeedTypeFormer]
    ]
  ASigma ->
    [ mk "sigma-from-witness" [NeedTypeFormer, NeedWitness] [[NeedTypeFormer, NeedWitness]] [[NeedTypeFormer, NeedCoherence]] [NeedTypeFormer]
    ]
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
    in
      if d <= 2
      then
        [ mk "id-coherence-chain" [NeedCoherence, NeedTypeFormer] [[NeedCoherence, NeedTypeFormer]] [[NeedCoherence, NeedLoop 1]] [NeedCoherence] ]
      else
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
    [ mk "reuse-needs-witness" [NeedWitness, NeedTypeFormer] [[NeedWitness, NeedTypeFormer]] [[NeedWitness, NeedCoherence]] [NeedWitness] ]
  AVar _ ->
    [ mk "var-needs-witness" [NeedWitness, NeedTypeFormer] [[NeedWitness, NeedTypeFormer]] [[NeedWitness, NeedCoherence]] [NeedWitness] ]
  AApp ->
    [ mk "app-witness-chain" [NeedWitness, NeedTypeFormer] [[NeedWitness, NeedTypeFormer]] [[NeedWitness, NeedCoherence]] [NeedWitness] ]
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
  NeedTemporal -> [[NeedModalBundle, NeedTemporalBridge]]
  NeedBridge -> [[NeedTypeFormer, NeedModalBundle]]

intentOr :: GoalIntent -> [[Obligation]]
intentOr intent = case intent of
  NeedBootstrap -> [[NeedWitness, NeedTypeFormer]]
  NeedFormer -> [[NeedTypeFormer, NeedLoop 1]]
  NeedHIT -> [[NeedLoop 1, NeedCoherence]]
  NeedModal -> [[NeedModalBundle, NeedCoherence]]
  NeedDifferential -> [[NeedDifferentialBridge, NeedCoherence]]
  NeedTemporal -> [[NeedTemporalBridge, NeedCoherence]]
  NeedBridge -> [[NeedModalBundle, NeedCoherence]]

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
  NeedModalBundle -> capModalCount caps >= 3
  NeedDifferentialBridge -> capHasDifferential caps
  NeedTemporalBridge -> capHasTemporal caps
  NeedCoherence ->
    capHasDepFns caps
    && (capHasWitness caps || capMaxPathDim caps > 0)

actionDischargesGoal :: Action -> MBTTExpr -> Obligation -> Bool
actionDischargesGoal act expr goal =
  let score = actionGoalGainFor goal act
  in score >= 4
     || (score >= 3 && exprSupportsGoal goal expr)
     || (score >= 2 && exprSupportsGoal goal expr && goal == NeedCoherence)

exprSupportsGoal :: Obligation -> MBTTExpr -> Bool
exprSupportsGoal goal expr = case goal of
  NeedTypeFormer -> hasPiSigma expr || hasLam expr
  NeedWitness -> hasWitness expr
  NeedLoop d -> hasPathAtLeast d expr || hasLoopFormer expr
  NeedLoopLift d -> hasPathAtLeast d expr || hasLoopFormer expr
  NeedLoopCoherence d -> hasCoherenceExpr expr && hasPathAtLeast (max 1 (d - 1)) expr
  NeedTruncBridge -> hasTruncExpr expr || hasLoopFormer expr
  NeedModalBundle -> hasModal expr
  NeedDifferentialBridge -> hasDifferentialBridge expr
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
      NeedTemporal ->
        [NeedTemporalBridge | not (capHasTemporal caps)]
      NeedBridge ->
        [NeedTypeFormer | not (capHasDepFns caps)]
        ++ [NeedModalBundle | capModalCount caps < 2]
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

topPremises :: Library -> [Obligation] -> [TeleEntry] -> [Int]
topPremises lib goals entries =
  let n = length lib
      idxs = [1..n]
      ranked = sortOn scoreKey idxs
      merged = refs ++ [i | i <- ranked, i `notElem` refs]
  in take 4 merged
  where
    scoreKey i =
      let e = lib !! (i - 1)
          total = premiseScore goals i (length lib) refs e
      in (Down total, Down i)
    refs = nub [i | e <- entries, i <- exprRefs (teType e), i >= 1, i <= length lib]

premiseScore :: [Obligation] -> Int -> Int -> [Int] -> LibraryEntry -> Int
premiseScore goals idx libSize refs e =
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
  NeedModalBundle ->
    if leHasModalOps e then 8 else 0
  NeedDifferentialBridge ->
    if leHasDifferentialOps e then 8
    else if leHasModalOps e then 3
    else 0
  NeedTemporalBridge ->
    if leHasTemporalOps e then 8
    else if leHasHilbert e then 4
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
    APathCon _ -> if d <= 1 then 4 else 2
    ASusp -> if d <= 1 then 3 else 4
    ATrunc -> if d <= 1 then 3 else 0
    AId -> if d <= 1 then 2 else 3
    _ -> 0
  NeedLoopLift d -> case act of
    APathCon _ -> if d <= 1 then 4 else 5
    ASusp -> 4
    ATrunc -> if d <= 1 then 3 else 0
    AId -> 2
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
    _ -> 0
  NeedTemporalBridge -> case act of
    ANext -> 4
    AEventually -> 4
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
  NeedModalBundle -> 8
  NeedDifferentialBridge -> 10
  NeedTemporalBridge -> 9
  NeedCoherence -> 6

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
    { capHasUniverse = not (null lib)
    , capHasWitness = any (\e -> leConstructors e > 0) lib
    , capHasDepFns = any leHasDependentFunctions lib
    , capHasTruncation = any hasTruncEntry lib
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
      , capHasHilbert = any hasHilbertExpr exprs
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

hasTemporalBridge :: MBTTExpr -> Bool
hasTemporalBridge expr = case expr of
  Next _ -> True
  Eventually _ -> True
  App a b -> hasTemporalBridge a || hasTemporalBridge b
  Lam a -> hasTemporalBridge a
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
