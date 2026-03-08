{-# LANGUAGE BangPatterns #-}

-- | Agenda-based obligation-driven search for telescope candidates.
-- Keeps search on explicit proof/search states rather than full-space enumeration.
module AgendaSearch
  ( AgendaConfig(..)
  , defaultAgendaConfig
  , DerivationRule(..)
  , DerivationCertificate(..)
  , NearMiss(..)
  , AgendaDiagnostics(..)
  , AgendaCandidate(..)
  , agendaGenerateCandidates
  , agendaGenerateCandidatesWithDiagnostics
  ) where

import Kolmogorov (MBTTExpr(..))
import MBTTEnum (enumerateExprs)
import Telescope (Telescope(..), TeleEntry(..), teleIsConnected, teleReferencesWindow, teleLibRefs)
import TelescopeCheck (checkTelescope, CheckResult(..))
import TelescopeGen (Action(..), GoalProfile(..), GoalIntent(..), Hole(..), HoleGoal(..), validActionsWithProfile, actionPriority, actionBitCost)
import ProofState ( SearchState(..)
                  , DebtGraph(..)
                  , CapabilitySig(..)
                  , PremiseBuckets(..)
                  , Obligation(..)
                  , CriticPlan(..)
                  , CriticPlanState(..)
                  , deriveSearchState
                  , advanceSearchState
                  , stateOpenGoals
                  , stateGoalCount
                  , stateCriticalGoal
                  , criticPlanStates
                  , actionGoalGain
                  , actionGoalGainFor
                  , isSafeAction
                  )
import Types (Library, LibraryEntry(..))

import Data.List (sortOn, nub, sort, foldl')
import Data.Ord (Down(..))
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Bits (shiftL, xor)
import Data.Word (Word64)

data AgendaConfig = AgendaConfig
  { agMaxEntries      :: !Int
  , agMaxAgendaStates :: !Int
  , agEnableDiversity :: !Bool
  , agBucketCap       :: !Int
  , agCriticPerAction :: !Int
  , agBranchPerState  :: !Int
  , agMaxCandidates   :: !Int
  , agBitBudget       :: !Int
  , agRequireConnected :: !Bool
  , agSafeClosureSteps :: !Int
  , agPremiseTopK      :: !Int
  , agBarFloor         :: !Double
  , agLeafExprBudget   :: !Int
  , agLeafExprDepth    :: !Int
  , agLeafExprCap      :: !Int
  } deriving (Show, Eq)

defaultAgendaConfig :: AgendaConfig
defaultAgendaConfig = AgendaConfig
  { agMaxEntries = 3
  , agMaxAgendaStates = 200
  , agEnableDiversity = True
  , agBucketCap = 4
  , agCriticPerAction = 2
  , agBranchPerState = 10
  , agMaxCandidates = 40
  , agBitBudget = 20
  , agRequireConnected = False
  , agSafeClosureSteps = 3
  , agPremiseTopK = 4
  , agBarFloor = 0.0
  , agLeafExprBudget = 12
  , agLeafExprDepth = 1
  , agLeafExprCap = 10
  }

data DerivationRule
  = IntroducePi
  | IntroduceSigma
  | IntroduceWitness
  | OpenLoopGoal
  | ApplyModal
  | ApplyTemporal
  | ReusePremise
  | ApplyMacro
  | CriticRewrite
  | Normalize
  deriving (Show, Eq, Ord)

data DerivationCertificate = DerivationCertificate
  { dcStep :: !Int
  , dcRule :: !DerivationRule
  , dcAction :: !Action
  , dcSafe :: !Bool
  , dcExpr :: !MBTTExpr
  , dcMacroTemplate :: !(Maybe String)
  , dcPremisesUsed :: ![Int]
  , dcDischarged :: ![Obligation]
  , dcCapabilitiesGained :: ![String]
  , dcGoalsAfter :: ![Obligation]
  } deriving (Show, Eq)

data AgendaCandidate = AgendaCandidate
  { acTelescope :: !Telescope
  , acState :: !SearchState
  , acCertificates :: ![DerivationCertificate]
  , acPriority :: !Double
  } deriving (Show, Eq)

data NearMiss = NearMiss
  { nmReason :: !String
  , nmPriority :: !Double
  , nmSigmaUpper :: !Double
  , nmCriticalGoal :: !(Maybe Obligation)
  , nmGoalCount :: !Int
  , nmKappa :: !Int
  , nmOpenGoals :: ![Obligation]
  , nmAndClauses :: ![[Obligation]]
  , nmOrClauses :: ![[Obligation]]
  , nmLastAction :: !(Maybe Action)
  , nmLastRule :: !(Maybe DerivationRule)
  } deriving (Show, Eq)

data AgendaDiagnostics = AgendaDiagnostics
  { adExpandedStates :: !Int
  , adSigmaPrunes :: !Int
  , adDominancePrunes :: !Int
  , adCriticTransitions :: !Int
  , adActionFailures :: !Int
  , adNearMisses :: ![NearMiss]
  } deriving (Show, Eq)

data AgendaNode = AgendaNode
  { anState :: !SearchState
  , anCerts :: ![DerivationCertificate]
  } deriving (Show, Eq)

data DominanceKey = DominanceKey
  { dkCaps :: !CapabilitySig
  , dkGoals :: ![Obligation]
  , dkAndShape :: ![[Obligation]]
  , dkOrShape :: ![[Obligation]]
  , dkPremiseBucket :: ![Int]
  , dkKappaBucket :: !Int
  } deriving (Show, Eq, Ord)

data FrontierPoint = FrontierPoint
  { fpNuLower :: !Int
  , fpNuUpper :: !Int
  , fpKappa :: !Int
  , fpGoalCount :: !Int
  } deriving (Show, Eq)

type DominanceCache = Map.Map DominanceKey [FrontierPoint]

type StateSig = Word64
type ExprSig = Word64
type PriorityKey = (Down Double, Down Int, Int, Int, Down Int)

data Frontier = Frontier
  { frQueue :: !(Map.Map PriorityKey [AgendaNode])
  , frSize  :: !Int
  } deriving (Show, Eq)

data MacroAtom
  = AtomTopPremise
  | AtomSecondPremise
  | AtomVar1
  | AtomVar2
  | AtomUniverse
  deriving (Show, Eq, Ord)

data MacroTemplate
  = MacroUnary !Action !MacroAtom
  | MacroBinary !Action !MacroAtom !MacroAtom
  deriving (Show, Eq, Ord)

data MacroStats = MacroStats
  { msSupport :: !Int
  , msDebtGain :: !Int
  , msCapGain :: !Int
  , msBranchSavings :: !Int
  , msUses :: !Int
  } deriving (Show, Eq)

type MacroBank = Map.Map MacroTemplate MacroStats

agendaGenerateCandidates :: Library -> GoalProfile -> AgendaConfig -> [AgendaCandidate]
agendaGenerateCandidates lib profile cfg =
  fst (agendaGenerateCandidatesWithDiagnostics lib profile cfg)

agendaGenerateCandidatesWithDiagnostics :: Library -> GoalProfile -> AgendaConfig -> ([AgendaCandidate], AgendaDiagnostics)
agendaGenerateCandidatesWithDiagnostics lib profile cfg =
  let start = AgendaNode (deriveSearchState lib profile []) []
      startDiag = addNearMiss "root" start emptyDiagnostics
      (_, _, _, finals, diag) = searchLoop 0 (frontierSingleton start) Set.empty Map.empty Map.empty [] startDiag
      cands = map finalize finals ++ seedCandidates
      valid = filter (\c -> case checkTelescope lib (acTelescope c) of
                       CheckOK -> not (isFloatingPathTele (acTelescope c))
                       _ -> False) cands
  in (take (agMaxCandidates cfg) (sortOn (Down . acPriority) valid), finalizeDiagnostics diag)
  where
    seedCandidates :: [AgendaCandidate]
    seedCandidates =
      [ let st = deriveSearchState lib profile entries
        in AgendaCandidate
            { acTelescope = tele
            , acState = st
            , acCertificates = []
            , acPriority = nodePriority st + 1.0
            }
      | tele@(Telescope entries) <- goalSeedTelescopes
      , length entries <= agMaxEntries cfg
      ]

    goalSeedTelescopes :: [Telescope]
    goalSeedTelescopes =
      let needsFormer = NeedFormer `elem` gpIntents profile && not (any leHasDependentFunctions lib)
          needsHIT = NeedHIT `elem` gpIntents profile && any leHasDependentFunctions lib && not (any leHasLoop lib)
          needsHITProgress = NeedHIT `elem` gpIntents profile && any leHasLoop lib
          needsBridge = NeedBridge `elem` gpIntents profile
          needsModal = NeedModal `elem` gpIntents profile
          needsDifferential = NeedDifferential `elem` gpIntents profile
          needsCurvature = NeedCurvature `elem` gpIntents profile
          needsMetric = NeedMetric `elem` gpIntents profile
          needsHilbert = NeedHilbert `elem` gpIntents profile
          loopRefs = [i | (i, e) <- zip [1..] lib, leHasLoop e]
          modalRefs = [i | (i, e) <- zip [1..] lib, leHasModalOps e]
          differentialRefs = [i | (i, e) <- zip [1..] lib, leHasDifferentialOps e]
          curvatureRefs = [i | (i, e) <- zip [1..] lib, leHasCurvature e]
          metricRefs = [i | (i, e) <- zip [1..] lib, leHasMetric e]
          loopRef = case reverse loopRefs of
            (i:_) -> i
            [] -> max 1 (length lib)
          modalRef = case reverse modalRefs of
            (i:_) -> i
            [] -> max 1 (length lib)
          differentialRef = case reverse differentialRefs of
            (i:_) -> i
            [] -> max 1 (length lib)
          curvatureRef = case reverse curvatureRefs of
            (i:_) -> i
            [] -> max 1 (length lib)
          metricRef = case reverse metricRefs of
            (i:_) -> i
            [] -> max 1 (length lib)
          bridgeRef = case reverse loopRefs of
            (_:j:_) -> j
            _ -> max 1 (loopRef - 1)
          formerSeeds =
            if needsFormer
            then
              [ Telescope
                  [ TeleEntry "c1" Univ
                  , TeleEntry "c2" (Lam (Var 1))
                  , TeleEntry "c3" (Pi (Var 1) (Var 2))
                  ]
              , Telescope
                  [ TeleEntry "c1" Univ
                  , TeleEntry "c2" (Lam (Var 1))
                  , TeleEntry "c3" (Sigma (Var 1) (Var 2))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Lam (Pi (Var 1) (Var 2)))
                  , TeleEntry "c2" (App (App (Var 1) (Var 2)) (Var 3))
                  , TeleEntry "c3" (App (Lam (Var 1)) (Var 2))
                  ]
              ]
            else []
          hitSeeds =
            if needsHIT
            then
              [ Telescope
                  [ TeleEntry "c1" (App Univ (Var 1))
                  , TeleEntry "c2" (Var 1)
                  , TeleEntry "c3" (PathCon 1)
                  ]
              , Telescope
                  [ TeleEntry "c1" (App Univ (Var 1))
                  , TeleEntry "c2" (Var 1)
                  , TeleEntry "c3" (PathCon 2)
                  ]
              ]
            else []
          hitProgressSeeds =
            if needsHITProgress
            then
              [ Telescope
                  [ TeleEntry "c1" (App Univ (Var 1))
                  , TeleEntry "c2" (Var 1)
                  , TeleEntry "c3" (Susp (Lib loopRef))
                  ]
              ]
            else []
          bridgeSeeds =
            if needsBridge
            then
              [ Telescope
                  [ TeleEntry "c1" (Lib loopRef)
                  , TeleEntry "c2" (Lib bridgeRef)
                  , TeleEntry "c3" (Pi (Lib loopRef) (Lib bridgeRef))
                  ]
              , Telescope
                  [ TeleEntry "c1" (App (Lib loopRef) (Lib bridgeRef))
                  , TeleEntry "c2" (App (Lib bridgeRef) (Lib loopRef))
                  , TeleEntry "c3" (Pi (Lib loopRef) (Lib bridgeRef))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Lib loopRef)
                  , TeleEntry "c2" (Lib bridgeRef)
                  , TeleEntry "c3" (Id (Lib bridgeRef) (Var 1) (Var 1))
                  , TeleEntry "c4" (Pi (Lib loopRef) (Lib bridgeRef))
                  ]
              ]
            else []
          modalSeeds =
            if needsModal
            then
              [ Telescope
                  [ TeleEntry "c1" (Flat (Lib loopRef))
                  , TeleEntry "c2" (Sharp (Lib loopRef))
                  , TeleEntry "c3" (Disc (Lib loopRef))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Flat (Lib loopRef))
                  , TeleEntry "c2" (Sharp (Lib loopRef))
                  , TeleEntry "c3" (Shape (Lib loopRef))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Flat (Lib loopRef))
                  , TeleEntry "c2" (Sharp (Lib loopRef))
                  , TeleEntry "c3" (Disc (Lib loopRef))
                  , TeleEntry "c4" (Pi (Lib loopRef) (Lib loopRef))
                  ]
              ]
            else []
          differentialSeeds =
            if needsDifferential
            then
              [ Telescope
                  [ TeleEntry "c1" (Pi (Lib modalRef) (Pi (Var 1) (Var 1)))
                  , TeleEntry "c2" (Lam (Pi (Var 1) (Var 1)))
                  , TeleEntry "c3" (Pi (Var 1) (Var 1))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Pi (Lib modalRef) (Sigma (Var 1) (Var 1)))
                  , TeleEntry "c2" (Lam (Sigma (Var 1) (Var 1)))
                  , TeleEntry "c3" (Sigma (Var 1) (Var 1))
                  ]
              , Telescope
                  [ TeleEntry "c1" (App (Lib modalRef) (Var 1))
                  , TeleEntry "c2" (Lam (Pi (Var 1) (Var 1)))
                  , TeleEntry "c3" (Pi (Var 1) (Var 1))
                  ]
               ]
            else []
          curvatureSeeds =
            if needsCurvature
            then
              [ Telescope
                  [ TeleEntry "c1" (Pi (Lib differentialRef) (Id (Var 1) (Var 1) (Var 1)))
                  , TeleEntry "c2" (Id (Lib differentialRef) (App (Lib differentialRef) (PathCon 2)) (App (Lib differentialRef) (PathCon 2)))
                  , TeleEntry "c3" (App (Lib differentialRef) (PathCon 2))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Sigma (Lib differentialRef) (Id (Var 1) (Var 1) (Var 1)))
                  , TeleEntry "c2" (App (Lib differentialRef) (PathCon 2))
                  , TeleEntry "c3" (Id (Lib differentialRef) (App (Lib differentialRef) (PathCon 2)) (App (Lib differentialRef) (PathCon 2)))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Pi (Lib differentialRef) (Pi (Var 1) (Var 1)))
                  , TeleEntry "c2" (App (Lib differentialRef) (Var 1))
                  , TeleEntry "c3" (Id (Lib differentialRef) (App (Lib differentialRef) (PathCon 2)) (App (Lib differentialRef) (PathCon 2)))
                  ]
              ]
            else []
          metricSeeds =
            if needsMetric
            then
              [ Telescope
                  [ TeleEntry "c1" (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))
                  , TeleEntry "c2" (Pi (Sigma (Var 1) (Var 1)) (Lib differentialRef))
                  , TeleEntry "c3" (Pi (Var 1) (Pi (Var 1) (Var 1)))
                  , TeleEntry "c4" (Lam (App (Var 1) (Var 1)))
                  , TeleEntry "c5" (Pi (Lib curvatureRef) (Lib curvatureRef))
                  , TeleEntry "c6" (Lam (Pi (Var 1) (Var 1)))
                  , TeleEntry "c7" (Pi (Lib curvatureRef) (Var 1))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))
                  , TeleEntry "c2" (Pi (Sigma (Var 1) (Var 1)) (Lib differentialRef))
                  , TeleEntry "c3" (Pi (Lib curvatureRef) (Lib curvatureRef))
                  , TeleEntry "c4" (Pi (Lib curvatureRef) (Var 1))
                  ]
              ]
            else []
          hilbertSeeds =
            if needsHilbert
            then
              [ Telescope
                  [ TeleEntry "c1" (Sigma (Pi (Var 1) (Pi (Var 1) Univ)) (Var 1))
                  , TeleEntry "c2" (Pi (Var 1) (Var 1))
                  , TeleEntry "c3" (Pi (Var 1) (Sigma (Var 1) (Var 1)))
                  , TeleEntry "c4" (Pi (Lam (Var 1)) (Sigma (Var 1) (Var 2)))
                  , TeleEntry "c5" (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))
                  , TeleEntry "c6" (Pi (Lib metricRef) (Var 1))
                  , TeleEntry "c7" (Pi (Lib curvatureRef) (Var 1))
                  , TeleEntry "c8" (Pi (Lib differentialRef) (Var 1))
                  , TeleEntry "c9" (Lam (Pi (Var 1) Univ))
                  ]
              , Telescope
                  [ TeleEntry "c1" (Sigma (Pi (Var 1) (Pi (Var 1) Univ)) (Var 1))
                  , TeleEntry "c2" (Pi (Var 1) (Sigma (Var 1) (Var 1)))
                  , TeleEntry "c3" (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))
                  , TeleEntry "c4" (Pi (Lib metricRef) (Var 1))
                  , TeleEntry "c5" (Pi (Lib curvatureRef) (Var 1))
                  , TeleEntry "c6" (Pi (Lib differentialRef) (Var 1))
                  , TeleEntry "c7" (Lam (Pi (Var 1) Univ))
                  ]
              ]
            else []
      in formerSeeds ++ hitSeeds ++ hitProgressSeeds ++ bridgeSeeds ++ modalSeeds ++ differentialSeeds ++ curvatureSeeds ++ metricSeeds ++ hilbertSeeds

    isFloatingPathTele :: Telescope -> Bool
    isFloatingPathTele tele@(Telescope entries) =
      let exprs = map teType entries
          hasPath = any hasPathExpr exprs
          hasFormation = any hasFormationExpr exprs
      in hasPath && not hasFormation && Set.null (teleLibRefs tele)

    hasPathExpr :: MBTTExpr -> Bool
    hasPathExpr expr = case expr of
      PathCon _ -> True
      Lam a -> hasPathExpr a
      App a b -> hasPathExpr a || hasPathExpr b
      Pi a b -> hasPathExpr a || hasPathExpr b
      Sigma a b -> hasPathExpr a || hasPathExpr b
      Id a x y -> hasPathExpr a || hasPathExpr x || hasPathExpr y
      Refl a -> hasPathExpr a
      Susp a -> hasPathExpr a
      Trunc a -> hasPathExpr a
      Flat a -> hasPathExpr a
      Sharp a -> hasPathExpr a
      Disc a -> hasPathExpr a
      Shape a -> hasPathExpr a
      Next a -> hasPathExpr a
      Eventually a -> hasPathExpr a
      _ -> False

    hasFormationExpr :: MBTTExpr -> Bool
    hasFormationExpr expr = case expr of
      Univ -> True
      App Univ _ -> True
      _ -> False

    frontierSingleton :: AgendaNode -> Frontier
    frontierSingleton node = frontierInsert node frontierEmpty

    frontierEmpty :: Frontier
    frontierEmpty = Frontier Map.empty 0

    frontierInsert :: AgendaNode -> Frontier -> Frontier
    frontierInsert node (Frontier q sz) =
      let key = frontierPriorityKey (anState node)
      in Frontier (Map.insertWith (++) key [node] q) (sz + 1)

    frontierInsertMany :: [AgendaNode] -> Frontier -> Frontier
    frontierInsertMany nodes fr = foldl' (flip frontierInsert) fr nodes

    frontierPop :: Frontier -> (Maybe AgendaNode, Frontier)
    frontierPop (Frontier q sz) =
      case Map.minViewWithKey q of
        Nothing -> (Nothing, frontierEmpty)
        Just ((key, nodes), qTail) ->
          case nodes of
            [] -> frontierPop (Frontier qTail sz)
            (n:rest) ->
              let q' = if null rest then qTail else Map.insert key rest qTail
                  sz' = max 0 (sz - 1)
              in (Just n, Frontier q' sz')

    frontierToList :: Frontier -> [AgendaNode]
    frontierToList (Frontier q _) = concatMap snd (Map.toAscList q)

    frontierFromList :: [AgendaNode] -> Frontier
    frontierFromList = foldl' (flip frontierInsert) frontierEmpty

    frontierTrim :: Frontier -> Frontier
    frontierTrim fr
      | frSize fr <= agMaxAgendaStates cfg = fr
      | agEnableDiversity cfg =
          let maxStates = agMaxAgendaStates cfg
              bucketCap = max 1 (agBucketCap cfg)
              nodes = frontierToList fr
              step (!keptAcc, !spillAcc, !counts) node =
                let key = frontierBucket (anState node)
                    used = Map.findWithDefault 0 key counts
                in if used < bucketCap
                   then (node : keptAcc, spillAcc, Map.insert key (used + 1) counts)
                   else (keptAcc, node : spillAcc, counts)
              (keptRev, spillRev, _) = foldl' step ([], [], Map.empty) nodes
              kept = reverse keptRev
              spill = reverse spillRev
              primary = take maxStates kept
              chosen = if length primary >= maxStates
                       then primary
                       else primary ++ take (maxStates - length primary) spill
          in frontierFromList chosen
      | otherwise =
          frontierFromList (take (agMaxAgendaStates cfg) (frontierToList fr))

    frontierPriorityKey :: SearchState -> PriorityKey
    frontierPriorityKey st =
      ( Down (nodePriority st)
      , Down (ssNuUpper st)
      , ssKappa st
      , stateGoalCount st
      , Down (length (ssEntries st))
      )

    searchLoop :: Int
               -> Frontier
               -> Set.Set StateSig
               -> DominanceCache
               -> MacroBank
               -> [AgendaNode]
               -> AgendaDiagnostics
               -> (Set.Set StateSig, DominanceCache, MacroBank, [AgendaNode], AgendaDiagnostics)
    searchLoop !iter !frontier !seen !domCache !macroBank !finals !diag
      | iter >= agMaxAgendaStates cfg =
          let diag' = foldl' (\d n -> addNearMiss "frontier_timeout" n d) diag (frontierToList frontier)
          in (seen, domCache, macroBank, finals, diag')
      | frSize frontier <= 0 = (seen, domCache, macroBank, finals, diag)
      | otherwise =
          let (mNodeRaw, frontierRest0) = frontierPop frontier
              frontierRest = frontierTrim frontierRest0
          in case mNodeRaw of
               Nothing -> (seen, domCache, macroBank, finals, diag)
               Just node ->
                 let (node', macroBank1) = closeSafe macroBank node
                     st = anState node'
                     sig = stateSignature st
                 in if Set.member sig seen
                    then searchLoop (iter + 1) frontierRest seen domCache macroBank1 finals diag
                    else if not (passesSigmaUB st)
                    then searchLoop (iter + 1) frontierRest seen domCache macroBank1 finals (addSigmaPrune node' diag)
                    else
                      let seen' = Set.insert sig seen
                          (dominated, domCache') = insertFrontier (dominanceKey st) (frontierPoint st) domCache
                      in if dominated
                         then searchLoop (iter + 1) frontierRest seen' domCache' macroBank1 finals (addDominancePrune node' diag)
                         else if isTerminal st
                         then searchLoop (iter + 1) frontierRest seen' domCache' macroBank1 (node' : finals) (diag { adExpandedStates = adExpandedStates diag + 1 })
                         else
                           let (domCache'', macroBank'', children, diagAfterExpand) =
                                 expandNode domCache' macroBank1 node' (diag { adExpandedStates = adExpandedStates diag + 1 })
                               frontierNext = frontierTrim (frontierInsertMany children frontierRest)
                               stalled = null children
                               closed = stateGoalCount st == 0
                               finals' = if stalled && closed then node' : finals else finals
                               diagStall = if stalled then addNearMiss "stalled" node' diagAfterExpand else diagAfterExpand
                           in searchLoop (iter + 1) frontierNext seen' domCache'' macroBank'' finals' diagStall

    frontierBucket :: SearchState -> (Maybe Obligation, [Obligation], Int, Int)
    frontierBucket st =
      let goals = take 4 (sort (map bucketGoal (stateOpenGoals st)))
      in (stateCriticalGoal st, goals, stateGoalCount st, ssKappa st `div` 2)

    bucketGoal :: Obligation -> Obligation
    bucketGoal goal = case goal of
      NeedLoop d -> NeedLoop (min 4 d)
      NeedLoopLift d -> NeedLoopLift (min 4 d)
      NeedLoopCoherence d -> NeedLoopCoherence (min 4 d)
      NeedSourceReuse d loopReq -> NeedSourceReuse (min 4 (max 1 d)) loopReq
      NeedTargetReuse d maxD -> NeedTargetReuse (min 4 (max 1 d)) (fmap (\u -> min 4 (max 1 u)) maxD)
      NeedCompatibilityPath d -> NeedCompatibilityPath (min 4 (max 1 d))
      _ -> goal

    emptyDiagnostics :: AgendaDiagnostics
    emptyDiagnostics = AgendaDiagnostics
      { adExpandedStates = 0
      , adSigmaPrunes = 0
      , adDominancePrunes = 0
      , adCriticTransitions = 0
      , adActionFailures = 0
      , adNearMisses = []
      }

    finalizeDiagnostics :: AgendaDiagnostics -> AgendaDiagnostics
    finalizeDiagnostics d =
      d { adNearMisses = take 8 (sortOn nearMissOrder (adNearMisses d)) }

    nearMissOrder :: NearMiss -> (Down Double, Down Double, Int)
    nearMissOrder nm = (Down (nmSigmaUpper nm), Down (nmPriority nm), nmGoalCount nm)

    addNearMiss :: String -> AgendaNode -> AgendaDiagnostics -> AgendaDiagnostics
    addNearMiss reason node diag =
      let miss = nearMissFromNode reason node
          misses = take 16 (sortOn nearMissOrder (miss : adNearMisses diag))
      in diag { adNearMisses = misses }

    addSigmaPrune :: AgendaNode -> AgendaDiagnostics -> AgendaDiagnostics
    addSigmaPrune node diag = (addNearMiss "sigma_prune" node diag)
      { adSigmaPrunes = adSigmaPrunes diag + 1 }

    addDominancePrune :: AgendaNode -> AgendaDiagnostics -> AgendaDiagnostics
    addDominancePrune node diag = (addNearMiss "dominance_prune" node diag)
      { adDominancePrunes = adDominancePrunes diag + 1 }

    addActionFailure :: AgendaNode -> Action -> AgendaDiagnostics -> AgendaDiagnostics
    addActionFailure node act diag =
      let withMiss = addNearMiss ("action_failed:" ++ show act) node diag
      in withMiss { adActionFailures = adActionFailures diag + 1 }

    nearMissFromNode :: String -> AgendaNode -> NearMiss
    nearMissFromNode reason node =
      let st = anState node
          dg = ssDebtGraph st
          lastCert = case reverse (anCerts node) of
            [] -> Nothing
            (c:_) -> Just c
      in NearMiss
          { nmReason = reason
          , nmPriority = nodePriority st
          , nmSigmaUpper = sigmaUpperBound st
          , nmCriticalGoal = stateCriticalGoal st
          , nmGoalCount = stateGoalCount st
          , nmKappa = ssKappa st
          , nmOpenGoals = stateOpenGoals st
          , nmAndClauses = dgAndClauses dg
          , nmOrClauses = dgOrClauses dg
          , nmLastAction = fmap dcAction lastCert
          , nmLastRule = fmap dcRule lastCert
          }

    closeSafe :: MacroBank -> AgendaNode -> (AgendaNode, MacroBank)
    closeSafe startBank startNode = go 0 Set.empty startBank startNode
      where
        go :: Int -> Set.Set StateSig -> MacroBank -> AgendaNode -> (AgendaNode, MacroBank)
        go !k !seenSafe !bank !node
          | k >= agSafeClosureSteps cfg = (node, bank)
          | isTerminal (anState node) = (node, bank)
          | otherwise =
              let st = anState node
                  sig = stateSignature st
              in if Set.member sig seenSafe
                then (node, bank)
                 else
                   let entries = ssEntries st
                       hole = Hole entries AnyHole 0 (agBitBudget cfg)
                       safeActions = deterministicSafeActions st hole
                       attempts =
                         [ child
                         | act <- safeActions
                         , let (mChild, _) = applyTransition bank node act
                         , child <- maybeToList mChild
                         , let after = anState child
                         , safeImproves st after
                         , safeAdmissible after
                         ]
                   in case attempts of
                        (best:_) ->
                          let bank' = learnFromTransition (max 1 (length safeActions)) best bank
                          in go (k + 1) (Set.insert sig seenSafe) bank' best
                        [] -> (node, bank)

        deterministicSafeActions :: SearchState -> Hole -> [Action]
        deterministicSafeActions st hole =
          let allActions = [ a
                           | a <- validActionsWithProfile profile hole lib
                           , premiseAllowed st a
                           ]
              shortlisted = take (agPremiseTopK cfg) (premisesForGoal st (stateCriticalGoal st))
              uniquePremise = case shortlisted of
                [p] -> Just p
                _ -> Nothing
              forcedRefl = case stateCriticalGoal st of
                Just NeedCoherence -> True
                Just (NeedLoopCoherence _) -> True
                _ -> False
              forcedWitness = stateCriticalGoal st == Just NeedWitness
              isDeterministic act = case act of
                AUniv -> True
                ARefl -> forcedRefl
                ALib i -> maybe False (== i) uniquePremise
                AVar i -> forcedWitness && i == 1
                _ -> False
              rank act = case act of
                ARefl -> 0 :: Int
                ALib _ -> 1
                AVar _ -> 2
                AUniv -> 3
                _ -> 9
          in sortOn (\a -> (rank a, Down (actionPriority (length lib) a))) (filter isDeterministic allActions)

    safeImproves :: SearchState -> SearchState -> Bool
    safeImproves before after =
      stateGoalCount after < stateGoalCount before
      || ssNuLower after > ssNuLower before
      || ssNuUpper after > ssNuUpper before

    safeAdmissible :: SearchState -> Bool
    safeAdmissible after =
      let atTerminal = length (ssEntries after) >= agMaxEntries cfg
          unresolved = stateGoalCount after > 0
      in not (atTerminal && unresolved)

    expandNode :: DominanceCache -> MacroBank -> AgendaNode -> AgendaDiagnostics -> (DominanceCache, MacroBank, [AgendaNode], AgendaDiagnostics)
    expandNode domCache macroBank node diag0 =
      let st = anState node
          entries = ssEntries st
          hole = Hole entries AnyHole 0 (agBitBudget cfg)
          allActions = validActionsWithProfile profile hole lib
          filtered = filter (premiseAllowed st) allActions
          unsafe = filter (not . isSafeAction) filtered
          branchPool0 = if null unsafe then filtered else unsafe
          critical = stateCriticalGoal st
          criticalPool = case critical of
            Just g ->
              [a | a <- branchPool0, actionGoalGainFor g a > 0]
            Nothing -> []
          branchPool = if null criticalPool then branchPool0 else criticalPool
          ranked = sortOn (actionRank macroBank st) branchPool
          chosen = take (agBranchPerState cfg) ranked
          branchCount = max 1 (length branchPool)
          promoteChild :: Bool -> AgendaNode -> (DominanceCache, MacroBank, [AgendaNode], AgendaDiagnostics) -> (DominanceCache, MacroBank, [AgendaNode], AgendaDiagnostics)
          promoteChild isCritic child (!cache, !bank, !acc, !diag) =
            let st' = anState child
            in if not (passesSigmaUB st')
               then (cache, bank, acc, addSigmaPrune child diag)
               else
                 let (dominated, cache') = insertFrontier (dominanceKey st') (frontierPoint st') cache
                 in if dominated
                    then (cache', bank, acc, addDominancePrune child diag)
                    else
                      let bank' = if isCritic then bank else learnFromTransition branchCount child bank
                          diag' = if isCritic then diag { adCriticTransitions = adCriticTransitions diag + 1 } else diag
                      in (cache', bank', child : acc, diag')
          addChild (!cache, !bank, !acc, !diag) act =
            case applyTransition bank node act of
              (Just child, _) ->
                promoteChild False child (cache, bank, acc, diag)
              (Nothing, _) ->
                let critics = criticNodes node act
                    diagFail = if null critics
                               then addActionFailure node act diag
                               else diag
                in foldl (\triple ch -> promoteChild True ch triple) (cache, bank, acc, diagFail) critics
          (domCache', macroBank', revChildren, diagFinal) = foldl addChild (domCache, macroBank, [], diag0) chosen
      in (domCache', macroBank', reverse revChildren, diagFinal)

    criticNodes :: AgendaNode -> Action -> [AgendaNode]
    criticNodes node act =
      let before = anState node
          beforeSig = stateSignature before
          beforeSigma = sigmaUpperBound before
          beforeGoals = stateGoalCount before
          beforeCritical = stateCriticalGoal before
          planStates = criticPlanStates act before
          mkNode idx (CriticPlanState plan after) =
            let cert = mkCriticCertificate before after act (cpLabel plan) idx
            in AgendaNode after (anCerts node ++ [cert])
          candidateNodes =
            [ mkNode i cps
            | (i, cps) <- zip [1..] planStates
            , let st' = cpsState cps
            , stateSignature st' /= beforeSig || st' /= before
            ]
          criticRank child =
            let st = anState child
                criticalDebt = case beforeCritical of
                  Just g -> criticalDebtCount g st
                  Nothing -> 0
            in ( Down (sigmaUpperBound st)
               , stateGoalCount st
               , criticalDebt
               )
          improves child =
            let st = anState child
                sigmaUp = sigmaUpperBound st
                criticalGain = case beforeCritical of
                  Nothing -> False
                  Just g -> criticalDebtCount g st < criticalDebtCount g before
            in sigmaUp > beforeSigma
               || stateGoalCount st < beforeGoals
               || criticalGain
          ranked =
            sortOn criticRank
              [child | child <- candidateNodes, improves child]
          criticCap = max 0 (agCriticPerAction cfg)
      in take criticCap ranked

    premiseAllowed :: SearchState -> Action -> Bool
    premiseAllowed st act = case act of
      ALib i -> null shortlist || i `elem` shortlist
      _ -> True
      where
        shortlist = take (agPremiseTopK cfg) (premisesForGoal st (stateCriticalGoal st))

    premisesForGoal :: SearchState -> Maybe Obligation -> [Int]
    premisesForGoal st maybeGoal =
      let buckets = ssPremiseBuckets st
          source = pbSource buckets
          target = pbTarget buckets
          law = pbLaw buckets
          aux = pbAux buckets
          merged = ssPremises st
      in case maybeGoal of
          Just (NeedSourceReuse _ _) -> source ++ aux ++ merged
          Just (NeedTargetReuse _ _) -> target ++ aux ++ merged
          Just NeedMapHead -> source ++ target ++ law ++ merged
          Just NeedFiberWitness -> source ++ aux ++ merged
          Just NeedTransportLaw -> law ++ target ++ merged
          Just (NeedCompatibilityPath _) -> target ++ law ++ merged
          Just NeedSealBridge -> law ++ target ++ source ++ merged
          Just NeedTruncBridge -> aux ++ law ++ merged
          Just (NeedLoopLift _) -> target ++ law ++ source ++ merged
          Just (NeedLoopCoherence _) -> law ++ target ++ merged
          Just NeedDifferentialBridge -> law ++ source ++ target ++ aux ++ merged
          Just NeedCurvatureBridge -> law ++ target ++ source ++ aux ++ merged
          Just NeedMetricBundle -> law ++ target ++ source ++ aux ++ merged
          Just NeedHilbertBundle -> law ++ target ++ source ++ aux ++ merged
          _ -> merged

    actionRank :: MacroBank -> SearchState -> Action -> (Down Int, Down Int, Down Int, Down Int, Down Int, Int)
    actionRank bank st act =
      let safe = if isSafeAction act then 1 else 0
          criticalBonus = case stateCriticalGoal st of
            Just g -> actionGoalGainFor g act
            Nothing -> 0
          macroBonus = macroActionBonus bank st act
          gain = actionGoalGain st act
          pri = actionPriority (length lib) act
          form = actionFormRank act
      in (Down safe, Down criticalBonus, Down macroBonus, Down gain, Down pri, form)

    applyTransition :: MacroBank -> AgendaNode -> Action -> (Maybe AgendaNode, Maybe MacroTemplate)
    applyTransition bank node act =
      let st = anState node
          entries = ssEntries st
          name = "c" ++ show (length entries + 1)
          candidates = exprCandidates bank st act
          tryExpr [] = (Nothing, Nothing)
          tryExpr ((expr, macroUsed):rest) =
            let entry = TeleEntry name expr
                newEntries = entries ++ [entry]
                tele = Telescope newEntries
            in case checkTelescope lib tele of
                 CheckFail _ -> tryExpr rest
                 CheckOK ->
                   let connectedOk = not (agRequireConnected cfg)
                                  || length newEntries <= 1
                                  || teleIsConnected tele
                                  || teleReferencesWindow tele (length lib)
                   in if not connectedOk
                      then tryExpr rest
                      else
                        let st' = advanceSearchState lib profile st act expr newEntries
                            cert = mkCertificate st st' act expr macroUsed (length (anCerts node) + 1)
                        in (Just (AgendaNode st' (anCerts node ++ [cert])), macroUsed)
      in tryExpr candidates

    mkCertificate :: SearchState -> SearchState -> Action -> MBTTExpr -> Maybe MacroTemplate -> Int -> DerivationCertificate
    mkCertificate before after act expr macroUsed stepNo =
      let usedPremises = [i | i <- exprLibRefs expr, i `elem` ssPremises before]
          discharged = [g | g <- stateOpenGoals before, g `notElem` stateOpenGoals after]
          gained = capabilityDelta (ssCaps before) (ssCaps after)
      in DerivationCertificate
          { dcStep = stepNo
          , dcRule = deriveRule before act expr macroUsed
          , dcAction = act
          , dcSafe = isSafeAction act
          , dcExpr = expr
          , dcMacroTemplate = fmap showMacroTemplate macroUsed
          , dcPremisesUsed = usedPremises
          , dcDischarged = discharged
          , dcCapabilitiesGained = gained
          , dcGoalsAfter = stateOpenGoals after
          }

    mkCriticCertificate :: SearchState -> SearchState -> Action -> String -> Int -> DerivationCertificate
    mkCriticCertificate before after act planLabel stepNo =
      let expr = deterministicExpr before act
          discharged = [g | g <- stateOpenGoals before, g `notElem` stateOpenGoals after]
      in DerivationCertificate
          { dcStep = stepNo
          , dcRule = CriticRewrite
          , dcAction = act
          , dcSafe = True
          , dcExpr = expr
          , dcMacroTemplate = Just ("Critic:" ++ planLabel)
          , dcPremisesUsed = []
          , dcDischarged = discharged
          , dcCapabilitiesGained = []
          , dcGoalsAfter = stateOpenGoals after
          }

    deriveRule :: SearchState -> Action -> MBTTExpr -> Maybe MacroTemplate -> DerivationRule
    deriveRule st act expr macroUsed
      | macroUsed /= Nothing = ApplyMacro
      | usesMacro st expr = ApplyMacro
      | otherwise = case act of
          APi -> IntroducePi
          ASigma -> IntroduceSigma
          AVar _ -> IntroduceWitness
          ALib _ -> ReusePremise
          APathCon _ -> OpenLoopGoal
          ASusp -> OpenLoopGoal
          ATrunc -> OpenLoopGoal
          AId -> OpenLoopGoal
          AFlat -> ApplyModal
          ASharp -> ApplyModal
          ADisc -> ApplyModal
          AShape -> ApplyModal
          ANext -> ApplyTemporal
          AEventually -> ApplyTemporal
          ALam -> Normalize
          ARefl -> Normalize
          AApp -> IntroduceWitness
          AUniv -> Normalize

    usesMacro :: SearchState -> MBTTExpr -> Bool
    usesMacro st expr =
      let refs = exprLibRefs expr
      in not (null refs) && any (`elem` ssPremises st) refs

    learnFromTransition :: Int -> AgendaNode -> MacroBank -> MacroBank
    learnFromTransition branchCount node bank = case reverse (anCerts node) of
      [] -> bank
      (cert:_) -> case macroTemplateFromExpr (dcAction cert) (dcExpr cert) of
        Nothing -> bank
        Just templ ->
          let debtGain = length (dcDischarged cert)
              capGain = length (dcCapabilitiesGained cert)
              useGain = case dcRule cert of
                ApplyMacro -> 1
                _ -> 0
              branchGain = max 0 (branchCount - 1)
              delta = MacroStats
                { msSupport = 1
                , msDebtGain = debtGain
                , msCapGain = capGain
                , msBranchSavings = branchGain
                , msUses = useGain
                }
              combine new old = MacroStats
                { msSupport = msSupport old + msSupport new
                , msDebtGain = msDebtGain old + msDebtGain new
                , msCapGain = msCapGain old + msCapGain new
                , msBranchSavings = msBranchSavings old + msBranchSavings new
                , msUses = msUses old + msUses new
                }
          in Map.insertWith combine templ delta bank

    macroActionBonus :: MacroBank -> SearchState -> Action -> Int
    macroActionBonus bank st act = case bestMacroOption bank st act of
      Nothing -> 0
      Just (_, stats, _) -> macroUtility stats

    exprCandidates :: MacroBank -> SearchState -> Action -> [(MBTTExpr, Maybe MacroTemplate)]
    exprCandidates bank st act =
      let critical = stateCriticalGoal st
          macroCand = case bestMacroOption bank st act of
            Just (templ, _, expr) -> [(expr, Just templ)]
            Nothing -> []
          criticalSeeds = [(expr, Nothing) | expr <- criticalGoalExprs st act critical]
          leafCands = [(expr, Nothing) | expr <- localLeafExprs st act critical]
          fallback = [(deterministicExpr st act, Nothing)]
      in dedupExprCandidates (criticalSeeds ++ macroCand ++ leafCands ++ fallback)

    dedupExprCandidates :: [(MBTTExpr, Maybe MacroTemplate)] -> [(MBTTExpr, Maybe MacroTemplate)]
    dedupExprCandidates = reverse . snd . foldl step (Set.empty, [])
      where
        step (seen, acc) cand@(expr, _) =
          let key = exprSignature expr
          in if Set.member key seen
             then (seen, acc)
             else (Set.insert key seen, cand : acc)

    localLeafExprs :: SearchState -> Action -> Maybe Obligation -> [MBTTExpr]
    localLeafExprs st act critical
      | not (useLeafSolverForAction act) = []
      | agLeafExprDepth cfg <= 0 = []
      | agLeafExprCap cfg <= 0 = []
      | otherwise =
          let entries = ssEntries st
              libSize = length lib
              ctxDepth = length entries
              minBudget = actionBitCost act + 4
              budget = max minBudget (min (agBitBudget cfg) (agLeafExprBudget cfg))
              poolCap = max (agLeafExprCap cfg) (agLeafExprCap cfg * 2)
              rawExprs = take poolCap (enumerateExprs libSize ctxDepth budget (agLeafExprDepth cfg) lib)
              matched = filter (exprMatchesAction act) rawExprs
              focused = case critical of
                Just g ->
                  let touch = [e | e <- matched, exprSupportsCritical g e]
                  in if null touch then matched else touch
                Nothing -> matched
              ranked = sortOn (leafExprRank st act) focused
          in take (agLeafExprCap cfg) ranked

    criticalGoalExprs :: SearchState -> Action -> Maybe Obligation -> [MBTTExpr]
    criticalGoalExprs st act critical = case critical of
      Nothing -> []
      Just NeedTypeFormer -> case act of
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        ALam -> [Lam termAtom]
        _ -> []
      Just NeedWitness -> case act of
        AVar _ -> [termAtom]
        ALib _ -> [libAtom]
        AApp -> [App libAtom termAtom]
        _ -> []
      Just (NeedLoop d) -> case act of
        APathCon _ -> [PathCon d]
        AId -> [Id typeAtom termAtom termAtom]
        ASusp -> [Susp typeAtom]
        ATrunc -> [Trunc typeAtom]
        _ -> []
      Just (NeedLoopLift d) -> case act of
        APathCon _ -> [PathCon d]
        AId -> [Id typeAtom termAtom termAtom]
        ASusp -> [Susp typeAtom]
        ATrunc -> [Trunc typeAtom]
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        AApp -> [App libAtom termAtom]
        _ -> []
      Just (NeedLoopCoherence _) -> case act of
        ARefl -> [Refl termAtom]
        AId -> [Id typeAtom termAtom termAtom]
        ALam -> [Lam termAtom]
        AApp -> [App libAtom termAtom]
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        _ -> []
      Just NeedTruncBridge -> case act of
        ATrunc -> [Trunc (PathCon loopTarget), Trunc typeAtom]
        ASusp -> [Susp typeAtom]
        APathCon _ -> [PathCon loopTarget]
        AId -> [Id typeAtom termAtom termAtom]
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        AApp -> [App libAtom termAtom]
        _ -> []
      Just (NeedSourceReuse d loopReq) -> case act of
        ALib _ -> [libAtom]
        AVar _ -> [termAtom]
        AApp -> [App libAtom termAtom]
        APathCon _ -> [PathCon (max 1 d) | loopReq]
        AId -> [Id typeAtom termAtom termAtom]
        _ -> []
      Just (NeedTargetReuse d _) -> case act of
        APathCon _ -> [PathCon (max 1 d)]
        AId -> [Id typeAtom termAtom termAtom]
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        AApp -> [App libAtom termAtom]
        _ -> []
      Just NeedMapHead -> case act of
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        ALam -> [Lam termAtom]
        AApp -> [App libAtom termAtom]
        _ -> []
      Just NeedFiberWitness -> case act of
        AVar _ -> [termAtom]
        ALib _ -> [libAtom]
        AApp -> [App libAtom termAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        _ -> []
      Just NeedTransportLaw -> case act of
        AId -> [Id typeAtom termAtom termAtom]
        ARefl -> [Refl termAtom]
        APi -> [Pi typeAtom binderAtom]
        AApp -> [App libAtom termAtom]
        ALam -> [Lam termAtom]
        _ -> []
      Just (NeedCompatibilityPath d) -> case act of
        APathCon _ -> [PathCon (max 1 d)]
        AId -> [Id typeAtom termAtom termAtom]
        ARefl -> [Refl termAtom]
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        _ -> []
      Just NeedSealBridge -> case act of
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        AId -> [Id typeAtom termAtom termAtom]
        AApp -> [App libAtom termAtom]
        ALam -> [Lam termAtom]
        _ -> []
      Just NeedModalBundle -> case act of
        AFlat -> [Flat typeAtom]
        ASharp -> [Sharp typeAtom]
        ADisc -> [Disc typeAtom]
        AShape -> [Shape typeAtom]
        _ -> []
      Just NeedTemporalBridge -> case act of
        ANext -> [Next typeAtom]
        AEventually -> [Eventually typeAtom]
        APi ->
          [ Pi (Next typeAtom) (Eventually typeAtom)
          , Pi typeAtom (Next typeAtom)
          ]
        ASigma ->
          [ Sigma (Next typeAtom) (Eventually typeAtom)
          ]
        AApp ->
          [ App libAtom (Next termAtom)
          , App libAtom (Eventually termAtom)
          ]
        ALib _ -> [libAtom]
        AFlat -> [Flat (Next typeAtom)]
        AShape -> [Shape (Next typeAtom)]
        AId -> [Id typeAtom (Next termAtom) (Eventually termAtom)]
        ALam -> [Lam (Next termAtom)]
        _ -> []
      Just NeedDifferentialBridge -> case act of
        APi -> [Pi typeAtom binderAtom]
        ASigma -> [Sigma typeAtom binderAtom]
        ALam -> [Lam (Pi termAtom termAtom)]
        AApp -> [App libAtom termAtom]
        ALib _ -> [libAtom]
        AId -> [Id typeAtom termAtom termAtom]
        _ -> []
      Just NeedCurvatureBridge -> case act of
        APathCon _ -> []
        AId -> [Id typeAtom (App libAtom (PathCon 2)) (App libAtom (PathCon 2))]
        APi -> [Pi typeAtom (Id (App libAtom (PathCon 2)) (App libAtom (PathCon 2)) (App libAtom (PathCon 2)))]
        ASigma -> [Sigma typeAtom (Id (App libAtom (PathCon 2)) (App libAtom (PathCon 2)) (App libAtom (PathCon 2)))]
        AApp -> [App libAtom (PathCon 2)]
        ALib _ -> [libAtom]
        _ -> []
      Just NeedMetricBundle -> case act of
        APi -> [Pi (Sigma (Pi typeAtom typeAtom) (Pi typeAtom typeAtom)) (libAtomOr typeAtom)]
        ASigma -> [Sigma (Pi typeAtom typeAtom) (Pi typeAtom typeAtom)]
        AId -> [Id typeAtom (App libAtom termAtom) (App libAtom termAtom)]
        AApp -> [App libAtom termAtom]
        ALam -> [Lam (Pi termAtom termAtom)]
        ALib _ -> [libAtom]
        _ -> []
      Just NeedHilbertBundle -> case act of
        APi -> [Pi (Var 1) (Sigma (Var 1) (Var 1)), Pi (libAtomOr typeAtom) (Var 1), Pi (Lam termAtom) (Sigma termAtom termAtom)]
        ASigma -> [Sigma (Pi typeAtom (Pi typeAtom Univ)) (Var 1), Sigma (Pi typeAtom typeAtom) (Pi typeAtom typeAtom)]
        AId -> [Id typeAtom (App libAtom termAtom) (App libAtom termAtom)]
        AApp -> [App libAtom termAtom]
        ALam -> [Lam (Pi termAtom Univ), Lam (Pi termAtom termAtom)]
        ALib _ -> [libAtom]
        _ -> []
      Just NeedCoherence -> case act of
        ARefl -> [Refl termAtom]
        AId -> [Id typeAtom termAtom termAtom]
        ALam -> [Lam termAtom]
        AApp -> [App libAtom termAtom]
        _ -> []
      where
        entries = ssEntries st
        premises = take (agPremiseTopK cfg) (premisesForGoal st critical)
        typeAtom = case premises of
          (p:_) -> Lib p
          [] -> if null entries then Univ else Var 1
        binderAtom = if null entries then typeAtom else Var 1
        termAtom = if null entries then Var 1 else Var 1
        libAtom = case premises of
          (p:_) -> Lib p
          [] -> if null entries then Univ else Var 1
        libAtomOr fallback = case premises of
          (p:_) -> Lib p
          [] -> fallback
        loopTarget =
          let dims =
                [ d
                | g <- ssGoals st
                , d <- case g of
                         NeedLoop q -> [q]
                         NeedLoopLift q -> [q]
                         NeedTargetReuse q _ -> [q]
                         NeedCompatibilityPath q -> [q]
                         _ -> []
                ]
          in max 1 (if null dims then 1 else maximum dims)

    exprSupportsCritical :: Obligation -> MBTTExpr -> Bool
    exprSupportsCritical goal expr = case goal of
      NeedTypeFormer -> case expr of
        Pi _ _ -> True
        Sigma _ _ -> True
        Lam _ -> True
        _ -> False
      NeedWitness -> case expr of
        Var _ -> True
        Lib _ -> True
        App _ _ -> True
        _ -> False
      NeedLoop d -> case expr of
        PathCon q -> q == d
        Id _ _ _ -> True
        Susp _ -> True
        Trunc _ -> True
        _ -> False
      NeedLoopLift d -> case expr of
        PathCon q -> q == d
        Id _ _ _ -> True
        Susp _ -> True
        Trunc _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        App _ _ -> True
        _ -> False
      NeedLoopCoherence _ -> case expr of
        Refl _ -> True
        Id _ _ _ -> True
        Lam _ -> True
        App _ _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        _ -> False
      NeedTruncBridge -> case expr of
        Trunc _ -> True
        Susp _ -> True
        Id _ _ _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        App _ _ -> True
        _ -> False
      NeedSourceReuse d loopReq -> case expr of
        Var _ -> True
        Lib _ -> True
        App _ _ -> True
        PathCon q -> loopReq && q == d
        Id _ _ _ -> True
        _ -> False
      NeedTargetReuse d _ -> case expr of
        PathCon q -> q == d || q == d + 1
        Id _ _ _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        App _ _ -> True
        _ -> False
      NeedMapHead -> case expr of
        Pi _ _ -> True
        Sigma _ _ -> True
        Lam _ -> True
        App _ _ -> True
        _ -> False
      NeedFiberWitness -> case expr of
        Var _ -> True
        Lib _ -> True
        App _ _ -> True
        Sigma _ _ -> True
        _ -> False
      NeedTransportLaw -> case expr of
        Id _ _ _ -> True
        Refl _ -> True
        Pi _ _ -> True
        App _ _ -> True
        Lam _ -> True
        _ -> False
      NeedCompatibilityPath d -> case expr of
        PathCon q -> q == d || q == d + 1
        Id _ _ _ -> True
        Refl _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        _ -> False
      NeedSealBridge -> case expr of
        Pi _ _ -> True
        Sigma _ _ -> True
        Id _ _ _ -> True
        App _ _ -> True
        Lam _ -> True
        _ -> False
      NeedModalBundle -> case expr of
        Flat _ -> True
        Sharp _ -> True
        Disc _ -> True
        Shape _ -> True
        _ -> False
      NeedDifferentialBridge -> case expr of
        Pi _ _ -> True
        Sigma _ _ -> True
        Lam _ -> True
        App _ _ -> True
        Lib _ -> True
        Id _ _ _ -> True
        _ -> False
      NeedCurvatureBridge -> case expr of
        PathCon d -> d >= 2
        Id _ _ _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        App _ _ -> True
        Lib _ -> True
        _ -> False
      NeedMetricBundle -> case expr of
        Sigma (Pi _ _) (Pi _ _) -> True
        Pi _ _ -> True
        Id _ _ _ -> True
        App _ _ -> True
        Lam _ -> True
        Lib _ -> True
        _ -> False
      NeedHilbertBundle -> case expr of
        Sigma (Pi _ _) _ -> True
        Pi _ (Sigma _ _) -> True
        Pi (Lam _) (Sigma _ _) -> True
        Pi (Lib _) _ -> True
        Lam (Pi _ Univ) -> True
        Id _ _ _ -> True
        App _ _ -> True
        Lib _ -> True
        _ -> False
      NeedTemporalBridge -> case expr of
        Next _ -> True
        Eventually _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        App _ _ -> True
        Lib _ -> True
        Flat _ -> True
        Shape _ -> True
        Id _ _ _ -> True
        Lam _ -> True
        _ -> False
      NeedCoherence -> case expr of
        Refl _ -> True
        Id _ _ _ -> True
        Lam _ -> True
        App _ _ -> True
        _ -> False

    useLeafSolverForAction :: Action -> Bool
    useLeafSolverForAction act = case act of
      APi -> True
      ASigma -> True
      ALam -> True
      AApp -> True
      AId -> True
      ARefl -> True
      ASusp -> True
      ATrunc -> True
      AFlat -> True
      ASharp -> True
      ADisc -> True
      AShape -> True
      ANext -> True
      AEventually -> True
      _ -> False

    exprMatchesAction :: Action -> MBTTExpr -> Bool
    exprMatchesAction act expr = case act of
      AUniv -> case expr of
        Univ -> True
        _ -> False
      AVar i -> case expr of
        Var j -> i == j
        _ -> False
      ALib i -> case expr of
        Lib j -> i == j
        _ -> False
      APathCon d -> case expr of
        PathCon q -> d == q
        _ -> False
      APi -> case expr of Pi _ _ -> True; _ -> False
      ASigma -> case expr of Sigma _ _ -> True; _ -> False
      ALam -> case expr of Lam _ -> True; _ -> False
      AApp -> case expr of App _ _ -> True; _ -> False
      AId -> case expr of Id _ _ _ -> True; _ -> False
      ARefl -> case expr of Refl _ -> True; _ -> False
      ASusp -> case expr of Susp _ -> True; _ -> False
      ATrunc -> case expr of Trunc _ -> True; _ -> False
      AFlat -> case expr of Flat _ -> True; _ -> False
      ASharp -> case expr of Sharp _ -> True; _ -> False
      ADisc -> case expr of Disc _ -> True; _ -> False
      AShape -> case expr of Shape _ -> True; _ -> False
      ANext -> case expr of Next _ -> True; _ -> False
      AEventually -> case expr of Eventually _ -> True; _ -> False

    leafExprRank :: SearchState -> Action -> MBTTExpr -> (Down Int, Int, Int, Down Int)
    leafExprRank st act expr =
      let shortlist = take (agPremiseTopK cfg) (premisesForGoal st (stateCriticalGoal st))
          refs = exprLibRefs expr
          refHits = length [i | i <- refs, i `elem` shortlist]
          goalGain = actionGoalGain st act
          nodePenalty = exprNodeCount expr
          tie = exprKindRank expr
      in (Down refHits, nodePenalty, tie, Down goalGain)

    exprKindRank :: MBTTExpr -> Int
    exprKindRank expr = case expr of
      Pi _ _ -> 0
      Sigma _ _ -> 1
      App _ _ -> 2
      Lam _ -> 3
      Id _ _ _ -> 4
      Susp _ -> 5
      Trunc _ -> 6
      Flat _ -> 7
      Sharp _ -> 8
      Disc _ -> 9
      Shape _ -> 10
      Next _ -> 11
      Eventually _ -> 12
      Refl _ -> 13
      _ -> 20

    bestMacroOption :: MacroBank -> SearchState -> Action -> Maybe (MacroTemplate, MacroStats, MBTTExpr)
    bestMacroOption bank st act =
      let opts =
            [ (templ, stats, expr)
            | (templ, stats) <- retainedMacrosForAction act bank
            , Just expr <- [instantiateMacro st templ]
            ]
          ranked = sortOn (\(_, stats, _) -> Down (macroUtility stats)) opts
      in case ranked of
           [] -> Nothing
           (best:_) -> Just best

    retainedMacrosForAction :: Action -> MacroBank -> [(MacroTemplate, MacroStats)]
    retainedMacrosForAction act bank =
      [ (templ, stats)
      | (templ, stats) <- Map.toList bank
      , templateAction templ == act
      , isRetainedMacro stats
      ]

    isRetainedMacro :: MacroStats -> Bool
    isRetainedMacro stats =
      msSupport stats >= 2
      && msDebtGain stats + msCapGain stats > 0
      && msBranchSavings stats > 0

    macroUtility :: MacroStats -> Int
    macroUtility stats =
      2 * msDebtGain stats
      + msCapGain stats
      + msBranchSavings stats
      + msUses stats

    templateAction :: MacroTemplate -> Action
    templateAction templ = case templ of
      MacroUnary a _ -> a
      MacroBinary a _ _ -> a

    showMacroTemplate :: MacroTemplate -> String
    showMacroTemplate templ = case templ of
      MacroUnary a atom ->
        show a ++ "(" ++ show atom ++ ")"
      MacroBinary a x y ->
        show a ++ "(" ++ show x ++ "," ++ show y ++ ")"

    instantiateMacro :: SearchState -> MacroTemplate -> Maybe MBTTExpr
    instantiateMacro st templ = case templ of
      MacroUnary act atom -> do
        a <- instantiateAtom st atom
        case act of
          ALam -> Just (Lam a)
          ARefl -> Just (Refl a)
          ASusp -> Just (Susp a)
          ATrunc -> Just (Trunc a)
          AFlat -> Just (Flat a)
          ASharp -> Just (Sharp a)
          ADisc -> Just (Disc a)
          AShape -> Just (Shape a)
          ANext -> Just (Next a)
          AEventually -> Just (Eventually a)
          _ -> Nothing
      MacroBinary act leftAtom rightAtom -> do
        x <- instantiateAtom st leftAtom
        y <- instantiateAtom st rightAtom
        case act of
          APi -> Just (Pi x y)
          ASigma -> Just (Sigma x y)
          AApp -> Just (App x y)
          _ -> Nothing

    instantiateAtom :: SearchState -> MacroAtom -> Maybe MBTTExpr
    instantiateAtom st atom =
      let premises = take (agPremiseTopK cfg) (premisesForGoal st (stateCriticalGoal st))
          entries = ssEntries st
      in case atom of
           AtomTopPremise -> case premises of
             (p:_) -> Just (Lib p)
             [] -> if null entries then Just Univ else Just (Var 1)
           AtomSecondPremise -> case premises of
             (_:q:_) -> Just (Lib q)
             (p:_) -> Just (Lib p)
             [] -> if length entries >= 2 then Just (Var 2) else Just (Var 1)
           AtomVar1 ->
             if null entries
             then case premises of
                    (p:_) -> Just (Lib p)
                    [] -> Just Univ
             else Just (Var 1)
           AtomVar2 ->
             if length entries >= 2 then Just (Var 2)
             else if not (null entries) then Just (Var 1)
             else case premises of
                    (_:q:_) -> Just (Lib q)
                    (p:_) -> Just (Lib p)
                    [] -> Just Univ
           AtomUniverse -> Just Univ

    macroTemplateFromExpr :: Action -> MBTTExpr -> Maybe MacroTemplate
    macroTemplateFromExpr act expr = case (act, expr) of
      (APi, Pi a b) -> do
        (x, y) <- abstractBinaryAtoms a b
        Just (MacroBinary APi x y)
      (ASigma, Sigma a b) -> do
        (x, y) <- abstractBinaryAtoms a b
        Just (MacroBinary ASigma x y)
      (AApp, App f x) -> do
        (a, b) <- abstractBinaryAtoms f x
        Just (MacroBinary AApp a b)
      (ALam, Lam a) -> MacroUnary ALam <$> abstractUnaryAtom a
      (ARefl, Refl a) -> MacroUnary ARefl <$> abstractUnaryAtom a
      (ASusp, Susp a) -> MacroUnary ASusp <$> abstractUnaryAtom a
      (ATrunc, Trunc a) -> MacroUnary ATrunc <$> abstractUnaryAtom a
      (AFlat, Flat a) -> MacroUnary AFlat <$> abstractUnaryAtom a
      (ASharp, Sharp a) -> MacroUnary ASharp <$> abstractUnaryAtom a
      (ADisc, Disc a) -> MacroUnary ADisc <$> abstractUnaryAtom a
      (AShape, Shape a) -> MacroUnary AShape <$> abstractUnaryAtom a
      (ANext, Next a) -> MacroUnary ANext <$> abstractUnaryAtom a
      (AEventually, Eventually a) -> MacroUnary AEventually <$> abstractUnaryAtom a
      _ -> Nothing

    abstractUnaryAtom :: MBTTExpr -> Maybe MacroAtom
    abstractUnaryAtom expr = case expr of
      Var 1 -> Just AtomVar1
      Var 2 -> Just AtomVar2
      Univ -> Just AtomUniverse
      Lib _ -> Just AtomTopPremise
      _ -> Nothing

    abstractBinaryAtoms :: MBTTExpr -> MBTTExpr -> Maybe (MacroAtom, MacroAtom)
    abstractBinaryAtoms left right = do
      let libs = [i | Lib i <- [left, right]]
          hi = if null libs then 0 else maximum libs
          lo = if null libs then 0 else minimum libs
      a <- abstractAtomFromPair hi lo left
      b <- abstractAtomFromPair hi lo right
      Just (a, b)

    abstractAtomFromPair :: Int -> Int -> MBTTExpr -> Maybe MacroAtom
    abstractAtomFromPair hi lo expr = case expr of
      Var 1 -> Just AtomVar1
      Var 2 -> Just AtomVar2
      Univ -> Just AtomUniverse
      Lib i
        | hi == 0 -> Just AtomTopPremise
        | i == hi -> Just AtomTopPremise
        | i == lo -> Just AtomSecondPremise
        | otherwise -> Just AtomSecondPremise
      _ -> Nothing

    deterministicExpr :: SearchState -> Action -> MBTTExpr
    deterministicExpr st act =
      let entries = ssEntries st
          premises = take (agPremiseTopK cfg) (premisesForGoal st (stateCriticalGoal st))
          typeAtom = case premises of
            (p:_) -> Lib p
            [] -> if null entries then Univ else Var 1
          altType = case premises of
            (_:q:_) -> Lib q
            _ -> typeAtom
          termAtom = if null entries
                     then case premises of
                            (p:_) -> Lib p
                            [] -> Var 1
                     else Var 1
          appFun = case premises of
            (p:_) -> Lib p
            [] -> if null entries then Univ else Var 1
          appArg = case termAtom of
            Univ -> Var 1
            _ -> termAtom
      in case act of
          AUniv -> Univ
          AVar i -> Var i
          ALib i -> Lib i
          APathCon d -> PathCon d
          APi -> Pi typeAtom (if null entries then altType else Var 1)
          ASigma -> Sigma typeAtom (if null entries then altType else Var 1)
          ALam -> Lam termAtom
          AApp -> App appFun appArg
          AId -> Id typeAtom termAtom termAtom
          ARefl -> Refl termAtom
          ASusp -> Susp typeAtom
          ATrunc -> Trunc typeAtom
          AFlat -> Flat typeAtom
          ASharp -> Sharp typeAtom
          ADisc -> Disc typeAtom
          AShape -> Shape typeAtom
          ANext -> Next typeAtom
          AEventually -> Eventually typeAtom

    isTerminal :: SearchState -> Bool
    isTerminal st = length (ssEntries st) >= agMaxEntries cfg

    finalize :: AgendaNode -> AgendaCandidate
    finalize node =
      let st = anState node
          tele = Telescope (ssEntries st)
          score = nodePriority st + fromIntegral (sum [length (dcDischarged c) | c <- anCerts node])
      in AgendaCandidate
           { acTelescope = tele
           , acState = st
           , acCertificates = anCerts node
           , acPriority = score
           }

    stateSignature :: SearchState -> StateSig
    stateSignature st =
      hashFold
        [ 0x53
        , fromIntegral (ssKappa st)
        , fromIntegral (ssNuLower st)
        , fromIntegral (ssNuUpper st)
        , hashList (map (exprSignature . teType) (ssEntries st))
        , hashList (map obligationSignature (stateOpenGoals st))
        , hashList (map fromIntegral (take (agPremiseTopK cfg + 2) (ssPremises st)))
        ]

    exprSignature :: MBTTExpr -> ExprSig
    exprSignature expr = case expr of
      Univ -> hashFold [0x01]
      Var i -> hashFold [0x02, fromIntegral i]
      Lib i -> hashFold [0x03, fromIntegral i]
      App a b -> hashFold [0x04, exprSignature a, exprSignature b]
      Lam a -> hashFold [0x05, exprSignature a]
      Pi a b -> hashFold [0x06, exprSignature a, exprSignature b]
      Sigma a b -> hashFold [0x07, exprSignature a, exprSignature b]
      Id a x y -> hashFold [0x08, exprSignature a, exprSignature x, exprSignature y]
      Refl a -> hashFold [0x09, exprSignature a]
      Susp a -> hashFold [0x0A, exprSignature a]
      Trunc a -> hashFold [0x0B, exprSignature a]
      PathCon d -> hashFold [0x0C, fromIntegral d]
      Flat a -> hashFold [0x0D, exprSignature a]
      Sharp a -> hashFold [0x0E, exprSignature a]
      Disc a -> hashFold [0x0F, exprSignature a]
      Shape a -> hashFold [0x10, exprSignature a]
      Next a -> hashFold [0x11, exprSignature a]
      Eventually a -> hashFold [0x12, exprSignature a]

    obligationSignature :: Obligation -> Word64
    obligationSignature goal = case goal of
      NeedTypeFormer -> hashFold [0x21]
      NeedWitness -> hashFold [0x22]
      NeedLoop d -> hashFold [0x23, fromIntegral d]
      NeedLoopLift d -> hashFold [0x24, fromIntegral d]
      NeedLoopCoherence d -> hashFold [0x25, fromIntegral d]
      NeedTruncBridge -> hashFold [0x26]
      NeedSourceReuse d loopReq -> hashFold [0x27, fromIntegral d, if loopReq then 1 else 0]
      NeedTargetReuse d maxD -> hashFold [0x28, fromIntegral d, maybe 0 fromIntegral maxD]
      NeedMapHead -> hashFold [0x29]
      NeedFiberWitness -> hashFold [0x2A]
      NeedTransportLaw -> hashFold [0x2B]
      NeedCompatibilityPath d -> hashFold [0x2C, fromIntegral d]
      NeedSealBridge -> hashFold [0x2D]
      NeedModalBundle -> hashFold [0x2E]
      NeedDifferentialBridge -> hashFold [0x2F]
      NeedCurvatureBridge -> hashFold [0x30]
      NeedMetricBundle -> hashFold [0x31]
      NeedHilbertBundle -> hashFold [0x32]
      NeedTemporalBridge -> hashFold [0x33]
      NeedCoherence -> hashFold [0x34]

    hashList :: [Word64] -> Word64
    hashList = hashFold

    hashFold :: [Word64] -> Word64
    hashFold = foldl' hashStep hashSeed

    hashStep :: Word64 -> Word64 -> Word64
    hashStep h w =
      let x = (w + 0x9e3779b97f4a7c15) `xor` (w `shiftL` 6) `xor` (w `shiftL` 16)
      in (h `xor` x) * hashPrime

    hashSeed :: Word64
    hashSeed = 1469598103934665603

    hashPrime :: Word64
    hashPrime = 1099511628211

    normalizeObligationForKey :: Obligation -> Obligation
    normalizeObligationForKey goal = case goal of
      NeedLoop d -> NeedLoop (min 4 (max 1 d))
      NeedLoopLift d -> NeedLoopLift (min 4 (max 1 d))
      NeedLoopCoherence d -> NeedLoopCoherence (min 4 (max 1 d))
      NeedSourceReuse d loopReq -> NeedSourceReuse (min 4 (max 1 d)) loopReq
      NeedTargetReuse d maxD -> NeedTargetReuse (min 4 (max 1 d)) (fmap (\u -> min 4 (max 1 u)) maxD)
      NeedCompatibilityPath d -> NeedCompatibilityPath (min 4 (max 1 d))
      _ -> goal

    normalizeClauseForKey :: [Obligation] -> [Obligation]
    normalizeClauseForKey clause = sort (nub (map normalizeObligationForKey clause))

    normalizeClausesForKey :: [[Obligation]] -> [[Obligation]]
    normalizeClausesForKey clauses =
      sort [c | c <- map normalizeClauseForKey clauses, not (null c)]

    criticalDebtCount :: Obligation -> SearchState -> Int
    criticalDebtCount goal st =
      let dg = ssDebtGraph st
          target = normalizeObligationForKey goal
          hit g = normalizeObligationForKey g == target
          openHits = length [() | g <- dgOpenAtoms dg, hit g]
          andHits = sum [length [() | g <- clause, hit g] | clause <- dgAndClauses dg]
          orHits = sum [length [() | g <- clause, hit g] | clause <- dgOrClauses dg]
      in openHits + andHits + orHits

    passesSigmaUB :: SearchState -> Bool
    passesSigmaUB st
      | agBarFloor cfg <= 0.0 = True
      | otherwise = sigmaUpperBound st >= 0.0

    sigmaUpperBound :: SearchState -> Double
    sigmaUpperBound st =
      let dg = ssDebtGraph st
          openGoals = stateOpenGoals st
          reachUB = bestFutureNu openGoals (ssCaps st) dg
          axisUB = axisFutureNuBound dg
          horizonUB = horizonFutureNuBound st
          futureNu = minimum [reachUB, axisUB, horizonUB]
          coverLB = coverLowerBoundKappa dg
          chainLB = chainLowerBoundKappa dg
          windowLB = windowLowerBoundKappa openGoals
          extraKappa = max coverLB (max chainLB windowLB)
          nuUpper = fromIntegral (ssNuUpper st + max 0 futureNu) :: Double
          kappaLower = fromIntegral (max 1 (ssKappa st + max 1 extraKappa)) :: Double
      in nuUpper - agBarFloor cfg * kappaLower

    bestFutureNu :: [Obligation] -> CapabilitySig -> DebtGraph -> Int
    bestFutureNu goals caps dg = sum (map goalPotential goals) + capBonus + andBonus + orBonus
      where
        capBonus =
          (if capHasDepFns caps then 1 else 0)
          + (if capHasWitness caps then 1 else 0)
          + min 2 (capModalCount caps)
        andBonus = sum [max 0 (length c - 1) | c <- dgAndClauses dg]
        orBonus = length (dgOrClauses dg)

    axisFutureNuBound :: DebtGraph -> Int
    axisFutureNuBound dg =
      let open = dgOpenAtoms dg
          atomAxis = sum (map axisPotential open)
          andAxis = sum [max 0 (sum (map axisPotential c) - 2) | c <- dgAndClauses dg]
          orAxis = sum [maximum (map axisPotential c) | c <- dgOrClauses dg, not (null c)]
      in atomAxis + andAxis + orAxis

    axisPotential :: Obligation -> Int
    axisPotential goal = case goal of
      NeedTypeFormer -> 7
      NeedWitness -> 6
      NeedLoop d -> 8 + 2 * max 1 d
      NeedLoopLift d -> 9 + 2 * max 1 d
      NeedLoopCoherence d -> 7 + max 1 d
      NeedTruncBridge -> 8
      NeedSourceReuse d _ -> 8 + 2 * max 1 d
      NeedTargetReuse d _ -> 8 + 2 * max 1 d
      NeedMapHead -> 10
      NeedFiberWitness -> 8
      NeedTransportLaw -> 10
      NeedCompatibilityPath d -> 9 + max 1 d
      NeedSealBridge -> 10
      NeedModalBundle -> 8
      NeedDifferentialBridge -> 9
      NeedCurvatureBridge -> 10
      NeedMetricBundle -> 11
      NeedHilbertBundle -> 12
      NeedTemporalBridge -> 8
      NeedCoherence -> 6

    horizonFutureNuBound :: SearchState -> Int
    horizonFutureNuBound st =
      let remainingEntries = max 0 (agMaxEntries cfg - length (ssEntries st))
          base = 10 * remainingEntries
          debtBoost = 2 * stateGoalCount st
      in base + debtBoost

    goalPotential :: Obligation -> Int
    goalPotential goal = case goal of
      NeedTypeFormer -> 6
      NeedWitness -> 5
      NeedLoop d -> 6 + 2 * max 1 d
      NeedLoopLift d -> 7 + 2 * max 1 d
      NeedLoopCoherence d -> 6 + max 1 d
      NeedTruncBridge -> 7
      NeedSourceReuse d _ -> 7 + 2 * max 1 d
      NeedTargetReuse d _ -> 7 + 2 * max 1 d
      NeedMapHead -> 8
      NeedFiberWitness -> 7
      NeedTransportLaw -> 8
      NeedCompatibilityPath d -> 7 + max 1 d
      NeedSealBridge -> 9
      NeedModalBundle -> 7
      NeedDifferentialBridge -> 8
      NeedCurvatureBridge -> 9
      NeedMetricBundle -> 10
      NeedHilbertBundle -> 11
      NeedTemporalBridge -> 7
      NeedCoherence -> 5

    coverLowerBoundKappa :: DebtGraph -> Int
    coverLowerBoundKappa dg =
      let atoms = sort (dgOpenAtoms dg)
          atomMax = maximum0 (map goalKappa atoms)
          andReq = maximum0 [sum (map goalKappa clause) `divUp` 2 | clause <- dgAndClauses dg]
          orReq = maximum0 [minimum (map goalKappa clause) | clause <- dgOrClauses dg, not (null clause)]
          debtTail = max 0 (sum [max 0 (goalKappa g - 1) | g <- atoms] `divUp` 2)
      in max atomMax (max andReq (max orReq debtTail))

    chainLowerBoundKappa :: DebtGraph -> Int
    chainLowerBoundKappa dg =
      let andDepth = maximum0 [length clause | clause <- dgAndClauses dg]
          orDepth = maximum0 [length clause | clause <- dgOrClauses dg]
          hardAtomDepth = maximum0 (map goalChainWeight (dgOpenAtoms dg))
          clauseHardDepth = maximum0 [sum (map goalChainWeight clause) `divUp` 3 | clause <- dgAndClauses dg]
      in max 1 (maximum [max 0 (andDepth - 1), max 0 (orDepth - 1), hardAtomDepth, clauseHardDepth])

    windowLowerBoundKappa :: [Obligation] -> Int
    windowLowerBoundKappa goals =
      let hasCoherenceDebt = NeedCoherence `elem` goals || any isLoopCoherence goals
          hasLoopDebt = any isLoopish goals
          hasTruncDebt = NeedTruncBridge `elem` goals
          hasHighLoopDebt = any isHighLoop goals
          base = if hasCoherenceDebt then 1 else 0
          loopBridge = if hasLoopDebt && hasTruncDebt then 1 else 0
          highLoop = if hasHighLoopDebt then 1 else 0
      in base + loopBridge + highLoop
      where
        isLoopCoherence g = case g of
          NeedLoopCoherence _ -> True
          _ -> False
        isLoopish g = case g of
          NeedLoop _ -> True
          NeedLoopLift _ -> True
          NeedLoopCoherence _ -> True
          NeedCompatibilityPath _ -> True
          _ -> False
        isHighLoop g = case g of
          NeedLoop d -> d >= 2
          NeedLoopLift d -> d >= 2
          NeedLoopCoherence d -> d >= 2
          NeedCompatibilityPath d -> d >= 2
          _ -> False

    goalChainWeight :: Obligation -> Int
    goalChainWeight goal = case goal of
      NeedTypeFormer -> 1
      NeedWitness -> 1
      NeedLoop d -> 1 + max 0 (d - 1)
      NeedLoopLift d -> 2 + max 0 (d - 1)
      NeedLoopCoherence d -> 1 + max 0 (d - 1)
      NeedTruncBridge -> 2
      NeedSourceReuse d _ -> 2 + max 0 (d - 1)
      NeedTargetReuse d _ -> 2 + max 0 (d - 1)
      NeedMapHead -> 2
      NeedFiberWitness -> 1
      NeedTransportLaw -> 2
      NeedCompatibilityPath d -> 2 + max 0 (d - 1)
      NeedSealBridge -> 2
      NeedModalBundle -> 2
      NeedDifferentialBridge -> 2
      NeedCurvatureBridge -> 2
      NeedMetricBundle -> 3
      NeedHilbertBundle -> 3
      NeedTemporalBridge -> 2
      NeedCoherence -> 1

    goalKappa :: Obligation -> Int
    goalKappa goal = case goal of
      NeedTypeFormer -> 1
      NeedWitness -> 1
      NeedLoop d -> if d >= 2 then 2 else 1
      NeedLoopLift d -> if d >= 2 then 3 else 2
      NeedLoopCoherence d -> if d >= 2 then 2 else 1
      NeedTruncBridge -> 3
      NeedSourceReuse d _ -> if d >= 2 then 2 else 1
      NeedTargetReuse d _ -> if d >= 2 then 2 else 1
      NeedMapHead -> 2
      NeedFiberWitness -> 1
      NeedTransportLaw -> 2
      NeedCompatibilityPath d -> if d >= 2 then 2 else 1
      NeedSealBridge -> 2
      NeedModalBundle -> 2
      NeedDifferentialBridge -> 2
      NeedCurvatureBridge -> 2
      NeedMetricBundle -> 4
      NeedHilbertBundle -> 4
      NeedTemporalBridge -> 2
      NeedCoherence -> 1

    divUp :: Int -> Int -> Int
    divUp x y
      | y <= 0 = x
      | otherwise = (x + y - 1) `div` y

    maximum0 :: [Int] -> Int
    maximum0 [] = 0
    maximum0 xs = maximum xs

    dominanceKey :: SearchState -> DominanceKey
    dominanceKey st =
      let dg = ssDebtGraph st
      in
      DominanceKey
        { dkCaps = ssCaps st
        , dkGoals = normalizeClauseForKey (stateOpenGoals st)
        , dkAndShape = normalizeClausesForKey (dgAndClauses dg)
        , dkOrShape = normalizeClausesForKey (dgOrClauses dg)
        , dkPremiseBucket = map (`div` 2) (take (agPremiseTopK cfg) (ssPremises st))
        , dkKappaBucket =
            if bridgePhase
            then ssKappa st
            else case stateCriticalGoal st of
              Just NeedTruncBridge -> ssKappa st
              _ -> ssKappa st `div` 2
        }

    frontierPoint :: SearchState -> FrontierPoint
    frontierPoint st =
      FrontierPoint
        { fpNuLower = ssNuLower st
        , fpNuUpper = ssNuUpper st
        , fpKappa = ssKappa st
        , fpGoalCount = stateGoalCount st
        }

    dominatesPoint :: FrontierPoint -> FrontierPoint -> Bool
    dominatesPoint a b =
      fpNuLower a >= fpNuLower b
      && fpNuUpper a >= fpNuUpper b
      && fpKappa a <= fpKappa b
      && fpGoalCount a <= fpGoalCount b
      && (fpNuLower a > fpNuLower b
          || fpNuUpper a > fpNuUpper b
          || fpKappa a < fpKappa b
          || fpGoalCount a < fpGoalCount b)

    insertFrontier :: DominanceKey -> FrontierPoint -> DominanceCache -> (Bool, DominanceCache)
    insertFrontier key point cache =
      let frontier = Map.findWithDefault [] key cache
      in if any (`dominatesPoint` point) frontier
         then (True, cache)
         else
           let trimmed = point : filter (not . (point `dominatesPoint`)) frontier
           in (False, Map.insert key trimmed cache)

    nodePriority :: SearchState -> Double
    nodePriority st =
      let gain = fromIntegral (ssNuUpper st + 2 * ssNuLower st) :: Double
          debt = fromIntegral (max 1 (ssKappa st + 1 + stateGoalCount st)) :: Double
      in gain / debt

    bridgePhase :: Bool
    bridgePhase =
      NeedHIT `elem` gpIntents profile
      && any leHasLoop lib
      && not (any hasTruncEntry lib)

    hasTruncEntry :: LibraryEntry -> Bool
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

    capabilityDelta :: CapabilitySig -> CapabilitySig -> [String]
    capabilityDelta before after = concat
      [ ["Universe" | not (capHasUniverse before) && capHasUniverse after]
      , ["Witness" | not (capHasWitness before) && capHasWitness after]
      , ["DependentFunctions" | not (capHasDepFns before) && capHasDepFns after]
      , ["PathDim>=" ++ show (capMaxPathDim after) | capMaxPathDim after > capMaxPathDim before]
      , ["ModalBundle" | capModalCount after > capModalCount before]
      , ["Differential" | not (capHasDifferential before) && capHasDifferential after]
      , ["Curvature" | not (capHasCurvature before) && capHasCurvature after]
      , ["Hilbert" | not (capHasHilbert before) && capHasHilbert after]
      , ["Temporal" | not (capHasTemporal before) && capHasTemporal after]
      ]

    actionFormRank :: Action -> Int
    actionFormRank act = case act of
      APi -> 0
      ASigma -> 1
      ALib _ -> 2
      AApp -> 3
      AVar _ -> 4
      APathCon _ -> 5
      ASusp -> 6
      ATrunc -> 7
      AFlat -> 8
      ASharp -> 9
      ADisc -> 10
      AShape -> 11
      ANext -> 12
      AEventually -> 13
      AId -> 14
      ALam -> 15
      ARefl -> 16
      AUniv -> 17

exprLibRefs :: MBTTExpr -> [Int]
exprLibRefs expr = nub (go expr)
  where
    go e = case e of
      Lib i -> [i]
      Lam a -> go a
      Refl a -> go a
      Susp a -> go a
      Trunc a -> go a
      Flat a -> go a
      Sharp a -> go a
      Disc a -> go a
      Shape a -> go a
      Next a -> go a
      Eventually a -> go a
      Pi a b -> go a ++ go b
      Sigma a b -> go a ++ go b
      App a b -> go a ++ go b
      Id a x y -> go a ++ go x ++ go y
      _ -> []

exprNodeCount :: MBTTExpr -> Int
exprNodeCount expr = case expr of
  Univ -> 1
  Var _ -> 1
  Lib _ -> 1
  PathCon _ -> 1
  Lam a -> 1 + exprNodeCount a
  Refl a -> 1 + exprNodeCount a
  Susp a -> 1 + exprNodeCount a
  Trunc a -> 1 + exprNodeCount a
  Flat a -> 1 + exprNodeCount a
  Sharp a -> 1 + exprNodeCount a
  Disc a -> 1 + exprNodeCount a
  Shape a -> 1 + exprNodeCount a
  Next a -> 1 + exprNodeCount a
  Eventually a -> 1 + exprNodeCount a
  Pi a b -> 1 + exprNodeCount a + exprNodeCount b
  Sigma a b -> 1 + exprNodeCount a + exprNodeCount b
  App a b -> 1 + exprNodeCount a + exprNodeCount b
  Id a x y -> 1 + exprNodeCount a + exprNodeCount x + exprNodeCount y
