{-# LANGUAGE BangPatterns #-}

-- | Agenda-based obligation-driven search for telescope candidates.
-- Keeps search on explicit proof/search states rather than full-space enumeration.
module AgendaSearch
  ( AgendaConfig(..)
  , defaultAgendaConfig
  , DerivationRule(..)
  , DerivationCertificate(..)
  , AgendaCandidate(..)
  , agendaGenerateCandidates
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (Telescope(..), TeleEntry(..), teleIsConnected, teleReferencesWindow)
import TelescopeCheck (checkTelescope, CheckResult(..))
import TelescopeGen (Action(..), GoalProfile, Hole(..), HoleGoal(..), validActionsWithProfile, actionPriority)
import ProofState (SearchState(..), CapabilitySig(..), Obligation(..), deriveSearchState, actionGoalGain, isSafeAction)
import Types (Library)

import Data.List (sortOn, nub)
import Data.Ord (Down(..))
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

data AgendaConfig = AgendaConfig
  { agMaxEntries      :: !Int
  , agMaxAgendaStates :: !Int
  , agBranchPerState  :: !Int
  , agMaxCandidates   :: !Int
  , agBitBudget       :: !Int
  , agRequireConnected :: !Bool
  } deriving (Show, Eq)

defaultAgendaConfig :: AgendaConfig
defaultAgendaConfig = AgendaConfig
  { agMaxEntries = 3
  , agMaxAgendaStates = 200
  , agBranchPerState = 10
  , agMaxCandidates = 40
  , agBitBudget = 20
  , agRequireConnected = False
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
  | Normalize
  deriving (Show, Eq, Ord)

data DerivationCertificate = DerivationCertificate
  { dcStep :: !Int
  , dcRule :: !DerivationRule
  , dcAction :: !Action
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

data AgendaNode = AgendaNode
  { anState :: !SearchState
  , anCerts :: ![DerivationCertificate]
  } deriving (Show, Eq)

agendaGenerateCandidates :: Library -> GoalProfile -> AgendaConfig -> [AgendaCandidate]
agendaGenerateCandidates lib profile cfg =
  let start = AgendaNode (deriveSearchState lib profile []) []
      (_, finals) = searchLoop 0 [start] Set.empty []
      cands = map finalize finals
      valid = filter (\c -> case checkTelescope lib (acTelescope c) of
                       CheckOK -> True
                       _ -> False) cands
  in take (agMaxCandidates cfg) (sortOn (Down . acPriority) valid)
  where
    searchLoop :: Int -> [AgendaNode] -> Set.Set String -> [AgendaNode] -> (Set.Set String, [AgendaNode])
    searchLoop !iter !agenda !seen !finals
      | iter >= agMaxAgendaStates cfg = (seen, finals ++ agenda)
      | null agenda = (seen, finals)
      | otherwise =
          let ordered = sortOn (Down . nodePriority . anState) agenda
          in case ordered of
               [] -> (seen, finals)
               (node:rest) ->
                 let key = stateKey (anState node)
                 in if Set.member key seen
                    then searchLoop (iter + 1) rest seen finals
                    else
                      let seen' = Set.insert key seen
                          st = anState node
                      in if isTerminal st
                         then searchLoop (iter + 1) rest seen' (node : finals)
                         else
                           let children = expandNode node
                               trimmed = take (agMaxAgendaStates cfg) (rest ++ children)
                           in searchLoop (iter + 1) trimmed seen' finals

    expandNode :: AgendaNode -> [AgendaNode]
    expandNode node =
      let st = anState node
          entries = ssEntries st
          hole = Hole entries AnyHole 0 (agBitBudget cfg)
          allActions = validActionsWithProfile profile hole lib
          filtered = filter (premiseAllowed st) allActions
          ranked = sortOn (actionRank st) filtered
          chosen = take (agBranchPerState cfg) ranked
      in mapMaybe (applyTransition node) chosen

    premiseAllowed :: SearchState -> Action -> Bool
    premiseAllowed st act = case act of
      ALib i -> null (ssPremises st) || i `elem` ssPremises st
      _ -> True

    actionRank :: SearchState -> Action -> (Down Int, Down Int, Down Int, Int)
    actionRank st act =
      let safe = if isSafeAction act then 1 else 0
          gain = actionGoalGain st act
          pri = actionPriority (length lib) act
          form = actionFormRank act
      in (Down safe, Down gain, Down pri, form)

    applyTransition :: AgendaNode -> Action -> Maybe AgendaNode
    applyTransition node act =
      let st = anState node
          entries = ssEntries st
          expr = deterministicExpr st act
          name = "c" ++ show (length entries + 1)
          entry = TeleEntry name expr
          newEntries = entries ++ [entry]
          tele = Telescope newEntries
      in case checkTelescope lib tele of
           CheckFail _ -> Nothing
           CheckOK ->
             let connectedOk = not (agRequireConnected cfg)
                            || length newEntries <= 1
                            || teleIsConnected tele
                            || teleReferencesWindow tele (length lib)
             in if not connectedOk
                then Nothing
                else
                  let st' = deriveSearchState lib profile newEntries
                      cert = mkCertificate st st' act expr (length (anCerts node) + 1)
                  in Just (AgendaNode st' (anCerts node ++ [cert]))

    mkCertificate :: SearchState -> SearchState -> Action -> MBTTExpr -> Int -> DerivationCertificate
    mkCertificate before after act expr stepNo =
      let usedPremises = [i | i <- exprLibRefs expr, i `elem` ssPremises before]
          discharged = [g | g <- ssGoals before, g `notElem` ssGoals after]
          gained = capabilityDelta (ssCaps before) (ssCaps after)
      in DerivationCertificate
          { dcStep = stepNo
          , dcRule = deriveRule before act expr
          , dcAction = act
          , dcPremisesUsed = usedPremises
          , dcDischarged = discharged
          , dcCapabilitiesGained = gained
          , dcGoalsAfter = ssGoals after
          }

    deriveRule :: SearchState -> Action -> MBTTExpr -> DerivationRule
    deriveRule st act expr
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

    deterministicExpr :: SearchState -> Action -> MBTTExpr
    deterministicExpr st act =
      let entries = ssEntries st
          premises = ssPremises st
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

    stateKey :: SearchState -> String
    stateKey st = show (map teType (ssEntries st), ssGoals st)

    nodePriority :: SearchState -> Double
    nodePriority st =
      let gain = fromIntegral (ssNuUpper st + 2 * ssNuLower st) :: Double
          debt = fromIntegral (max 1 (ssKappa st + 1 + length (ssGoals st))) :: Double
      in gain / debt

    capabilityDelta :: CapabilitySig -> CapabilitySig -> [String]
    capabilityDelta before after = concat
      [ ["Universe" | not (capHasUniverse before) && capHasUniverse after]
      , ["Witness" | not (capHasWitness before) && capHasWitness after]
      , ["DependentFunctions" | not (capHasDepFns before) && capHasDepFns after]
      , ["PathDim>=" ++ show (capMaxPathDim after) | capMaxPathDim after > capMaxPathDim before]
      , ["ModalBundle" | capModalCount after > capModalCount before]
      , ["Differential" | not (capHasDifferential before) && capHasDifferential after]
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

