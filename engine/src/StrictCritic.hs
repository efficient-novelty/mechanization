{-# LANGUAGE BangPatterns #-}

module StrictCritic
  ( MissingSignature(..)
  , ObligationSummary(..)
  , FrontierDiagnostics(..)
  , analyzeObligations
  , interfaceDensity
  , genericBinderCount
  , dependentMotiveDensity
  , internalAdjointScore
  , trueEliminatorScore
  , formerLifecyclePhaseCount
  , formerLifecycleScore
  , closureScore
  , missingGoalProfile
  , frontierDiagnostics
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (CoreJudgment(..), TeleEntry(..), Telescope(..), desugarTelescope, teleLibRefs)
import TelescopeGen (GoalProfile(..))
import qualified TelescopeGen as TG
import Types (Library, LibraryEntry(..))

import Data.List (nub, sort)
import qualified Data.Set as Set

data MissingSignature
  = NeedIntro !Int
  | NeedElim !Int
  | NeedTransport !Int
  | NeedCoherence !Int
  | NeedBridge ![Int]
  deriving (Show, Eq, Ord)

data ObligationSummary = ObligationSummary
  { osScore :: !Int
  , osAdjointDebt :: !Int
  , osFloatingClauses :: !Int
  , osInterfaceDensity :: !Int
  , osGenericBinderCount :: !Int
  , osDependentMotiveDensity :: !Int
  , osInternalAdjointScore :: !Int
  , osTrueEliminatorScore :: !Int
  , osFormerLifecycleScore :: !Int
  , osClosureScore :: !Int
  , osMissingSignatures :: ![MissingSignature]
  } deriving (Show, Eq)

data FrontierDiagnostics = FrontierDiagnostics
  { fdFrontierCandidates :: !Int
  , fdFrontierKept :: !Int
  , fdBestObligationScore :: !(Maybe Int)
  , fdBestInterfaceDensity :: !(Maybe Int)
  , fdBestGenericityScore :: !(Maybe Int)
  , fdBestClosureScore :: !(Maybe Int)
  } deriving (Show, Eq)

data ExprStats = ExprStats
  { esFormerSurface :: !Int
  , esIntroSurface :: !Int
  , esElimSurface :: !Int
  , esTransportSurface :: !Int
  , esCoherenceSurface :: !Int
  , esBinderSurface :: !Int
  , esLibRefs :: !(Set.Set Int)
  , esVarRefs :: !(Set.Set Int)
  } deriving (Show, Eq)

emptyExprStats :: ExprStats
emptyExprStats =
  ExprStats
    { esFormerSurface = 0
    , esIntroSurface = 0
    , esElimSurface = 0
    , esTransportSurface = 0
    , esCoherenceSurface = 0
    , esBinderSurface = 0
    , esLibRefs = Set.empty
    , esVarRefs = Set.empty
    }

combineExprStats :: ExprStats -> ExprStats -> ExprStats
combineExprStats a b =
  ExprStats
    { esFormerSurface = esFormerSurface a + esFormerSurface b
    , esIntroSurface = esIntroSurface a + esIntroSurface b
    , esElimSurface = esElimSurface a + esElimSurface b
    , esTransportSurface = esTransportSurface a + esTransportSurface b
    , esCoherenceSurface = esCoherenceSurface a + esCoherenceSurface b
    , esBinderSurface = esBinderSurface a + esBinderSurface b
    , esLibRefs = Set.union (esLibRefs a) (esLibRefs b)
    , esVarRefs = Set.union (esVarRefs a) (esVarRefs b)
    }

exprStats :: MBTTExpr -> ExprStats
exprStats expr = case expr of
  Univ ->
    emptyExprStats { esFormerSurface = 1 }
  Var i ->
    emptyExprStats
      { esIntroSurface = 1
      , esVarRefs = Set.singleton i
      }
  Lib i ->
    emptyExprStats
      { esFormerSurface = 1
      , esLibRefs = Set.singleton i
      }
  Lam body ->
    let bodyStats = exprStats body
    in bodyStats
         { esIntroSurface = esIntroSurface bodyStats + 1
         , esBinderSurface = esBinderSurface bodyStats + binderBonus bodyStats
         }
  App f x ->
    let fStats = exprStats f
        xStats = exprStats x
        combined = combineExprStats fStats xStats
        formationBoost = case f of
          Univ -> 2
          App Univ _ -> 1
          _ -> 0
        introBoost = case (f, x) of
          (Lib _, _) -> 1
          (App _ _, _) -> 1
          _ -> 0
        elimBoost = 1 + if isTransportExpr f || isTransportExpr x then 1 else 0
    in combined
         { esFormerSurface = esFormerSurface combined + formationBoost
         , esIntroSurface = esIntroSurface combined + introBoost
         , esElimSurface = esElimSurface combined + elimBoost
         , esTransportSurface = esTransportSurface combined + if isTransportExpr f then 1 else 0
         }
  Pi a b ->
    binderExpr a b
  Sigma a b ->
    binderExpr a b
  Id a x y ->
    let combined = combineExprStats (exprStats a) (combineExprStats (exprStats x) (exprStats y))
    in combined
         { esFormerSurface = esFormerSurface combined + 1
         , esTransportSurface = esTransportSurface combined + 1
         , esCoherenceSurface = esCoherenceSurface combined + 1
         }
  Refl a ->
    let inner = exprStats a
    in inner
         { esIntroSurface = esIntroSurface inner + 1
         , esTransportSurface = esTransportSurface inner + 1
         , esCoherenceSurface = esCoherenceSurface inner + 1
         }
  Susp a ->
    let inner = exprStats a
    in inner
         { esFormerSurface = esFormerSurface inner + 1
         , esCoherenceSurface = esCoherenceSurface inner + 1
         }
  Trunc a ->
    let inner = exprStats a
    in inner
         { esFormerSurface = esFormerSurface inner + 1
         , esCoherenceSurface = esCoherenceSurface inner + 1
         }
  PathCon _ ->
    emptyExprStats
      { esTransportSurface = 1
      , esCoherenceSurface = 1
      }
  Flat a ->
    unaryTransport a
  Sharp a ->
    unaryTransport a
  Disc a ->
    unaryTransport a
  Shape a ->
    unaryTransport a
  Next a ->
    unaryTransport a
  Eventually a ->
    unaryTransport a
  where
    binderExpr a b =
      let aStats = exprStats a
          bStats = exprStats b
          combined = combineExprStats aStats bStats
      in combined
           { esFormerSurface = esFormerSurface combined + 1
           , esBinderSurface = esBinderSurface combined + 1 + binderBonus bStats
           }
    unaryTransport a =
      let inner = exprStats a
      in inner
           { esFormerSurface = esFormerSurface inner + 1
           , esTransportSurface = esTransportSurface inner + 1
           }
    binderBonus stats
      | Set.null (esVarRefs stats) = 0
      | otherwise = 1

isTransportExpr :: MBTTExpr -> Bool
isTransportExpr expr = case expr of
  Id _ _ _ -> True
  Refl _ -> True
  PathCon _ -> True
  Flat _ -> True
  Sharp _ -> True
  Disc _ -> True
  Shape _ -> True
  Next _ -> True
  Eventually _ -> True
  _ -> False

entryHasBinder :: MBTTExpr -> Bool
entryHasBinder expr = case expr of
  Lam a -> True || entryHasBinder a
  Pi a b -> True || entryHasBinder a || entryHasBinder b
  Sigma a b -> True || entryHasBinder a || entryHasBinder b
  App a b -> entryHasBinder a || entryHasBinder b
  Id a x y -> entryHasBinder a || entryHasBinder x || entryHasBinder y
  Refl a -> entryHasBinder a
  Susp a -> entryHasBinder a
  Trunc a -> entryHasBinder a
  Flat a -> entryHasBinder a
  Sharp a -> entryHasBinder a
  Disc a -> entryHasBinder a
  Shape a -> entryHasBinder a
  Next a -> entryHasBinder a
  Eventually a -> entryHasBinder a
  _ -> False

entryHasElim :: MBTTExpr -> Bool
entryHasElim expr = case expr of
  App _ _ -> True
  Lam a -> entryHasElim a
  Pi a b -> entryHasElim a || entryHasElim b
  Sigma a b -> entryHasElim a || entryHasElim b
  Id a x y -> entryHasElim a || entryHasElim x || entryHasElim y
  Refl a -> entryHasElim a
  Susp a -> entryHasElim a
  Trunc a -> entryHasElim a
  Flat a -> entryHasElim a
  Sharp a -> entryHasElim a
  Disc a -> entryHasElim a
  Shape a -> entryHasElim a
  Next a -> entryHasElim a
  Eventually a -> entryHasElim a
  _ -> False

entryHasCoherence :: MBTTExpr -> Bool
entryHasCoherence expr = case expr of
  PathCon _ -> True
  Id _ _ _ -> True
  Refl _ -> True
  Lam a -> entryHasCoherence a
  App a b -> entryHasCoherence a || entryHasCoherence b
  Pi a b -> entryHasCoherence a || entryHasCoherence b
  Sigma a b -> entryHasCoherence a || entryHasCoherence b
  Susp a -> entryHasCoherence a
  Trunc a -> entryHasCoherence a
  Flat a -> entryHasCoherence a
  Sharp a -> entryHasCoherence a
  Disc a -> entryHasCoherence a
  Shape a -> entryHasCoherence a
  Next a -> entryHasCoherence a
  Eventually a -> entryHasCoherence a
  _ -> False

entryUsesLocalSupport :: MBTTExpr -> Bool
entryUsesLocalSupport expr =
  let stats = exprStats expr
  in not (Set.null (esVarRefs stats)) || not (Set.null (esLibRefs stats))

interfaceDensity :: Telescope -> Int
interfaceDensity tele =
  let refs = teleLibRefs tele
      entryTouchCount =
        length
          [ ()
          | TeleEntry _ expr <- teleEntries tele
          , let stats = exprStats expr
          , not (Set.null (esLibRefs stats)) || not (Set.null (esVarRefs stats))
          ]
  in 2 * Set.size refs + entryTouchCount

genericBinderCount :: Telescope -> Int
genericBinderCount (Telescope entries) =
  sum
    [ esBinderSurface stats + min 2 (Set.size (esVarRefs stats))
    | TeleEntry _ expr <- entries
    , let stats = exprStats expr
    ]

dependentMotiveDensity :: Telescope -> Int
dependentMotiveDensity (Telescope entries) =
  sum [dependentBinderDensityExpr (teType entry) | entry <- entries]

internalAdjointScore :: Telescope -> Int
internalAdjointScore tele@(Telescope entries) =
  baseAdjoints + trueEliminatorScore tele
  where
    baseAdjoints =
      sum
        [ 20
          + if hasIntro then 30 else 0
          + if hasElim || hasPath then 40 else 0
        | (ix, TeleEntry _ expr) <- zip [0..] entries
        , opensAdjointDimension expr
        , let (hasIntro, hasElim, hasPath) =
                foldr
                  (\(laterIx, TeleEntry _ laterExpr) (!introAcc, !elimAcc, !pathAcc) ->
                     if laterIx <= ix
                     then (introAcc, elimAcc, pathAcc)
                     else
                       let target = laterIx - ix
                       in ( introAcc || isIntroReference target laterExpr
                          , elimAcc || isElimReference target laterExpr
                          , pathAcc || isPathReference target laterExpr
                          ))
                  (False, False, False)
                  (zip [0..] entries)
        ]

trueEliminatorScore :: Telescope -> Int
trueEliminatorScore (Telescope entries) =
  50 * length [() | TeleEntry _ expr <- entries, hasTrueEliminatorShape expr]

formerLifecyclePhaseCount :: Telescope -> Int
formerLifecyclePhaseCount (Telescope entries) =
  length (filter id [hasFormationPhase, hasIntroPhase, hasElimPhase])
  where
    exprs = map teType entries
    hasFormationPhase = any hasFormerFormationExpr exprs
    hasIntroPhase = any hasFormerIntroExpr exprs
    hasElimPhase = any hasFormerElimExpr exprs

formerLifecycleScore :: Telescope -> Int
formerLifecycleScore tele =
  let phaseCount = formerLifecyclePhaseCount tele
  in 18 * phaseCount + if phaseCount == 3 then 34 else 0

closureScore :: Library -> Telescope -> Int
closureScore lib tele =
  let entries = teleEntries tele
      stats = foldr (combineExprStats . exprStats . teType) emptyExprStats entries
      judgments = desugarTelescope tele
      formationJudgments = length [() | JFormation <- judgments]
      introJudgments = length [() | JIntroduction <- judgments]
      elimJudgments = length [() | JElimination <- judgments] + length [() | JComputation <- judgments]
      pathJudgments = length [() | JPathAttach <- judgments]
      formerSurface = esFormerSurface stats + formationJudgments
      introSurface = esIntroSurface stats + introJudgments
      elimSurface = esElimSurface stats + elimJudgments
      coherenceSurface = esCoherenceSurface stats + pathJudgments
      transportSurface = esTransportSurface stats + pathJudgments
      recentHits = length (activeWindowRefs lib tele)
      binderEntries = length [() | TeleEntry _ expr <- entries, entryHasBinder expr]
      elimEntries = length [() | TeleEntry _ expr <- entries, entryHasElim expr]
      coherenceEntries = length [() | TeleEntry _ expr <- entries, entryHasCoherence expr]
      participatingEntries = length [() | TeleEntry _ expr <- entries, entryUsesLocalSupport expr]
      clauseBundleBonus =
        3 * min binderEntries elimEntries
        + 2 * min coherenceEntries participatingEntries
        + if binderEntries > 0 && elimEntries > 0 && participatingEntries >= 3
          then 6
          else 0
      formerDebtBonus =
        if not (any leHasDependentFunctions lib)
           && binderEntries > 0
           && elimEntries > 0
           && participatingEntries >= 3
        then 14
        else 0
      bridgeBonus =
        if Set.size (teleLibRefs tele) >= 2
        then 2
        else 0
  in min formerSurface introSurface
     + min formerSurface elimSurface
     + min coherenceSurface transportSurface
     + 2 * recentHits
     + clauseBundleBonus
     + formerDebtBonus
     + bridgeBonus

analyzeObligations :: Library -> Telescope -> ObligationSummary
analyzeObligations lib tele =
  let stats = foldr (combineExprStats . exprStats . teType) emptyExprStats (teleEntries tele)
      judgments = desugarTelescope tele
      formationJudgments = length [() | JFormation <- judgments]
      introJudgments = length [() | JIntroduction <- judgments]
      elimJudgments =
        length [() | JElimination <- judgments] + length [() | JComputation <- judgments]
      pathJudgments = length [() | JPathAttach <- judgments]
      formerSurface = esFormerSurface stats + formationJudgments
      introSurface = esIntroSurface stats + introJudgments
      elimSurface = esElimSurface stats + elimJudgments
      transportSurface = esTransportSurface stats + pathJudgments
      coherenceSurface = esCoherenceSurface stats + pathJudgments
      density = interfaceDensity tele
      genericity = genericBinderCount tele
      dependentDensity = dependentMotiveDensity tele
      internalAdjoints = internalAdjointScore tele
      trueElims = trueEliminatorScore tele
      formerLifecycle =
        if any leHasDependentFunctions lib
        then 0
        else formerLifecycleScore tele
      universeShells =
        if any leHasDependentFunctions lib
        then 0
        else universeShellPenalty tele
      closure = closureScore lib tele
      introDebt = max 0 (formerSurface - introSurface)
      elimDebt = max 0 (formerSurface - elimSurface)
      transportDebt = max 0 (coherenceSurface - transportSurface)
      coherenceDebt = max 0 (transportSurface - coherenceSurface)
      bridgeDebtRefs = missingBridgeRefs lib tele
      floating = floatingClauses tele
      dependentBonus =
        if dependentDensity <= 0
        then 0
        else 20 + 10 * dependentDensity
      missing = concat
        [ [NeedIntro introDebt | introDebt > 0]
        , [NeedElim elimDebt | elimDebt > 0]
        , [NeedTransport transportDebt | transportDebt > 0]
        , [NeedCoherence coherenceDebt | coherenceDebt > 0]
        , [NeedBridge bridgeDebtRefs | not (null bridgeDebtRefs)]
        ]
      adjointDebt = introDebt + elimDebt + transportDebt + coherenceDebt + length bridgeDebtRefs
      score =
           6 * closure
         + internalAdjoints
         + formerLifecycle
         + dependentBonus
         + 3 * density
         + 2 * genericity
         - 5 * adjointDebt
         - 4 * floating
         - universeShells
  in ObligationSummary
       { osScore = score
       , osAdjointDebt = adjointDebt
       , osFloatingClauses = floating
       , osInterfaceDensity = density
       , osGenericBinderCount = genericity
       , osDependentMotiveDensity = dependentDensity
       , osInternalAdjointScore = internalAdjoints
       , osTrueEliminatorScore = trueElims
       , osFormerLifecycleScore = formerLifecycle
       , osClosureScore = closure
       , osMissingSignatures = sort missing
       }

missingGoalProfile :: ObligationSummary -> GoalProfile
missingGoalProfile summary =
  let intents = nub (concatMap signatureIntent (osMissingSignatures summary))
  in GoalProfile
       { gpIntents = intents
       , gpPreferReuse = osInterfaceDensity summary > 0
       }
  where
    signatureIntent sig = case sig of
      NeedIntro _ -> [TG.NeedFormer]
      NeedElim _ -> [TG.NeedFormer]
      NeedTransport _ -> [TG.NeedHIT, TG.NeedBridge]
      NeedCoherence _ -> [TG.NeedHIT]
      NeedBridge _ -> [TG.NeedBridge]

frontierDiagnostics :: [ObligationSummary] -> FrontierDiagnostics
frontierDiagnostics summaries =
  FrontierDiagnostics
    { fdFrontierCandidates = length summaries
    , fdFrontierKept = length summaries
    , fdBestObligationScore = bestOf osScore
    , fdBestInterfaceDensity = bestOf osInterfaceDensity
    , fdBestGenericityScore = bestOf osGenericBinderCount
    , fdBestClosureScore = bestOf osClosureScore
    }
  where
    bestOf field =
      case summaries of
        [] -> Nothing
        _ -> Just (maximum (map field summaries))

activeWindowRefs :: Library -> Telescope -> [Int]
activeWindowRefs lib tele =
  let refs = teleLibRefs tele
      candidates = filter (>= 1) [length lib, length lib - 1]
  in [ref | ref <- candidates, Set.member ref refs]

missingBridgeRefs :: Library -> Telescope -> [Int]
missingBridgeRefs lib tele =
  let activeRefs = filter (>= 1) [length lib, length lib - 1]
      usedRefs = activeWindowRefs lib tele
  in if null usedRefs
     then []
     else [ref | ref <- activeRefs, ref `notElem` usedRefs]

floatingClauses :: Telescope -> Int
floatingClauses (Telescope entries) =
  length
    [ ()
    | TeleEntry _ expr <- entries
    , let stats = exprStats expr
    , Set.null (esLibRefs stats)
    , Set.null (esVarRefs stats)
    , esFormerSurface stats == 0
    , esIntroSurface stats == 0
    , esCoherenceSurface stats == 0
    ]

dependentBinderDensityExpr :: MBTTExpr -> Int
dependentBinderDensityExpr expr = case expr of
  Lam body ->
    binderBonus body + dependentBinderDensityExpr body
  Pi a b ->
    dependentBinderDensityExpr a + binderBonus b + dependentBinderDensityExpr b
  Sigma a b ->
    dependentBinderDensityExpr a + binderBonus b + dependentBinderDensityExpr b
  App a b ->
    dependentBinderDensityExpr a + dependentBinderDensityExpr b
  Id a x y ->
    dependentBinderDensityExpr a
      + dependentBinderDensityExpr x
      + dependentBinderDensityExpr y
  Refl a ->
    dependentBinderDensityExpr a
  Susp a ->
    dependentBinderDensityExpr a
  Trunc a ->
    dependentBinderDensityExpr a
  Flat a ->
    dependentBinderDensityExpr a
  Sharp a ->
    dependentBinderDensityExpr a
  Disc a ->
    dependentBinderDensityExpr a
  Shape a ->
    dependentBinderDensityExpr a
  Next a ->
    dependentBinderDensityExpr a
  Eventually a ->
    dependentBinderDensityExpr a
  _ ->
    0
  where
    binderBonus body
      | mentionsBoundVar 1 body = 1
      | otherwise = 0

mentionsBoundVar :: Int -> MBTTExpr -> Bool
mentionsBoundVar target = go 0
  where
    go !shift expr = case expr of
      Var i -> i == target + shift
      App f x -> go shift f || go shift x
      Lam body -> go (shift + 1) body
      Pi a b -> go shift a || go (shift + 1) b
      Sigma a b -> go shift a || go (shift + 1) b
      Id a x y -> go shift a || go shift x || go shift y
      Refl a -> go shift a
      Susp a -> go shift a
      Trunc a -> go shift a
      Flat a -> go shift a
      Sharp a -> go shift a
      Disc a -> go shift a
      Shape a -> go shift a
      Next a -> go shift a
      Eventually a -> go shift a
      _ -> False

opensAdjointDimension :: MBTTExpr -> Bool
opensAdjointDimension expr = case expr of
  App Univ _ -> True
  Pi _ _ -> True
  Sigma _ _ -> True
  Susp _ -> True
  Trunc _ -> True
  Flat _ -> True
  Sharp _ -> True
  Disc _ -> True
  Shape _ -> True
  Next _ -> True
  Eventually _ -> True
  _ -> False

isIntroReference :: Int -> MBTTExpr -> Bool
isIntroReference target expr = case expr of
  Var i -> i == target
  App (Lib _) arg -> mentionsClauseRef target arg
  App f x -> isIntroReference target f || isIntroReference target x
  Lam body -> isIntroReference (target + 1) body
  Pi a b -> isIntroReference target a || isIntroReference (target + 1) b
  Sigma a b -> isIntroReference target a || isIntroReference (target + 1) b
  _ -> False

isElimReference :: Int -> MBTTExpr -> Bool
isElimReference target expr =
  mentionsClauseRef target expr
  && not (isIntroReference target expr)
  && not (isPathReference target expr)

isPathReference :: Int -> MBTTExpr -> Bool
isPathReference target expr = case expr of
  Id a x y ->
    mentionsClauseRef target a || mentionsClauseRef target x || mentionsClauseRef target y
  Refl a ->
    mentionsClauseRef target a
  PathCon _ ->
    False
  App f x ->
    isPathReference target f || isPathReference target x
  Lam body ->
    isPathReference (target + 1) body
  Pi a b ->
    isPathReference target a || isPathReference (target + 1) b
  Sigma a b ->
    isPathReference target a || isPathReference (target + 1) b
  Susp a ->
    isPathReference target a
  Trunc a ->
    isPathReference target a
  Flat a ->
    isPathReference target a
  Sharp a ->
    isPathReference target a
  Disc a ->
    isPathReference target a
  Shape a ->
    isPathReference target a
  Next a ->
    isPathReference target a
  Eventually a ->
    isPathReference target a
  _ ->
    False

mentionsClauseRef :: Int -> MBTTExpr -> Bool
mentionsClauseRef target = go 0
  where
    go !shift expr = case expr of
      Var i -> i == target + shift
      App f x -> go shift f || go shift x
      Lam body -> go (shift + 1) body
      Pi a b -> go shift a || go (shift + 1) b
      Sigma a b -> go shift a || go (shift + 1) b
      Id a x y -> go shift a || go shift x || go shift y
      Refl a -> go shift a
      Susp a -> go shift a
      Trunc a -> go shift a
      Flat a -> go shift a
      Sharp a -> go shift a
      Disc a -> go shift a
      Shape a -> go shift a
      Next a -> go shift a
      Eventually a -> go shift a
      _ -> False

hasTrueEliminatorShape :: MBTTExpr -> Bool
hasTrueEliminatorShape expr =
  go False expr
  where
    go underBinder body = case body of
      Lam inner -> go True inner
      Pi _ inner -> go True inner
      Sigma _ inner -> go True inner
      App f x -> underBinder && isBoundHead f && isBoundArgument x
      _ -> False

    isBoundHead headExpr = case headExpr of
      Var _ -> True
      App f _ -> isBoundHead f
      _ -> False

    isBoundArgument argExpr = case argExpr of
      Var _ -> True
      App f x -> isBoundHead f || isBoundArgument f || isBoundArgument x
      _ -> False

hasFormerFormationExpr :: MBTTExpr -> Bool
hasFormerFormationExpr expr = case expr of
  Pi _ _ -> True
  Sigma _ _ -> True
  App Univ inner -> hasFormerFormationExpr inner
  App a b -> hasFormerFormationExpr a || hasFormerFormationExpr b
  Lam body -> hasFormerFormationExpr body
  Id a x y -> hasFormerFormationExpr a || hasFormerFormationExpr x || hasFormerFormationExpr y
  Refl a -> hasFormerFormationExpr a
  Susp a -> hasFormerFormationExpr a
  Trunc a -> hasFormerFormationExpr a
  Flat a -> hasFormerFormationExpr a
  Sharp a -> hasFormerFormationExpr a
  Disc a -> hasFormerFormationExpr a
  Shape a -> hasFormerFormationExpr a
  Next a -> hasFormerFormationExpr a
  Eventually a -> hasFormerFormationExpr a
  _ -> False

hasFormerIntroExpr :: MBTTExpr -> Bool
hasFormerIntroExpr expr = case expr of
  Lam body -> hasFormerFormationExpr body
  App (App (Var _) (Var _)) (Var _) -> True
  App a b -> hasFormerIntroExpr a || hasFormerIntroExpr b
  Pi a b -> hasFormerIntroExpr a || hasFormerIntroExpr b
  Sigma a b -> hasFormerIntroExpr a || hasFormerIntroExpr b
  Id a x y -> hasFormerIntroExpr a || hasFormerIntroExpr x || hasFormerIntroExpr y
  Refl a -> hasFormerIntroExpr a
  Susp a -> hasFormerIntroExpr a
  Trunc a -> hasFormerIntroExpr a
  Flat a -> hasFormerIntroExpr a
  Sharp a -> hasFormerIntroExpr a
  Disc a -> hasFormerIntroExpr a
  Shape a -> hasFormerIntroExpr a
  Next a -> hasFormerIntroExpr a
  Eventually a -> hasFormerIntroExpr a
  _ -> False

hasFormerElimExpr :: MBTTExpr -> Bool
hasFormerElimExpr expr = case expr of
  App (Lam _) _ -> True
  App a b -> hasFormerElimExpr a || hasFormerElimExpr b
  Pi a b -> hasFormerElimExpr a || hasFormerElimExpr b
  Sigma a b -> hasFormerElimExpr a || hasFormerElimExpr b
  Id a x y -> hasFormerElimExpr a || hasFormerElimExpr x || hasFormerElimExpr y
  Refl a -> hasFormerElimExpr a
  Susp a -> hasFormerElimExpr a
  Trunc a -> hasFormerElimExpr a
  Flat a -> hasFormerElimExpr a
  Sharp a -> hasFormerElimExpr a
  Disc a -> hasFormerElimExpr a
  Shape a -> hasFormerElimExpr a
  Next a -> hasFormerElimExpr a
  Eventually a -> hasFormerElimExpr a
  _ -> False

universeShellPenalty :: Telescope -> Int
universeShellPenalty (Telescope entries) =
  sum
    [ 18 + 2 * min 6 (exprNodeCount inner)
    | TeleEntry _ (App Univ inner) <- entries
    , not (isSimpleUniverseCarrier inner)
    ]

isSimpleUniverseCarrier :: MBTTExpr -> Bool
isSimpleUniverseCarrier expr = case expr of
  Var _ -> True
  Lib _ -> True
  _ -> False

exprNodeCount :: MBTTExpr -> Int
exprNodeCount expr = case expr of
  Univ -> 1
  Var _ -> 1
  Lib _ -> 1
  PathCon _ -> 1
  Lam body -> 1 + exprNodeCount body
  App f x -> 1 + exprNodeCount f + exprNodeCount x
  Pi a b -> 1 + exprNodeCount a + exprNodeCount b
  Sigma a b -> 1 + exprNodeCount a + exprNodeCount b
  Id a x y -> 1 + exprNodeCount a + exprNodeCount x + exprNodeCount y
  Refl a -> 1 + exprNodeCount a
  Susp a -> 1 + exprNodeCount a
  Trunc a -> 1 + exprNodeCount a
  Flat a -> 1 + exprNodeCount a
  Sharp a -> 1 + exprNodeCount a
  Disc a -> 1 + exprNodeCount a
  Shape a -> 1 + exprNodeCount a
  Next a -> 1 + exprNodeCount a
  Eventually a -> 1 + exprNodeCount a
