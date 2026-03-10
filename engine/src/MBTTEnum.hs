{-# LANGUAGE BangPatterns #-}

-- | Typed MBTT Enumerator — exhaustive budget-split enumeration
--
-- Phase 1 of the MBTT-first migration. Generates well-typed anonymous
-- MBTT telescopes under bit-budget and depth bounds.
--
-- Unlike TelescopeGen (which uses bestChild shortcut and truncated iteration),
-- this module does proper exhaustive budget-split enumeration:
--   - For Pi(a,b), iterate all (budA, budB) splits
--   - For Id(A,x,y), iterate all three-way splits
--   - Library-gating reuses TelescopeGen.actionGatedByLibrary
--   - Telescopes filtered by TelescopeCheck, teleIsConnected, teleReferencesWindow

module MBTTEnum
  ( -- * Expression enumeration
    enumerateExprs
    -- * Telescope enumeration
  , enumerateMBTTTelescopes
  , enumerateMBTTTelescopesWithCache
  , enumerateMBTTTelescopesWithDiagnostics
    -- * Candidate type
  , MBTTCandidate(..)
    -- * Configuration
  , EnumConfig(..)
  , FrontierMode(..)
  , EnumBandDiagnostics(..)
  , EnumStepCache
  , emptyEnumStepCache
  , defaultEnumConfig
  ) where

import Kolmogorov (MBTTExpr(..), eliasGammaLength)
import Telescope (Telescope(..), TeleEntry(..), CoreJudgment(..), desugarEntry, teleIsConnected,
                  teleReferencesWindow, teleMaxLibRef, teleBitCost, teleLibRefs,
                  classifyTelescope, TelescopeClass(..),
                  desugaredKappa, isTriviallyDerivable)
import MBTTCanonical (CanonKey(..), canonicalKeySpec)
import TelescopeGen (GoalProfile(..), GoalIntent(..), deriveGoalProfile)
import TelescopeCheck (checkTelescope, CheckResult(..))
import StrictCritic (FrontierDiagnostics(..), analyzeObligations, frontierDiagnostics,
                     osScore, trueEliminatorScore)
import Types (Library, LibraryEntry(..))
import Data.List (foldl', intercalate, sort, sortOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================
-- Configuration
-- ============================================

-- | Enumeration parameters.
data EnumConfig = EnumConfig
  { ecMaxBitBudget  :: !Int    -- ^ Max total bit budget per entry (default 40)
  , ecMaxEntries    :: !Int    -- ^ Max telescope entries (default 8)
  , ecMaxASTDepth   :: !Int    -- ^ Max AST depth per expression (default 5)
  , ecMaxCandidates :: !Int    -- ^ Max candidates to return (default 5000)
  , ecExactClauseCount :: !(Maybe Int) -- ^ Optional exact desugaredKappa band
  , ecExprCapLimit  :: !(Maybe Int) -- ^ Optional per-context expression cap override
  , ecGoalProfile   :: !(Maybe GoalProfile) -- ^ Optional goal profile steering
  , ecEnableMacros  :: !Bool   -- ^ Enable macro-reuse candidate lane
  , ecFrontierMode  :: !FrontierMode -- ^ Frontier retention policy
  } deriving (Show)

data FrontierMode
  = FrontierNeutral
  | FrontierGoalDirected
  | FrontierObligationGuided
  deriving (Show, Eq)

data TargetHead
  = TargetUniverse
  | TargetPi
  | TargetSigma
  | TargetId
  | TargetSusp
  | TargetTrunc
  | TargetFlat
  | TargetSharp
  | TargetDisc
  | TargetShape
  | TargetNext
  | TargetEventually
  deriving (Show, Eq, Ord)

data RoleTarget
  = Ambient !TargetHead
  | Local !Int
  deriving (Show, Eq, Ord)

data ClauseRole
  = RoleFormation !RoleTarget
  | RoleIntro !RoleTarget
  | RoleElim !RoleTarget
  | RolePath !RoleTarget
  | RoleMacro
  deriving (Show, Eq, Ord)

newtype PrefixBucket = PrefixBucket [ClauseRole]
  deriving (Show, Eq, Ord)

data PendingObligation
  = NeedIntroFor !RoleTarget
  | NeedElimOrPathFor !RoleTarget
  deriving (Show, Eq, Ord)

data PrefixState = PrefixState
  { psRoleHistory :: ![ClauseRole]
  , psOpenObligations :: ![PendingObligation]
  , psFulfillmentBonus :: !Int
  } deriving (Show, Eq)

data EnumBandDiagnostics = EnumBandDiagnostics
  { ebdFrontier :: !FrontierDiagnostics
  , ebdExprCountsByCtx :: ![Int]
  , ebdScannedTelescopes :: !Int
  , ebdValidTelescopes :: !Int
  } deriving (Show, Eq)

-- | Default enumeration config. Budget is per-entry, not total.
defaultEnumConfig :: EnumConfig
defaultEnumConfig = EnumConfig
  { ecMaxBitBudget  = 40
  , ecMaxEntries    = 8
  , ecMaxASTDepth   = 5
  , ecMaxCandidates = 5000
  , ecExactClauseCount = Nothing
  , ecExprCapLimit  = Nothing
  , ecGoalProfile   = Nothing
  , ecEnableMacros  = True
  , ecFrontierMode  = FrontierGoalDirected
  }

data ExprCacheKey = ExprCacheKey
  { eckGoalProfile :: !String
  , eckLibrary     :: !String
  , eckCtxDepth    :: !Int
  , eckBudget      :: !Int
  , eckMaxDepth    :: !Int
  , eckModal       :: !Bool
  , eckTemporal    :: !Bool
  } deriving (Show, Eq, Ord)

data EnumStepCache = EnumStepCache
  { escExprs :: !(Map.Map ExprCacheKey [MBTTExpr])
  } deriving (Show)

emptyEnumStepCache :: EnumStepCache
emptyEnumStepCache = EnumStepCache Map.empty

-- ============================================
-- Candidate Type
-- ============================================

-- | Anonymous candidate: a telescope with cost metadata.
data MBTTCandidate = MBTTCandidate
  { mcTelescope   :: !Telescope   -- ^ The anonymous telescope
  , mcBitKappa    :: !Int         -- ^ MBTT bit-length (primary κ)
  , mcClauseCount :: !Int         -- ^ desugaredKappa (secondary)
  , mcNodeCount   :: !Int         -- ^ AST node count
  , mcHeuristic   :: !Int         -- ^ Semantic relevance heuristic (higher is better)
  } deriving (Show, Eq)

instance Ord MBTTCandidate where
  compare a b = compare (Down (mcHeuristic a), mcBitKappa a, mcClauseCount a)
                        (Down (mcHeuristic b), mcBitKappa b, mcClauseCount b)

-- ============================================
-- Expression Enumeration
-- ============================================

-- | Enumerate all well-typed MBTTExpr up to a bit budget.
-- Uses exhaustive budget-split for compound expressions.
--
-- Args: libSize, ctxDepth, bitBudget, maxASTDepth, library (for gating)
enumerateExprs :: Int -> Int -> Int -> Int -> Library -> [MBTTExpr]
enumerateExprs = enumerateExprsWithGoal Nothing

-- | Goal-profile aware expression enumeration.
-- When a goal profile is present, constructor families are pruned to
-- structurally relevant ones (with a permissive fallback when intents are empty).
enumerateExprsWithGoal :: Maybe GoalProfile -> Int -> Int -> Int -> Int -> Library -> [MBTTExpr]
enumerateExprsWithGoal goalProfile libSize ctxDepth budget maxDepth lib
  | budget <= 0 = []
  | maxDepth <= 0 = terminals
  | otherwise = terminals ++ compounds
  where
    intents = maybe [] gpIntents goalProfile
    goalAny needs = null intents || any (`elem` intents) needs
    -- Allow a small implicit ambient context for schematic open terms.
    -- This keeps search expressive enough to propose witness-like candidates
    -- without relying on hard-coded sequence templates.
    ambientVars = 2

    -- Modal/temporal gating flags (structural, not name-based)
    hasModalPrereqs =
      any leHasDependentFunctions lib
      && any hasTruncEntry lib
      && maximum (0 : concatMap lePathDims lib) >= 3
    hasModal   = any leHasModalOps lib || hasModalPrereqs
    hasTemporal = any leHasHilbert lib
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

    -- Terminal expressions
    terminals = concat
      [ [Univ | budget >= 4]
      , [Var i | i <- [1..ctxDepth + ambientVars], budget >= 3 + eliasGammaLength i]
      , [Lib i | i <- [1..libSize], budget >= 3 + eliasGammaLength i]
      , [PathCon d | d <- [1..3], budget >= 6 + eliasGammaLength d
                   , goalAny [NeedHIT, NeedBootstrap, NeedCurvature, NeedMetric, NeedHilbert]]
      ]

    -- Compound expressions with budget splits
    compounds = concat
      [ if goalAny [NeedBootstrap, NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
          then enumBinaryNoExt App 2 else []
      , if goalAny [NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
          then enumUnary Lam 2 True else []
      , enumUnary Refl 5 False
      , if goalAny [NeedHIT] then enumUnary Susp 5 False else []
      , if goalAny [NeedHIT] then enumUnary Trunc 6 False else []
      , if goalAny [NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert] then enumBinary Pi 3 True else []
      , if goalAny [NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert] then enumBinary Sigma 4 True else []
      , if hasModal then concat
          [ if goalAny [NeedModal, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert] then enumUnary Flat 7 False else []
          , if goalAny [NeedModal, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert] then enumUnary Sharp 7 False else []
          , if goalAny [NeedModal, NeedBridge] then enumUnary Disc 7 False else []
          , if goalAny [NeedModal, NeedBridge] then enumUnary Shape 8 False else []
          ] else []
      , if hasTemporal then concat
          [ if goalAny [NeedTemporal] then enumUnary Next 9 False else []
          , if goalAny [NeedTemporal] then enumUnary Eventually 9 False else []
          ] else []
      , if goalAny [NeedHIT, NeedFormer, NeedCurvature, NeedMetric, NeedHilbert] then enumTernary Id 5 else []
      ]

    -- Unary: constructor(child) with prefix cost, optionally extending context
    enumUnary ctor prefix extendsCtx =
      let remaining = budget - prefix
          subDepth = ctxDepth + (if extendsCtx then 1 else 0)
      in if remaining < 4  -- minimum child size
         then []
          else [ ctor child
               | child <- enumerateExprsWithGoal goalProfile libSize subDepth remaining (maxDepth - 1) lib
               ]

    -- Binary with context extension for second child (Pi, Sigma)
    enumBinary ctor prefix _extendsCtx =
      let remaining = budget - prefix
          minChild = 4  -- smallest expression is 4 bits (Univ)
          splits = binaryBudgetSplits remaining minChild
      in if remaining < 2 * minChild
         then []
          else [ ctor a b
               | (budA, budB) <- splits
               , a <- enumerateExprsWithGoal goalProfile libSize ctxDepth budA (maxDepth - 1) lib
               , b <- enumerateExprsWithGoal goalProfile libSize (ctxDepth + 1) budB (maxDepth - 1) lib
               ]

    -- Binary without context extension (App)
    enumBinaryNoExt ctor prefix =
      let remaining = budget - prefix
          minArg = 4
          splits = binaryBudgetSplits remaining minArg
      in if remaining < 2 * minArg
         then []
          else [ ctor f x
               | (budF, budX) <- splits
               -- Reject App(_ Univ) per TelescopeCheck
               , f <- enumerateExprsWithGoal goalProfile libSize ctxDepth budF (maxDepth - 1) lib
               , x <- enumerateExprsWithGoal goalProfile libSize ctxDepth budX (maxDepth - 1) lib
               , case x of { Univ -> False; _ -> True }
               ]

    -- Ternary: Id(A, x, y) — three-way split
    enumTernary ctor prefix =
      let remaining = budget - prefix
          minChild = 4
          splits = ternaryBudgetSplits remaining minChild
      in if remaining < 3 * minChild
         then []
          else [ ctor a x y
               | (budA, budX, budY) <- splits
               , a <- enumerateExprsWithGoal goalProfile libSize ctxDepth budA (maxDepth - 1) lib
               , x <- enumerateExprsWithGoal goalProfile libSize ctxDepth budX (maxDepth - 1) lib
               , y <- enumerateExprsWithGoal goalProfile libSize ctxDepth budY (maxDepth - 1) lib
               ]

-- | Heuristic binary budget splits: balanced plus two asymmetric probes.
-- This replaces full split enumeration and removes the worst combinatorial blow-up.
binaryBudgetSplits :: Int -> Int -> [(Int, Int)]
binaryBudgetSplits remaining minChild =
  let low = minChild
      high = remaining - minChild
      mid = remaining `div` 2
      cands = [(mid, remaining - mid), (low, high), (high, low)]
      valid (a, b) = a >= minChild && b >= minChild
  in uniquePairs (filter valid cands)

-- | Heuristic ternary splits: balanced plus two edge-biased variants.
ternaryBudgetSplits :: Int -> Int -> [(Int, Int, Int)]
ternaryBudgetSplits remaining minChild =
  let base = remaining `div` 3
      rem1 = remaining - 2 * base
      balanced = (base, base, rem1)
      edgeA = (minChild, minChild, remaining - 2 * minChild)
      edgeB = (remaining - 2 * minChild, minChild, minChild)
      cands = [balanced, edgeA, edgeB]
      valid (a, b, c) = a >= minChild && b >= minChild && c >= minChild
  in uniqueTriples (filter valid cands)

uniquePairs :: [(Int, Int)] -> [(Int, Int)]
uniquePairs [] = []
uniquePairs (x:xs)
  | x `elem` xs = uniquePairs xs
  | otherwise = x : uniquePairs xs

uniqueTriples :: [(Int, Int, Int)] -> [(Int, Int, Int)]
uniqueTriples [] = []
uniqueTriples (x:xs)
  | x `elem` xs = uniqueTriples xs
  | otherwise = x : uniqueTriples xs

-- | Count AST nodes in an MBTTExpr.
nodeCount :: MBTTExpr -> Int
nodeCount expr = case expr of
  Univ       -> 1
  Var _      -> 1
  Lib _      -> 1
  PathCon _  -> 1
  Lam e      -> 1 + nodeCount e
  Refl e     -> 1 + nodeCount e
  Susp e     -> 1 + nodeCount e
  Trunc e    -> 1 + nodeCount e
  Flat e     -> 1 + nodeCount e
  Sharp e    -> 1 + nodeCount e
  Disc e     -> 1 + nodeCount e
  Shape e    -> 1 + nodeCount e
  Next e     -> 1 + nodeCount e
  Eventually e -> 1 + nodeCount e
  Pi a b     -> 1 + nodeCount a + nodeCount b
  Sigma a b  -> 1 + nodeCount a + nodeCount b
  App f x    -> 1 + nodeCount f + nodeCount x
  Id a x y   -> 1 + nodeCount a + nodeCount x + nodeCount y

-- | Total node count of a telescope.
teleNodeCount :: Telescope -> Int
teleNodeCount (Telescope entries) = sum [nodeCount (teType e) | e <- entries]

-- ============================================
-- Telescope Enumeration
-- ============================================

-- | Enumerate all valid MBTT telescopes for a given library state.
-- Returns candidates ordered by ascending bit cost.
--
-- Memory discipline:
--   * Stream telescopes depth-first (no full `allTeles` materialization)
--   * Evaluate validity immediately
--   * Keep only a bounded sorted frontier of top candidates
enumerateMBTTTelescopes :: Library -> EnumConfig -> [MBTTCandidate]
enumerateMBTTTelescopes lib cfg =
  fst (fst (enumerateMBTTTelescopesWithDiagnostics lib cfg emptyEnumStepCache))

enumerateMBTTTelescopesWithCache :: Library -> EnumConfig -> EnumStepCache -> ([MBTTCandidate], EnumStepCache)
enumerateMBTTTelescopesWithCache lib cfg cache0 =
  let ((candidates, _diag), cache1) = enumerateMBTTTelescopesWithDiagnostics lib cfg cache0
  in (candidates, cache1)

enumerateMBTTTelescopesWithDiagnostics
  :: Library
  -> EnumConfig
  -> EnumStepCache
  -> (([MBTTCandidate], EnumBandDiagnostics), EnumStepCache)
enumerateMBTTTelescopesWithDiagnostics lib cfg cache0 =
  let libSize = length lib
      maxK = ecMaxEntries cfg
      budget = ecMaxBitBudget cfg
      depth = ecMaxASTDepth cfg
      maxCand = max 1 (ecMaxCandidates cfg)
      macroQuota = if ecEnableMacros cfg then max 2 (min 3 (maxCand `div` 10 + 1)) else 0
      exactClauseCount = ecExactClauseCount cfg
      goalProfile = maybe (deriveGoalProfile lib) id (ecGoalProfile cfg)
      exprCapPerCtx =
        maybe defaultExprCap (`min` defaultExprCap) (ecExprCapLimit cfg)
      defaultExprCap = min 192 (max 48 (maxCand * 3))
      goalKey = goalProfileFingerprint goalProfile
      libKey = libraryFingerprint lib
      (exprsByCtx, cache1) =
        buildExprsByCtx (Just goalProfile) goalKey libKey lib libSize budget depth exprCapPerCtx
          [0 .. max 0 (maxK - 1)]
          cache0
      scoreHeuristic tele = frontierHeuristic cfg tele lib goalProfile
      prefixLimit = prefixFrontierLimit maxCand exactClauseCount
      prefixAdmissible tele heuristic =
        checkTelescope lib tele == CheckOK
        && heuristic > prefixHeuristicFloor cfg
      asCandidate tele =
        MBTTCandidate tele (teleBitCost tele) (desugaredKappa tele) (teleNodeCount tele)
          (scoreHeuristic tele)
      macroSeed =
        if ecEnableMacros cfg
        then [ asCandidate tele
             | tele <- macroReuseTelescopes lib goalProfile
             , isValidCandidate lib libSize tele
             , maybe True (== desugaredKappa tele) exactClauseCount
             ]
        else []
      macroReserved = take macroQuota (sortOn (candidateFrontierKey cfg lib goalProfile . mcTelescope) macroSeed)

      extendPrefixes :: Int -> [MBTTCandidate] -> [MBTTCandidate]
      extendPrefixes ctxDepth prefixes =
        let entryName = "c" ++ show (ctxDepth + 1)
            exprs = if ctxDepth < length exprsByCtx then exprsByCtx !! ctxDepth else []
            bucketed =
              foldl'
                (\bucketAcc prefix ->
                   let Telescope entries = mcTelescope prefix
                   in foldl'
                        (\bucketAcc' expr ->
                           let tele = Telescope (entries ++ [TeleEntry entryName expr])
                               heuristic = scoreHeuristic tele
                           in if prefixAdmissible tele heuristic
                              then insertBucketCandidate prefixLimit
                                     (structuralSignature tele)
                                     (MBTTCandidate tele (teleBitCost tele) (desugaredKappa tele) (teleNodeCount tele) heuristic)
                                     bucketAcc'
                              else bucketAcc')
                        bucketAcc
                        exprs)
                Map.empty
                prefixes
        in retainStratifiedPrefixes prefixLimit bucketed

      scanLengths :: Int -> [MBTTCandidate] -> ([MBTTCandidate], Int, Int) -> ([MBTTCandidate], Int, Int)
      scanLengths !ctxDepth !prefixes (!best, !scannedCount, !validCount)
        | ctxDepth >= maxK || null prefixes = (best, scannedCount, validCount)
        | otherwise =
            let expanded = extendPrefixes ctxDepth prefixes
                scanned' = scannedCount + length expanded
                (best', validCount') =
                  foldl'
                    (\(!bestAcc, !validAcc) cand ->
                       let tele = mcTelescope cand
                           heuristic = mcHeuristic cand
                           clauseCount = mcClauseCount cand
                           matchesBand = maybe True (== clauseCount) exactClauseCount
                       in if heuristic > 0 && matchesBand && isValidCandidate lib libSize tele
                          then (insertBestCandidate maxCand cand bestAcc, validAcc + 1)
                          else (bestAcc, validAcc))
                    (best, validCount)
                    expanded
            in scanLengths (ctxDepth + 1) expanded (best', scanned', validCount')

      scanLimit = max 1 (maxCand - length macroReserved)
      seedPrefixes = [MBTTCandidate (Telescope []) 0 0 0 0]
      (scanned, scannedTelescopes, validTelescopes) = scanLengths 0 seedPrefixes ([], 0, 0)
      kept = mergeWithMacroQuota maxCand macroReserved (take scanLimit scanned)
      keptSummaries =
        case ecFrontierMode cfg of
          FrontierObligationGuided -> map (analyzeObligations lib . mcTelescope) kept
          _ -> []
      frontier =
        case ecFrontierMode cfg of
          FrontierObligationGuided ->
            let fd = frontierDiagnostics keptSummaries
            in fd
                 { fdFrontierCandidates = validTelescopes + length macroSeed
                 , fdFrontierKept = length kept
                 }
          _ ->
            FrontierDiagnostics
              { fdFrontierCandidates = validTelescopes + length macroSeed
              , fdFrontierKept = length kept
              , fdBestObligationScore = Nothing
              , fdBestInterfaceDensity = Nothing
              , fdBestGenericityScore = Nothing
              , fdBestClosureScore = Nothing
              }
      diagnostics =
        EnumBandDiagnostics
          { ebdFrontier = frontier
          , ebdExprCountsByCtx = map length exprsByCtx
          , ebdScannedTelescopes = scannedTelescopes
          , ebdValidTelescopes = validTelescopes + length macroSeed
          }
  in ((kept, diagnostics), cache1)

buildExprsByCtx
  :: Maybe GoalProfile
  -> String
  -> String
  -> Library
  -> Int
  -> Int
  -> Int
  -> Int
  -> [Int]
  -> EnumStepCache
  -> ([[MBTTExpr]], EnumStepCache)
buildExprsByCtx _ _ _ _ _ _ _ _ [] cache = ([], cache)
buildExprsByCtx goalProfile goalKey libKey lib libSize budget depth cap (ctx:rest) cache0 =
  let (exprs, cache1) =
        enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctx budget depth cap cache0
      (restExprs, cache2) =
        buildExprsByCtx goalProfile goalKey libKey lib libSize budget depth cap rest cache1
  in (take cap exprs : restExprs, cache2)

enumerateExprsCachedCap
  :: Maybe GoalProfile
  -> String
  -> String
  -> Library
  -> Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> EnumStepCache
  -> ([MBTTExpr], EnumStepCache)
enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctxDepth budget maxDepth cap cache0 =
  case Map.lookup key (escExprs cache0) of
    Just cached -> (take cap cached, cache0)
    Nothing ->
      let terminals =
            concat
              [ [Univ | budget >= 4]
              , [Var i | i <- [1..ctxDepth + ambientVars], budget >= 3 + eliasGammaLength i]
              , [Lib i | i <- [1..libSize], budget >= 3 + eliasGammaLength i]
              , [PathCon d | d <- [1..3], budget >= 6 + eliasGammaLength d
                           , goalAny [NeedHIT, NeedBootstrap, NeedCurvature, NeedMetric, NeedHilbert]]
              ]
          compounds =
            if budget <= 0 || maxDepth <= 0
            then ([], cache0)
            else runGenerators cache0
          result = take cacheCap (terminals ++ fst compounds)
          cacheAfter = snd compounds
          cache1 =
            cacheAfter {escExprs = Map.insert key result (escExprs cacheAfter)}
      in (take cap result, cache1)
  where
    intents = maybe [] gpIntents goalProfile
    goalAny needs = null intents || any (`elem` intents) needs
    ambientVars = 2
    cacheCap = max 192 cap
    hasModalPrereqs =
      any leHasDependentFunctions lib
      && any hasTruncEntry lib
      && maximum (0 : concatMap lePathDims lib) >= 3
    hasModal = any leHasModalOps lib || hasModalPrereqs
    hasTemporal = any leHasHilbert lib
    key =
      ExprCacheKey
        { eckGoalProfile = goalKey
        , eckLibrary = libKey
        , eckCtxDepth = ctxDepth
        , eckBudget = budget
        , eckMaxDepth = maxDepth
        , eckModal = hasModal
        , eckTemporal = hasTemporal
        }
    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

    runGenerators cache =
      concatGenerators
        [ if goalAny [NeedBootstrap, NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
          then enumBinaryNoExt App 2 else emptyGen
        , if goalAny [NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
          then enumUnary Lam 2 True else emptyGen
        , enumUnary Refl 5 False
        , if goalAny [NeedHIT] then enumUnary Susp 5 False else emptyGen
        , if goalAny [NeedHIT] then enumUnary Trunc 6 False else emptyGen
        , if goalAny [NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
          then enumBinary Pi 3 else emptyGen
        , if goalAny [NeedFormer, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
          then enumBinary Sigma 4 else emptyGen
        , if hasModal
          then concatGenerators
                 [ if goalAny [NeedModal, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
                   then enumUnary Flat 7 False else emptyGen
                 , if goalAny [NeedModal, NeedBridge, NeedDifferential, NeedCurvature, NeedMetric, NeedHilbert]
                   then enumUnary Sharp 7 False else emptyGen
                 , if goalAny [NeedModal, NeedBridge] then enumUnary Disc 7 False else emptyGen
                 , if goalAny [NeedModal, NeedBridge] then enumUnary Shape 8 False else emptyGen
                 ]
          else emptyGen
        , if hasTemporal
          then concatGenerators
                 [ if goalAny [NeedTemporal] then enumUnary Next 9 False else emptyGen
                 , if goalAny [NeedTemporal] then enumUnary Eventually 9 False else emptyGen
                 ]
          else emptyGen
        , if goalAny [NeedHIT, NeedFormer, NeedCurvature, NeedMetric, NeedHilbert]
          then enumTernary Id 5 else emptyGen
        ]
        cache

    emptyGen cache = ([], cache)

    concatGenerators gens cache0' = foldl' step ([], cache0') gens
      where
        step (acc, cacheAcc) gen =
          let (chunk, cacheNext) = gen cacheAcc
          in (acc ++ chunk, cacheNext)

    enumUnary ctor prefix extendsCtx cache
      | remaining < 4 = ([], cache)
      | otherwise =
          let subDepth = ctxDepth + if extendsCtx then 1 else 0
              (children, cache1) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize subDepth remaining (maxDepth - 1) cacheCap cache
          in (map ctor children, cache1)
      where
        remaining = budget - prefix

    enumBinary ctor prefix cache
      | remaining < 2 * minChild = ([], cache)
      | otherwise =
          foldl' step ([], cache) splits
      where
        remaining = budget - prefix
        minChild = 4
        splits = binaryBudgetSplits remaining minChild
        step (acc, cacheAcc) (budA, budB) =
          let (as, cache1) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctxDepth budA (maxDepth - 1) cacheCap cacheAcc
              (bs, cache2) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize (ctxDepth + 1) budB (maxDepth - 1) cacheCap cache1
          in (acc ++ [ctor a b | a <- as, b <- bs], cache2)

    enumBinaryNoExt ctor prefix cache
      | remaining < 2 * minArg = ([], cache)
      | otherwise =
          foldl' step ([], cache) splits
      where
        remaining = budget - prefix
        minArg = 4
        splits = binaryBudgetSplits remaining minArg
        step (acc, cacheAcc) (budF, budX) =
          let (fs, cache1) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctxDepth budF (maxDepth - 1) cacheCap cacheAcc
              (xs, cache2) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctxDepth budX (maxDepth - 1) cacheCap cache1
              validXs = filter (\x -> case x of { Univ -> False; _ -> True }) xs
          in (acc ++ [ctor f x | f <- fs, x <- validXs], cache2)

    enumTernary ctor prefix cache
      | remaining < 3 * minChild = ([], cache)
      | otherwise =
          foldl' step ([], cache) splits
      where
        remaining = budget - prefix
        minChild = 4
        splits = ternaryBudgetSplits remaining minChild
        step (acc, cacheAcc) (budA, budX, budY) =
          let (as, cache1) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctxDepth budA (maxDepth - 1) cacheCap cacheAcc
              (xs, cache2) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctxDepth budX (maxDepth - 1) cacheCap cache1
              (ys, cache3) =
                enumerateExprsCachedCap goalProfile goalKey libKey lib libSize ctxDepth budY (maxDepth - 1) cacheCap cache2
          in (acc ++ [ctor a x y | a <- as, x <- xs, y <- ys], cache3)

goalProfileFingerprint :: GoalProfile -> String
goalProfileFingerprint profile =
  intercalate "|" (map show (sort (gpIntents profile)))

libraryFingerprint :: Library -> String
libraryFingerprint lib =
  intercalate ";"
    [ intercalate ":"
        [ show ix
        , show (leConstructors entry)
        , show (leHasDependentFunctions entry)
        , show (sort (lePathDims entry))
        , show (leHasLoop entry)
        , show (fmap id (leIsTruncated entry))
        , show (leHasModalOps entry)
        , show (leHasDifferentialOps entry)
        , show (leHasCurvature entry)
        , show (leHasMetric entry)
        , show (leHasHilbert entry)
        , show (leHasTemporalOps entry)
        ]
    | (ix, entry) <- zip [(1 :: Int)..] lib
    ]

-- | Insert a candidate into an ascending-score frontier, trimming to max size.
insertBestCandidate :: Int -> MBTTCandidate -> [MBTTCandidate] -> [MBTTCandidate]
insertBestCandidate limit cand = go (max 0 limit)
  where
    go 0 _ = []
    go _ [] = [cand]
    go !n ys@(y:ys')
      | candidateScore cand <= candidateScore y = cand : take (n - 1) ys
      | otherwise = y : go (n - 1) ys'

candidateScore :: MBTTCandidate -> (Down Int, Int, Int, Int, String)
candidateScore c =
  ( Down (mcHeuristic c)
  , mcBitKappa c
  , mcClauseCount c
  , mcNodeCount c
  , candidateCanonKeyText (mcTelescope c)
  )

candidateCanonKeyText :: Telescope -> String
candidateCanonKeyText tele =
  case canonicalKeySpec (map teType (teleEntries tele)) of
    CanonKey key -> key

frontierHeuristic :: EnumConfig -> Telescope -> Library -> GoalProfile -> Int
frontierHeuristic cfg tele lib profile =
  let connectedness = frontierConnectednessScore cfg tele
      prefixAdjustment = prefixStateAdjustment (reifyPrefixState tele)
  in case ecFrontierMode cfg of
       FrontierNeutral -> 1
       FrontierGoalDirected -> goalDirectedHeuristic tele lib profile + connectedness + prefixAdjustment
       FrontierObligationGuided -> osScore (analyzeObligations lib tele) + connectedness + prefixAdjustment

candidateFrontierKey
  :: EnumConfig
  -> Library
  -> GoalProfile
  -> Telescope
  -> (Down Int, Int, Int, Int, String)
candidateFrontierKey cfg lib profile tele =
  ( Down (frontierHeuristic cfg tele lib profile)
  , teleBitCost tele
  , desugaredKappa tele
  , teleNodeCount tele
  , candidateCanonKeyText tele
  )

-- | Preserve a small macro lane while keeping global cap bounded.
mergeWithMacroQuota :: Int -> [MBTTCandidate] -> [MBTTCandidate] -> [MBTTCandidate]
mergeWithMacroQuota maxCand macros scanned =
  let macroKeys = Set.fromList (map candKey macros)
      scannedUnique = [c | c <- scanned, not (Set.member (candKey c) macroKeys)]
  in take maxCand (macros ++ scannedUnique)
  where
    candKey c = canonicalKeySpec (map teType (teleEntries (mcTelescope c)))

insertBucketCandidate
  :: Int
  -> PrefixBucket
  -> MBTTCandidate
  -> Map.Map PrefixBucket [MBTTCandidate]
  -> Map.Map PrefixBucket [MBTTCandidate]
insertBucketCandidate limit bucket cand =
  Map.alter step bucket
  where
    step Nothing = Just [cand]
    step (Just existing) = Just (insertBestCandidate limit cand existing)

retainStratifiedPrefixes :: Int -> Map.Map PrefixBucket [MBTTCandidate] -> [MBTTCandidate]
retainStratifiedPrefixes limit buckets =
  case nonEmptyBuckets of
    [] -> []
    [single] -> take limit single
    _ ->
      let activeCount = length nonEmptyBuckets
          quota = max 1 (limit `div` activeCount)
          seeded = concatMap (take quota) nonEmptyBuckets
          leftovers = sortOn candidateScore (concatMap (drop quota) nonEmptyBuckets)
      in take limit (sortOn candidateScore seeded ++ leftovers)
  where
    nonEmptyBuckets = filter (not . null) (Map.elems buckets)

structuralSignature :: Telescope -> PrefixBucket
structuralSignature tele =
  PrefixBucket (psRoleHistory (reifyPrefixState tele))

prefixHeuristicFloor :: EnumConfig -> Int
prefixHeuristicFloor cfg =
  case ecFrontierMode cfg of
    FrontierNeutral -> 0
    _ -> -900

prefixFrontierLimit :: Int -> Maybe Int -> Int
prefixFrontierLimit maxCand exactClauseCount =
  max maxCand $
    case exactClauseCount of
      Just k | k <= 3 -> min 3072 (maxCand * 2)
      Just k | k <= 5 -> min 4096 (maxCand * 3)
      _ -> min 6144 (maxCand * 4)

frontierConnectednessScore :: EnumConfig -> Telescope -> Int
frontierConnectednessScore cfg (Telescope entries) =
  case ecFrontierMode cfg of
    FrontierNeutral -> 0
    _ -> fst (foldl' step (0, []) entries)
  where
    step (!score, earlier) entry =
      let role = classifyClauseRole earlier entry
          edgeCount = clauseConnectednessEdges earlier entry
          delta
            | null earlier = 0
            | edgeCount == 0 && roleAllowsDisconnected role = 0
            | edgeCount == 0 = -1000
            | otherwise = edgeCount * 15
      in (score + delta, earlier ++ [entry])

clauseConnectednessEdges :: [TeleEntry] -> TeleEntry -> Int
clauseConnectednessEdges earlier entry =
  let clauseIx = length earlier
      expr = teType entry
      explicitRefs = [target | target <- [1..clauseIx], mentionsClauseRef target expr]
      typologicalRefs =
        [ target
        | target <- typologicalSupportRefs earlier entry
        , not (elem target explicitRefs)
        ]
  in length (explicitRefs ++ typologicalRefs)

typologicalSupportRefs :: [TeleEntry] -> TeleEntry -> [Int]
typologicalSupportRefs earlier entry =
  uniqueRefs $
    case classifyClauseRole earlier entry of
      RolePath target -> catMaybes [latestFormationRefFor target earlier, latestIntroRefFor target earlier]
      RoleElim target -> catMaybes [latestFormationRefFor target earlier, latestIntroRefFor target earlier]
      RoleIntro target -> catMaybes [latestFormationRefFor target earlier]
      _ -> []

latestFormationRefFor :: RoleTarget -> [TeleEntry] -> Maybe Int
latestFormationRefFor target = latestSupportRef (== RoleFormation target)

latestIntroRefFor :: RoleTarget -> [TeleEntry] -> Maybe Int
latestIntroRefFor target = latestSupportRef (roleSupportsIntro target)

latestSupportRef :: (ClauseRole -> Bool) -> [TeleEntry] -> Maybe Int
latestSupportRef predicate earlier = go [] 0 Nothing earlier
  where
    total = length earlier

    go _ _ best [] = best
    go prefix ix best (teleEntry:rest) =
      let role = classifyClauseRole prefix teleEntry
          ref = total - ix
          best' = if predicate role then Just ref else best
      in go (prefix ++ [teleEntry]) (ix + 1) best' rest

roleSupportsIntro :: RoleTarget -> ClauseRole -> Bool
roleSupportsIntro target role =
  role == RoleIntro target || role == RolePath target

roleAllowsDisconnected :: ClauseRole -> Bool
roleAllowsDisconnected role = case role of
  RoleFormation _ -> True
  RoleIntro _ -> True
  RoleElim _ -> True
  _ -> False

emptyPrefixState :: PrefixState
emptyPrefixState = PrefixState [] [] 0

reifyPrefixState :: Telescope -> PrefixState
reifyPrefixState (Telescope entries) = fst (foldl' step (emptyPrefixState, []) entries)
  where
    step (!state, earlier) entry =
      let role = classifyClauseRole earlier entry
      in (updatePrefixState state role, earlier ++ [entry])

updatePrefixState :: PrefixState -> ClauseRole -> PrefixState
updatePrefixState state role =
  let obligations0 = psOpenObligations state
      (pushedObligations, bonusDelta) =
        case role of
          RoleFormation target ->
            (appendObligation (NeedIntroFor target) obligations0, 0)
          RoleIntro target ->
            let (afterIntroDebt, introBonus) =
                  fulfillObligation (NeedIntroFor target) obligations0
                directAmbientBonus =
                  if introBonus == 0 && canStartAmbientLifecycle target
                  then 30
                  else 0
                shouldOpenElim =
                  introBonus > 0 || directAmbientBonus > 0
                obligations1 =
                  if shouldOpenElim
                  then appendObligation (NeedElimOrPathFor target) afterIntroDebt
                  else afterIntroDebt
            in (obligations1, introBonus + directAmbientBonus)
          RoleElim target ->
            fulfillObligation (NeedElimOrPathFor target) obligations0
          RolePath target ->
            fulfillObligation (NeedElimOrPathFor target) obligations0
          RoleMacro ->
            (obligations0, 0)
  in PrefixState
       { psRoleHistory = psRoleHistory state ++ [role]
       , psOpenObligations = pushedObligations
       , psFulfillmentBonus = psFulfillmentBonus state + bonusDelta
       }

fulfillObligation :: PendingObligation -> [PendingObligation] -> ([PendingObligation], Int)
fulfillObligation obligation obligations =
  case break (== obligation) obligations of
    (_, []) -> (obligations, 0)
    (before, _:after) -> (before ++ after, 50)

prefixStateAdjustment :: PrefixState -> Int
prefixStateAdjustment state =
  psFulfillmentBonus state - 12 * length (psOpenObligations state)

classifyClauseRole :: [TeleEntry] -> TeleEntry -> ClauseRole
classifyClauseRole earlier entry =
  case pathTarget earlier expr of
    Just target -> RolePath target
    Nothing ->
      case elimTarget entry of
        Just target -> RoleElim target
        Nothing ->
          case formationTarget earlier expr of
            Just target -> RoleFormation target
            Nothing ->
              case introTarget earlier expr of
                Just target -> RoleIntro target
                Nothing ->
                  case judgments of
                    js
                      | JPathAttach `elem` js ->
                          maybe RoleMacro RolePath (latestPendingElimTarget earlier)
                      | JElimination `elem` js || JComputation `elem` js ->
                          maybe RoleMacro RoleElim (latestPendingElimTarget earlier)
                      | otherwise -> RoleMacro
  where
    expr = teType entry
    judgments = desugarEntry entry

formationTarget :: [TeleEntry] -> MBTTExpr -> Maybe RoleTarget
formationTarget earlier expr = case expr of
  Univ -> Just (Ambient TargetUniverse)
  Pi _ _ -> Just (Ambient TargetPi)
  Sigma _ _ -> Just (Ambient TargetSigma)
  Id _ _ _ -> Just (Ambient TargetId)
  Susp _ -> Just (Ambient TargetSusp)
  Trunc _ -> Just (Ambient TargetTrunc)
  Flat _ -> Just (Ambient TargetFlat)
  Sharp _ -> Just (Ambient TargetSharp)
  Disc _ -> Just (Ambient TargetDisc)
  Shape _ -> Just (Ambient TargetShape)
  Next _ -> Just (Ambient TargetNext)
  Eventually _ -> Just (Ambient TargetEventually)
  App Univ inner
    | isSimpleUniverseCarrierExpr inner -> Just (Local (length earlier + 1))
  _ -> Nothing

pathTarget :: [TeleEntry] -> MBTTExpr -> Maybe RoleTarget
pathTarget earlier expr = case expr of
  PathCon _ -> latestPendingElimTarget earlier `orElse` latestIntroTarget earlier `orElse` latestFormationTarget earlier
  Refl _ -> latestPendingElimTarget earlier `orElse` latestIntroTarget earlier
  _ -> Nothing

elimTarget :: TeleEntry -> Maybe RoleTarget
elimTarget entry
  | trueEliminatorScore (Telescope [entry]) > 0 = Just (Ambient TargetPi)
  | otherwise =
      case teType entry of
        App (Lam _) _ -> Just (Ambient TargetPi)
        _ -> Nothing

introTarget :: [TeleEntry] -> MBTTExpr -> Maybe RoleTarget
introTarget earlier expr = case expr of
  Lam body
    | hasPiIntroBody body -> Just (Ambient TargetPi)
  Var i -> formationTargetFromVar earlier i
  App _ _
    | isPairIntroExpr expr -> Just (Ambient TargetSigma)
  App _ _ ->
    let (headExpr, args) = collectAppSpine expr
    in if null args || not (all isSimpleConstructorArg args)
       then Nothing
       else headTarget earlier headExpr
  _ -> Nothing

hasPiIntroBody :: MBTTExpr -> Bool
hasPiIntroBody expr = case expr of
  Pi _ _ -> True
  App a b -> hasPiIntroBody a || hasPiIntroBody b
  Sigma a b -> hasPiIntroBody a || hasPiIntroBody b
  Id a x y -> hasPiIntroBody a || hasPiIntroBody x || hasPiIntroBody y
  Lam body -> hasPiIntroBody body
  Refl a -> hasPiIntroBody a
  Susp a -> hasPiIntroBody a
  Trunc a -> hasPiIntroBody a
  Flat a -> hasPiIntroBody a
  Sharp a -> hasPiIntroBody a
  Disc a -> hasPiIntroBody a
  Shape a -> hasPiIntroBody a
  Next a -> hasPiIntroBody a
  Eventually a -> hasPiIntroBody a
  _ -> False

isPairIntroExpr :: MBTTExpr -> Bool
isPairIntroExpr expr = case expr of
  App (App headExpr leftArg) rightArg ->
    isPairHead headExpr && isSimpleConstructorArg leftArg && isSimpleConstructorArg rightArg
  _ -> False

headTarget :: [TeleEntry] -> MBTTExpr -> Maybe RoleTarget
headTarget earlier expr = case expr of
  Var i -> formationTargetFromVar earlier i
  Pi _ _ -> Just (Ambient TargetPi)
  Sigma _ _ -> Just (Ambient TargetSigma)
  Id _ _ _ -> Just (Ambient TargetId)
  Susp _ -> Just (Ambient TargetSusp)
  Trunc _ -> Just (Ambient TargetTrunc)
  Flat _ -> Just (Ambient TargetFlat)
  Sharp _ -> Just (Ambient TargetSharp)
  Disc _ -> Just (Ambient TargetDisc)
  Shape _ -> Just (Ambient TargetShape)
  Next _ -> Just (Ambient TargetNext)
  Eventually _ -> Just (Ambient TargetEventually)
  _ -> Nothing

isPairHead :: MBTTExpr -> Bool
isPairHead expr = case expr of
  Var _ -> True
  Lib _ -> True
  _ -> False

formationTargetFromVar :: [TeleEntry] -> Int -> Maybe RoleTarget
formationTargetFromVar earlier ref =
  case roleAtClauseRef earlier ref of
    Just (RoleFormation target) -> Just target
    _ -> Nothing

roleAtClauseRef :: [TeleEntry] -> Int -> Maybe ClauseRole
roleAtClauseRef earlier ref
  | ref < 1 || ref > length earlier = Nothing
  | otherwise = go [] 0 earlier
  where
    targetIx = length earlier - ref

    go _ _ [] = Nothing
    go prefix ix (teleEntry:rest) =
      let role = classifyClauseRole prefix teleEntry
      in if ix == targetIx
         then Just role
         else go (prefix ++ [teleEntry]) (ix + 1) rest

latestPendingElimTarget :: [TeleEntry] -> Maybe RoleTarget
latestPendingElimTarget earlier =
  firstPendingElim (reverse (psOpenObligations (reifyPrefixState (Telescope earlier))))
  where
    firstPendingElim [] = Nothing
    firstPendingElim (NeedElimOrPathFor target:_) = Just target
    firstPendingElim (_:rest) = firstPendingElim rest

latestIntroTarget :: [TeleEntry] -> Maybe RoleTarget
latestIntroTarget = latestTargetMatching isIntroLikeRole

latestFormationTarget :: [TeleEntry] -> Maybe RoleTarget
latestFormationTarget = latestTargetMatching isFormationRole

latestTargetMatching :: (ClauseRole -> Bool) -> [TeleEntry] -> Maybe RoleTarget
latestTargetMatching predicate earlier =
  case latestRoleMatching predicate earlier of
    Just role -> roleTarget role
    Nothing -> Nothing

latestRoleMatching :: (ClauseRole -> Bool) -> [TeleEntry] -> Maybe ClauseRole
latestRoleMatching predicate earlier = go [] Nothing earlier
  where
    go _ best [] = best
    go prefix best (teleEntry:rest) =
      let role = classifyClauseRole prefix teleEntry
          best' = if predicate role then Just role else best
      in go (prefix ++ [teleEntry]) best' rest

roleTarget :: ClauseRole -> Maybe RoleTarget
roleTarget role = case role of
  RoleFormation target -> Just target
  RoleIntro target -> Just target
  RoleElim target -> Just target
  RolePath target -> Just target
  RoleMacro -> Nothing

isFormationRole :: ClauseRole -> Bool
isFormationRole role = case role of
  RoleFormation _ -> True
  _ -> False

isIntroLikeRole :: ClauseRole -> Bool
isIntroLikeRole role = case role of
  RoleIntro _ -> True
  RolePath _ -> True
  _ -> False

canStartAmbientLifecycle :: RoleTarget -> Bool
canStartAmbientLifecycle target = case target of
  Ambient TargetUniverse -> False
  Ambient _ -> True
  Local _ -> False

appendObligation :: PendingObligation -> [PendingObligation] -> [PendingObligation]
appendObligation obligation obligations
  | obligation `elem` obligations = obligations
  | otherwise = obligations ++ [obligation]

orElse :: Maybe a -> Maybe a -> Maybe a
orElse left right =
  case left of
    Just _ -> left
    Nothing -> right

isSimpleUniverseCarrierExpr :: MBTTExpr -> Bool
isSimpleUniverseCarrierExpr expr = case expr of
  Var _ -> True
  Lib _ -> True
  _ -> False

collectAppSpine :: MBTTExpr -> (MBTTExpr, [MBTTExpr])
collectAppSpine = go []
  where
    go args expr = case expr of
      App f x -> go (x:args) f
      _ -> (expr, args)

isSimpleConstructorArg :: MBTTExpr -> Bool
isSimpleConstructorArg expr = case expr of
  Var _ -> True
  Lib _ -> True
  PathCon _ -> True
  Refl _ -> True
  _ -> False

uniqueRefs :: [Int] -> [Int]
uniqueRefs =
  Set.toList . Set.fromList

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

-- | Semantic relevance heuristic for frontier retention.
-- Larger means more mathematically meaningful in current library context.
goalDirectedHeuristic :: Telescope -> Library -> GoalProfile -> Int
goalDirectedHeuristic tele lib profile =
  let entries = teleEntries tele
      exprs = map teType entries
      libSize = length lib
      refs = teleLibRefs tele
      hasTypeFormation = any isTypeFormationExpr exprs
      hasFormer = any isFormerExpr exprs
      hasMapLike = any isMapExpr exprs
      hasWitnessLike = any isWitnessExpr exprs
      hasHit = any isHitExpr exprs
      hasModal = any isModalExpr exprs
      hasTemporal = any isTemporalExpr exprs
      hasVacuousLamOnly = not hasTypeFormation && not hasFormer && all isLamLike exprs
      floatingPath = hasHit && not hasTypeFormation && Set.null refs
      refsRecent = not (Set.null refs) && (Set.member libSize refs || Set.member (libSize - 1) refs)
      connectivity = if teleIsConnected tele then 2 else 0
      intentBonus = sum [2 | intent <- gpIntents profile, teleSupportsIntent tele intent]
      kappa = desugaredKappa tele
      bridgePhase = any leHasLoop lib && not (any hasTruncEntry lib)
      hasTruncBridgeExpr = any hasTruncExpr exprs
      truncBridgeRichness =
        if bridgePhase && hasTruncBridgeExpr
        then
          (if kappa >= 3 then 8 else 0)
          + (if any hasBridgeInteraction exprs then 3 else 0)
          + (if any hasCoherenceExpr exprs then 2 else 0)
          + (if hasFormer then 2 else 0)
        else 0
      truncShortcutPenalty =
        if bridgePhase && hasTruncBridgeExpr && kappa <= 2 && not (any hasBridgeInteraction exprs)
        then 4
        else 0
      base =
          (if hasTypeFormation then 6 else 0)
        + (if hasFormer then 5 else 0)
        + (if hasMapLike then 4 else 0)
        + (if hasWitnessLike && length lib <= 2 && not (any leHasDependentFunctions lib) then 4 else 0)
        + (if hasHit then 4 else 0)
        + (if hasModal then 3 else 0)
        + (if hasTemporal then 3 else 0)
        + (if refsRecent then 3 else 0)
        + connectivity
        + intentBonus
      penalties =
          (if hasVacuousLamOnly then 6 else 0)
        + (if floatingPath then 5 else 0)
        + (if teleMaxLibRef tele == 0 && libSize > 0 && not hasTypeFormation && not hasFormer then 3 else 0)
      readinessPenalty =
        let cls = classifyTelescope tele lib
            bootstrapLock = if length lib < 2 && cls /= TCFoundation then 12 else 0
        in bootstrapLock + case cls of
          TCFoundation -> 0
          TCFormer -> if any leConstructorsPositive lib then 0 else 4
          TCHIT -> if any leHasDependentFunctions lib then 0 else 6
          TCSuspension -> if any leHasLoop lib then 0 else 6
          TCMap -> if countConstructed lib >= 2 then 0 else 4
          TCModal -> if any leHasLoop lib && any leHasDependentFunctions lib then 0 else 6
          TCAxiomatic -> if any leHasModalOps lib then 0 else 6
          TCSynthesis -> if any leHasHilbert lib then 0 else 8
          TCUnknown -> 4
  in base + truncBridgeRichness - penalties - readinessPenalty - truncShortcutPenalty
  where
    isTypeFormationExpr Univ = True
    isTypeFormationExpr (App Univ _) = True
    isTypeFormationExpr _ = False

    isFormerExpr (Pi _ _) = True
    isFormerExpr (Sigma _ _) = True
    isFormerExpr _ = False

    isMapExpr (App (Lib _) _) = True
    isMapExpr (Pi (Lib _) _) = True
    isMapExpr (Pi _ (Lib _)) = True
    isMapExpr _ = False

    isWitnessExpr (App (Lib _) (Var _)) = True
    isWitnessExpr _ = False

    isHitExpr (PathCon _) = True
    isHitExpr (Susp _) = True
    isHitExpr (Trunc _) = True
    isHitExpr _ = False

    isModalExpr (Flat _) = True
    isModalExpr (Sharp _) = True
    isModalExpr (Disc _) = True
    isModalExpr (Shape _) = True
    isModalExpr _ = False

    isTemporalExpr (Next _) = True
    isTemporalExpr (Eventually _) = True
    isTemporalExpr _ = False

    isLamLike (Lam _) = True
    isLamLike _ = False

    countConstructed = length . filter leConstructorsPositive
    leConstructorsPositive e = leConstructors e > 0

    hasTruncEntry e = case leIsTruncated e of
      Just _ -> True
      Nothing -> False

    hasTruncExpr expr = case expr of
      Trunc _ -> True
      Lam a -> hasTruncExpr a
      Refl a -> hasTruncExpr a
      Susp a -> hasTruncExpr a
      Flat a -> hasTruncExpr a
      Sharp a -> hasTruncExpr a
      Disc a -> hasTruncExpr a
      Shape a -> hasTruncExpr a
      Next a -> hasTruncExpr a
      Eventually a -> hasTruncExpr a
      Pi a b -> hasTruncExpr a || hasTruncExpr b
      Sigma a b -> hasTruncExpr a || hasTruncExpr b
      App a b -> hasTruncExpr a || hasTruncExpr b
      Id a x y -> hasTruncExpr a || hasTruncExpr x || hasTruncExpr y
      _ -> False

    hasBridgeInteraction expr = case expr of
      App _ _ -> True
      Pi _ _ -> True
      Sigma _ _ -> True
      Id _ _ _ -> True
      Lam a -> hasBridgeInteraction a
      Refl a -> hasBridgeInteraction a
      Susp a -> hasBridgeInteraction a
      Trunc a -> hasBridgeInteraction a
      Flat a -> hasBridgeInteraction a
      Sharp a -> hasBridgeInteraction a
      Disc a -> hasBridgeInteraction a
      Shape a -> hasBridgeInteraction a
      Next a -> hasBridgeInteraction a
      Eventually a -> hasBridgeInteraction a
      _ -> False

    hasCoherenceExpr expr = case expr of
      PathCon _ -> True
      Id _ _ _ -> True
      Refl _ -> True
      Lam a -> hasCoherenceExpr a
      App a b -> hasCoherenceExpr a || hasCoherenceExpr b
      Pi a b -> hasCoherenceExpr a || hasCoherenceExpr b
      Sigma a b -> hasCoherenceExpr a || hasCoherenceExpr b
      Susp a -> hasCoherenceExpr a
      Trunc a -> hasCoherenceExpr a
      Flat a -> hasCoherenceExpr a
      Sharp a -> hasCoherenceExpr a
      Disc a -> hasCoherenceExpr a
      Shape a -> hasCoherenceExpr a
      Next a -> hasCoherenceExpr a
      Eventually a -> hasCoherenceExpr a
      _ -> False

-- | Small macro-reuse lane: compose existing library structures directly.
-- Keeps the lane tiny and deterministic, providing intelligent reuse seeds.
macroReuseTelescopes :: Library -> GoalProfile -> [Telescope]
macroReuseTelescopes lib profile
  | null lib = []
  | otherwise =
      let n = length lib
          refs = take 2 [n, n - 1]
          pairRefs = [(i, j) | i <- refs, j <- refs, i >= 1, j >= 1]
          oneEntry = concat
            [ [ Telescope [TeleEntry "c1" (Susp (Lib i))]
              , Telescope [TeleEntry "c1" (Trunc (Lib i))]
              , Telescope [TeleEntry "c1" (Pi (Lib i) (Lib i))]
              , Telescope [TeleEntry "c1" (Sigma (Lib i) (Lib i))]
              , Telescope [TeleEntry "c1" (App (Lib i) (Var 1))]
              ]
            | i <- refs, i >= 1
            ]
          twoEntry = concat
            [ [ Telescope [TeleEntry "c1" (Lib i), TeleEntry "c2" (Susp (Var 1))]
              , Telescope [TeleEntry "c1" (Lib i), TeleEntry "c2" (Trunc (Var 1))]
              , Telescope [TeleEntry "c1" (Lib i), TeleEntry "c2" (Pi (Var 1) (Var 1))]
              ]
            | i <- refs, i >= 1
            ]
          bridge = [ Telescope [TeleEntry "c1" (Pi (Lib i) (Lib j))]
                   | (i, j) <- pairRefs
                   ]
          allMacros = oneEntry ++ twoEntry ++ bridge
          intents = gpIntents profile
          keep tele =
            null intents || any (teleSupportsIntent tele) intents
      in filter keep allMacros

teleSupportsIntent :: Telescope -> GoalIntent -> Bool
teleSupportsIntent (Telescope entries) intent = any (exprSupportsIntent . teType) entries
  where
    exprSupportsIntent expr = case intent of
      NeedBootstrap -> case expr of
        Lib _ -> True
        Pi _ _ -> True
        App (Lib _) _ -> True
        _ -> False
      NeedFormer -> case expr of
        Pi _ _ -> True
        Sigma _ _ -> True
        _ -> False
      NeedHIT -> case expr of
        Susp _ -> True
        Trunc _ -> True
        _ -> False
      NeedModal -> case expr of
        Flat _ -> True
        Sharp _ -> True
        Disc _ -> True
        Shape _ -> True
        _ -> False
      NeedDifferential -> case expr of
        Pi _ _ -> True
        Sigma _ _ -> True
        Id _ _ _ -> True
        App _ _ -> True
        PathCon _ -> True
        _ -> False
      NeedCurvature -> case expr of
        PathCon d -> d >= 2
        Id _ _ _ -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        App _ _ -> True
        _ -> False
      NeedMetric -> case expr of
        Sigma (Pi _ _) (Pi _ _) -> True
        Pi _ _ -> True
        Sigma _ _ -> True
        Id _ _ _ -> True
        App _ _ -> True
        Lam _ -> True
        _ -> False
      NeedHilbert -> case expr of
        Sigma (Pi _ _) _ -> True
        Pi _ (Sigma _ _) -> True
        Pi (Lam _) (Sigma _ _) -> True
        Pi (Lib _) _ -> True
        Lam (Pi _ Univ) -> True
        Id _ _ _ -> True
        App _ _ -> True
        _ -> False
      NeedTemporal -> case expr of
        Next _ -> True
        Eventually _ -> True
        _ -> False
      NeedBridge -> case expr of
        Pi _ _ -> True
        App _ _ -> True
        Susp _ -> True
        Trunc _ -> True
        _ -> False

-- | Check if a telescope is a valid candidate.
isValidCandidate :: Library -> Int -> Telescope -> Bool
isValidCandidate lib libSize tele =
  -- Must pass well-formedness check
  checkTelescope lib tele == CheckOK
  -- Must be structurally connected (no axiom packing)
  && teleIsConnected tele
  -- Must reference the library window OR be a pure type former
  && (teleReferencesWindow tele libSize || teleMaxLibRef tele == 0)
  -- Must not be trivially derivable
  && not (isTriviallyDerivable tele lib)
