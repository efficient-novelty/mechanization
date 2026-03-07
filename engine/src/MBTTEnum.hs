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
    -- * Candidate type
  , MBTTCandidate(..)
    -- * Configuration
  , EnumConfig(..)
  , defaultEnumConfig
  ) where

import Kolmogorov (MBTTExpr(..), eliasGammaLength)
import Telescope (Telescope(..), TeleEntry(..), teleIsConnected,
                  teleReferencesWindow, teleMaxLibRef, teleBitCost, teleLibRefs,
                  classifyTelescope, TelescopeClass(..),
                  desugaredKappa, isTriviallyDerivable)
import MBTTCanonical (canonicalKeySpec)
import TelescopeGen (GoalProfile(..), GoalIntent(..), deriveGoalProfile)
import TelescopeCheck (checkTelescope, CheckResult(..))
import Types (Library, LibraryEntry(..))
import Data.List (foldl', sortOn)
import Data.Ord (Down(..))
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
  , ecGoalProfile   :: !(Maybe GoalProfile) -- ^ Optional goal profile steering
  , ecEnableMacros  :: !Bool   -- ^ Enable macro-reuse candidate lane
  } deriving (Show)

-- | Default enumeration config. Budget is per-entry, not total.
defaultEnumConfig :: EnumConfig
defaultEnumConfig = EnumConfig
  { ecMaxBitBudget  = 40
  , ecMaxEntries    = 8
  , ecMaxASTDepth   = 5
  , ecMaxCandidates = 5000
  , ecGoalProfile   = Nothing
  , ecEnableMacros  = True
  }

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
    hasModal   = any leHasModalOps lib
    hasTemporal = any leHasHilbert lib

    -- Terminal expressions
    terminals = concat
      [ [Univ | budget >= 4]
      , [Var i | i <- [1..ctxDepth + ambientVars], budget >= 3 + eliasGammaLength i]
      , [Lib i | i <- [1..libSize], budget >= 3 + eliasGammaLength i]
      , [PathCon d | d <- [1..3], budget >= 6 + eliasGammaLength d
                   , goalAny [NeedHIT, NeedBootstrap]]
      ]

    -- Compound expressions with budget splits
    compounds = concat
      [ if goalAny [NeedBootstrap, NeedFormer, NeedBridge, NeedDifferential]
          then enumBinaryNoExt App 2 else []
      , if goalAny [NeedFormer, NeedBridge, NeedDifferential]
          then enumUnary Lam 2 True else []
      , enumUnary Refl 5 False
      , if goalAny [NeedHIT] then enumUnary Susp 5 False else []
      , if goalAny [NeedHIT] then enumUnary Trunc 6 False else []
      , if goalAny [NeedFormer, NeedBridge, NeedDifferential] then enumBinary Pi 3 True else []
      , if goalAny [NeedFormer, NeedBridge, NeedDifferential] then enumBinary Sigma 4 True else []
      , if hasModal then concat
          [ if goalAny [NeedModal, NeedBridge, NeedDifferential] then enumUnary Flat 7 False else []
          , if goalAny [NeedModal, NeedBridge, NeedDifferential] then enumUnary Sharp 7 False else []
          , if goalAny [NeedModal, NeedBridge] then enumUnary Disc 7 False else []
          , if goalAny [NeedModal, NeedBridge] then enumUnary Shape 8 False else []
          ] else []
      , if hasTemporal then concat
          [ if goalAny [NeedTemporal] then enumUnary Next 9 False else []
          , if goalAny [NeedTemporal] then enumUnary Eventually 9 False else []
          ] else []
      , if goalAny [NeedHIT, NeedFormer] then enumTernary Id 5 else []
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
  let libSize = length lib
      maxK = ecMaxEntries cfg
      budget = ecMaxBitBudget cfg
      depth = ecMaxASTDepth cfg
      maxCand = max 1 (ecMaxCandidates cfg)
      macroQuota = if ecEnableMacros cfg then max 2 (min 3 (maxCand `div` 10 + 1)) else 0
      goalProfile = maybe (deriveGoalProfile lib) id (ecGoalProfile cfg)
      exprCapPerCtx = max 120 (min 800 (maxCand * 6))
      exprsByCtx = [ take exprCapPerCtx
                       (enumerateExprsWithGoal (Just goalProfile) libSize ctx budget depth lib)
                    | ctx <- [0 .. max 0 (maxK - 1)]
                    ]
      macroSeed =
        if ecEnableMacros cfg
        then [ MBTTCandidate tele (teleBitCost tele) (desugaredKappa tele) (teleNodeCount tele)
                                (candidateHeuristic tele lib goalProfile)
             | tele <- macroReuseTelescopes lib goalProfile
             , isValidCandidate lib libSize tele
             ]
        else []
      macroReserved = take macroQuota (sortOn candidateScore macroSeed)

      scanLengths :: Int -> [MBTTCandidate] -> [MBTTCandidate]
      scanLengths !k !best
        | k > maxK = best
        | otherwise = scanLengths (k + 1) (scanLength k [] best)

      scanLength :: Int -> [TeleEntry] -> [MBTTCandidate] -> [MBTTCandidate]
      scanLength 0 !acc !best =
        let tele = Telescope (reverse acc)
            heuristic = candidateHeuristic tele lib goalProfile
        in if isValidCandidate lib libSize tele && heuristic > 0
           then insertBestCandidate maxCand
                  (MBTTCandidate tele (teleBitCost tele) (desugaredKappa tele) (teleNodeCount tele) heuristic)
                  best
           else best
      scanLength !n !acc !best =
        let ctxDepth = length acc
            entryName = "c" ++ show (ctxDepth + 1)
            exprs = if ctxDepth < length exprsByCtx then exprsByCtx !! ctxDepth else []
        in foldl'
             (\bestAcc expr ->
               let entry = TeleEntry entryName expr
               in scanLength (n - 1) (entry : acc) bestAcc)
             best
             exprs
      scanLimit = max 1 (maxCand - length macroReserved)
      scanned = scanLengths 1 []
  in mergeWithMacroQuota maxCand macroReserved (take scanLimit scanned)

-- | Insert a candidate into an ascending-score frontier, trimming to max size.
insertBestCandidate :: Int -> MBTTCandidate -> [MBTTCandidate] -> [MBTTCandidate]
insertBestCandidate limit cand = go (max 0 limit)
  where
    go 0 _ = []
    go _ [] = [cand]
    go !n ys@(y:ys')
      | candidateScore cand <= candidateScore y = cand : take (n - 1) ys
      | otherwise = y : go (n - 1) ys'

candidateScore :: MBTTCandidate -> (Down Int, Int, Int, Int)
candidateScore c = (Down (mcHeuristic c), mcBitKappa c, mcClauseCount c, mcNodeCount c)

-- | Preserve a small macro lane while keeping global cap bounded.
mergeWithMacroQuota :: Int -> [MBTTCandidate] -> [MBTTCandidate] -> [MBTTCandidate]
mergeWithMacroQuota maxCand macros scanned =
  let macroKeys = Set.fromList (map candKey macros)
      scannedUnique = [c | c <- scanned, not (Set.member (candKey c) macroKeys)]
  in take maxCand (macros ++ scannedUnique)
  where
    candKey c = canonicalKeySpec (map teType (teleEntries (mcTelescope c)))

-- | Semantic relevance heuristic for frontier retention.
-- Larger means more mathematically meaningful in current library context.
candidateHeuristic :: Telescope -> Library -> GoalProfile -> Int
candidateHeuristic tele lib profile =
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
