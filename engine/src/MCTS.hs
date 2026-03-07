{-# LANGUAGE BangPatterns #-}

-- | Monte Carlo Tree Search for Mathematical Synthesis
--
-- Treats mathematical structure discovery as a Reinforcement Learning problem:
--   State:  A partially constructed telescope (list of TeleEntry)
--   Action: Choosing the top-level MBTTExpr for the next entry
--   Reward: Efficiency ρ = ν/κ on completed telescopes, with structural bonuses
--
-- Uses UCT (Upper Confidence Bounds applied to Trees) to balance
-- exploration and exploitation. Each tree node represents a partial
-- telescope; children correspond to different choices for the next entry.
-- The sub-expression structure within each entry uses random rollout.
--
-- Full UCT cycle per iteration:
--   1. SELECTION:    Walk tree from root following UCT formula
--   2. EXPANSION:    At leaf, create children for available actions
--   3. ROLLOUT:      Random-complete telescope from expanded node, evaluate
--   4. BACKPROPAGATION: Update reward/visits up the selection path

module MCTS
  ( -- * Core types
    MCTSConfig(..)
  , MCTSResult(..)
  , MCTSStats(..)
    -- * Search
  , mctsSearch
  , mctsSearchStep
    -- * Configuration
  , defaultMCTSConfig
  ) where

import Kolmogorov (MBTTExpr(..))
import Telescope (Telescope(..), TeleEntry(..), teleIsConnected, teleReferencesWindow)
import TelescopeGen (Action(..), GoalProfile(..), GoalIntent(..), deriveGoalProfile, validActionsWithProfile,
                     Hole(..), HoleGoal(..), actionPriority)
import TelescopeEval (EvalMode(..), evaluateTelescope)
import TelescopeCheck (checkTelescope, CheckResult(..))
import ProofState (deriveSearchState, actionGoalGain, isSafeAction)
import Types (Library)

import qualified Data.Map.Strict as Map
import Data.List (sortOn)
import Data.Ord (Down(..))
import System.Random (StdGen, mkStdGen, randomR)
import Data.IORef

-- ============================================
-- Configuration
-- ============================================

data MCTSConfig = MCTSConfig
  { mctsIterations   :: !Int     -- ^ Number of MCTS iterations
  , mctsMaxKappa     :: !Int     -- ^ Maximum telescope length
  , mctsMaxDepth     :: !Int     -- ^ Maximum AST depth per entry
  , mctsExploreC     :: !Double  -- ^ UCT exploration constant (√2 typical)
  , mctsNuDepth      :: !Int     -- ^ Depth for ν computation (2 = standard)
  , mctsTopK         :: !Int     -- ^ Return top-K telescopes
  , mctsSeed         :: !Int     -- ^ Random seed
  , mctsVerbose      :: !Bool    -- ^ Print progress
  , mctsWidenC       :: !Double  -- ^ Progressive widening coefficient (default 1.0)
  , mctsWidenAlpha   :: !Double  -- ^ Progressive widening exponent (default 0.5)
  , mctsGoalProfile  :: !(Maybe GoalProfile) -- ^ Optional semantic intent profile
  } deriving (Show)

defaultMCTSConfig :: MCTSConfig
defaultMCTSConfig = MCTSConfig
  { mctsIterations   = 10000
  , mctsMaxKappa     = 5
  , mctsMaxDepth     = 2
  , mctsExploreC     = 1.414
  , mctsNuDepth      = 2
  , mctsTopK         = 10
  , mctsSeed         = 42
  , mctsVerbose      = True
  , mctsWidenC       = 1.0
  , mctsWidenAlpha   = 0.5
  , mctsGoalProfile  = Nothing
  }

activeGoalProfile :: MCTSConfig -> Library -> GoalProfile
activeGoalProfile cfg lib = case mctsGoalProfile cfg of
  Just gp -> gp
  Nothing -> deriveGoalProfile lib

-- ============================================
-- MCTS Tree
-- ============================================

-- | A node in the MCTS tree represents a partial telescope.
-- Children are indexed by the top-level Action chosen for the next entry.
data MCTSNode = MCTSNode
  { nodeEntries  :: ![TeleEntry]           -- ^ Telescope entries so far
  , nodeVisits   :: !Int                   -- ^ Number of visits
  , nodeReward   :: !Double                -- ^ Total accumulated reward
  , nodeChildren :: !(Map.Map Action MCTSNode)  -- ^ Child nodes by action
  , nodeExpanded :: !Bool                  -- ^ Have we expanded this node?
  } deriving (Show)

freshNode :: [TeleEntry] -> MCTSNode
freshNode entries = MCTSNode entries 0 0.0 Map.empty False

-- ============================================
-- Result Types
-- ============================================

data MCTSResult = MCTSResult
  { mrTelescopes :: ![(Telescope, Double)]  -- ^ Top telescopes with rewards
  , mrStats      :: !MCTSStats              -- ^ Search statistics
  } deriving (Show)

data MCTSStats = MCTSStats
  { msIterations       :: !Int
  , msNodesExpanded    :: !Int
  , msRolloutsRun      :: !Int
  , msBestReward       :: !Double
  , msAvgReward        :: !Double
  , msValidRollouts    :: !Int     -- ^ Rollouts that passed TelescopeCheck
  , msRejectedRollouts :: !Int     -- ^ Rollouts rejected by TelescopeCheck
  } deriving (Show)

-- ============================================
-- UCT Selection
-- ============================================

-- | UCT formula: Q/N + C * sqrt(ln(N_parent) / N)
uctScore :: Double -> MCTSNode -> Int -> Double
uctScore exploreC node parentVisits
  | nodeVisits node == 0 = 1e9  -- unvisited nodes have infinite priority
  | otherwise =
    let exploitation = nodeReward node / fromIntegral (nodeVisits node)
        exploration  = exploreC * sqrt (log (fromIntegral parentVisits)
                                        / fromIntegral (nodeVisits node))
    in exploitation + exploration

-- ============================================
-- Core MCTS Loop
-- ============================================

mctsSearch :: EvalMode -> MCTSConfig -> Library -> IO MCTSResult
mctsSearch evalMode cfg lib = do
  rootRef  <- newIORef (freshNode [])
  genRef   <- newIORef (mkStdGen (mctsSeed cfg))
  statsRef <- newIORef (MCTSStats 0 0 0 0.0 0.0 0 0)
  bestRef  <- newIORef ([] :: [(Telescope, Double)])

  let loop iter
        | iter >= mctsIterations cfg = return ()
        | otherwise = do
          root <- readIORef rootRef
          gen <- readIORef genRef

          let (root', gen', reward, maybeTele, valid) = mctsIteration evalMode cfg lib root gen
          writeIORef rootRef root'
          writeIORef genRef gen'

          -- Track statistics
          stats <- readIORef statsRef
          let totalReward = msAvgReward stats * fromIntegral (msRolloutsRun stats) + reward
              newRollouts = msRolloutsRun stats + 1
          writeIORef statsRef stats
            { msIterations = iter + 1
            , msRolloutsRun = newRollouts
            , msBestReward = max (msBestReward stats) reward
            , msAvgReward = totalReward / fromIntegral newRollouts
            , msValidRollouts = msValidRollouts stats + if valid then 1 else 0
            , msRejectedRollouts = msRejectedRollouts stats + if valid then 0 else 1
            }

          -- Track best telescopes
          case maybeTele of
            Just tele | reward > 0 -> do
              best <- readIORef bestRef
              let best' = insertBest (mctsTopK cfg) (tele, reward) best
              writeIORef bestRef best'
            _ -> return ()

          loop (iter + 1)

  loop 0

  stats <- readIORef statsRef
  best <- readIORef bestRef
  return $ MCTSResult (sortOn (Down . snd) best) stats

insertBest :: Int -> (Telescope, Double) -> [(Telescope, Double)] -> [(Telescope, Double)]
insertBest k new xs = take k $ sortOn (Down . snd) (new : xs)

-- ============================================
-- Full UCT Iteration
-- ============================================

-- | One MCTS iteration with proper UCT: select → expand → rollout → backprop.
--
-- Tree structure: each node at depth d has committed to d telescope entries.
-- Children are indexed by the Action for the (d+1)th entry's top-level constructor.
-- Sub-expression filling within entries uses the random rollout policy.
mctsIteration :: EvalMode -> MCTSConfig -> Library -> MCTSNode -> StdGen
              -> (MCTSNode, StdGen, Double, Maybe Telescope, Bool)
mctsIteration evalMode cfg lib root gen0 =
  let maxK = mctsMaxKappa cfg
      (pathToLeaf, leaf) = selectPath cfg lib root
      currentDepth = length (nodeEntries leaf)
      (leafExpanded, gen1)
        | currentDepth >= maxK = (leaf, gen0)
        | otherwise = progressiveExpand cfg lib leaf gen0
      (rolloutPath, rolloutNode, gen2)
        | currentDepth >= maxK || Map.null (nodeChildren leafExpanded) =
            (pathToLeaf, leafExpanded, gen1)
        | otherwise =
            let (act, child, g) = selectChildForRollout cfg leafExpanded gen1
            in (pathToLeaf ++ [act], child, g)
      (reward, maybeTele, valid, gen3) = rolloutFromNode evalMode cfg lib rolloutNode gen2
      rootPrepared = replaceNodeAtPath root pathToLeaf leafExpanded
      root' = backpropagateAlongPath rootPrepared rolloutPath reward

  in (root', gen3, reward, maybeTele, valid)
-- | Walk down the tree following UCT until we reach a leaf or a node that
-- needs widening (more children allowed than currently exist).
-- Returns (path, leaf_node, gen).
-- Path is a list of (Action, child_node) pairs from root to leaf.
selectPath :: MCTSConfig -> Library -> MCTSNode -> ([Action], MCTSNode)
selectPath cfg lib = go []
  where
    go path node
      | length (nodeEntries node) >= mctsMaxKappa cfg = (reverse path, node)
      | Map.null (nodeChildren node) = (reverse path, node)
      | needsWidening cfg lib node = (reverse path, node)
      | otherwise =
        let parentN = max 1 (nodeVisits node)
            children = Map.toList (nodeChildren node)
            scored = [(a, child, uctScore (mctsExploreC cfg) child parentN)
                     | (a, child) <- children]
            (bestA, bestChild, _) = maximumByScore scored
        in go (bestA : path) bestChild

    maximumByScore [] = error "maximumByScore: empty list"
    maximumByScore xs = foldr1 (\a@(_,_,s1) b@(_,_,s2) -> if s1 >= s2 then a else b) xs
-- ============================================
-- Progressive Widening
-- ============================================

-- | Maximum children allowed at a node with N visits.
-- k(N) = C_pw * N^alpha, where C_pw and alpha are config parameters.
-- At low visits, only top-priority actions get children; as visits grow,
-- the node widens to include more actions.
wideningLimit :: Double -> Double -> Int -> Int
wideningLimit c alpha visits =
  max 1 (floor (c * fromIntegral (max 1 visits) ** alpha))

-- | Check if a node should be widened (more children added before descending).
-- Returns True if the current child count is below the widening limit AND
-- there are unexpanded actions available.
needsWidening :: MCTSConfig -> Library -> MCTSNode -> Bool
needsWidening cfg lib node =
  let visits    = max 1 (nodeVisits node)
      limit     = wideningLimit (mctsWidenC cfg) (mctsWidenAlpha cfg) visits
      current   = Map.size (nodeChildren node)
      entries   = nodeEntries node
      hole      = Hole entries AnyHole 0 100
      available = length (validActionsWithProfile (activeGoalProfile cfg lib) hole lib)
  in current < limit && current < available

-- | Progressive expansion: add children up to the widening limit.
-- Actions are added in priority order (validActions sorts by descending priority),
-- so high-value actions (recent library refs, Pi, Sigma) get explored first.
progressiveExpand :: MCTSConfig -> Library -> MCTSNode -> StdGen -> (MCTSNode, StdGen)
progressiveExpand cfg lib node gen =
  let visits     = max 1 (nodeVisits node + 1)  -- +1 for the upcoming visit
      limit      = wideningLimit (mctsWidenC cfg) (mctsWidenAlpha cfg) visits
      current    = Map.size (nodeChildren node)
  in if current >= limit
     then (node, gen)
     else
       let entries    = nodeEntries node
           hole       = Hole entries AnyHole 0 100
           goalProfile = activeGoalProfile cfg lib
           state = deriveSearchState lib goalProfile entries
           allActions = validActionsWithProfile goalProfile hole lib
           rankAction a =
             let safeBonus = if isSafeAction a then 1 :: Int else 0
                 goalGain = actionGoalGain state a
                 pri = actionPriority (length lib) a
             in (Down safeBonus, Down goalGain, Down pri)
           rankedActions = sortOn rankAction allActions
           unexpanded = [a | a <- rankedActions, not (Map.member a (nodeChildren node))]
           toAdd      = take (max 1 (limit - current)) unexpanded
           (added, gen') = buildChildren entries toAdd gen
           newChildren = Map.fromList added
           allChildren = Map.union (nodeChildren node) newChildren
           fullyDone   = Map.size allChildren >= length allActions
       in (node { nodeChildren = allChildren, nodeExpanded = fullyDone }, gen')
  where
    buildChildren _ [] g = ([], g)
    buildChildren entries (a:as) g =
      let (entry, g1) = instantiateActionEntry cfg lib entries a g
          child = freshNode (entries ++ [entry])
          (rest, g2) = buildChildren entries as g1
      in ((a, child) : rest, g2)

selectChildForRollout :: MCTSConfig -> MCTSNode -> StdGen -> (Action, MCTSNode, StdGen)
selectChildForRollout cfg node gen =
  let children = Map.toList (nodeChildren node)
      unvisited = [(a, c) | (a, c) <- children, nodeVisits c == 0]
  in case unvisited of
       [] ->
         let parentN = max 1 (nodeVisits node)
             scored = [(a, c, uctScore (mctsExploreC cfg) c parentN) | (a, c) <- children]
             (bestA, bestChild, _) = foldr1
               (\x@(_,_,sx) y@(_,_,sy) -> if sx >= sy then x else y)
               scored
         in (bestA, bestChild, gen)
       _ ->
         let (idx, gen') = randomR (0, length unvisited - 1) gen
             (a, c) = unvisited !! idx
         in (a, c, gen')

instantiateActionEntry :: MCTSConfig -> Library -> [TeleEntry] -> Action -> StdGen -> (TeleEntry, StdGen)
instantiateActionEntry cfg lib ctx act gen =
  let name = "c" ++ show (length ctx + 1)
      (expr, gen') = randomExprFromAction cfg act lib ctx gen (max 1 (mctsMaxDepth cfg))
  in (TeleEntry name expr, gen')
-- | Rollout: complete the telescope from the current node and evaluate.
-- Ill-formed telescopes (detected by TelescopeCheck) receive reward 0
-- and are not added to the candidate pool. UCT naturally depresses
-- action paths that repeatedly produce invalid rollouts.
rolloutFromNode :: EvalMode -> MCTSConfig -> Library -> MCTSNode -> StdGen
               -> (Double, Maybe Telescope, Bool, StdGen)
rolloutFromNode evalMode cfg lib node gen0 =
  let currentEntries = nodeEntries node
      currentK = length currentEntries
      maxK = mctsMaxKappa cfg
      remainingK = maxK - currentK

      -- If already at max entries, evaluate what we have
      -- Otherwise, random-complete the remaining entries
      (finalEntries, gen1)
        | remainingK <= 0 && currentK > 0 = (currentEntries, gen0)
        | otherwise =
          let (extraEntries, g) = generateRandomEntries cfg lib remainingK currentEntries gen0
          in (currentEntries ++ extraEntries, g)

      tele = Telescope finalEntries

  in case checkTelescope lib tele of
    CheckFail _ ->
      -- Invalid telescope: reward 0, no candidate, mark as rejected
      (0.0, Nothing, False, gen1)
    CheckOK ->
      -- Valid telescope: evaluate normally
      let name = "mcts_candidate"
          (nu, _kappa, rho) = evaluateTelescope evalMode tele lib (mctsNuDepth cfg) name
          connected = teleIsConnected tele
          refsWindow = teleReferencesWindow tele (length lib)
          connectBonus = if connected then 1.0 else 0.5 :: Double
          windowBonus = if refsWindow then 1.1 else 1.0 :: Double
          reward = rho * connectBonus * windowBonus
      in (reward, if nu > 0 then Just tele else Nothing, True, gen1)

-- | Replace a subtree at an action path.
replaceNodeAtPath :: MCTSNode -> [Action] -> MCTSNode -> MCTSNode
replaceNodeAtPath _ [] replacement = replacement
replaceNodeAtPath node (a:as) replacement =
  case Map.lookup a (nodeChildren node) of
    Nothing -> node
    Just child ->
      let child' = replaceNodeAtPath child as replacement
      in node { nodeChildren = Map.insert a child' (nodeChildren node) }

-- | Backpropagate reward along the full action path.
backpropagateAlongPath :: MCTSNode -> [Action] -> Double -> MCTSNode
backpropagateAlongPath node [] reward =
  node { nodeVisits = nodeVisits node + 1
       , nodeReward = nodeReward node + reward
       }
backpropagateAlongPath node (a:as) reward =
  let fallback = freshNode (nodeEntries node)
      child = Map.findWithDefault fallback a (nodeChildren node)
      child' = backpropagateAlongPath child as reward
  in node { nodeVisits = nodeVisits node + 1
          , nodeReward = nodeReward node + reward
          , nodeChildren = Map.insert a child' (nodeChildren node)
          }
-- ============================================
-- Random Telescope Generation (Rollout Policy)
-- ============================================

-- | Generate a random well-typed telescope (used by rollout policy).
_randomTelescope :: MCTSConfig -> Library -> StdGen -> (Telescope, StdGen)
_randomTelescope cfg lib gen0 =
  let maxK = mctsMaxKappa cfg
      libSize = length lib
      minK = if libSize <= 1 then 1
             else if libSize <= 3 then 2
             else 2
      (kappaIdx, gen1) = randomR (0 :: Int, max 0 (maxK - minK)) gen0
      kappa = minK + kappaIdx
      (entries, gen2) = generateRandomEntries cfg lib kappa [] gen1
      tele = Telescope entries
  in (tele, gen2)

-- | Generate N fresh entries extending an existing context.
-- Returns only newly generated entries (in forward order).
generateRandomEntries :: MCTSConfig -> Library -> Int -> [TeleEntry] -> StdGen -> ([TeleEntry], StdGen)
generateRandomEntries _   _   0 _ctx gen = ([], gen)
generateRandomEntries cfg lib n ctx gen =
  let (entry, gen1) = randomEntry cfg lib ctx gen
      ctx' = ctx ++ [entry]
      (rest, gen2) = generateRandomEntries cfg lib (n - 1) ctx' gen1
  in (entry : rest, gen2)
-- | Generate a single random telescope entry.
randomEntry :: MCTSConfig -> Library -> [TeleEntry] -> StdGen -> (TeleEntry, StdGen)
randomEntry cfg lib ctx gen =
  let hole = Hole ctx AnyHole 0 100
      goalProfile = activeGoalProfile cfg lib
      state = deriveSearchState lib goalProfile ctx
      actions = validActionsWithProfile goalProfile hole lib
      name = "c" ++ show (length ctx + 1)
      macroExprs = macroReuseExprs goalProfile lib ctx
      (macroTicket, gen0) = randomR (0 :: Int, 99) gen
  in if null actions
     then (TeleEntry name Univ, gen)
     else if not (null macroExprs) && macroTicket < 35
          then let (idx, gen') = randomR (0, length macroExprs - 1) gen0
               in (TeleEntry name (macroExprs !! idx), gen')
     else let libSize = length lib
              weighted = [ (a, w a) | a <- actions ]
              w a =
                let base = fromIntegral (max 1 (actionPriority libSize a)) :: Double
                    gain = fromIntegral (1 + actionGoalGain state a) :: Double
                    safety = if isSafeAction a then 1.15 else 1.0 :: Double
                in base * gain * safety
              (act, gen') = weightedChoice weighted gen0
              expr = randomExprFromAction cfg act lib ctx gen' 3
          in (TeleEntry name (fst expr), snd expr)

-- | Macro-reuse expression lane for rollout policy.
-- Reuses recent library/context structures instead of constructing from scratch.
macroReuseExprs :: GoalProfile -> Library -> [TeleEntry] -> [MBTTExpr]
macroReuseExprs profile lib ctx =
  let recentLib = [length lib, length lib - 1]
      libAtoms = [Lib i | i <- recentLib, i >= 1]
      ctxAtom = if null ctx then [] else [Var 1]
      atoms = if null libAtoms then ctxAtom else libAtoms ++ ctxAtom
      intents = gpIntents profile
      allow needs = null intents || any (`elem` intents) needs
      former = if allow [NeedFormer, NeedBridge, NeedDifferential]
               then [Pi a a | a <- atoms] ++ [Sigma a a | a <- atoms]
               else []
      hit = if allow [NeedHIT]
            then [Susp a | a <- atoms] ++ [Trunc a | a <- atoms]
            else []
      modal = if allow [NeedModal, NeedBridge]
              then [Flat a | a <- atoms] ++ [Sharp a | a <- atoms]
              else []
      temporal = if allow [NeedTemporal]
                 then [Next a | a <- atoms] ++ [Eventually a | a <- atoms]
                 else []
  in former ++ hit ++ modal ++ temporal

-- | Generate a random MBTT expression from an action.
randomExprFromAction :: MCTSConfig -> Action -> Library -> [TeleEntry] -> StdGen -> Int
                     -> (MBTTExpr, StdGen)
randomExprFromAction cfg act lib ctx gen maxD = case act of
  AUniv        -> (Univ, gen)
  AVar i       -> (Var i, gen)
  ALib i       -> (Lib i, gen)
  APathCon d   -> (PathCon d, gen)

  APi    | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
        (b, gen2) = randomSubExpr cfg lib ctx gen1 (maxD - 1)
    in (Pi a b, gen2)

  ASigma | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
        (b, gen2) = randomSubExpr cfg lib ctx gen1 (maxD - 1)
    in (Sigma a b, gen2)

  ALam   | maxD > 0 ->
    let (body, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Lam body, gen1)

  AApp   | maxD > 0 ->
    let (f, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
        (x, gen2) = randomSubExpr cfg lib ctx gen1 (maxD - 1)
    in (App f x, gen2)

  ASusp  | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Susp a, gen1)

  ATrunc | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Trunc a, gen1)

  ARefl  | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Refl a, gen1)

  AId    | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
        (x, gen2) = randomSubExpr cfg lib ctx gen1 (maxD - 1)
        (y, gen3) = randomSubExpr cfg lib ctx gen2 (maxD - 1)
    in (Id a x y, gen3)

  AFlat  | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Flat a, gen1)

  ASharp | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Sharp a, gen1)

  ADisc  | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Disc a, gen1)

  AShape | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Shape a, gen1)

  ANext  | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Next a, gen1)

  AEventually | maxD > 0 ->
    let (a, gen1) = randomSubExpr cfg lib ctx gen (maxD - 1)
    in (Eventually a, gen1)

  -- Depth exceeded: fall back to terminal
  _ -> randomTerminal lib ctx gen

-- | Generate a random sub-expression.
randomSubExpr :: MCTSConfig -> Library -> [TeleEntry] -> StdGen -> Int -> (MBTTExpr, StdGen)
randomSubExpr cfg lib ctx gen maxD
  | maxD <= 0 = randomTerminal lib ctx gen
  | otherwise =
    let hole = Hole ctx AnyHole 0 100
        actions = validActionsWithProfile (activeGoalProfile cfg lib) hole lib
        terminalWeight = if maxD <= 1 then 3.0 else 1.0 :: Double
        weighted = [(a, w * (if isTerminalAction a then terminalWeight else 1.0))
                   | a <- actions
                   , let w = fromIntegral (actionPriority (length lib) a)]
    in if null weighted
       then (Univ, gen)
       else let (act, gen') = weightedChoice weighted gen
            in randomExprFromAction cfg act lib ctx gen' maxD

isTerminalAction :: Action -> Bool
isTerminalAction AUniv      = True
isTerminalAction (AVar _)   = True
isTerminalAction (ALib _)   = True
isTerminalAction (APathCon _) = True
isTerminalAction _          = False

randomTerminal :: Library -> [TeleEntry] -> StdGen -> (MBTTExpr, StdGen)
randomTerminal lib ctx gen =
  let libSize = length lib
      ctxSize = length ctx
      terminals = [AUniv]
                ++ [AVar i | i <- [1..ctxSize]]
                ++ [ALib i | i <- [1..libSize]]
      weighted = [(a, fromIntegral (actionPriority libSize a)) | a <- terminals]
  in if null weighted
     then (Univ, gen)
     else let (act, gen') = weightedChoice weighted gen
              expr = case act of
                AUniv  -> Univ
                AVar i -> Var i
                ALib i -> Lib i
                _      -> Univ
          in (expr, gen')

weightedChoice :: [(a, Double)] -> StdGen -> (a, StdGen)
weightedChoice [] _gen = error "weightedChoice: empty list"
weightedChoice items gen =
  let totalWeight = sum (map snd items)
      (r, gen') = randomR (0.0, totalWeight) gen
      pick _ [] = fst (head items)
      pick remaining ((item, w):rest)
        | remaining <= w = item
        | otherwise = pick (remaining - w) rest
  in (pick r items, gen')

-- ============================================
-- Single-Step Search (for integration with Synthesis)
-- ============================================

mctsSearchStep :: EvalMode -> MCTSConfig -> Library -> Double -> IO ([(Telescope, Int, Int, Double)], MCTSStats)
mctsSearchStep evalMode cfg lib bar = do
  result <- mctsSearch evalMode cfg lib
  let evaluated = [ (tele, nu, kappa, rho)
                  | (tele, _) <- mrTelescopes result
                  , let name = "candidate"
                  , let (nu, kappa, rho) = evaluateTelescope evalMode tele lib (mctsNuDepth cfg) name
                  , rho >= bar
                  ]
  return (sortOn (\(_, _, _, rho) -> Down rho) evaluated, mrStats result)

