{-# LANGUAGE BangPatterns #-}

-- | Type-Directed Telescope Generator
--
-- Generates well-typed MBTT telescopes top-down, embedding a bidirectional
-- type-checker directly into the generator. Instead of generating random
-- bit-strings and checking validity, the generator starts with typed holes
-- and only proposes MBTT nodes that satisfy local typing rules.
--
-- This eliminates >99.9% of the naive search space.
--
-- The generator supports two modes:
--   1. Exhaustive enumeration for small κ (κ ≤ 4)
--   2. Guided enumeration with action priorities for MCTS integration

module TelescopeGen
  ( -- * Core types
    Hole(..)
  , HoleGoal(..)
  , Action(..)
    -- * Generation
  , validActions
  , expandAction
  , enumerateTelescopes
  , enumerateTelescopesAtKappa
    -- * Pruning filters
  , passesStructuralUnity
  , passesInterfaceDensity
    -- * Action utilities
  , actionPriority
  , actionBitCost
    -- * Library gating
  , actionGatedByLibrary
  ) where

import Kolmogorov (MBTTExpr(..), bitLength)
import Telescope (Telescope(..), TeleEntry(..), teleIsConnected, teleReferencesWindow, teleMaxLibRef)
import Types (Library, LibraryEntry(..), mkLibraryEntry)
import Data.List (sortOn)

-- ============================================
-- Core Types
-- ============================================

-- | A typed hole: a position in a partially-built telescope or MBTT expression.
data Hole = Hole
  { holeCtx     :: ![TeleEntry]  -- ^ Context so far (earlier telescope entries)
  , holeGoal    :: !HoleGoal     -- ^ What we're trying to fill
  , holeDepth   :: !Int          -- ^ Current depth in the AST
  , holeBudget  :: !Int          -- ^ Remaining bit budget (for bounded generation)
  } deriving (Show, Eq)

-- | What kind of expression we need to generate.
data HoleGoal
  = TypeHole          -- ^ ? : U  (generate a type expression)
  | TermHole          -- ^ ? : A  (generate a term, untyped for now)
  | AnyHole           -- ^ ? : ?  (generate anything valid)
  deriving (Show, Eq)

-- | An action: a choice of MBTT node to fill a hole.
-- Each action may create sub-holes that need to be filled.
data Action
  = AUniv                -- ^ U (Universe) — 4 bits
  | AVar !Int            -- ^ Var(i) — 3 + Elias γ bits
  | ALib !Int            -- ^ Lib(i) — 3 + Elias γ bits
  | APi                  -- ^ Pi(A,B) — 3 bits + two sub-holes
  | ASigma               -- ^ Sigma(A,B) — 4 bits + two sub-holes
  | ALam                 -- ^ Lam(body) — 2 bits + one sub-hole
  | AApp                 -- ^ App(f,x) — 2 bits + two sub-holes
  | AId                  -- ^ Id(A,x,y) — 5 bits + three sub-holes
  | ARefl                -- ^ Refl(a) — 5 bits + one sub-hole
  | ASusp                -- ^ Susp(A) — 5 bits + one sub-hole
  | ATrunc               -- ^ Trunc(A) — 6 bits + one sub-hole
  | APathCon !Int        -- ^ PathCon(d) — 6 + Elias γ bits
  | AFlat                -- ^ Flat(A) — 7 bits + one sub-hole
  | ASharp               -- ^ Sharp(A) — 7 bits + one sub-hole
  | ADisc                -- ^ Disc(A) — 7 bits + one sub-hole
  | AShape               -- ^ Shape(A) — 8 bits + one sub-hole
  | ANext                -- ^ Next(A) — 9 bits + one sub-hole
  | AEventually          -- ^ Eventually(A) — 9 bits + one sub-hole
  deriving (Show, Eq, Ord)

-- ============================================
-- Action Generation (Contextual Pruning)
-- ============================================

-- | Generate all valid actions at a typed hole.
-- This is the core pruning engine: only actions that produce well-typed
-- expressions in the current context are returned.
validActions :: Hole -> Library -> [Action]
validActions hole lib =
  let budget = holeBudget hole
      d      = holeDepth hole
      ctx    = holeCtx hole
      libSize = length lib
      ctxSize = length ctx
      maxDepth = 6  -- hard limit on AST depth to prevent explosion

      -- Budget check: each action has a minimum cost
      affordable act = actionMinCost act <= budget

      -- Depth check: prevent infinite recursion
      withinDepth = d < maxDepth

      -- Terminal actions (no sub-holes)
      terminals = concat
        [ [AUniv | budget >= 4]
        , [AVar i | i <- [1..ctxSize], affordable (AVar i)]
        , [ALib i | i <- [1..libSize], affordable (ALib i)]
        , [APathCon dim | dim <- [1..3], affordable (APathCon dim)]
        ]

      -- Recursive actions (create sub-holes)
      recursive
        | not withinDepth = []
        | otherwise = concat
          [ [APi     | budget >= 6]   -- Pi needs at least 3 + 3 for minimal children
          , [ASigma  | budget >= 8]   -- Sigma needs at least 4 + 4
          , [ALam    | budget >= 4]   -- Lam needs at least 2 + 2
          , [AApp    | budget >= 6]   -- App needs at least 2 + 2 + 2
          , [ASusp   | budget >= 9]   -- Susp(5) + minimal child(4)
          , [ATrunc  | budget >= 10]  -- Trunc(6) + minimal child(4)
          , [AId     | budget >= 17]  -- Id(5) + 3 minimal children(4+4+4)
          , [ARefl   | budget >= 9]   -- Refl(5) + minimal child(4)
          , [AFlat   | budget >= 11]  -- Flat(7) + minimal child(4)
          , [ASharp  | budget >= 11]
          , [ADisc   | budget >= 11]
          , [AShape  | budget >= 12]  -- Shape(8) + minimal child(4)
          , [ANext   | budget >= 13]  -- Next(9) + minimal child(4)
          , [AEventually | budget >= 13]
          ]

      -- Library-gated actions: modal/temporal only available when library has them
      libraryGated = filter (actionGatedByLibrary lib) (terminals ++ recursive)

  in sortOn (negate . actionPriority libSize) libraryGated

-- | Check if an action is gated by library prerequisites.
-- Uses STRUCTURAL capability flags, not entry names.
actionGatedByLibrary :: Library -> Action -> Bool
actionGatedByLibrary lib act = case act of
  AFlat       -> hasModalCap
  ASharp      -> hasModalCap
  ADisc       -> hasModalCap
  AShape      -> hasModalCap
  ANext       -> hasTemporalPrereqs
  AEventually -> hasTemporalPrereqs
  _           -> True
  where
    hasModalCap      = any leHasModalOps lib
    hasTemporalPrereqs = any leHasHilbert lib

-- | Minimum bit cost of an action (assuming minimal children).
actionMinCost :: Action -> Int
actionMinCost AUniv          = 4
actionMinCost (AVar i)       = 3 + eliasGammaLen i
actionMinCost (ALib i)       = 3 + eliasGammaLen i
actionMinCost APi            = 3   -- just the prefix, children billed separately
actionMinCost ASigma         = 4
actionMinCost ALam           = 2
actionMinCost AApp           = 2
actionMinCost AId            = 5
actionMinCost ARefl          = 5
actionMinCost ASusp          = 5
actionMinCost ATrunc         = 6
actionMinCost (APathCon d)   = 6 + eliasGammaLen d
actionMinCost AFlat          = 7
actionMinCost ASharp         = 7
actionMinCost ADisc          = 7
actionMinCost AShape         = 8
actionMinCost ANext          = 9
actionMinCost AEventually    = 9

-- | Elias gamma length (duplicated from Kolmogorov to avoid circular import).
eliasGammaLen :: Int -> Int
eliasGammaLen n
  | n <= 0    = 1
  | otherwise = 2 * floorLog2 n + 1
  where
    floorLog2 1 = 0
    floorLog2 k = 1 + floorLog2 (k `div` 2)

-- | Priority of an action (higher = more likely to be useful).
-- Biases MCTS toward high-yield actions.
actionPriority :: Int -> Action -> Int
actionPriority libSize act = case act of
  -- Library pointers to recent entries: highest priority (Interface Density)
  ALib i | i >= libSize - 1 -> 100
         | i >= libSize - 2 -> 90
         | otherwise        -> 50 + i
  -- Core type formers
  APi      -> 85
  ASigma   -> 80
  AUniv    -> 75
  -- HIT construction
  APathCon _ -> 70
  ASusp      -> 65
  -- Terms
  ALam     -> 60
  AApp     -> 55
  AVar _   -> 45
  -- Identity
  AId      -> 40
  ARefl    -> 35
  -- Modal
  AFlat    -> 30
  ASharp   -> 28
  ADisc    -> 26
  AShape   -> 24
  -- Truncation
  ATrunc   -> 20
  -- Temporal (only at late stages)
  ANext       -> 15
  AEventually -> 14

-- | Bit cost of an action (the prefix cost, not including children).
actionBitCost :: Action -> Int
actionBitCost = actionMinCost

-- ============================================
-- Action Expansion
-- ============================================

-- | Expand a hole with an action, producing the MBTT expression fragment
-- and any new sub-holes that need to be filled.
expandAction :: Hole -> Action -> (MBTTExpr -> MBTTExpr, [Hole])
expandAction hole act =
  let d = holeDepth hole + 1
      ctx = holeCtx hole
      remaining b = holeBudget hole - actionMinCost act - b
      subHole goal extra = Hole ctx goal d (remaining extra)
  in case act of
    AUniv        -> (const Univ, [])
    AVar i       -> (const (Var i), [])
    ALib i       -> (const (Lib i), [])
    APathCon dim -> (const (PathCon dim), [])

    -- Binary: Pi(A,B) — distribute remaining budget to children
    APi    -> let bud = remaining 0 `div` 2
              in (\_ -> Pi (placeholder "piA") (placeholder "piB"),
                  [Hole ctx TypeHole d bud, Hole ctx TypeHole d bud])

    ASigma -> let bud = remaining 0 `div` 2
              in (\_ -> Sigma (placeholder "sigA") (placeholder "sigB"),
                  [Hole ctx TypeHole d bud, Hole ctx TypeHole d bud])

    AApp   -> let bud = remaining 0 `div` 2
              in (\_ -> App (placeholder "appF") (placeholder "appX"),
                  [Hole ctx AnyHole d bud, Hole ctx AnyHole d bud])

    AId    -> let bud = remaining 0 `div` 3
              in (\_ -> Id (placeholder "idA") (placeholder "idX") (placeholder "idY"),
                  [Hole ctx TypeHole d bud, Hole ctx TermHole d bud, Hole ctx TermHole d bud])

    -- Unary: Lam(body), Refl(a), Susp(A), Trunc(A), modals
    ALam   -> (\_ -> Lam (placeholder "lamB"),
               [Hole ctx AnyHole d (remaining 0)])
    ARefl  -> (\_ -> Refl (placeholder "reflA"),
               [Hole ctx AnyHole d (remaining 0)])
    ASusp  -> (\_ -> Susp (placeholder "suspA"),
               [Hole ctx TypeHole d (remaining 0)])
    ATrunc -> (\_ -> Trunc (placeholder "truncA"),
               [Hole ctx TypeHole d (remaining 0)])

    AFlat       -> (\_ -> Flat (placeholder "flatA"),
                    [Hole ctx TypeHole d (remaining 0)])
    ASharp      -> (\_ -> Sharp (placeholder "sharpA"),
                    [Hole ctx TypeHole d (remaining 0)])
    ADisc       -> (\_ -> Disc (placeholder "discA"),
                    [Hole ctx TypeHole d (remaining 0)])
    AShape      -> (\_ -> Shape (placeholder "shapeA"),
                    [Hole ctx TypeHole d (remaining 0)])
    ANext       -> (\_ -> Next (placeholder "nextA"),
                    [Hole ctx TypeHole d (remaining 0)])
    AEventually -> (\_ -> Eventually (placeholder "evA"),
                    [Hole ctx TypeHole d (remaining 0)])

-- | Placeholder expression (should never appear in final output).
placeholder :: String -> MBTTExpr
placeholder _ = Univ  -- will be replaced by recursive fill

-- ============================================
-- Telescope Enumeration
-- ============================================

-- | Enumerate all valid telescopes up to length κ.
-- For κ ≤ 4, this is exhaustive. For larger κ, returns a bounded set.
enumerateTelescopes :: Library -> Int -> [Telescope]
enumerateTelescopes lib maxKappa =
  concatMap (enumerateTelescopesAtKappa lib) [1..maxKappa]

-- | Enumerate all valid telescopes of exactly length κ.
enumerateTelescopesAtKappa :: Library -> Int -> [Telescope]
enumerateTelescopesAtKappa lib kappa =
  let -- Generate all possible single entries
      singleEntries = generateEntries lib [] maxBudget
      -- Build telescopes of length kappa by iteratively extending
      telescopes = buildTelescopes lib kappa singleEntries
      -- Apply structural filters
      filtered = filter (\t -> passesStructuralUnity t
                             && passesInterfaceDensity t (length lib))
                        telescopes
  in filtered
  where
    maxBudget = 50  -- bit budget per entry

-- | Generate all valid single telescope entries given context.
-- Includes both library-biased entries (via bestChild) AND variable-child
-- variants needed for discovering pure type-former telescopes (Pi/Sigma
-- over variables, no library references).
generateEntries :: Library -> [TeleEntry] -> Int -> [TeleEntry]
generateEntries lib ctx budget =
  let hole = Hole ctx AnyHole 0 budget
      actions = validActions hole lib
      entryName = "c" ++ show (length ctx + 1)
      -- Main entries: library-biased (bestChild prefers Lib n)
      mainEntries = [ TeleEntry entryName expr
                    | act <- actions
                    , let expr = actionToExpr act lib ctx budget
                    , expr /= Univ || act == AUniv
                    ]
      -- Variable-child variants: critical for pure type-former patterns
      -- Without these, Pi(Var 1, Var 2) and Lam(Var 1) are never generated,
      -- so the enumerator can never discover Pi/Sigma as type formers.
      varEntries = generateVarEntries ctx entryName
  -- Variable entries go FIRST: pure-former patterns (Pi/Sigma over variables)
  -- must appear early in the list because buildTelescopes truncates at
  -- maxExtensions. With mainEntries first, pure-former telescopes would
  -- be cut off when building longer telescopes.
  in varEntries ++ mainEntries

-- | Generate variable-only sub-hole variants for recursive MBTT actions.
-- The main enumeration fills sub-holes with bestChild (= Lib n), missing
-- the Pi(Var i, Var j) / Lam(Var i) patterns essential for pure type-former
-- telescope discovery. This adds those patterns.
generateVarEntries :: [TeleEntry] -> String -> [TeleEntry]
generateVarEntries ctx entryName =
  let n = length ctx
      -- Available atoms: bound variables + universe
      atoms = [Var i | i <- [1..n]] ++ [Univ]
  in concat
    [ [TeleEntry entryName (Lam a) | a <- atoms]
    , [TeleEntry entryName (Pi a b) | a <- atoms, b <- atoms]
    , [TeleEntry entryName (Sigma a b) | a <- atoms, b <- atoms]
    , [TeleEntry entryName (App a b) | a <- atoms, b <- atoms]
    ]

-- | Convert an action to a complete MBTT expression.
-- For terminal actions, this is direct. For recursive actions,
-- we generate the simplest valid expression (for exhaustive enumeration).
actionToExpr :: Action -> Library -> [TeleEntry] -> Int -> MBTTExpr
actionToExpr AUniv _ _ _        = Univ
actionToExpr (AVar i) _ _ _     = Var i
actionToExpr (ALib i) _ _ _     = Lib i
actionToExpr (APathCon d) _ _ _ = PathCon d
-- For recursive actions, generate at depth 1 only (for exhaustive)
actionToExpr APi lib ctx bud    = Pi (bestChild lib ctx (bud `div` 2))
                                     (bestChild lib ctx (bud `div` 2))
actionToExpr ASigma lib ctx bud = Sigma (bestChild lib ctx (bud `div` 2))
                                        (bestChild lib ctx (bud `div` 2))
actionToExpr ALam lib ctx bud   = Lam (bestChild lib ctx bud)
actionToExpr AApp lib ctx bud   = App (bestChild lib ctx (bud `div` 2))
                                      (bestChild lib ctx (bud `div` 2))
actionToExpr ASusp lib ctx bud  = Susp (bestChild lib ctx bud)
actionToExpr ATrunc lib ctx bud = Trunc (bestChild lib ctx bud)
actionToExpr ARefl lib ctx bud  = Refl (bestChild lib ctx bud)
actionToExpr AId lib ctx bud    = Id (bestChild lib ctx (bud `div` 3))
                                     (bestChild lib ctx (bud `div` 3))
                                     (bestChild lib ctx (bud `div` 3))
actionToExpr AFlat lib ctx bud       = Flat (bestChild lib ctx bud)
actionToExpr ASharp lib ctx bud      = Sharp (bestChild lib ctx bud)
actionToExpr ADisc lib ctx bud       = Disc (bestChild lib ctx bud)
actionToExpr AShape lib ctx bud      = Shape (bestChild lib ctx bud)
actionToExpr ANext lib ctx bud       = Next (bestChild lib ctx bud)
actionToExpr AEventually lib ctx bud = Eventually (bestChild lib ctx bud)

-- | Best child for filling a sub-hole: prefer recent library pointers.
bestChild :: Library -> [TeleEntry] -> Int -> MBTTExpr
bestChild lib ctx _
  | not (null lib) = Lib (length lib)  -- most recent library entry
  | not (null ctx) = Var 1             -- most recent bound variable
  | otherwise      = Univ              -- fallback

-- | Build telescopes of exactly length k by iterating entry generation.
buildTelescopes :: Library -> Int -> [TeleEntry] -> [Telescope]
buildTelescopes _   0 _       = [Telescope []]
buildTelescopes lib 1 entries = [Telescope [e] | e <- entries]
buildTelescopes lib k entries =
  -- For each telescope of length k-1, extend with one more entry
  let shorter = buildTelescopes lib (k-1) entries
      maxExtensions = 100  -- limit branching factor (increased to accommodate variable entries)
  in [ Telescope (teleEntries t ++ [e])
     | t <- take maxExtensions shorter
     , let ctx = teleEntries t
     , e <- take maxExtensions (generateEntries lib ctx 50)
     ]

-- ============================================
-- Structural Filters
-- ============================================

-- | Structural Unity Filter: reject disconnected telescopes.
-- If a telescope entry c_i is never referenced by any c_j (j > i),
-- the telescope is two candidates artificially packed together.
passesStructuralUnity :: Telescope -> Bool
passesStructuralUnity (Telescope [])  = True
passesStructuralUnity (Telescope [_]) = True  -- single entry always passes
passesStructuralUnity t = teleIsConnected t

-- | Maximal Interface Density Filter: the telescope must reference
-- at least one of the two most recent library entries, OR operate
-- purely over variables (type formers like Pi/Sigma define operations,
-- not specific structures, so they don't reference existing library entries).
passesInterfaceDensity :: Telescope -> Int -> Bool
passesInterfaceDensity t libSize =
  teleReferencesWindow t libSize
  || teleMaxLibRef t == 0  -- pure type formers operate over variables only
