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
                  teleReferencesWindow, teleMaxLibRef, teleBitCost,
                  desugaredKappa, isTriviallyDerivable)
import TelescopeCheck (checkTelescope, CheckResult(..))
import Types (Library, LibraryEntry(..))
import Data.List (sortOn)

-- ============================================
-- Configuration
-- ============================================

-- | Enumeration parameters.
data EnumConfig = EnumConfig
  { ecMaxBitBudget  :: !Int    -- ^ Max total bit budget per entry (default 40)
  , ecMaxEntries    :: !Int    -- ^ Max telescope entries (default 8)
  , ecMaxASTDepth   :: !Int    -- ^ Max AST depth per expression (default 5)
  , ecMaxCandidates :: !Int    -- ^ Max candidates to return (default 5000)
  } deriving (Show)

-- | Default enumeration config. Budget is per-entry, not total.
defaultEnumConfig :: EnumConfig
defaultEnumConfig = EnumConfig
  { ecMaxBitBudget  = 40
  , ecMaxEntries    = 8
  , ecMaxASTDepth   = 5
  , ecMaxCandidates = 5000
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
  } deriving (Show, Eq)

instance Ord MBTTCandidate where
  compare a b = compare (mcBitKappa a, mcClauseCount a)
                        (mcBitKappa b, mcClauseCount b)

-- ============================================
-- Expression Enumeration
-- ============================================

-- | Enumerate all well-typed MBTTExpr up to a bit budget.
-- Uses exhaustive budget-split for compound expressions.
--
-- Args: libSize, ctxDepth, bitBudget, maxASTDepth, library (for gating)
enumerateExprs :: Int -> Int -> Int -> Int -> Library -> [MBTTExpr]
enumerateExprs libSize ctxDepth budget maxDepth lib
  | budget <= 0 = []
  | maxDepth <= 0 = terminals
  | otherwise = terminals ++ compounds
  where
    -- Modal/temporal gating flags (structural, not name-based)
    hasModal   = any leHasModalOps lib
    hasTemporal = any leHasHilbert lib

    -- Terminal expressions
    terminals = concat
      [ [Univ | budget >= 4]
      , [Var i | i <- [1..ctxDepth], budget >= 3 + eliasGammaLength i]
      , [Lib i | i <- [1..libSize], budget >= 3 + eliasGammaLength i]
      , [PathCon d | d <- [1..3], budget >= 6 + eliasGammaLength d]
      ]

    -- Compound expressions with budget splits
    compounds = concat
      [ enumUnary Lam 2 True        -- Lam extends context
      , enumUnary Refl 5 False
      , enumUnary Susp 5 False
      , enumUnary Trunc 6 False
      , if hasModal then concat
          [ enumUnary Flat 7 False
          , enumUnary Sharp 7 False
          , enumUnary Disc 7 False
          , enumUnary Shape 8 False
          ] else []
      , if hasTemporal then concat
          [ enumUnary Next 9 False
          , enumUnary Eventually 9 False
          ] else []
      , enumBinary Pi 3 True         -- Pi extends context for codomain
      , enumBinary Sigma 4 True      -- Sigma extends context for second component
      , enumBinaryNoExt App 2        -- App doesn't extend context
      , enumTernary Id 5
      ]

    -- Unary: constructor(child) with prefix cost, optionally extending context
    enumUnary ctor prefix extendsCtx =
      let remaining = budget - prefix
          subDepth = ctxDepth + (if extendsCtx then 1 else 0)
      in if remaining < 4  -- minimum child size
         then []
         else [ ctor child
              | child <- enumerateExprs libSize subDepth remaining (maxDepth - 1) lib
              ]

    -- Binary with context extension for second child (Pi, Sigma)
    enumBinary ctor prefix _extendsCtx =
      let remaining = budget - prefix
          minChild = 4  -- smallest expression is 4 bits (Univ)
      in if remaining < 2 * minChild
         then []
         else [ ctor a b
              | budA <- [minChild .. remaining - minChild]
              , let budB = remaining - budA
              , a <- enumerateExprs libSize ctxDepth budA (maxDepth - 1) lib
              , b <- enumerateExprs libSize (ctxDepth + 1) budB (maxDepth - 1) lib
              ]

    -- Binary without context extension (App)
    enumBinaryNoExt ctor prefix =
      let remaining = budget - prefix
          minArg = 4
      in if remaining < 2 * minArg
         then []
         else [ ctor f x
              | budF <- [minArg .. remaining - minArg]
              , let budX = remaining - budF
              -- Reject App(_ Univ) per TelescopeCheck
              , f <- enumerateExprs libSize ctxDepth budF (maxDepth - 1) lib
              , x <- enumerateExprs libSize ctxDepth budX (maxDepth - 1) lib
              , case x of { Univ -> False; _ -> True }
              ]

    -- Ternary: Id(A, x, y) — three-way split
    enumTernary ctor prefix =
      let remaining = budget - prefix
          minChild = 4
      in if remaining < 3 * minChild
         then []
         else [ ctor a x y
              | budA <- [minChild .. remaining - 2 * minChild]
              , budX <- [minChild .. remaining - budA - minChild]
              , let budY = remaining - budA - budX
              , a <- enumerateExprs libSize ctxDepth budA (maxDepth - 1) lib
              , x <- enumerateExprs libSize ctxDepth budX (maxDepth - 1) lib
              , y <- enumerateExprs libSize ctxDepth budY (maxDepth - 1) lib
              ]

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
enumerateMBTTTelescopes :: Library -> EnumConfig -> [MBTTCandidate]
enumerateMBTTTelescopes lib cfg =
  let libSize = length lib
      maxK = ecMaxEntries cfg
      budget = ecMaxBitBudget cfg
      depth = ecMaxASTDepth cfg
      maxCand = ecMaxCandidates cfg

      -- Generate telescopes of each length
      allTeles = concatMap (telescopesOfLength lib libSize budget depth) [1..maxK]

      -- Filter: well-formed, connected, not trivially derivable
      valid = filter (isValidCandidate lib libSize) allTeles

      -- Wrap in MBTTCandidate, sort by bit cost, take limit
      candidates = [ MBTTCandidate tele (teleBitCost tele) (desugaredKappa tele) (teleNodeCount tele)
                   | tele <- valid
                   ]
      sorted = sortOn mcBitKappa candidates
  in take maxCand sorted

-- | Generate all telescopes of exactly length k.
telescopesOfLength :: Library -> Int -> Int -> Int -> Int -> [Telescope]
telescopesOfLength lib libSize budget depth k = go k []
  where
    go 0 acc = [Telescope (reverse acc)]
    go !n acc =
      let ctxDepth = length acc
          exprs = enumerateExprs libSize ctxDepth budget depth lib
          entryName = "c" ++ show (ctxDepth + 1)
      in [ tele
         | expr <- exprs
         , let entry = TeleEntry entryName expr
         , tele <- go (n - 1) (entry : acc)
         ]

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
