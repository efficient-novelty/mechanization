{-# LANGUAGE BangPatterns #-}

-- | Agda Rosetta Bridge — Export discovered telescopes as Cubical Agda stubs
--
-- Translates the MBTT telescope representation into Cubical Agda source files.
-- Each genesis step becomes a module with:
--   - Correct imports from prior steps (via Lib references)
--   - Type signatures for each telescope entry
--   - Postulates for abstract structure
--   - Comments linking each entry to its MBTT specification
--
-- All 15 steps generate postulate-based stubs that document the mathematical
-- structure discovered by the engine. The stubs are not expected to typecheck
-- in Cubical Agda (they use abstract type signatures), but they serve as a
-- machine-readable Rosetta stone between the MBTT encoding and Agda notation.

module AgdaExport
  ( -- * Export a single step
    exportStep
    -- * Export all steps
  , exportAllSteps
    -- * MBTT → Agda translation
  , exprToAgda
  , teleToAgda
  ) where

import Telescope
import Kolmogorov (MBTTExpr(..))

import qualified Data.Set as Set

-- ============================================
-- Step Name Mapping
-- ============================================

-- | Agda module name for a genesis step.
stepModuleName :: Int -> String -> String
stepModuleName n name = "PEN.Genesis.Step" ++ show n ++ "-" ++ name

-- | Canonical step names indexed 1-15.
stepNames :: [(Int, String)]
stepNames =
  [ (1,  "Universe"), (2,  "Unit"), (3,  "Witness"), (4,  "Pi")
  , (5,  "S1"), (6,  "Trunc"), (7,  "S2"), (8,  "S3")
  , (9,  "Hopf"), (10, "Cohesion"), (11, "Connections"), (12, "Curvature")
  , (13, "Metric"), (14, "Hilbert"), (15, "DCT")
  ]

-- | Library reference name: maps step index → canonical name for imports.
libRefName :: Int -> String
libRefName i = case lookup i stepNames of
  Just name -> name
  Nothing   -> "Step" ++ show i

-- ============================================
-- MBTT → Agda Expression Translation
-- ============================================

-- | Translate an MBTT expression to Agda syntax.
-- Context maps de Bruijn indices to named variables from the telescope.
exprToAgda :: [String] -> MBTTExpr -> String
exprToAgda ctx expr = case expr of
  Univ        -> "Type"
  Var i       -> lookupVar ctx i
  Lib i       -> libRefName i ++ ".T"
  App f x     -> wrapApp (exprToAgda ctx f) (atomToAgda ctx x)
  Lam body    -> let v = fresh ctx
                 in "(λ " ++ v ++ " → " ++ exprToAgda (v : ctx) body ++ ")"
  Pi a b      -> let v = fresh ctx
                 in "(" ++ v ++ " : " ++ exprToAgda ctx a ++ ") → " ++
                    exprToAgda (v : ctx) b
  Sigma a b   -> let v = fresh ctx
                 in "Σ " ++ atomToAgda ctx a ++ " (λ " ++ v ++ " → " ++
                    exprToAgda (v : ctx) b ++ ")"
  Id _a x y   -> atomToAgda ctx x ++ " ≡ " ++ atomToAgda ctx y
  Refl _      -> "refl"
  PathCon d   -> "pathCon" ++ show d  -- d-cell attachment
  Susp a      -> "Susp " ++ atomToAgda ctx a
  Trunc a     -> "∥ " ++ exprToAgda ctx a ++ " ∥₁"
  Flat a      -> "♭ " ++ atomToAgda ctx a
  Sharp a     -> "♯ " ++ atomToAgda ctx a
  Disc a      -> "Disc " ++ atomToAgda ctx a
  Shape a     -> "Shape " ++ atomToAgda ctx a
  Next a      -> "○ " ++ atomToAgda ctx a
  Eventually a -> "◇ " ++ atomToAgda ctx a

-- | Translate to Agda, wrapping in parens if not atomic.
atomToAgda :: [String] -> MBTTExpr -> String
atomToAgda ctx e = case e of
  Univ  -> exprToAgda ctx e
  Var _ -> exprToAgda ctx e
  Lib _ -> exprToAgda ctx e
  _     -> "(" ++ exprToAgda ctx e ++ ")"

-- | Application: "f x" (no extra parens around f).
wrapApp :: String -> String -> String
wrapApp f x = f ++ " " ++ x

-- | Look up a de Bruijn variable in the context.
lookupVar :: [String] -> Int -> String
lookupVar ctx i
  | i >= 1 && i <= length ctx = ctx !! (i - 1)
  | otherwise                 = "v" ++ show i

-- | Generate a fresh variable name not in context.
fresh :: [String] -> String
fresh ctx = head [v | v <- candidates, v `notElem` ctx]
  where candidates = ["x", "y", "z", "w", "a", "b", "c"] ++
                     ["x" ++ show n | n <- [(1::Int)..]]

-- ============================================
-- Telescope → Agda Module
-- ============================================

-- | Generate Agda source for a single telescope entry as a postulate line.
-- The context contains only entries declared *before* this one (de Bruijn).
entryToAgda :: [String] -> TeleEntry -> String
entryToAgda ctx entry =
  let name = sanitizeName (teName entry)
      ty   = exprToAgda ctx (teType entry)
      mbtt = show (teType entry)
  in "  " ++ name ++ " : " ++ ty ++ "\n" ++
     "    -- MBTT: " ++ mbtt

-- | Sanitize a name for Agda (replace spaces with dashes).
sanitizeName :: String -> String
sanitizeName = map (\c -> if c == ' ' then '-' else c)

-- | Generate all entries with incrementally-built de Bruijn contexts.
-- Entry k sees context [name_{k-1}, ..., name_0] (most recent first).
allEntriesToAgda :: [TeleEntry] -> [String]
allEntriesToAgda = go []
  where
    go _ctx [] = []
    go ctx (entry:rest) =
      entryToAgda ctx entry : go (teName entry : ctx) rest

-- | Generate a full Agda module for a genesis step.
teleToAgda :: Int -> String -> Telescope -> String
teleToAgda step name tele =
  let entries  = teleEntries tele
      modName  = stepModuleName step name
      libRefs  = teleLibRefs tele
      imports  = generateImports libRefs
      header   = agdaHeader step name
      body     = unlines $ allEntriesToAgda entries
      kappa    = desugaredKappa tele
  in unlines
    [ header
    , "-- NOTE: This is a postulate stub generated from the MBTT telescope."
    , "-- It documents the type-theoretic structure discovered by the PEN engine."
    , "-- The type signatures use abstract notation and are not expected to"
    , "-- typecheck directly in Cubical Agda without additional definitions."
    , ""
    , "module " ++ modName ++ " where"
    , ""
    , imports
    , "-- | Step " ++ show step ++ ": " ++ name
    , "-- Construction effort κ = " ++ show kappa
    , "-- Telescope entries: " ++ show (length entries)
    , ""
    , "postulate"
    , body
    ]

-- | Generate import statements for library references.
generateImports :: Set.Set Int -> String
generateImports refs
  | Set.null refs = "-- No library dependencies (bootstrap step)"
  | otherwise =
    let refList = Set.toAscList refs
        importLine i = "open import " ++ stepModuleName i (libRefName i) ++
                       " as " ++ libRefName i
    in unlines (["-- Library dependencies:"] ++ map importLine refList)

-- | File header with provenance comment.
agdaHeader :: Int -> String -> String
agdaHeader step name = unlines
  [ "-- ============================================"
  , "-- PEN Generative Sequence — Step " ++ show step ++ ": " ++ name
  , "-- Auto-generated by the PEN Agda Rosetta Bridge"
  , "-- Source: engine/src/Telescope.hs (referenceTelescope " ++ show step ++ ")"
  , "-- ============================================"
  ]

-- ============================================
-- Export Functions
-- ============================================

-- | Export a single step to an Agda source string.
exportStep :: Int -> String
exportStep step =
  let tele = referenceTelescope step
      name = case lookup step stepNames of
               Just n  -> n
               Nothing -> "Unknown"
  in teleToAgda step name tele

-- | Export all 15 steps, returning [(filename, content)].
exportAllSteps :: [(FilePath, String)]
exportAllSteps =
  [ (filename step name, teleToAgda step name (referenceTelescope step))
  | (step, name) <- stepNames
  ]
  where
    filename step name = "PEN/Genesis/Step" ++ show step ++ "-" ++ name ++ ".agda"
