{-# LANGUAGE BangPatterns #-}

-- | Uniform nu computation — Priority 2 core algorithm
--
-- Computes novelty for ALL 15 Genesis structures from first principles
-- using a single uniform algorithm, with zero domain knowledge.
--
-- Key insight: novelty comes from two sources:
--   (A) New types: adding S1 to the library makes "S1", "L → S1", etc. newly
--       inhabitable. These are types that mention the new type by name.
--   (B) New operations: adding Pi/Sigma makes dependent function types expressible.
--       Adding Cohesion makes ♭X, ♯X, etc. expressible. These are types that
--       use the new operation on existing atoms.
--
-- The uniform algorithm handles both via BEFORE/AFTER comparison:
--   1. Enumerate all inhabited types at depth <= d BEFORE adding X
--   2. Enumerate all inhabited types at depth <= d AFTER adding X
--   3. New types = After \ Before
--   4. Normalize, schematize (all library atoms → L, candidate → X)
--   5. Filter trivial schemas
--   6. Count distinct non-trivial schemas
--
-- This replaces the hand-tuned capability rules in Capability.hs
-- and the latent bonus in Cluster.hs with a single uniform procedure.

module UniformNu
  ( UniformNuResult(..)
  , computeUniformNu
  , computeUniformNuAtDepth
  , uniformNuAllSteps
  , genesisLibrarySteps
  , GenesisStep(..)
  ) where

import Types
import ProofRank (schemaize, normalize, depth, enumWindowExact, availableFormers)
import Inhabitation (checkInhab, isInhabited)
import Equivalence (canonicalize, mapChildren)
import Independence (isTrivialSchema)
import qualified Data.Set as Set
import Data.List (sortOn)

-- ============================================
-- Data Types
-- ============================================

data UniformNuResult = UniformNuResult
  { unrStep          :: Int
  , unrName          :: String
  , unrPaperNu       :: Int                      -- paper's nu value
  , unrUniformNu     :: Int                      -- uniform algorithm's nu
  , unrSchemaCount   :: Int                      -- distinct non-trivial schemas
  , unrRawNew        :: Int                      -- raw new inhabited types
  , unrSchemas       :: [(TypeExpr, [TypeExpr])] -- (schema, concrete members)
  , unrPerDepth      :: [(Int, Int, Int)]        -- (depth, rawCount, schemaCount)
  , unrOrdering      :: String                   -- "OK" or description of failure
  , unrAdjointCredit :: Int                      -- adjoint completion credit (Lemma 9.4)
  } deriving (Show)

-- | A step in the Genesis sequence with all metadata
data GenesisStep = GenesisStep
  { gsStep     :: Int
  , gsName     :: String
  , gsEntry    :: LibraryEntry
  , gsPaperNu  :: Int
  , gsPaperK   :: Int
  , gsCategory :: String  -- "Foundation", "Former", "HIT", "Map", "Modal", "Axiom", "Synthesis"
  } deriving (Show)

-- ============================================
-- Genesis Sequence Definition
-- ============================================

genesisLibrarySteps :: [GenesisStep]
genesisLibrarySteps =
  [ GenesisStep  1 "Universe"    (mkLibraryEntry "Universe"    1 []  False Nothing)                                            1  2 "Foundation"
  , GenesisStep  2 "Unit"        (mkLibraryEntry "Unit"        1 []  False Nothing)                                            1  1 "Foundation"
  , GenesisStep  3 "Witness"     (mkLibraryEntry "Witness"     1 []  False Nothing)                                            2  1 "Foundation"
  , GenesisStep  4 "Pi/Sigma"    ((mkLibraryEntry "Pi"         0 []  False Nothing) { leHasDependentFunctions = True })        5  3 "Former"
  , GenesisStep  5 "S1"          (mkLibraryEntry "S1"          1 [1] True  Nothing)                                            7  3 "HIT"
  , GenesisStep  6 "PropTrunc"   (mkLibraryEntry "Trunc"       0 []  False (Just 0))                                           8  3 "Former"
  , GenesisStep  7 "S2"          (mkLibraryEntry "S2"          1 [2] True  Nothing)                                           10  3 "HIT"
  , GenesisStep  8 "S3"          (mkLibraryEntry "S3"          1 [3] True  Nothing)                                           18  5 "HIT"
  , GenesisStep  9 "Hopf"        (mkLibraryEntry "Hopf"        0 []  False Nothing)                                           17  4 "Map"
  , GenesisStep 10 "Cohesion"    ((mkLibraryEntry "Cohesion"   0 []  False Nothing) { leHasModalOps = True })                 19  4 "Modal"
  , GenesisStep 11 "Connections" ((mkLibraryEntry "Connections" 0 [] False Nothing) { leHasDifferentialOps = True })           26  5 "Axiom"
  , GenesisStep 12 "Curvature"   ((mkLibraryEntry "Curvature"  0 []  False Nothing) { leHasCurvature = True })                34  6 "Axiom"
  , GenesisStep 13 "Metric"      ((mkLibraryEntry "Metric"     0 []  False Nothing) { leHasMetric = True })                   43  7 "Axiom"
  , GenesisStep 14 "Hilbert"     ((mkLibraryEntry "Hilbert"    0 []  False Nothing) { leHasHilbert = True })                  60  9 "Axiom"
  , GenesisStep 15 "DCT"         ((mkLibraryEntry "DCT"        0 []  False Nothing) { leHasTemporalOps = True })             105  8 "Synthesis"
  ]

-- ============================================
-- Atom Collection
-- ============================================

-- | ALL library atoms — uses the FULL library.
allAtoms :: Library -> [TypeExpr]
allAtoms lib =
  let libRefs = map (TRef . leName) lib
  in Set.toList $ Set.fromList $ [TUnit, TVoid] ++ libRefs

-- ============================================
-- Set-Based Enumeration (deduplicating)
-- ============================================

-- | Enumerate all types at depth <= maxD, deduplicated.
-- At depth <= 1: full enumeration (unary + binary operations).
-- At depth 2+: unary-only operations on depth-1 types.
-- After the main enumeration, extends with additional Omega iterations
-- to capture homotopy group contributions (pi_n detected via Omega^n chains).
enumBounded :: [TypeExpr] -> Library -> Int -> Set.Set TypeExpr
enumBounded atoms lib maxD =
  let -- Full enumeration at depths 0 and 1
      depth01 = Set.unions [Set.fromList (enumWindowExact atoms lib d) | d <- [0..min 1 maxD]]
      -- Unary-only extension at depth 2+
      unaryExtension
        | maxD >= 2 = applyUnaryOps lib depth01 (maxD - 1)
        | otherwise = Set.empty
      base = Set.union depth01 unaryExtension
      -- Omega chain extension: apply additional Omega iterations beyond base depth.
      -- This captures homotopy group information (pi_n requires Omega^n)
      -- without the combinatorial explosion of full depth-(n+1) enumeration.
      omegaExtended = applyOmegaOnly lib base (omegaExtraLevels lib maxD)
  in omegaExtended

-- | How many extra Omega levels to add beyond the base enumeration depth.
-- Based on the maximum path dimension in the library: types with n-dimensional
-- paths have interesting Omega^n chains. We add enough extra levels to reach
-- one beyond the max path dimension.
omegaExtraLevels :: Library -> Int -> Int
omegaExtraLevels lib maxD =
  let maxPathDim = maximum (0 : [d | entry <- lib, d <- lePathDims entry])
      -- We need Omega^maxPathDim. Base enumeration gives Omega^maxD.
      -- Extra levels needed: max(0, maxPathDim - maxD)
      -- Also add 1 extra level unconditionally if maxD >= 2 to capture
      -- Omega of depth-2 composites
      extra = max 0 (maxPathDim - maxD) + (if maxD >= 2 then 1 else 0)
  in min extra 3  -- cap at 3 extra levels for performance

-- | Apply only Omega to a set of types, iterating `levels` times.
-- Much cheaper than applyUnaryOps since it only adds one operation.
applyOmegaOnly :: Library -> Set.Set TypeExpr -> Int -> Set.Set TypeExpr
applyOmegaOnly _   types 0 = types
applyOmegaOnly lib types levels =
  let formers = availableFormers lib
      typeList = Set.toList types
      newOmegas
        | "Omega" `elem` formers = Set.fromList [TOmega t | t <- typeList]
        | otherwise = Set.empty
      combined = Set.union types newOmegas
  in applyOmegaOnly lib combined (levels - 1)

-- | Apply unary operations to a set of types, iterating `levels` times.
-- Each iteration applies all available unary formers to the current types.
applyUnaryOps :: Library -> Set.Set TypeExpr -> Int -> Set.Set TypeExpr
applyUnaryOps _   types 0 = types
applyUnaryOps lib types levels =
  let formers = availableFormers lib
      typeList = Set.toList types
      newTypes = Set.fromList $ concat
        [ [TOmega t | t <- typeList, "Omega" `elem` formers]
        , [TSusp t | t <- typeList, "Susp" `elem` formers]
        , [TTrunc 0 t | t <- typeList, "Trunc" `elem` formers]
        , [TFlat t | t <- typeList, "Flat" `elem` formers]
        , [TSharp t | t <- typeList, "Sharp" `elem` formers]
        , [TDisc t | t <- typeList, "Disc" `elem` formers]
        , [TPiCoh t | t <- typeList, "PiCoh" `elem` formers]
        , [TNext t | t <- typeList, "Next" `elem` formers]
        , [TEventually t | t <- typeList, "Eventually" `elem` formers]
        , [TInf t | t <- typeList, "Inf" `elem` formers]
        , [TTangent t | t <- typeList, "Tangent" `elem` formers]
        , [TConnection t | t <- typeList, "Connection" `elem` formers]
        , [TCurvature t | t <- typeList, "Curvature" `elem` formers]
        , [TMetric t | t <- typeList, "Metric" `elem` formers]
        , [THilbert t | t <- typeList, "Hilbert" `elem` formers]
        ]
      combined = Set.union types newTypes
  in applyUnaryOps lib combined (levels - 1)

-- | Enumerate and filter to inhabited types only.
inhabitedTypes :: [TypeExpr] -> Library -> Int -> Set.Set TypeExpr
inhabitedTypes atoms lib maxD =
  let allTypes = enumBounded atoms lib maxD
  in Set.filter (\t -> isInhabited (checkInhab t lib) && t /= TUnit && t /= TVoid) allTypes

-- ============================================
-- Core Algorithm: Before/After Comparison
-- ============================================

-- | Compute uniform nu at a single depth via before/after comparison.
--
-- For EVERY structure category (types, formers, modals, axioms):
--   1. Enumerate all inhabited types BEFORE adding X
--   2. Enumerate all inhabited types AFTER adding X
--   3. Take the set difference: new types = After \ Before
--   4. Schematize FIRST (before canonicalizing), then deep-collapse,
--      normalize, canonicalize, filter trivials, count
--
-- IMPORTANT: Schematization must happen before canonicalization.
-- Canonicalize has domain-specific rules (e.g., Susp(S1)→S2) that
-- destroy structural information needed for schema analysis.
-- By schematizing first, Susp(S1) → Susp(X), preserving the structure.
computeUniformNuAtDepth :: LibraryEntry -> Library -> Int
                        -> (Int, Int, [(TypeExpr, [TypeExpr])])
computeUniformNuAtDepth candidate lib maxDepth =
  let candidateName = leName candidate
      -- Library state BEFORE adding candidate
      atomsBefore = allAtoms lib
      libBefore = lib
      -- Library state AFTER adding candidate
      fullLib = lib ++ [candidate]
      atomsAfter = allAtoms fullLib

      -- Step 1: enumerate all inhabited types BEFORE
      !inhBefore = inhabitedTypes atomsBefore libBefore maxDepth

      -- Step 2: enumerate all inhabited types AFTER
      !inhAfter = inhabitedTypes atomsAfter fullLib maxDepth

      -- Step 3: new types = After \ Before
      -- These are types that became inhabited by adding X
      newTypes = Set.toList $ Set.difference inhAfter inhBefore

      -- Step 4: filter out unit/void
      nonTrivial = filter (\t -> t /= TUnit && t /= TVoid) newTypes
      rawCount = length nonTrivial

      -- Step 5: schematize → deep-collapse → normalize → canonicalize
      -- Schematize FIRST: library atoms → L, candidate → X
      -- This preserves structural information that canonicalize would destroy
      -- (e.g., Susp(S1) → Susp(X) instead of Susp(S1) → S2 → L)
      typeSchemas = [(t, canonicalize (normalize (deepSchemaize
                          (schemaize candidateName lib t))))
                    | t <- nonTrivial]
      schemaGroups = groupBySchema typeSchemas

      -- Step 6: filter trivial schemas
      nonTrivialSchemas = filter (not . isTrivialSchema . fst) schemaGroups

      -- Sort by group size descending
      sorted = sortOn (negate . length . snd) nonTrivialSchemas
      schemaCount = length sorted

  in (rawCount, schemaCount, sorted)

-- | Compute uniform nu up to maxDepth with per-depth breakdown.
-- Nu = schema count + former novelty + adjoint completion credit.
-- Former novelty counts new type formers unlocked by this step.
-- Adjoint completion (Lemma 9.4): every Introduction rule canonically
-- determines a corresponding Elimination rule.  Two components:
--   1. Dependent type former elimination: Pi→1 (app), Sigma→2 (fst,snd)
--   2. Base type elimination: if schema X exists but Omega(X) doesn't,
--      the type's eliminator is term-level and invisible to type inhabitation.
computeUniformNu :: LibraryEntry -> Library -> Int -> UniformNuResult
computeUniformNu candidate lib maxDepth =
  let perDepth = [ let (raw, sc, _) = computeUniformNuAtDepth candidate lib d
                   in (d, raw, sc)
                 | d <- [1..maxDepth] ]
      (rawFull, scFull, schemasFull) = computeUniformNuAtDepth candidate lib maxDepth
      -- Former novelty: count new type formers unlocked by this step
      formersBefore = availableFormers lib
      formersAfter  = availableFormers (lib ++ [candidate])
      newFormerNames = [f | f <- formersAfter, f `notElem` formersBefore]
      newFormers    = length newFormerNames

      -- Adjoint Completion (Lemma 9.4)
      -- Component 1: Dependent type former elimination arity.
      -- Pi-types have 1 elimination (application); Sigma-types have 2
      -- (fst, snd).  These are term-level and invisible to type inhabitation.
      depFormerCredit = sum [ formerElimArity f
                            | f <- newFormerNames
                            , f `elem` ["Pi", "Sigma"] ]

      -- Component 2: Base type elimination.
      -- If the candidate's bare schema "X" is detected (= the type is newly
      -- inhabited) but no homotopy schema "Omega(X)" exists, the type's
      -- recursor/eliminator is term-level and invisible to the comparison.
      schemaExprs = map fst schemasFull
      hasSchemaX  = TRef "X" `elem` schemaExprs
      hasOmegaX   = TOmega (TRef "X") `elem` schemaExprs
      baseTypeCredit = if hasSchemaX && not hasOmegaX then 1 else 0

      adjointCredit = depFormerCredit + baseTypeCredit
      totalNu = scFull + newFormers + adjointCredit
  in UniformNuResult
    { unrStep          = 0
    , unrName          = leName candidate
    , unrPaperNu       = 0
    , unrUniformNu     = totalNu
    , unrSchemaCount   = scFull
    , unrRawNew        = rawFull
    , unrSchemas       = schemasFull
    , unrPerDepth      = perDepth
    , unrOrdering      = ""
    , unrAdjointCredit = adjointCredit
    }

-- | Elimination arity of a dependent type former.
-- Sigma has 2 projections (fst, snd); all others have 1 elimination.
formerElimArity :: String -> Int
formerElimArity "Sigma" = 2   -- two projections: fst, snd
formerElimArity _       = 1   -- one elimination: application, recursion, etc.

-- ============================================
-- Full 15-Step Evaluation
-- ============================================

uniformNuAllSteps :: Int -> [UniformNuResult]
uniformNuAllSteps maxDepth = go [] genesisLibrarySteps
  where
    go _ [] = []
    go lib (step:rest) =
      let candidate = gsEntry step
          result0 = computeUniformNu candidate lib maxDepth
          nu = unrUniformNu result0
          kappa = gsPaperK step
          n = gsStep step
          rho = if kappa > 0 then fromIntegral nu / fromIntegral kappa else 0 :: Double
          bar = computeBar n
          ordering
            | n <= 1 = "OK (first step)"
            | rho >= bar = "OK (rho=" ++ showF 2 rho ++ " >= bar=" ++ showF 2 bar ++ ")"
            | otherwise  = "FAIL (rho=" ++ showF 2 rho ++ " < bar=" ++ showF 2 bar ++ ")"
          result = result0
            { unrStep    = gsStep step
            , unrName    = gsName step
            , unrPaperNu = gsPaperNu step
            , unrOrdering = ordering
            }
          newLib = lib ++ [candidate]
      in result : go newLib rest

    fib :: Int -> Int
    fib 1 = 1
    fib 2 = 1
    fib n = fib (n-1) + fib (n-2)

    -- | Compute bar using paper values for consistent comparison.
    computeBar :: Int -> Double
    computeBar n
      | n <= 2 = 0.5
      | otherwise =
        let delta_n = fib n
            delta_nm1 = fib (n-1)
            phi_n = fromIntegral delta_n / fromIntegral delta_nm1 :: Double
            priorSteps = take (n-1) genesisLibrarySteps
            sumNu = sum [gsPaperNu s | s <- priorSteps]
            sumK  = sum [gsPaperK s | s <- priorSteps]
            omega = if sumK > 0 then fromIntegral sumNu / fromIntegral sumK else 1.0
        in phi_n * omega

    showF :: Int -> Double -> String
    showF d x = let m = 10 ^ d :: Int
                    r = fromIntegral (round (x * fromIntegral m) :: Int) / fromIntegral m :: Double
                in show r

-- ============================================
-- Deep Schematization
-- ============================================

-- | Collapse derivable subexpressions to L.
-- Applied after schematize to eliminate redundant depth-2+ schemas.
--
-- Three collapse rules, applied bottom-up:
--
-- Rule 1 (Basic algebra collapse): A subexpression built only from basic type
-- algebra (Arrow, Prod, Coprod, Pi, Sigma, SelfId, Id) on {X, L, 1, 0}
-- at depth > 0 collapses to L. The novelty comes from the OPERATION, not
-- the specific trivial argument.
--   Example: Susp(L → X) → Susp(L)
--
-- Rule 2 (X-free child collapse): Any child that is X-free and has depth > 0
-- collapses to L. Such children represent existing library constructions whose
-- internal structure is irrelevant to novelty. This prevents depth-2
-- compositions of structural operations from being overcounted.
--   Example: flat(Susp(L)) → flat(L), Susp(flat(L)) → Susp(L)
--   But:     flat(X) stays (X is depth 0)
--            flat(Susp(X)) stays (contains X)
--
-- Rule 3 (Basic algebra top-level): After child collapse, if the entire
-- expression is built from basic ops at depth > 0, collapse to L.
deepSchemaize :: TypeExpr -> TypeExpr
deepSchemaize t =
  let t' = mapChildren deepSchemaize t  -- bottom-up: collapse children first
      t'' = mapChildren collapseXFreeChild t'  -- Rule 2: collapse X-free deep children
  in case t'' of
    -- Rule 3: basic algebra depth>0 → L
    _ | builtFromBasicOps t'' && depth t'' > 0 -> TRef "L"
    _ -> t''

-- | Collapse an X-free subexpression at depth > 0 to L.
-- Used to simplify children whose internal structure is library-derivable.
collapseXFreeChild :: TypeExpr -> TypeExpr
collapseXFreeChild t
  | noX t && depth t > 0 = TRef "L"
  | otherwise = t

-- | Check if a type expression is X-free (doesn't contain TRef "X").
-- Used to identify subexpressions that represent existing library constructions.
noX :: TypeExpr -> Bool
noX (TRef "X") = False
noX (TRef _)   = True
noX TUnit      = True
noX TVoid      = True
noX other      = all noX (getChildren other)

-- | Get immediate children of a type expression.
getChildren :: TypeExpr -> [TypeExpr]
getChildren (TArrow a b)     = [a, b]
getChildren (TProd a b)      = [a, b]
getChildren (TCoprod a b)    = [a, b]
getChildren (TId a x y)     = [a, x, y]
getChildren (TSelfId a)      = [a]
getChildren (TOmega a)       = [a]
getChildren (TSusp a)        = [a]
getChildren (TTrunc _ a)     = [a]
getChildren (TPi _ a b)     = [a, b]
getChildren (TSigma _ a b)  = [a, b]
getChildren (TFiber a b)    = [a, b]
getChildren (TDeloop a)      = [a]
getChildren (TFlat a)        = [a]
getChildren (TSharp a)       = [a]
getChildren (TDisc a)        = [a]
getChildren (TPiCoh a)       = [a]
getChildren (TNext a)        = [a]
getChildren (TEventually a)  = [a]
getChildren (TInf a)         = [a]
getChildren (TTangent a)     = [a]
getChildren (TConnection a)  = [a]
getChildren (TCurvature a)   = [a]
getChildren (TMetric a)      = [a]
getChildren (THilbert a)     = [a]
getChildren _                = []

-- | Check if a type expression uses only basic type algebra.
builtFromBasicOps :: TypeExpr -> Bool
builtFromBasicOps TUnit            = True
builtFromBasicOps TVoid            = True
builtFromBasicOps (TRef _)         = True  -- X, L, or any ref
builtFromBasicOps (TArrow a b)     = builtFromBasicOps a && builtFromBasicOps b
builtFromBasicOps (TProd a b)      = builtFromBasicOps a && builtFromBasicOps b
builtFromBasicOps (TCoprod a b)    = builtFromBasicOps a && builtFromBasicOps b
builtFromBasicOps (TSelfId a)      = builtFromBasicOps a
builtFromBasicOps (TId a x y)      = builtFromBasicOps a && builtFromBasicOps x
                                      && builtFromBasicOps y
builtFromBasicOps (TPi _ a b)      = builtFromBasicOps a && builtFromBasicOps b
builtFromBasicOps (TSigma _ a b)   = builtFromBasicOps a && builtFromBasicOps b
builtFromBasicOps _                = False  -- structural operations are NOT basic

-- ============================================
-- Helpers
-- ============================================

groupBySchema :: [(TypeExpr, TypeExpr)] -> [(TypeExpr, [TypeExpr])]
groupBySchema pairs =
  let schemas = Set.toList $ Set.fromList $ map snd pairs
      groups = [(s, [t | (t, s') <- pairs, s' == s]) | s <- schemas]
  in groups
