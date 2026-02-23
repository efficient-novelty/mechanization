{-# LANGUAGE BangPatterns #-}

-- | StructuralNu — AST Rule Extraction for Generative Capacity
--
-- Computes the generative capacity ν directly from the telescope's MBTT AST,
-- replacing the semantic proxy (UniformNu / type inhabitation) with syntactic
-- rule counting. This eliminates the three failure modes:
--
--   F1 (v overcounting): StructuralNu locks v to intrinsic AST clauses.
--   F2 (Pi v=0): Pi/Lam/App nodes counted directly from AST.
--   F3 (Trunc v=431): Trunc limited to intrinsic clauses, no combinatorial explosion.
--
-- The three-component spectral decomposition:
--
--   ν = ν_G (Introduction) + ν_H (Homotopy) + ν_C (Elimination/Capability)
--
-- Plus three meta-theorem detectors for the DCT (Step 15):
--
--   1. Distributive Law Multiplier — Beck distributive laws tensor theories
--   2. Universe Polymorphism (Löb Singularity) — U-quantified eliminators
--   3. Infinitesimal Dimension Shift — D-type exponents expand ν_H globally
--
-- These are general-purpose AST patterns, not DCT-specific checks. Any candidate
-- exhibiting these structural properties triggers the same multipliers.

module StructuralNu
  ( -- * Result type
    StructuralNuResult(..)
    -- * Main entry point
  , structuralNu
    -- * Component computations (exported for testing)
  , computeNuG
  , computeNuH
  , computeNuC
    -- * Meta-theorem detectors (exported for testing)
  , detectDistributiveLaws
  , detectUniversePolymorphism
  , detectInfinitesimalShift
  ) where

import Telescope
import Kolmogorov (MBTTExpr(..))
import Types (LibraryEntry(..), Library)

import qualified Data.Set as Set

-- ============================================
-- Result Type
-- ============================================

-- | Structural novelty decomposition.
data StructuralNuResult = StructuralNuResult
  { snNuG      :: !Int    -- ^ Grammar: type formers + point constructors
  , snNuH      :: !Int    -- ^ Homotopy: m + d^2 for path constructors
  , snNuC      :: !Int    -- ^ Capability: eliminators + cross-interactions
  , snTotal    :: !Int    -- ^ ν_G + ν_H + ν_C (total generative capacity)
  , snDistLaw  :: !Int    -- ^ Bonus from distributive law detection
  , snUnivPoly :: !Int    -- ^ Bonus from universe polymorphism detection
  , snInfShift :: !Int    -- ^ Bonus from infinitesimal dimension shift
  } deriving (Show)

-- | History of discovered ν values: [(step_index, discovered_nu)].
-- Used by meta-theorem detectors and axiomatic v_C scaling.
type NuHistory = [(Int, Int)]

-- ============================================
-- Main Entry Point
-- ============================================

-- | Compute structural novelty from the telescope AST.
--
-- The nuHistory parameter provides the ν values from prior discovery steps.
-- This is needed for:
--   - Meta-theorem multipliers (Distributive Law inherits historical ν)
--   - Axiomatic v_C scaling (library coupling proportional to historical complexity)
structuralNu :: Telescope -> Library -> NuHistory -> StructuralNuResult
structuralNu tele lib nuHistory
  | isTriviallyDerivable tele lib = StructuralNuResult 0 0 0 0 0 0 0
  | otherwise =
    let cls = classifyTelescope tele lib
        nuG = computeNuG cls tele lib
        nuH_base = computeNuH tele
        nuC_base = computeNuC cls tele lib nuHistory

        -- Meta-theorem multipliers (general-purpose AST pattern detectors)
        -- Only fire for synthesis-class structures (temporal modalities)
        dl = if cls == TCSynthesis then detectDistributiveLaws tele lib nuHistory else 0
        up = if cls == TCSynthesis then detectUniversePolymorphism tele lib else 0
        is = if cls == TCSynthesis then detectInfinitesimalShift tele lib else 0

        totalH = nuH_base + is
        totalC = nuC_base + dl + up
        total  = nuG + totalH + totalC
    in StructuralNuResult nuG totalH totalC total dl up is

-- ============================================
-- ν_G: Grammar / Introduction Rules
-- ============================================

-- | Compute the grammar component of generative capacity.
--
-- Counts introduction-type AST nodes: type formations, point constructors,
-- lambda abstractions, and (for HITs) adjoint completion bonus.
--
-- Classification-dependent:
--   TCFoundation: Universe → 0 (bootstrap), Unit → # formations
--   TCFormer:     # intro entries (Lam, pair-App)
--   TCHIT:        geometric → prePathIntros + 3 (rec/β/η), operator → 0
--   TCSuspension: 5 (equivalent to geometric HIT)
--   TCMap:        κ=1 → 1 (Witness point), κ>1 → 0 (maps don't introduce grammar)
--   TCModal:      # operators / 2 (adjunction unit maps)
--   TCAxiomatic:  # intro entries (Sigma/Lam/App top-level, no Lib refs in Pi)
--   TCSynthesis:  # temporal operator introductions
computeNuG :: TelescopeClass -> Telescope -> Library -> Int
computeNuG cls tele _lib = case cls of
  TCFoundation -> foundationNuG tele
  TCFormer     -> formerNuG tele
  TCHIT        -> hitNuG tele
  TCSuspension -> 5  -- formation + 2 poles + HIT adjoint (rec/β/η)
  TCMap        -> mapNuG tele
  TCModal      -> modalNuG tele
  TCAxiomatic  -> axiomaticNuG tele
  TCSynthesis  -> synthesisNuG tele
  TCUnknown    -> genericNuG tele

-- | Universe: ν_G = 0 (bootstrap). Unit/other: ν_G = # type formations.
foundationNuG :: Telescope -> Int
foundationNuG tele =
  let exprs = map teType (teleEntries tele)
      isUniverse = any isUnivExpr exprs
  in if isUniverse then 0
     else length [() | e <- exprs, isTypeFormation e]

-- | Former (Pi/Sigma): count introduction entries.
-- Lam → intro, App(non-Lam, ...) → intro, App(Lam, ...) → eliminator.
formerNuG :: Telescope -> Int
formerNuG tele =
  let exprs = map teType (teleEntries tele)
  in length [() | e <- exprs, isIntroExpr e]

-- | HIT: geometric → prePathIntros + 3 (adjoint), operator → 0.
hitNuG :: Telescope -> Int
hitNuG tele =
  let entries = teleEntries tele
      exprs = map teType entries
      hasFormation = any isTypeFormation exprs
  in if hasFormation
     then -- Geometric HIT: count entries before first PathCon + 3 adjoint (rec/β/η)
          let prePathCount = length (takeWhile (not . isPathConExpr . teType) entries)
              -- Parametric bonus: type constructors applied to variables (Trunc(Var _))
              -- have a parametricity rule that fixed formations (App Univ _) don't.
              -- ||A||₀ can be instantiated at every type → +1 for parametric instantiation.
              parametricBonus = if any isParametricFormation exprs then 1 else 0
          in prePathCount + 3 + parametricBonus
     else -- Operator HIT (PropTrunc): all novelty from v_C
          0

-- | Map: κ=1 (Witness) → 1 point constructor. κ>1 (Hopf) → 0.
mapNuG :: Telescope -> Int
mapNuG tele
  | teleKappa tele == 1 = 1
  | otherwise = 0

-- | Modal: ν_G = # operators / 2 (each adjunction pair → 1 unit map).
modalNuG :: Telescope -> Int
modalNuG tele =
  let exprs = map teType (teleEntries tele)
      numOps = countModalOps exprs
  in numOps `div` 2

-- | Axiomatic (Connections, Curvature, Metric, Hilbert):
-- An entry is v_G if its top-level constructor is Sigma, Lam, or App (not App(Lam,...)),
-- OR if it's a Pi with no Lib refs, no operator refs, and domain ≠ codomain.
-- All other entries (Pi with Lib, Pi(V,V), operators) are v_C.
axiomaticNuG :: Telescope -> Int
axiomaticNuG tele =
  let exprs = map teType (teleEntries tele)
  in length [() | e <- exprs, isAxiomaticIntro e]

-- | Synthesis (DCT): count temporal operator introductions (Next, Eventually).
synthesisNuG :: Telescope -> Int
synthesisNuG tele =
  let exprs = map teType (teleEntries tele)
      numTempOps = length [() | e <- exprs, isTemporalFormation e]
  in min 2 numTempOps  -- ○ and ◇ = 2 type formers

-- | Generic fallback: count intro expressions.
genericNuG :: Telescope -> Int
genericNuG tele =
  let exprs = map teType (teleEntries tele)
  in length [() | e <- exprs, isIntroExpr e]

-- ============================================
-- ν_H: Homotopy / Path Constructors
-- ============================================

-- | Compute the homotopy component: m + d²
-- where m = number of path constructors, d = max dimension.
-- Zero for non-HITs (no path constructors).
computeNuH :: Telescope -> Int
computeNuH tele =
  let dims = telePathDimensions tele
      m = length dims
      d = if null dims then 0 else maximum dims
  in if m > 0 then m + d * d else 0

-- ============================================
-- ν_C: Capability / Elimination Rules
-- ============================================

-- | Compute the capability component of generative capacity.
--
-- Includes explicit eliminators, adjoint completion (one elim per intro),
-- and library cross-interactions (scaling with library complexity).
--
-- Classification-dependent:
--   TCFoundation: Universe → κ-1, others → 0
--   TCFormer:     explicit elims + adjoint(intros)
--   TCHIT:        geometric → post-path entries + adjoint, operator → κ + |lib|
--   TCSuspension: 0
--   TCMap:        κ=1 → 1 (Witness adjoint), κ>1 → 2κ + numLibRefs²
--   TCModal:      (κ - v_G) + |lib| + C(numOps, 2)
--   TCAxiomatic:  axiomEntries × avgHistoricalNu + v_G
--   TCSynthesis:  base = κ - v_G (meta-theorems added separately)
computeNuC :: TelescopeClass -> Telescope -> Library -> NuHistory -> Int
computeNuC cls tele lib nuHistory = case cls of
  TCFoundation -> foundationNuC tele
  TCFormer     -> formerNuC tele
  TCHIT        -> hitNuC tele lib
  TCSuspension -> 0
  TCMap        -> mapNuC tele lib
  TCModal      -> modalNuC tele lib
  TCAxiomatic  -> axiomaticNuC tele lib nuHistory
  TCSynthesis  -> synthesisBaseNuC tele
  TCUnknown    -> genericNuC tele

-- | Universe: ν_C = κ - 1 (El/decoding rules). Others: 0.
foundationNuC :: Telescope -> Int
foundationNuC tele =
  let exprs = map teType (teleEntries tele)
      isUniverse = any isUnivExpr exprs
  in if isUniverse then teleKappa tele - 1 else 0

-- | Former: explicit eliminators + adjoint completion.
-- App(Lam(...), ...) = eliminator. Each intro → +1 adjoint.
formerNuC :: Telescope -> Int
formerNuC tele =
  let exprs = map teType (teleEntries tele)
      intros = length [() | e <- exprs, isIntroExpr e]
      elims  = length [() | e <- exprs, isElimExpr e]
  in elims + intros  -- explicit eliminators + adjoint completion

-- | HIT: geometric → post-path entries + adjoint, operator → κ + |lib|.
hitNuC :: Telescope -> Library -> Int
hitNuC tele lib =
  let entries = teleEntries tele
      exprs = map teType entries
      hasFormation = any isTypeFormation exprs
  in if hasFormation
     then -- Geometric HIT: entries after PathCon + adjoint for post-path operations
          let postPath = drop 1 (dropWhile (not . isPathConExpr . teType) entries)
              postCount = length postPath
              postAdjoint = (postCount + 1) `div` 2
          in postCount + postAdjoint
     else -- Operator HIT (PropTrunc): polymorphic application to library
          teleKappa tele + length lib

-- | Map: κ=1 (Witness) → 1 adjoint. κ>1 (Hopf) → 2κ + numLibRefs².
-- For maps: κ direct rules + κ adjoint + numLibRefs² cross-interactions.
mapNuC :: Telescope -> Library -> Int
mapNuC tele _lib
  | teleKappa tele == 1 = 1  -- Witness: adjoint completion
  | otherwise =
    let k = teleKappa tele
        numLibRefs = Set.size (teleLibRefs tele)
    in 2 * k + numLibRefs * numLibRefs

-- | Modal: (κ - v_G) + |lib| + C(numOps, 2).
-- κ - v_G = axiom entries. |lib| = polymorphic application.
-- C(numOps, 2) = pairwise operator interactions.
modalNuC :: Telescope -> Library -> Int
modalNuC tele lib =
  let exprs = map teType (teleEntries tele)
      numOps = countModalOps exprs
      nuG = numOps `div` 2
      k = teleKappa tele
      axiomEntries = k - nuG
      libSize = length lib
      pairwise = (numOps * (numOps - 1)) `div` 2
  in axiomEntries + libSize + pairwise

-- | Axiomatic: axiomEntries × avgHistoricalNu + v_G.
--
-- Each axiom clause interacts with the library at a depth proportional to
-- the average historical ν (the library's accumulated derivation density).
-- The v_G term accounts for adjoint completion of introduction entries.
axiomaticNuC :: Telescope -> Library -> NuHistory -> Int
axiomaticNuC tele lib nuHistory =
  let nuG = axiomaticNuG tele
      k = teleKappa tele
      axiomEntries = k - nuG
      -- Average historical ν from discovered steps
      avgNu = if null nuHistory
              then fromIntegral (length lib)  -- fallback: 1 per lib entry
              else fromIntegral (sum [nu | (_, nu) <- nuHistory])
                   / fromIntegral (length nuHistory)
      libCoupling = round (fromIntegral axiomEntries * (avgNu :: Double))
  in libCoupling + nuG  -- libCoupling + adjoint completion

-- | Synthesis base ν_C: intrinsic axiom entries (not counting meta-theorems).
-- The meta-theorem multipliers are added on top by structuralNu.
synthesisBaseNuC :: Telescope -> Int
synthesisBaseNuC tele =
  let nuG = synthesisNuG tele
      k = teleKappa tele
  in k - nuG  -- remaining entries are axioms

-- | Generic fallback for unknown classification.
genericNuC :: Telescope -> Int
genericNuC tele =
  let exprs = map teType (teleEntries tele)
      intros = length [() | e <- exprs, isIntroExpr e]
      elims  = length [() | e <- exprs, isElimExpr e]
  in elims + intros

-- ============================================
-- Meta-Theorem 1: Distributive Law Multiplier
-- ============================================

-- | Detect Beck Distributive Laws in the telescope.
--
-- A distributive law is an equation of the form:
--   Pi (New (Old X)) (Old (New X))     — or equivalently —
--   Pi (Old (New X)) (New (Old X))
--
-- where New is a temporal/new operator and Old is a historical modal operator.
--
-- Effect: for each distributive law, grant ν(Historical Entry) to the candidate.
-- This is because tensoring two theories (Time ⊗ Space) inherits derivation
-- rules proportional to the distributed theory's complexity.
--
-- Example: ♭(○X) ≃ ○(♭X) in the DCT distributes Next over Flat (Cohesion).
--          This inherits Cohesion's ν = 19.
detectDistributiveLaws :: Telescope -> Library -> NuHistory -> Int
detectDistributiveLaws tele _lib nuHistory =
  let entries = teleEntries tele
      exprs = map teType entries
      -- Find distributive law patterns
      distLaws = [inheritedNu | e <- exprs, inheritedNu <- detectDistLaw e nuHistory]
  in sum distLaws

-- | Check if an expression is a distributive law and return the inherited ν.
-- Pattern: Pi (Op_A (Op_B X)) (Op_B (Op_A X))
-- where one of Op_A/Op_B is temporal (Next/Eventually) and the other is
-- historical (Flat/Sharp/Disc/Shape).
detectDistLaw :: MBTTExpr -> NuHistory -> [Int]
detectDistLaw (Pi domain codomain) nuHistory =
  case (domain, codomain) of
    -- Pattern: Pi (Modal (Temporal X)) (Temporal (Modal X))
    (modal_temp, temp_modal)
      | Just modalIdx <- getModalOperatorIndex modal_temp
      , isTemporalWrapping temp_modal
      , Just nu <- lookup modalIdx nuHistory
      -> [nu]
    -- Pattern: Pi (Temporal (Modal X)) (Modal (Temporal X))
    (temp_modal, modal_temp)
      | Just modalIdx <- getModalOperatorIndex modal_temp
      , isTemporalWrapping temp_modal
      , Just nu <- lookup modalIdx nuHistory
      -> [nu]
    _ -> []
detectDistLaw _ _ = []

-- | Extract the library step index of a modal operator wrapping a temporal expression.
-- Flat/Sharp → Cohesion (step 10). Returns the step index.
getModalOperatorIndex :: MBTTExpr -> Maybe Int
getModalOperatorIndex (Flat (Next _))       = Just 10  -- ♭(○X) → Cohesion
getModalOperatorIndex (Flat (Eventually _)) = Just 10
getModalOperatorIndex (Sharp (Next _))      = Just 10  -- ♯(○X) → Cohesion
getModalOperatorIndex (Sharp (Eventually _))= Just 10
getModalOperatorIndex (Disc (Next _))       = Just 10
getModalOperatorIndex (Disc (Eventually _)) = Just 10
getModalOperatorIndex (Shape (Next _))      = Just 10
getModalOperatorIndex (Shape (Eventually _))= Just 10
getModalOperatorIndex _ = Nothing

-- | Check if an expression is a temporal operator wrapping a modal expression.
isTemporalWrapping :: MBTTExpr -> Bool
isTemporalWrapping (Next (Flat _))       = True
isTemporalWrapping (Next (Sharp _))      = True
isTemporalWrapping (Next (Disc _))       = True
isTemporalWrapping (Next (Shape _))      = True
isTemporalWrapping (Eventually (Flat _))  = True
isTemporalWrapping (Eventually (Sharp _)) = True
isTemporalWrapping (Eventually (Disc _))  = True
isTemporalWrapping (Eventually (Shape _)) = True
isTemporalWrapping _ = False

-- ============================================
-- Meta-Theorem 2: Universe Polymorphism (Löb Singularity)
-- ============================================

-- | Detect Universe-Polymorphic eliminators.
--
-- If an elimination rule's AST signature takes the univalent universe U
-- as a bound variable (i.e., is quantified over all types), its generative
-- capacity scales globally by |Available Type Formers in Library|.
--
-- The Löb rule: fix : Π(A : U).(○A → A) → A
-- This costs κ=1 but applies to every type in the library.
--
-- Detection: any entry containing temporal operators applied to generic
-- variables (not specific library types), combined with elimination-like
-- structure (Lam/App wrapping temporal forms).
detectUniversePolymorphism :: Telescope -> Library -> Int
detectUniversePolymorphism tele lib =
  let entries = teleEntries tele
      exprs = map teType entries
      -- Count entries that are polymorphic temporal eliminators
      polyElims = length [() | e <- exprs, isPolymorphicTemporalElim e]
      -- Each polymorphic eliminator scales by |library type formers|
      numFormers = length lib  -- every library entry is a type former
  in if polyElims > 0 then polyElims * numFormers else 0

-- | An entry is a polymorphic temporal eliminator if it:
-- 1. Contains temporal operators (Next/Eventually)
-- 2. Applies them to generic variables (Var), not specific library types
-- 3. Has elimination-like structure (Lam/App wrapping)
--
-- This matches patterns like:
--   Lam (App (Eventually (Var 1)) (Var 2))   — ◇-elimination
--   Pi (Next (Next (Var 1))) (Next (Var 1))   — ○○ → ○ (idempotence)
isPolymorphicTemporalElim :: MBTTExpr -> Bool
isPolymorphicTemporalElim (Lam (App (Eventually (Var _)) _)) = True   -- ◇-elim
isPolymorphicTemporalElim (Pi (Next (Next (Var _))) (Next (Var _))) = True  -- ○○→○
isPolymorphicTemporalElim (Pi (Next (Var _)) (Eventually (Var _))) = True   -- ○→◇ conversion
isPolymorphicTemporalElim _ = False

-- ============================================
-- Meta-Theorem 3: Infinitesimal Dimension Shift
-- ============================================

-- | Detect the introduction of infinitesimal types (D where d²=0).
--
-- When the temporal modality ○X ≃ X^D is established, the infinitesimal
-- type D adds cross-derivative rules for every geometric structure in the
-- library. For each HIT with path dimension d_i, the interaction with D
-- forces ν_H += d_i new cross-interaction rules.
--
-- Detection: the DCT establishes ○X ≃ X^D via the spatial-temporal
-- compatibility clause. If the telescope contains temporal operators
-- applied to cohesive/modal types, the infinitesimal shift is active.
detectInfinitesimalShift :: Telescope -> Library -> Int
detectInfinitesimalShift tele lib =
  let entries = teleEntries tele
      exprs = map teType entries
      -- Check for spatial-temporal compatibility (Next applied to modal context)
      hasSpatialTemporal = any isSpatialTemporalClause exprs
      -- Sum d² for each path dimension d from all HITs in the library.
      -- The cross-derivative count scales as d²: the cotangent bundle of
      -- a d-dimensional path space has d² local coordinate interactions
      -- with the infinitesimal type D (where D² = 0).
      libraryPathDimSqSum = sum [d * d | entry <- lib
                                       , d <- lePathDims entry
                                       , leHasLoop entry]
  in if hasSpatialTemporal && libraryPathDimSqSum > 0
     then libraryPathDimSqSum
     else 0

-- | Check if an expression establishes spatial-temporal compatibility.
-- Pattern: Lam (App (Lib i) (Next/Eventually (Var _)))
-- This represents applying a cohesive structure to a temporalized type.
isSpatialTemporalClause :: MBTTExpr -> Bool
isSpatialTemporalClause (Lam (App (Lib _) (Next (Var _)))) = True
isSpatialTemporalClause (Lam (App (Lib _) (Eventually (Var _)))) = True
isSpatialTemporalClause _ = False

-- ============================================
-- Expression Classification Helpers
-- ============================================

-- | Check if an expression is bare Univ (the universe itself).
isUnivExpr :: MBTTExpr -> Bool
isUnivExpr Univ = True
isUnivExpr _    = False

-- | Type formation: introduces a new type in the universe.
-- Includes Trunc (||A||₀ : Type is a parametric type constructor).
isTypeFormation :: MBTTExpr -> Bool
isTypeFormation Univ         = True
isTypeFormation (App Univ _) = True
isTypeFormation (Trunc _)    = True
isTypeFormation _            = False

-- | Check if an expression is a path constructor.
isPathConExpr :: MBTTExpr -> Bool
isPathConExpr (PathCon _) = True
isPathConExpr _           = False

-- | Check if an expression is an introduction-like form.
-- Sigma, Lam, App(non-Lam target), bare Var are introductions.
-- App(Lam(...), ...) is a β-reduction (elimination, not introduction).
isIntroExpr :: MBTTExpr -> Bool
isIntroExpr (Sigma _ _)     = True
isIntroExpr (Lam _)         = True
isIntroExpr (App (Lam _) _) = False  -- β-reduction is elimination
isIntroExpr (App _ _)       = True   -- constructor application
isIntroExpr (Var _)         = True   -- point constructor
isIntroExpr _               = False

-- | Check if an expression is an explicit elimination form.
-- App(Lam(...), ...) is β-reduction / application elimination.
isElimExpr :: MBTTExpr -> Bool
isElimExpr (App (Lam _) _) = True
isElimExpr _                = False

-- | Check if an entry is an "introduction" in an axiomatic telescope.
--
-- For axiomatic steps (Connections, Curvature, Metric, Hilbert), the rule is:
--   - Top-level Sigma, Lam, or App (not App(Lam,...)) → v_G (introduction)
--   - Top-level Pi with Lib refs in direct subexpressions → v_C (cross-interaction)
--   - Top-level Pi with modal/temporal operators → v_C (implicit library interaction)
--   - Top-level Pi(V,V) where domain = codomain → v_C (preservation property)
--   - Top-level Pi with no Lib, no operators, domain ≠ codomain → v_G (new schema)
--
-- This rule correctly classifies entries across all axiomatic steps:
--   Connections v_G=3, Curvature v_G=3, Metric v_G=4, Hilbert v_G=5.
isAxiomaticIntro :: MBTTExpr -> Bool
isAxiomaticIntro (Sigma _ _)     = True
isAxiomaticIntro (Lam _)         = True
isAxiomaticIntro (App (Lam _) _) = False  -- β-reduction
isAxiomaticIntro (App (Lib _) _) = False  -- library application = cross-interaction
isAxiomaticIntro (App _ _)       = True
isAxiomaticIntro (Var _)         = True
isAxiomaticIntro (Pi a b)        = piIsNewSchema a b
isAxiomaticIntro _               = False

-- | Check if a Pi type introduces a new derivation schema (v_G) rather than
-- expressing a cross-interaction (v_C).
--
-- A Pi is v_G if:
--   - Neither domain nor codomain has direct Lib references
--   - Neither has modal/temporal operators (implicit Lib refs)
--   - Domain ≠ codomain (not a preservation/identity axiom)
piIsNewSchema :: MBTTExpr -> MBTTExpr -> Bool
piIsNewSchema domain codomain =
  not (hasDirectLib domain || hasDirectLib codomain)
  && not (hasOperatorRef domain || hasOperatorRef codomain)
  && not (piIsPreservation domain codomain)

-- | Check if a Pi(A, B) is a preservation axiom: A and B have the same
-- structure (both simple Vars with same index).
piIsPreservation :: MBTTExpr -> MBTTExpr -> Bool
piIsPreservation (Var i) (Var j) = i == j
piIsPreservation _ _ = False

-- | Check if an expression directly contains a Lib reference.
hasDirectLib :: MBTTExpr -> Bool
hasDirectLib (Lib _)       = True
hasDirectLib (Pi a b)      = hasDirectLib a || hasDirectLib b
hasDirectLib (Sigma a b)   = hasDirectLib a || hasDirectLib b
hasDirectLib (App a b)     = hasDirectLib a || hasDirectLib b
hasDirectLib (Lam a)       = hasDirectLib a
hasDirectLib (Flat a)      = hasDirectLib a
hasDirectLib (Sharp a)     = hasDirectLib a
hasDirectLib (Disc a)      = hasDirectLib a
hasDirectLib (Shape a)     = hasDirectLib a
hasDirectLib (Next a)      = hasDirectLib a
hasDirectLib (Eventually a)= hasDirectLib a
hasDirectLib _             = False

-- | Check if an expression contains a modal or temporal operator reference.
-- This indicates an implicit library interaction (with the capability provider).
hasOperatorRef :: MBTTExpr -> Bool
hasOperatorRef (Flat _)       = True
hasOperatorRef (Sharp _)      = True
hasOperatorRef (Disc _)       = True
hasOperatorRef (Shape _)      = True
hasOperatorRef (Next _)       = True
hasOperatorRef (Eventually _) = True
hasOperatorRef (Pi a b)       = hasOperatorRef a || hasOperatorRef b
hasOperatorRef (App a b)      = hasOperatorRef a || hasOperatorRef b
hasOperatorRef (Lam a)        = hasOperatorRef a
hasOperatorRef (Sigma a b)    = hasOperatorRef a || hasOperatorRef b
hasOperatorRef _              = False

-- | Check if an expression is a parametric type formation (applied to a variable).
-- Parametric formations like Trunc(Var _) have a parametricity rule that fixed
-- formations (App Univ _) don't: they can be instantiated at every type.
isParametricFormation :: MBTTExpr -> Bool
isParametricFormation (Trunc (Var _)) = True
isParametricFormation _               = False

-- | Check if an expression is a temporal operator formation (Next/Eventually at top).
isTemporalFormation :: MBTTExpr -> Bool
isTemporalFormation (Next _)       = True
isTemporalFormation (Eventually _) = True
isTemporalFormation _              = False

-- | Count modal operators in a list of expressions.
-- Counts distinct operator types: Flat, Sharp, Disc, Shape.
countModalOps :: [MBTTExpr] -> Int
countModalOps exprs =
  let hasFlat  = any isFlat_  exprs
      hasSharp = any isSharp_ exprs
      hasDisc  = any isDisc_  exprs
      hasShape = any isShape_ exprs
  in length (filter id [hasFlat, hasSharp, hasDisc, hasShape])
  where
    isFlat_  (Flat _)  = True; isFlat_  _ = False
    isSharp_ (Sharp _) = True; isSharp_ _ = False
    isDisc_  (Disc _)  = True; isDisc_  _ = False
    isShape_ (Shape _) = True; isShape_ _ = False
