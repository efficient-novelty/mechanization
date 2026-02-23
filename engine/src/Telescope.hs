{-# LANGUAGE BangPatterns #-}

-- | Context Telescopes — the universal representation for mathematical structures
--
-- In dependent type theory, every structure — from S¹ to the Riemannian Metric —
-- can be represented identically as a Context Telescope: an ordered sequence of
-- interdependent typing declarations extending the library B.
--
--   Δ = [c₁ : A₁, c₂ : A₂(c₁), ..., cκ : Aκ(c₁...cκ₋₁)]
--
-- The Genetic Code: we use the MBTT grammar from Appendix C (implemented in
-- Kolmogorov.hs) as the universal AST. The algorithm's only job is to generate
-- well-typed telescopes Δ of length κ ≤ H that maximize ρ = ν/κ.
--
-- The engine no longer needs to know what a "HIT" is; it simply discovers that
-- generating an Identity type between two point constructors yields a massive
-- ν_H topological payoff.

module Telescope
  ( -- * Core types
    TeleEntry(..)
  , Telescope(..)
    -- * Telescope metrics
  , teleKappa
  , teleBitCost
  , teleLibRefs
  , teleVarRefs
  , teleMaxLibRef
    -- * Conversion to existing infrastructure
  , teleToEntry
  , teleToTypeExprs
    -- * Structural analysis
  , teleIsConnected
  , teleReferencesWindow
  , teleHasPointConstructor
  , telePathDimensions
  , teleHasLoop
  , isTriviallyDerivable
    -- * Classification (shared by TelescopeEval and StructuralNu)
  , TelescopeClass(..)
  , classifyTelescope
  , hasLibPointer
    -- * Reference telescopes (ground truth from paper)
  , referenceTelescope
  , allReferenceTelescopes
  ) where

import Kolmogorov (MBTTExpr(..), bitLength, eliasGammaLength)
import Types (LibraryEntry(..), Library, TypeExpr(..), mkLibraryEntry)
import qualified Data.Set as Set

-- ============================================
-- Core Data Types
-- ============================================

-- | A telescope entry: a named declaration with a type expressed in MBTT.
-- In the telescope [c₁:A₁, c₂:A₂(c₁)], entry c₂ may reference c₁ via Var(1).
data TeleEntry = TeleEntry
  { teName   :: !String     -- ^ Bound name (e.g., "c1", "base", "loop")
  , teType   :: !MBTTExpr   -- ^ The type of this declaration in MBTT
  } deriving (Show, Eq, Ord)

-- | A context telescope: ordered sequence of dependent declarations.
-- This is the universal representation for all mathematical structures.
newtype Telescope = Telescope { teleEntries :: [TeleEntry] }
  deriving (Show, Eq, Ord)

-- ============================================
-- Telescope Metrics
-- ============================================

-- | Telescope length = κ (construction effort in clause count).
teleKappa :: Telescope -> Int
teleKappa = length . teleEntries

-- | Total MBTT bit cost (Kolmogorov complexity upper bound).
teleBitCost :: Telescope -> Int
teleBitCost = sum . map (bitLength . teType) . teleEntries

-- | Collect all library pointer indices referenced in the telescope.
teleLibRefs :: Telescope -> Set.Set Int
teleLibRefs (Telescope entries) = Set.unions (map (exprLibRefs . teType) entries)

-- | Collect all bound variable indices referenced in the telescope.
teleVarRefs :: Telescope -> Set.Set Int
teleVarRefs (Telescope entries) = Set.unions (map (exprVarRefs . teType) entries)

-- | Maximum library reference index (0 if no library refs).
teleMaxLibRef :: Telescope -> Int
teleMaxLibRef t = case Set.toDescList (teleLibRefs t) of
  (x:_) -> x
  []    -> 0

-- ============================================
-- MBTT Expression Analysis
-- ============================================

-- | Collect library pointer indices from an MBTT expression.
exprLibRefs :: MBTTExpr -> Set.Set Int
exprLibRefs (App f x)      = Set.union (exprLibRefs f) (exprLibRefs x)
exprLibRefs (Lam body)     = exprLibRefs body
exprLibRefs (Pi a b)       = Set.union (exprLibRefs a) (exprLibRefs b)
exprLibRefs (Sigma a b)    = Set.union (exprLibRefs a) (exprLibRefs b)
exprLibRefs Univ           = Set.empty
exprLibRefs (Var _)        = Set.empty
exprLibRefs (Lib i)        = Set.singleton i
exprLibRefs (Id a x y)     = Set.unions [exprLibRefs a, exprLibRefs x, exprLibRefs y]
exprLibRefs (Refl a)       = exprLibRefs a
exprLibRefs (Susp a)       = exprLibRefs a
exprLibRefs (Trunc a)      = exprLibRefs a
exprLibRefs (PathCon _)    = Set.empty
exprLibRefs (Flat a)       = exprLibRefs a
exprLibRefs (Sharp a)      = exprLibRefs a
exprLibRefs (Disc a)       = exprLibRefs a
exprLibRefs (Shape a)      = exprLibRefs a
exprLibRefs (Next a)       = exprLibRefs a
exprLibRefs (Eventually a) = exprLibRefs a

-- | Collect bound variable indices from an MBTT expression.
exprVarRefs :: MBTTExpr -> Set.Set Int
exprVarRefs (App f x)      = Set.union (exprVarRefs f) (exprVarRefs x)
exprVarRefs (Lam body)     = exprVarRefs body
exprVarRefs (Pi a b)       = Set.union (exprVarRefs a) (exprVarRefs b)
exprVarRefs (Sigma a b)    = Set.union (exprVarRefs a) (exprVarRefs b)
exprVarRefs Univ           = Set.empty
exprVarRefs (Var i)        = Set.singleton i
exprVarRefs (Lib _)        = Set.empty
exprVarRefs (Id a x y)     = Set.unions [exprVarRefs a, exprVarRefs x, exprVarRefs y]
exprVarRefs (Refl a)       = exprVarRefs a
exprVarRefs (Susp a)       = exprVarRefs a
exprVarRefs (Trunc a)      = exprVarRefs a
exprVarRefs (PathCon _)    = Set.empty
exprVarRefs (Flat a)       = exprVarRefs a
exprVarRefs (Sharp a)      = exprVarRefs a
exprVarRefs (Disc a)       = exprVarRefs a
exprVarRefs (Shape a)      = exprVarRefs a
exprVarRefs (Next a)       = exprVarRefs a
exprVarRefs (Eventually a) = exprVarRefs a

-- ============================================
-- Structural Analysis
-- ============================================

-- | Check if a telescope is structurally connected (Structural Unity Filter).
-- A telescope is connected if every entry except the last is referenced by
-- at least one subsequent entry. Prevents axiom packing.
teleIsConnected :: Telescope -> Bool
teleIsConnected (Telescope [])  = True
teleIsConnected (Telescope [_]) = True
teleIsConnected (Telescope entries) =
  -- For each entry i (0-based), check if some entry j>i references Var(j-i)
  -- (the de Bruijn index of entry i as seen from entry j)
  all isReferenced [0 .. length entries - 2]
  where
    isReferenced i =
      -- Entry i is referenced if any later entry j uses Var(j-i)
      any (\j -> let deBruijn = j - i
                     refs = exprVarRefs (teType (entries !! j))
                 in Set.member deBruijn refs)
          [i+1 .. length entries - 1]
      -- Also count as connected if entry i references the library
      -- (single-entry telescopes that interact with the library are fine)
      || hasLibRef (teType (entries !! i))

-- | Check if an expression contains any library reference.
hasLibRef :: MBTTExpr -> Bool
hasLibRef expr = not (Set.null (exprLibRefs expr))

-- | Check if the telescope references the d=2 window of the library.
-- Maximal Interface Density (Remark 2.5): bias toward Lib(n) and Lib(n-1).
teleReferencesWindow :: Telescope -> Int -> Bool
teleReferencesWindow tele libSize
  | libSize <= 0 = True  -- empty library, no constraint
  | otherwise =
    let refs = teleLibRefs tele
        -- Must reference at least one of the top-2 library entries
    in Set.member libSize refs || Set.member (libSize - 1) refs
       || libSize <= 2  -- relaxed for bootstrap (steps 1-2)

-- | Check if the telescope contains a point constructor (an App Univ term or similar).
-- This determines leConstructors for the LibraryEntry.
teleHasPointConstructor :: Telescope -> Bool
teleHasPointConstructor (Telescope entries) = any isPointEntry entries
  where
    isPointEntry (TeleEntry _ (App Univ _)) = True
    isPointEntry (TeleEntry _ (Var _))      = True  -- a term (not a type)
    isPointEntry (TeleEntry _ (App (Lib _) _)) = True  -- applying a constructor
    isPointEntry _ = False

-- | Extract path constructor dimensions from the telescope.
telePathDimensions :: Telescope -> [Int]
telePathDimensions (Telescope entries) =
  [ d | TeleEntry _ (PathCon d) <- entries ]

-- | Check if the telescope defines a loopy type (has path constructors).
teleHasLoop :: Telescope -> Bool
teleHasLoop = not . null . telePathDimensions

-- ============================================
-- Conversion to Existing Infrastructure
-- ============================================

-- | Convert a telescope to a LibraryEntry for evaluation by UniformNu.
--
-- The type-theoretic semantics:
--   - A telescope introduces a CONCRETE TYPE (leConstructors > 0) if it contains
--     a type formation rule (Univ, App Univ _, Susp _) or provides a term
--     constructor for an existing type.
--   - A telescope introduces an OPERATION (leConstructors = 0) if it consists
--     of inference rules, modal operators, or function types between library types.
--
-- This distinction matters because UniformNu's `checkInhab (TRef name) lib`
-- returns Inhabited iff leConstructors > 0.  Only inhabited atoms generate
-- the full spectrum of compositional schemas.
teleToEntry :: Telescope -> String -> LibraryEntry
teleToEntry tele@(Telescope entries) name =
  let exprs = map teType entries
      pathDims = telePathDimensions tele
      hasLoop = not (null pathDims)
      truncLevel = findTruncation entries

      -- Does the telescope introduce a new concrete type?
      -- Type formation: "this is a type in U" (App Univ _) or Universe itself
      hasTypeFormation = any isTypeFormation exprs
      -- Suspension: Susp(X) always produces an inhabited type (north pole)
      hasSuspension = any isSuspension exprs

      -- Does the telescope specify operations on library types?
      -- Top-level Pi between library references: a map or rule, not a concrete type
      hasLibOperation = any isLibOperation exprs
      -- Top-level type former / modal / temporal operators: not a concrete type
      hasOperatorTop = any isOperatorTop exprs

      -- Count genuine point constructors.
      -- When the telescope has library-operation patterns (Pi (Lib _) _),
      -- App (Lib _) (Var _) is ambiguous (could be type-level assertion
      -- like "fiber ≃ S¹" rather than a term constructor like "★ : 1").
      -- We only count App (Lib _) (Var _) as a point term when there are
      -- NO top-level library operation patterns.
      pointCount
        | hasLibOperation = length [() | e <- exprs, isBareVarTerm e]
        | otherwise       = length [() | e <- exprs, isPointTerm e]

      -- The classification (following the paper's LibraryEntry semantics):
      --   Concrete type (formation/suspension): leConstructors ≥ 1
      --   Term provider (no formation, but has points, no lib-operations): ≥ 1
      --   Everything else (type former / modal / axiomatic / map): 0
      constructors
        | hasTypeFormation || hasSuspension = max 1 pointCount
        | hasLibOperation || hasOperatorTop = 0
        | pointCount > 0 = pointCount
        | otherwise = 0

  in mkLibraryEntry name constructors pathDims hasLoop truncLevel

-- | Type formation: introduces a new type in the universe.
isTypeFormation :: MBTTExpr -> Bool
isTypeFormation Univ          = True   -- Universe formation (U : Type)
isTypeFormation (App Univ _)  = True   -- Type-in-universe (A : U)
isTypeFormation _             = False

-- | Suspension: creates a new inhabited type with north/south poles.
isSuspension :: MBTTExpr -> Bool
isSuspension (Susp _) = True
isSuspension _        = False

-- | Library operation: a function type between library types.
-- Indicates a map or rule, not a concrete type.
isLibOperation :: MBTTExpr -> Bool
isLibOperation (Pi (Lib _) _) = True
isLibOperation (Pi _ (Lib _)) = True
isLibOperation _              = False

-- | Top-level operator: modal, temporal, or truncation type former.
-- These expand the type vocabulary but don't introduce new concrete types.
isOperatorTop :: MBTTExpr -> Bool
isOperatorTop (Flat _)       = True
isOperatorTop (Sharp _)      = True
isOperatorTop (Disc _)       = True
isOperatorTop (Shape _)      = True
isOperatorTop (Next _)       = True
isOperatorTop (Eventually _) = True
isOperatorTop (Trunc _)      = True
isOperatorTop _              = False

-- | Point term: a term-level expression that provides an inhabitant.
--   - Var i: a bare term variable (base : S¹)
--   - App (Lib i) (Var j): a constructor applied to existing type
--     (★ : 1 in Step 3, encoded as App (Lib 2) (Var 1))
--
-- IMPORTANT: We do NOT count App (Lib i) (Var j) as a point term when the
-- telescope also has library-level operations (Pi (Lib _) _). In that case,
-- App (Lib i) (Var j) is a type-level assertion (e.g., "fiber ≃ S¹" in Hopf),
-- not a term constructor.
isPointTerm :: MBTTExpr -> Bool
isPointTerm (Var _)                = True
isPointTerm (App (Lib _) (Var _))  = True  -- constructor of library type
isPointTerm _                      = False

-- | Strict point term: only bare Var, excluding App patterns.
-- Used when library operations are present (where App (Lib _) (Var _)
-- is ambiguous between a term and a type assertion).
isBareVarTerm :: MBTTExpr -> Bool
isBareVarTerm (Var _) = True
isBareVarTerm _       = False

-- | Check if a telescope is trivially derivable from the existing library.
-- A trivially derivable telescope adds no genuine new structure and should
-- receive ν = 0.  This prevents MCTS from gaming the evaluation by
-- generating bare references to existing library entries.
--
-- Trivially derivable:
--   - Empty telescope
--   - Single bare library reference [Lib i]
--   - Single bare variable [Var i]
--   - All entries are bare Lib or Var references
--   - Universe re-declaration when Universe already exists
isTriviallyDerivable :: Telescope -> Library -> Bool
isTriviallyDerivable (Telescope entries) lib
  | null entries = True
  | all (isBareRef lib) entries = True
  | otherwise = False

-- | Check if a telescope entry is a bare reference (no new structure).
isBareRef :: Library -> TeleEntry -> Bool
isBareRef _   (TeleEntry _ (Lib _)) = True
isBareRef _   (TeleEntry _ (Var _)) = True
isBareRef lib (TeleEntry _ Univ)    = any ((== "Universe") . leName) lib
isBareRef _   _                     = False

-- | Find truncation level in telescope entries.
findTruncation :: [TeleEntry] -> Maybe Int
findTruncation entries = case [() | TeleEntry _ (Trunc _) <- entries] of
  (_:_) -> Just 0  -- PropTrunc = 0-truncation
  []    -> Nothing

-- | Convert a telescope to TypeExprs for inhabitation checking.
-- Each entry becomes a TypeExpr that can be evaluated by the existing engine.
teleToTypeExprs :: Telescope -> Library -> [TypeExpr]
teleToTypeExprs (Telescope entries) _lib =
  map (mbttToTypeExpr . teType) entries

-- | Convert an MBTT expression to a TypeExpr.
-- This bridges the MBTT world (Kolmogorov.hs) to the TypeExpr world (Types.hs).
mbttToTypeExpr :: MBTTExpr -> TypeExpr
mbttToTypeExpr Univ           = TRef "U"
mbttToTypeExpr (Var i)        = TRef ("v" ++ show i)
mbttToTypeExpr (Lib i)        = TRef ("lib" ++ show i)
mbttToTypeExpr (App f x)      = case mbttToTypeExpr f of
                                   TRef "U" -> mbttToTypeExpr x  -- U applied = the type itself
                                   tf -> TArrow tf (mbttToTypeExpr x)
mbttToTypeExpr (Lam body)     = mbttToTypeExpr body  -- simplified
mbttToTypeExpr (Pi a b)       = TPi "x" (mbttToTypeExpr a) (mbttToTypeExpr b)
mbttToTypeExpr (Sigma a b)    = TSigma "x" (mbttToTypeExpr a) (mbttToTypeExpr b)
mbttToTypeExpr (Id a x y)     = TId (mbttToTypeExpr a) (mbttToTypeExpr x) (mbttToTypeExpr y)
mbttToTypeExpr (Refl a)       = TSelfId (mbttToTypeExpr a)
mbttToTypeExpr (Susp a)       = TSusp (mbttToTypeExpr a)
mbttToTypeExpr (Trunc a)      = TTrunc 0 (mbttToTypeExpr a)
mbttToTypeExpr (PathCon d)    = THIT 1 [d]  -- approximate: a path of dimension d
mbttToTypeExpr (Flat a)       = TFlat (mbttToTypeExpr a)
mbttToTypeExpr (Sharp a)      = TSharp (mbttToTypeExpr a)
mbttToTypeExpr (Disc a)       = TDisc (mbttToTypeExpr a)
mbttToTypeExpr (Shape a)      = TPiCoh (mbttToTypeExpr a)
mbttToTypeExpr (Next a)       = TNext (mbttToTypeExpr a)
mbttToTypeExpr (Eventually a) = TEventually (mbttToTypeExpr a)

-- ============================================
-- Telescope Classification (shared infrastructure)
-- ============================================

-- | What kind of structure does a telescope define?
data TelescopeClass
  = TCFoundation    -- ^ Bootstrap types (U, 1, ★)
  | TCFormer        -- ^ Type formers (Π/Σ)
  | TCHIT           -- ^ Higher inductive types (S¹, S², S³, PropTrunc)
  | TCSuspension    -- ^ Suspension of existing type
  | TCMap           -- ^ Map between existing types (Hopf)
  | TCModal         -- ^ Modal operators (Cohesion)
  | TCAxiomatic     -- ^ Axiomatic extension (Connections, Curvature, Metric, Hilbert)
  | TCSynthesis     -- ^ Synthesis (DCT)
  | TCUnknown       -- ^ Unclassified
  deriving (Show, Eq)

-- | Classify a telescope by analyzing its MBTT structure.
--
-- Classification priority: the ORDER matters. We check the most specific
-- patterns first (HIT, modal, temporal, suspension) before the more generic
-- ones (map, axiomatic, former).
classifyTelescope :: Telescope -> Library -> TelescopeClass
classifyTelescope tele@(Telescope entries) _lib
  | null entries = TCUnknown
  | teleKappa tele == 1 && isFormationOnlyC (head entries) = classifySingleEntryC (head entries)
  | teleKappa tele == 1 && isTermIntroC (head entries)     = TCMap
  | isFoundationLikeC     = TCFoundation
  | hasPathConstructorsC   = TCHIT
  | hasModalOpsC           = TCModal
  | hasTemporalOpsC        = TCSynthesis
  | hasSuspC               = TCSuspension
  | hasLibMapPatternC      = TCMap
  | hasLibAxiomPatternC    = TCAxiomatic
  | allPureFormerC         = TCFormer
  | otherwise              = TCUnknown
  where
    exprs = map teType entries
    -- Multi-entry foundation: all entries are basic formations (Univ, App Univ _, Var)
    -- with no library, modal, temporal, or path structure.
    isFoundationLikeC = teleKappa tele >= 2
                     && all isBasicFormationEntryC entries
                     && not (any hasLibPointer exprs)
    isBasicFormationEntryC (TeleEntry _ Univ)         = True
    isBasicFormationEntryC (TeleEntry _ (App Univ _)) = True
    isBasicFormationEntryC (TeleEntry _ (Var _))      = True
    isBasicFormationEntryC _                          = False
    hasPathConstructorsC = any isPathConC exprs
    hasModalOpsC = any isModalExprC exprs && not (any isTemporalExprC exprs)
    hasTemporalOpsC = any isTemporalExprC exprs
    hasSuspC = any isSuspExprC exprs
    hasLibMapPatternC = length entries >= 2 && length entries <= 4
                     && all hasLibPointer (take 2 exprs)
    hasLibAxiomPatternC = length entries >= 3
                       && any hasLibPointer exprs
                       && not hasModalOpsC
    allPureFormerC = all isPiSigmaExprC exprs
                  && not (any hasLibPointer exprs)

    isFormationOnlyC (TeleEntry _ Univ) = True
    isFormationOnlyC (TeleEntry _ (App Univ _)) = True
    isFormationOnlyC (TeleEntry _ (Susp _)) = True
    isFormationOnlyC _ = False

    isTermIntroC (TeleEntry _ (App (Lib _) _)) = True
    isTermIntroC _ = False

    classifySingleEntryC (TeleEntry _ Univ) = TCFoundation
    classifySingleEntryC (TeleEntry _ (App Univ _)) = TCFoundation
    classifySingleEntryC (TeleEntry _ (Susp _)) = TCSuspension
    classifySingleEntryC _ = TCUnknown

isPathConC :: MBTTExpr -> Bool
isPathConC (PathCon _) = True
isPathConC _           = False

isModalExprC :: MBTTExpr -> Bool
isModalExprC (Flat _)  = True
isModalExprC (Sharp _) = True
isModalExprC (Disc _)  = True
isModalExprC (Shape _) = True
isModalExprC _         = False

isTemporalExprC :: MBTTExpr -> Bool
isTemporalExprC (Next _)       = True
isTemporalExprC (Eventually _) = True
isTemporalExprC (Pi a b)       = isTemporalExprC a || isTemporalExprC b
isTemporalExprC (Lam a)        = isTemporalExprC a
isTemporalExprC (App a b)      = isTemporalExprC a || isTemporalExprC b
isTemporalExprC _              = False

isSuspExprC :: MBTTExpr -> Bool
isSuspExprC (Susp _) = True
isSuspExprC _        = False

isPiSigmaExprC :: MBTTExpr -> Bool
isPiSigmaExprC (Pi _ _)    = True
isPiSigmaExprC (Sigma _ _) = True
isPiSigmaExprC (Lam _)     = True
isPiSigmaExprC (App _ _)   = True
isPiSigmaExprC _           = False

-- | Check if an expression contains any library pointer reference.
hasLibPointer :: MBTTExpr -> Bool
hasLibPointer (Lib _)        = True
hasLibPointer (App a b)      = hasLibPointer a || hasLibPointer b
hasLibPointer (Lam a)        = hasLibPointer a
hasLibPointer (Pi a b)       = hasLibPointer a || hasLibPointer b
hasLibPointer (Sigma a b)    = hasLibPointer a || hasLibPointer b
hasLibPointer (Flat a)       = hasLibPointer a
hasLibPointer (Sharp a)      = hasLibPointer a
hasLibPointer (Disc a)       = hasLibPointer a
hasLibPointer (Shape a)      = hasLibPointer a
hasLibPointer (Next a)       = hasLibPointer a
hasLibPointer (Eventually a) = hasLibPointer a
hasLibPointer _              = False

-- ============================================
-- Reference Telescopes (Ground Truth)
-- ============================================

-- | Reference telescope for a genesis step (1-indexed).
-- These are the known-correct telescopes derived from Kolmogorov.hs genesisSpecs,
-- serving as ground truth for validation.
referenceTelescope :: Int -> Telescope
referenceTelescope step = case step of

  -- Step 1: Universe  (paper κ = 2)
  -- U-formation + universe level
  1 -> Telescope
    [ TeleEntry "U-form"  Univ                     -- U : Type
    , TeleEntry "U-level" (App Univ (Var 1))       -- U₀ : U
    ]

  -- Step 2: Unit type  (paper κ = 1)
  -- 1 : U
  2 -> Telescope
    [ TeleEntry "1-form" (App Univ (Var 1))        -- 1 : U
    ]

  -- Step 3: Witness  (paper κ = 1)
  -- ★ : 1 (introduction rule; elimination is derivable by adjoint completion)
  3 -> Telescope
    [ TeleEntry "star"  (App (Lib 2) (Var 1))      -- ★ : 1
    ]

  -- Step 4: Π/Σ types  (paper κ = 3)
  -- Core rules: λ-intro, pair-intro, application
  -- (fst/snd are derivable from Σ-elimination, not counted in κ)
  4 -> Telescope
    [ TeleEntry "lam"   (Lam (Pi (Var 1) (Var 2)))              -- λ-intro
    , TeleEntry "pair"  (App (App (Var 1) (Var 2)) (Var 3))     -- pair
    , TeleEntry "app"   (App (Lam (Var 1)) (Var 2))             -- application
    ]

  -- Step 5: S¹ (Circle)
  -- S¹ : U, base : S¹, loop : base =_{S¹} base
  5 -> Telescope
    [ TeleEntry "S1-form" (App Univ (Var 1))    -- S¹ : U
    , TeleEntry "base"    (Var 1)               -- base : S¹
    , TeleEntry "loop"    (PathCon 1)           -- loop (1-path)
    ]

  -- Step 6: PropTrunc (||A||₀)
  6 -> Telescope
    [ TeleEntry "trunc-form"  (Trunc (Var 1))               -- ||A||₀ formation
    , TeleEntry "trunc-intro" (App (Trunc (Var 1)) (Var 2)) -- |a| introduction
    , TeleEntry "squash"      (PathCon 1)                    -- squash path
    ]

  -- Step 7: S² (2-sphere)  (paper κ = 3)
  -- Explicit HIT: formation + base + 2-path (surface)
  -- Equivalent to Susp(S¹) but with κ matching the paper
  7 -> Telescope
    [ TeleEntry "S2-form" (App Univ (Var 1))    -- S² : U
    , TeleEntry "base"    (Var 1)               -- base : S²
    , TeleEntry "surf"    (PathCon 2)           -- surf (2-path: north =_{S²} south)
    ]

  -- Step 8: S³ (3-sphere)  (paper κ = 5)
  -- Explicit HIT: formation + base + 3-path + higher structure
  -- Equivalent to Susp(S²) but with κ matching the paper
  8 -> Telescope
    [ TeleEntry "S3-form" (App Univ (Var 1))    -- S³ : U
    , TeleEntry "base"    (Var 1)               -- base : S³
    , TeleEntry "surf"    (PathCon 3)           -- surf (3-path)
    , TeleEntry "fill-n"  (Lam (Var 1))         -- north hemisphere filling
    , TeleEntry "fill-s"  (Lam (Var 2))         -- south hemisphere filling
    ]

  -- Step 9: Hopf fibration (h : S³ → S²)
  9 -> Telescope
    [ TeleEntry "hopf-map"    (Pi (Lib 8) (Lib 7))                -- h : S³ → S²
    , TeleEntry "hopf-fiber"  (App (Lib 5) (Var 1))               -- fiber ≃ S¹
    , TeleEntry "hopf-total"  (Lam (App (Lib 8) (Lib 7)))         -- total space
    , TeleEntry "hopf-class"  (Pi (Lib 7) (Lib 8))                -- classifying data
    ]

  -- Step 10: Cohesion (♭, ♯, Disc, Π_coh)
  10 -> Telescope
    [ TeleEntry "flat-form"  (Flat (Var 1))       -- ♭X
    , TeleEntry "sharp-form" (Sharp (Var 1))      -- ♯X
    , TeleEntry "disc-form"  (Disc (Var 1))       -- Disc(X)
    , TeleEntry "shape-form" (Shape (Var 1))      -- Π_coh(X)
    ]

  -- Step 11: Connections (∇ on cohesive types)
  11 -> Telescope
    [ TeleEntry "conn-form"  (Pi (Lib 10) (Pi (Var 1) (Var 1)))  -- ∇ : TX → TX
    , TeleEntry "transport"  (Lam (Pi (Var 1) (Var 2)))          -- parallel transport
    , TeleEntry "cov-deriv"  (Pi (Flat (Var 1)) (Var 1))         -- covariant derivative
    , TeleEntry "horiz-lift" (App (Lib 10) (Var 1))              -- horizontal lift
    , TeleEntry "leibniz"    (Lam (Var 1))                       -- Leibniz rule
    ]

  -- Step 12: Curvature (R = d∇ + ∇∧∇)
  12 -> Telescope
    [ TeleEntry "R-form"     (Pi (Lib 11) (Pi (Var 1) (Var 1)))    -- R : conn → 2-form
    , TeleEntry "bianchi"    (Lam (App (Lib 11) (Var 1)))          -- Bianchi identity
    , TeleEntry "holonomy"   (Pi (Var 1) (Lib 11))                 -- holonomy map
    , TeleEntry "chern-weil" (App (Lib 11) (App (Var 1) (Var 2))) -- Chern-Weil
    , TeleEntry "char-class" (Lam (Pi (Var 1) (Var 2)))           -- characteristic class
    , TeleEntry "R-compose"  (Pi (Lib 11) (Lib 11))               -- curvature composition
    ]

  -- Step 13: Metric (g : TX ⊗ TX → ℝ)
  13 -> Telescope
    [ TeleEntry "g-form"     (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))  -- symmetric bilinear
    , TeleEntry "levi-civ"   (Pi (Sigma (Var 1) (Var 2)) (Lib 11))              -- Levi-Civita
    , TeleEntry "geodesic"   (Pi (Var 1) (Pi (Var 1) (Var 1)))                  -- geodesic
    , TeleEntry "vol-form"   (Lam (App (Var 1) (Var 2)))                        -- volume form
    , TeleEntry "hodge-star" (Pi (Lib 12) (Lib 12))                              -- Hodge star
    , TeleEntry "laplacian"  (Lam (Pi (Var 1) (Var 1)))                         -- Laplacian
    , TeleEntry "ricci"      (Pi (Lib 12) (Var 1))                               -- Ricci/scalar
    ]

  -- Step 14: Hilbert (inner product + spectral)
  14 -> Telescope
    [ TeleEntry "inner-prod"  (Sigma (Pi (Var 1) (Pi (Var 1) Univ)) (Var 1))   -- ⟨·,·⟩
    , TeleEntry "complete"    (Pi (Var 1) (Var 1))                              -- completeness
    , TeleEntry "orth-decomp" (Pi (Var 1) (Sigma (Var 1) (Var 1)))             -- orthogonal
    , TeleEntry "spectral"    (Pi (Lam (Var 1)) (Sigma (Var 1) (Var 2)))       -- spectral decomp
    , TeleEntry "cstar-alg"   (Sigma (Pi (Var 1) (Var 1)) (Pi (Var 1) (Var 1)))-- C*-algebra
    , TeleEntry "g-compat"    (Pi (Lib 13) (Var 1))                             -- metric compat
    , TeleEntry "R-op"        (Pi (Lib 12) (Var 1))                             -- curvature op
    , TeleEntry "conn-op"     (Pi (Lib 11) (Var 1))                             -- connection op
    , TeleEntry "func-deriv"  (Lam (Pi (Var 1) Univ))                          -- functional deriv
    ]

  -- Step 15: DCT (Dynamical Cohesive Topos)
  15 -> Telescope
    [ TeleEntry "next-form"   (Next (Var 1))                                              -- ○X
    , TeleEntry "ev-form"     (Eventually (Var 1))                                        -- ◇X
    , TeleEntry "next-to-ev"  (Pi (Next (Var 1)) (Eventually (Var 1)))                    -- ○→◇
    , TeleEntry "spat-temp"   (Lam (App (Lib 10) (Next (Var 1))))                         -- spatial-temporal
    , TeleEntry "flat-next"   (Pi (Flat (Next (Var 1))) (Next (Flat (Var 1))))             -- ♭○↔○♭
    , TeleEntry "sharp-ev"    (Pi (Sharp (Eventually (Var 1))) (Eventually (Sharp (Var 1))))-- ♯◇↔◇♯
    , TeleEntry "ev-elim"     (Lam (App (Eventually (Var 1)) (Var 2)))                    -- ◇-elim
    , TeleEntry "next-idem"   (Pi (Next (Next (Var 1))) (Next (Var 1)))                   -- ○○→○
    ]

  _ -> Telescope []

-- | All 15 reference telescopes with their names.
allReferenceTelescopes :: [(Int, String, Telescope)]
allReferenceTelescopes =
  [ (1,  "Universe",    referenceTelescope 1)
  , (2,  "Unit",        referenceTelescope 2)
  , (3,  "Witness",     referenceTelescope 3)
  , (4,  "Pi",          referenceTelescope 4)
  , (5,  "S1",          referenceTelescope 5)
  , (6,  "Trunc",       referenceTelescope 6)
  , (7,  "S2",          referenceTelescope 7)
  , (8,  "S3",          referenceTelescope 8)
  , (9,  "Hopf",        referenceTelescope 9)
  , (10, "Cohesion",    referenceTelescope 10)
  , (11, "Connections", referenceTelescope 11)
  , (12, "Curvature",   referenceTelescope 12)
  , (13, "Metric",      referenceTelescope 13)
  , (14, "Hilbert",     referenceTelescope 14)
  , (15, "DCT",         referenceTelescope 15)
  ]
