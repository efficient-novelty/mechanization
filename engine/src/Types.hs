{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Core type expressions for PEN information-theoretic framework
--
-- This module defines the AST for types in HoTT/dependent type theory,
-- used for computing Kolmogorov complexity (κ) and Shannon surprise (ν).

module Types where

import GHC.Generics (Generic)
import Data.List (intercalate)

-- ============================================
-- Type Expressions (for theorem enumeration)
-- ============================================

-- | Type expressions that can be formed using library ingredients
data TypeExpr
  = TUnit                         -- ^ The unit type 1
  | TVoid                         -- ^ The empty type 0
  | TRef String                   -- ^ Reference to a library type by name
  | TArrow TypeExpr TypeExpr      -- ^ Function type A → B
  | TProd TypeExpr TypeExpr       -- ^ Product type A × B
  | TCoprod TypeExpr TypeExpr     -- ^ Coproduct type A + B
  | TId TypeExpr TypeExpr TypeExpr -- ^ Identity type a =_A b
  | TSelfId TypeExpr              -- ^ Self-identity x =_A x (for any x : A)
  | TOmega TypeExpr               -- ^ Loop space Ω(A) = (pt =_A pt)
  | TSusp TypeExpr                -- ^ Suspension Σ(A)
  | TTrunc Int TypeExpr           -- ^ n-truncation ‖A‖_n
  | TPi String TypeExpr TypeExpr  -- ^ Dependent product (x : A) → B(x)
  | TSigma String TypeExpr TypeExpr -- ^ Dependent sum (x : A) × B(x)
  | THIT Int [Int]                -- ^ HIT with k points and paths of given dimensions
  | TFiber TypeExpr TypeExpr      -- ^ Fiber of a map f : A → B over point b
  | TDeloop TypeExpr              -- ^ Delooping BA (classifying space)
  -- Modal operators (Cohesion)
  | TFlat TypeExpr                -- ^ Flat modality ♭X (discrete)
  | TSharp TypeExpr               -- ^ Sharp modality ♯X (codiscrete)
  | TDisc TypeExpr                -- ^ Disc modality (discrete → continuous)
  | TPiCoh TypeExpr               -- ^ Cohesive shape Π_coh (continuous → discrete)
  -- Temporal operators (LTL)
  | TNext TypeExpr                -- ^ Next modality ○X
  | TEventually TypeExpr          -- ^ Eventually modality ◇X
  -- Differential/Axiomatic
  | TInf TypeExpr                 -- ^ Infinitesimal type X^D (tangent microbundle)
  | TTangent TypeExpr             -- ^ Tangent bundle TX
  | TConnection TypeExpr          -- ^ Connection on X (∇ : TX → TX)
  | TCurvature TypeExpr           -- ^ Curvature of a connection on X
  | TMetric TypeExpr              -- ^ Metric structure on X
  | THilbert TypeExpr             -- ^ Hilbert space functional on X
  deriving (Eq, Ord, Show, Generic)

-- ============================================
-- Inference Rules (for Generative Capacity)
-- ============================================

-- | Classification of inference rules into spectral axes
data RuleClass = Intro | Elim | Comp
  deriving (Eq, Ord, Show, Generic)

-- | Atomic inference rule in the derivation logic
data InferenceRule
  = IntroRule
      { irName   :: String
      , irOutput :: TypeExpr     -- type of the constructed term
      }
  | ElimRule
      { irName   :: String
      , irInput  :: TypeExpr     -- type being analyzed
      , irOutput :: TypeExpr     -- type of the result
      }
  | CompRule
      { irName   :: String
      , irLHS    :: TypeExpr     -- left-hand side of reduction
      , irRHS    :: TypeExpr     -- right-hand side
      }
  deriving (Eq, Ord, Show, Generic)

-- | Get the class (spectral axis) of an inference rule
ruleClass :: InferenceRule -> RuleClass
ruleClass (IntroRule {}) = Intro
ruleClass (ElimRule {})  = Elim
ruleClass (CompRule {})  = Comp

-- | Decomposed novelty: spectral decomposition into three axes
data DecomposedNu = DecomposedNu
  { dnIntro :: Int    -- ^ nu_G: Introduction rules (Grammar/syntactic)
  , dnElim  :: Int    -- ^ nu_C: Elimination rules (Capability/logical)
  , dnComp  :: Int    -- ^ nu_H: Computation rules (Homotopy/topological)
  , dnTotal :: Int    -- ^ nu = nu_G + nu_C + nu_H
  } deriving (Eq, Show, Generic)

-- | Syntactic complexity of a type expression
complexity :: TypeExpr -> Int
complexity TUnit = 1
complexity TVoid = 1
complexity (TRef _) = 1
complexity (TArrow a b) = 1 + complexity a + complexity b
complexity (TProd a b) = 1 + complexity a + complexity b
complexity (TCoprod a b) = 1 + complexity a + complexity b
complexity (TId a x y) = 1 + complexity a + complexity x + complexity y
complexity (TSelfId a) = 1 + complexity a
complexity (TOmega a) = 1 + complexity a
complexity (TSusp a) = 1 + complexity a
complexity (TTrunc _ a) = 2 + complexity a
complexity (TPi _ a b) = 1 + complexity a + complexity b
complexity (TSigma _ a b) = 1 + complexity a + complexity b
complexity (THIT pts paths) = 1 + pts + sum paths
complexity (TFiber a b) = 1 + complexity a + complexity b
complexity (TDeloop a) = 1 + complexity a
-- Modal operators
complexity (TFlat a) = 1 + complexity a
complexity (TSharp a) = 1 + complexity a
complexity (TDisc a) = 1 + complexity a
complexity (TPiCoh a) = 1 + complexity a
-- Temporal operators
complexity (TNext a) = 1 + complexity a
complexity (TEventually a) = 1 + complexity a
-- Differential/Axiomatic
complexity (TInf a) = 1 + complexity a
complexity (TTangent a) = 1 + complexity a
complexity (TConnection a) = 1 + complexity a
complexity (TCurvature a) = 1 + complexity a
complexity (TMetric a) = 1 + complexity a
complexity (THilbert a) = 1 + complexity a

-- ============================================
-- Type Programs (for Kolmogorov κ)
-- ============================================

-- | Type programs: ways to construct types using library operations
-- The Kolmogorov complexity κ is the size of the shortest program.
data TypeProgram
  = -- Atoms (cost 1 each)
    PLitUnit                      -- ^ The unit type
  | PLitVoid                      -- ^ The empty type
  | PRef String                   -- ^ Reference a library type

  -- Unary operations (cost 1 + argument cost)
  | PSusp TypeProgram             -- ^ Suspension
  | POmega TypeProgram            -- ^ Loop space
  | PTrunc Int TypeProgram        -- ^ Truncation
  | PDeloop TypeProgram           -- ^ Delooping/classifying space
  | PFree TypeProgram             -- ^ Free group on a type

  -- Binary operations (cost 1 + argument costs)
  | PArrow TypeProgram TypeProgram
  | PProd TypeProgram TypeProgram
  | PCoprod TypeProgram TypeProgram
  | PFiber TypeProgram TypeProgram

  -- HIT specification (cost 1 + pts + sum of path dims)
  | PMakeHIT Int [Int]            -- ^ HIT with points and path dimensions

  -- Type formers (cost 1 each - these are "axiom" level)
  | PTypeFormerPi                 -- ^ "Π-types exist"
  | PTypeFormerSigma              -- ^ "Σ-types exist"
  | PTypeFormerId                 -- ^ "Identity types exist"
  deriving (Eq, Ord, Show, Generic)

-- | Cost of a type program (approximates Kolmogorov complexity)
programCost :: TypeProgram -> Int
programCost PLitUnit = 1
programCost PLitVoid = 1
programCost (PRef _) = 1
programCost (PSusp p) = 1 + programCost p
programCost (POmega p) = 1 + programCost p
programCost (PTrunc _ p) = 2 + programCost p
programCost (PDeloop p) = 1 + programCost p
programCost (PFree p) = 1 + programCost p
programCost (PArrow a b) = 1 + programCost a + programCost b
programCost (PProd a b) = 1 + programCost a + programCost b
programCost (PCoprod a b) = 1 + programCost a + programCost b
programCost (PFiber a b) = 1 + programCost a + programCost b
programCost (PMakeHIT pts paths) = 1 + pts + sum paths
programCost PTypeFormerPi = 1
programCost PTypeFormerSigma = 1
programCost PTypeFormerId = 1

-- | Convert a type program to the type expression it denotes
programToExpr :: TypeProgram -> TypeExpr
programToExpr PLitUnit = TUnit
programToExpr PLitVoid = TVoid
programToExpr (PRef s) = TRef s
programToExpr (PSusp p) = TSusp (programToExpr p)
programToExpr (POmega p) = TOmega (programToExpr p)
programToExpr (PTrunc n p) = TTrunc n (programToExpr p)
programToExpr (PDeloop p) = TDeloop (programToExpr p)
programToExpr (PFree _) = TRef "FreeGroup" -- Placeholder
programToExpr (PArrow a b) = TArrow (programToExpr a) (programToExpr b)
programToExpr (PProd a b) = TProd (programToExpr a) (programToExpr b)
programToExpr (PCoprod a b) = TCoprod (programToExpr a) (programToExpr b)
programToExpr (PFiber a b) = TFiber (programToExpr a) (programToExpr b)
programToExpr (PMakeHIT pts paths) = THIT pts paths
programToExpr PTypeFormerPi = TRef "Pi"
programToExpr PTypeFormerSigma = TRef "Sigma"
programToExpr PTypeFormerId = TRef "Id"

-- ============================================
-- Library
-- ============================================

-- | A library entry contains a type name and its structure.
--
-- The capability flags encode what type-theoretic operations this entry
-- unlocks for subsequent structures. These are STRUCTURAL properties,
-- not name-based: they are derived from the telescope classification
-- or set explicitly for genesis steps.
data LibraryEntry = LibraryEntry
  { leName :: String           -- ^ Type name
  , leConstructors :: Int      -- ^ Number of point constructors
  , lePathDims :: [Int]        -- ^ Path constructor dimensions
  , leHasLoop :: Bool          -- ^ Does this type have a non-trivial loop?
  , leIsTruncated :: Maybe Int -- ^ Is this type n-truncated?
  -- Structural capability flags (used by availableFormers)
  , leHasDependentFunctions :: Bool  -- ^ Provides Pi/Sigma type formers
  , leHasModalOps :: Bool            -- ^ Provides cohesive modalities (Flat, Sharp, Disc, Shape)
  , leHasDifferentialOps :: Bool     -- ^ Provides differential structure (Inf, Tangent, Connection)
  , leHasCurvature :: Bool           -- ^ Provides curvature operations
  , leHasMetric :: Bool              -- ^ Provides metric structure
  , leHasHilbert :: Bool             -- ^ Provides Hilbert space / functional analysis
  , leHasTemporalOps :: Bool         -- ^ Provides temporal modalities (Next, Eventually)
  } deriving (Eq, Show, Generic)

-- | Smart constructor with all capability flags defaulting to False.
-- Use record update syntax to set capabilities:
--   mkLibraryEntry "Pi" 0 [] False Nothing { leHasDependentFunctions = True }
mkLibraryEntry :: String -> Int -> [Int] -> Bool -> Maybe Int -> LibraryEntry
mkLibraryEntry name ctors dims loop trunc = LibraryEntry
  { leName = name
  , leConstructors = ctors
  , lePathDims = dims
  , leHasLoop = loop
  , leIsTruncated = trunc
  , leHasDependentFunctions = False
  , leHasModalOps = False
  , leHasDifferentialOps = False
  , leHasCurvature = False
  , leHasMetric = False
  , leHasHilbert = False
  , leHasTemporalOps = False
  }

-- | A library is a list of type entries
type Library = [LibraryEntry]

-- | Empty library
emptyLibrary :: Library
emptyLibrary = []

-- | Check if a type name is in the library
inLibrary :: String -> Library -> Bool
inLibrary name lib = any ((== name) . leName) lib

-- | Get a library entry by name
getEntry :: String -> Library -> Maybe LibraryEntry
getEntry name lib = case filter ((== name) . leName) lib of
  (e:_) -> Just e
  []    -> Nothing

-- ============================================
-- Pretty printing
-- ============================================

prettyTypeExpr :: TypeExpr -> String
prettyTypeExpr TUnit = "1"
prettyTypeExpr TVoid = "0"
prettyTypeExpr (TRef s) = s
prettyTypeExpr (TArrow a b) = "(" ++ prettyTypeExpr a ++ " -> " ++ prettyTypeExpr b ++ ")"
prettyTypeExpr (TProd a b) = "(" ++ prettyTypeExpr a ++ " x " ++ prettyTypeExpr b ++ ")"
prettyTypeExpr (TCoprod a b) = "(" ++ prettyTypeExpr a ++ " + " ++ prettyTypeExpr b ++ ")"
prettyTypeExpr (TId a x y) = "(" ++ prettyTypeExpr x ++ " =_{" ++ prettyTypeExpr a ++ "} " ++ prettyTypeExpr y ++ ")"
prettyTypeExpr (TSelfId a) = "(x =_{" ++ prettyTypeExpr a ++ "} x)"
prettyTypeExpr (TOmega a) = "Omega(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TSusp a) = "Susp(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TTrunc n a) = "||" ++ prettyTypeExpr a ++ "||_" ++ show n
prettyTypeExpr (TPi x a b) = "((" ++ x ++ " : " ++ prettyTypeExpr a ++ ") -> " ++ prettyTypeExpr b ++ ")"
prettyTypeExpr (TSigma x a b) = "((" ++ x ++ " : " ++ prettyTypeExpr a ++ ") x " ++ prettyTypeExpr b ++ ")"
prettyTypeExpr (THIT pts paths) = "HIT(" ++ show pts ++ ", [" ++ intercalate "," (map show paths) ++ "])"
prettyTypeExpr (TFiber a b) = "Fiber(" ++ prettyTypeExpr a ++ ", " ++ prettyTypeExpr b ++ ")"
prettyTypeExpr (TDeloop a) = "B(" ++ prettyTypeExpr a ++ ")"
-- Modal operators
prettyTypeExpr (TFlat a) = "flat(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TSharp a) = "sharp(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TDisc a) = "Disc(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TPiCoh a) = "PiCoh(" ++ prettyTypeExpr a ++ ")"
-- Temporal operators
prettyTypeExpr (TNext a) = "Next(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TEventually a) = "Ev(" ++ prettyTypeExpr a ++ ")"
-- Differential/Axiomatic
prettyTypeExpr (TInf a) = prettyTypeExpr a ++ "^D"
prettyTypeExpr (TTangent a) = "T(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TConnection a) = "Conn(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TCurvature a) = "Curv(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (TMetric a) = "Met(" ++ prettyTypeExpr a ++ ")"
prettyTypeExpr (THilbert a) = "Hilb(" ++ prettyTypeExpr a ++ ")"
