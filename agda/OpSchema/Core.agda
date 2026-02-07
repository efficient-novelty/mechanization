{-# OPTIONS --guardedness --without-K #-}

module OpSchema.Core where

-- ============================================
-- The Operation Schema Grammar
-- ============================================
--
-- This module defines the formal grammar for operation schemas
-- as specified in "Pen nu research plan.md", Stage 5.
--
-- An operation schema represents a "new capability" that becomes
-- available when a type X is added to a library L.
-- ============================================

open import Agda.Builtin.List public
open import Agda.Builtin.String public
open import Agda.Builtin.Bool public
open import Agda.Builtin.Unit public
open import Agda.Builtin.Nat public

-- Identity type
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}

infix 4 _≡_

-- Maybe type
data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

-- ============================================
-- Type Names and Constructors
-- ============================================

-- A type name (references types in the library or the new type X)
TypeName : Set
TypeName = String

-- A constructor name
ConstructorName : Set
ConstructorName = String

-- ============================================
-- Type Expressions (simplified AST)
-- ============================================

-- Represents type expressions that can appear in schemas
data TypeExpr : Set where
  -- Reference to a named type (library type or X)
  TRef     : TypeName → TypeExpr
  -- Function type A → B
  TArrow   : TypeExpr → TypeExpr → TypeExpr
  -- Product type A × B
  TProd    : TypeExpr → TypeExpr → TypeExpr
  -- Dependent sum Σ(x : A).B (simplified: just store component types)
  TSigma   : TypeExpr → TypeExpr → TypeExpr
  -- Identity type a =_A b (simplified: just store the type)
  TId      : TypeExpr → TypeExpr
  -- Loop space Ωⁿ(A)
  TLoop    : Nat → TypeExpr → TypeExpr
  -- Suspension ΣA
  TSusp    : TypeExpr → TypeExpr
  -- Truncation ‖A‖ₙ
  TTrunc   : Nat → TypeExpr → TypeExpr

-- ============================================
-- Family Shapes (for dependent elimination)
-- ============================================

-- What kind of type family P : X → U is being considered
data FamilyShape : Set where
  FSConst     : FamilyShape  -- P(x) = A for some fixed A
  FSLibValued : FamilyShape  -- P(x) ranges over library types
  FSXValued   : FamilyShape  -- P(x) involves X
  FSMixed     : FamilyShape  -- Complex combination

-- ============================================
-- Algebraic Operation Kinds
-- ============================================

data AlgOpKind : Set where
  OpMult  : AlgOpKind  -- Multiplication
  OpInv   : AlgOpKind  -- Inversion
  OpUnit  : AlgOpKind  -- Unit element
  OpAssoc : AlgOpKind  -- Associativity witness
  OpComm  : AlgOpKind  -- Commutativity witness

-- ============================================
-- Characteristic Classes
-- ============================================

data CharClass : Set where
  Euler          : CharClass
  Chern          : CharClass
  StiefelWhitney : CharClass
  Pontryagin     : CharClass

-- ============================================
-- Operation Schemas (the main grammar)
-- ============================================

-- An operation schema represents a single "capability"
-- that may become newly available when X is added to library L.

data OpSchema : Set where
  -- ========== EXISTENCE ==========
  -- X : U is a new type
  Exist : TypeExpr → OpSchema

  -- ========== PATH OPERATIONS ==========
  -- Non-trivial identity type a =_X b between constructors
  PathBetween : TypeExpr → ConstructorName → ConstructorName → OpSchema
  -- Non-trivial n-dimensional path in X
  PathDim : TypeExpr → Nat → OpSchema

  -- ========== MAPPING OPERATIONS ==========
  -- A → X for some library type A
  MapIn : TypeExpr → TypeExpr → OpSchema
  -- X → A for some library type A
  MapOut : TypeExpr → TypeExpr → OpSchema
  -- X → X (non-trivial self-map)
  MapSelf : TypeExpr → OpSchema
  -- A → X → B (bridge through X)
  Bridge : TypeExpr → TypeExpr → TypeExpr → OpSchema

  -- ========== DEPENDENT OPERATIONS ==========
  -- (x : X) → P(x) dependent elimination
  DepElim : TypeExpr → FamilyShape → OpSchema
  -- Σ(x : X).P(x) dependent pair
  DepPair : TypeExpr → FamilyShape → OpSchema

  -- ========== ALGEBRAIC OPERATIONS ==========
  -- X has group structure
  Group : TypeExpr → OpSchema
  -- Specific group operation
  GroupOp : TypeExpr → AlgOpKind → OpSchema
  -- X has ring structure
  Ring : TypeExpr → OpSchema
  -- X-module structure on Y
  Module : TypeExpr → TypeExpr → OpSchema

  -- ========== HOMOTOPICAL OPERATIONS ==========
  -- Ωⁿ(X) loop space
  LoopSpace : TypeExpr → Nat → OpSchema
  -- πₙ(X) homotopy group
  HomotopyGroup : TypeExpr → Nat → OpSchema
  -- ΣX suspension
  Suspension : TypeExpr → OpSchema
  -- πₙ(Y) computed via X
  HomotopyCalc : TypeExpr → TypeExpr → Nat → OpSchema

  -- ========== FIBRATION OPERATIONS ==========
  -- Fiber of a map X → Y
  Fiber : TypeExpr → TypeExpr → OpSchema
  -- X as total space
  TotalSpace : TypeExpr → OpSchema
  -- Section existence/obstruction
  Section : TypeExpr → OpSchema
  -- Long exact sequence connecting map
  LESConnect : TypeExpr → OpSchema
  -- X classifies some structure
  Classifying : TypeExpr → OpSchema
  -- Characteristic class
  CharacteristicClass : TypeExpr → CharClass → OpSchema

  -- ========== TRUNCATION OPERATIONS ==========
  -- ‖X‖ₙ behavior
  TruncLevel : TypeExpr → Nat → OpSchema
  -- Interaction of ‖X‖ with library types
  TruncInteract : TypeExpr → TypeExpr → OpSchema

-- ============================================
-- Library Entry and Library
-- ============================================

-- A library entry records a type's name and cardinality
record LibEntry : Set where
  constructor mkLibEntry
  field
    libName        : TypeName
    libCardinality : Nat
    libConstructors : List ConstructorName

-- A library is a list of entries
Library : Set
Library = List LibEntry

-- Empty library
emptyLib : Library
emptyLib = []

-- ============================================
-- Schema Depth (for bounded enumeration)
-- ============================================

-- Compute the depth of a type expression
typeDepth : TypeExpr → Nat
typeDepth (TRef _)       = 0
typeDepth (TArrow t₁ t₂) = suc (max (typeDepth t₁) (typeDepth t₂))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
typeDepth (TProd t₁ t₂)  = suc (max (typeDepth t₁) (typeDepth t₂))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
typeDepth (TSigma t₁ t₂) = suc (max (typeDepth t₁) (typeDepth t₂))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
typeDepth (TId t)        = suc (typeDepth t)
typeDepth (TLoop n t)    = suc (typeDepth t)
typeDepth (TSusp t)      = suc (typeDepth t)
typeDepth (TTrunc n t)   = suc (typeDepth t)

-- Compute the depth of an operation schema
schemaDepth : OpSchema → Nat
schemaDepth (Exist t)              = typeDepth t
schemaDepth (PathBetween t _ _)    = typeDepth t
schemaDepth (PathDim t _)          = typeDepth t
schemaDepth (MapIn s t)            = suc (max (typeDepth s) (typeDepth t))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
schemaDepth (MapOut s t)           = suc (max (typeDepth s) (typeDepth t))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
schemaDepth (MapSelf t)            = typeDepth t
schemaDepth (Bridge s t u)         = suc (max3 (typeDepth s) (typeDepth t) (typeDepth u))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
    max3 : Nat → Nat → Nat → Nat
    max3 a b c = max a (max b c)
schemaDepth (DepElim t _)          = typeDepth t
schemaDepth (DepPair t _)          = typeDepth t
schemaDepth (Group t)              = typeDepth t
schemaDepth (GroupOp t _)          = typeDepth t
schemaDepth (Ring t)               = typeDepth t
schemaDepth (Module s t)           = suc (max (typeDepth s) (typeDepth t))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
schemaDepth (LoopSpace t _)        = suc (typeDepth t)
schemaDepth (HomotopyGroup t _)    = suc (typeDepth t)
schemaDepth (Suspension t)         = suc (typeDepth t)
schemaDepth (HomotopyCalc s t _)   = suc (max (typeDepth s) (typeDepth t))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
schemaDepth (Fiber s t)            = suc (max (typeDepth s) (typeDepth t))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)
schemaDepth (TotalSpace t)         = typeDepth t
schemaDepth (Section t)            = typeDepth t
schemaDepth (LESConnect t)         = typeDepth t
schemaDepth (Classifying t)        = typeDepth t
schemaDepth (CharacteristicClass t _) = typeDepth t
schemaDepth (TruncLevel t _)       = suc (typeDepth t)
schemaDepth (TruncInteract s t)    = suc (max (typeDepth s) (typeDepth t))
  where
    max : Nat → Nat → Nat
    max zero    n       = n
    max (suc m) zero    = suc m
    max (suc m) (suc n) = suc (max m n)

-- ============================================
-- Pretty Printing (for debugging)
-- ============================================

-- Append strings
_++s_ : String → String → String
_++s_ = primStringAppend

infixr 5 _++s_

-- Show a type expression
showTypeExpr : TypeExpr → String
showTypeExpr (TRef n)       = n
showTypeExpr (TArrow t₁ t₂) = "(" ++s showTypeExpr t₁ ++s " → " ++s showTypeExpr t₂ ++s ")"
showTypeExpr (TProd t₁ t₂)  = "(" ++s showTypeExpr t₁ ++s " × " ++s showTypeExpr t₂ ++s ")"
showTypeExpr (TSigma t₁ t₂) = "Σ(" ++s showTypeExpr t₁ ++s ")." ++s showTypeExpr t₂
showTypeExpr (TId t)        = "Id(" ++s showTypeExpr t ++s ")"
showTypeExpr (TLoop n t)    = "Ω" ++s showTypeExpr t
showTypeExpr (TSusp t)      = "Σ" ++s showTypeExpr t
showTypeExpr (TTrunc n t)   = "‖" ++s showTypeExpr t ++s "‖"

-- Show a schema
showSchema : OpSchema → String
showSchema (Exist t)              = "EXIST(" ++s showTypeExpr t ++s ")"
showSchema (PathBetween t c₁ c₂)  = "PATH(" ++s c₁ ++s " =_" ++s showTypeExpr t ++s " " ++s c₂ ++s ")"
showSchema (PathDim t n)          = "PATH-DIM-" ++s showTypeExpr t
showSchema (MapIn s t)            = "MAP-IN(" ++s showTypeExpr s ++s " → " ++s showTypeExpr t ++s ")"
showSchema (MapOut s t)           = "MAP-OUT(" ++s showTypeExpr s ++s " → " ++s showTypeExpr t ++s ")"
showSchema (MapSelf t)            = "MAP-SELF(" ++s showTypeExpr t ++s ")"
showSchema (Bridge s t u)         = "BRIDGE(" ++s showTypeExpr s ++s " → " ++s showTypeExpr t ++s " → " ++s showTypeExpr u ++s ")"
showSchema (DepElim t _)          = "DEP-ELIM(" ++s showTypeExpr t ++s ")"
showSchema (DepPair t _)          = "DEP-PAIR(" ++s showTypeExpr t ++s ")"
showSchema (Group t)              = "GROUP(" ++s showTypeExpr t ++s ")"
showSchema (GroupOp t _)          = "GROUP-OP(" ++s showTypeExpr t ++s ")"
showSchema (Ring t)               = "RING(" ++s showTypeExpr t ++s ")"
showSchema (Module s t)           = "MODULE(" ++s showTypeExpr s ++s ", " ++s showTypeExpr t ++s ")"
showSchema (LoopSpace t n)        = "LOOP-SPACE(" ++s showTypeExpr t ++s ")"
showSchema (HomotopyGroup t n)    = "HOMOTOPY-GROUP(" ++s showTypeExpr t ++s ")"
showSchema (Suspension t)         = "SUSPENSION(" ++s showTypeExpr t ++s ")"
showSchema (HomotopyCalc s t n)   = "HOMOTOPY-CALC(" ++s showTypeExpr s ++s ", " ++s showTypeExpr t ++s ")"
showSchema (Fiber s t)            = "FIBER(" ++s showTypeExpr s ++s " → " ++s showTypeExpr t ++s ")"
showSchema (TotalSpace t)         = "TOTAL-SPACE(" ++s showTypeExpr t ++s ")"
showSchema (Section t)            = "SECTION(" ++s showTypeExpr t ++s ")"
showSchema (LESConnect t)         = "LES-CONNECT(" ++s showTypeExpr t ++s ")"
showSchema (Classifying t)        = "CLASSIFYING(" ++s showTypeExpr t ++s ")"
showSchema (CharacteristicClass t _) = "CHAR-CLASS(" ++s showTypeExpr t ++s ")"
showSchema (TruncLevel t n)       = "TRUNC-LEVEL(" ++s showTypeExpr t ++s ")"
showSchema (TruncInteract s t)    = "TRUNC-INTERACT(" ++s showTypeExpr s ++s ", " ++s showTypeExpr t ++s ")"
