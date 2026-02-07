{-# OPTIONS --guardedness --without-K #-}

module Oracle.Nu where

-- ============================================
-- Note: This module uses --without-K instead of --cubical
-- because Agda 2.8.0's reflection API has compatibility
-- issues with --cubical (InfectiveImport errors).
--
-- IMPORTANT: First run requires --ignore-all-interfaces flag
-- to clear cached interface files:
--   agda --ignore-all-interfaces Oracle/Nu.agda
-- ============================================

-- ============================================
-- Imports (same pattern as Kappa.agda)
-- ============================================

open import Agda.Builtin.Reflection public
open import Agda.Builtin.List public
open import Agda.Builtin.String public
open import Agda.Builtin.Bool public
open import Agda.Builtin.Unit public
open import Agda.Builtin.Nat public

-- Identity type (standard, not cubical)
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}

infix 4 _≡_

-- Maybe type
data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

-- ============================================
-- The ν-Oracle: Measuring Novelty
-- ============================================

-- ν(X) measures the "enabling power" of a type X.
-- This is the hardest and most important measure.
--
-- From the Inductive Exponentiality Theorem:
--   ν ≤ 2^Δ (maps to Bool)
--
-- But actual ν values are much smaller than this upper bound.

-- ============================================
-- List and Arithmetic Utilities
-- ============================================

len : {A : Set} → List A → Nat
len [] = zero
len (x ∷ xs) = suc (len xs)

-- Exponentiation: m ^ n
_^_ : Nat → Nat → Nat
m ^ zero = 1
m ^ suc n = m * (m ^ n)

infixr 8 _^_

-- Sum a list of naturals
sumList : List Nat → Nat
sumList [] = 0
sumList (x ∷ xs) = x + sumList xs

-- Map over a list
mapList : {A B : Set} → (A → B) → List A → List B
mapList f [] = []
mapList f (x ∷ xs) = f x ∷ mapList f xs

-- Fold over a list
foldr : {A B : Set} → (A → B → B) → B → List A → B
foldr f z [] = z
foldr f z (x ∷ xs) = f x (foldr f z xs)

-- ============================================
-- Library: Collection of Types with Cardinalities
-- ============================================

-- A LibraryEntry records a type's name and its cardinality
-- (number of elements / constructors for finite types)
record LibraryEntry : Set where
  constructor mkEntry
  field
    entryName : String
    cardinality : Nat

-- A Library is a list of entries
Library : Set
Library = List LibraryEntry

-- Get cardinalities from a library
getCardinalities : Library → List Nat
getCardinalities = mapList LibraryEntry.cardinality

-- ============================================
-- Definition A: Eliminator Branch Count
-- ============================================

-- Given type X with k constructors, the novelty is:
--   ν_A(X) = Σ_{Y ∈ Library} |Y|^k
--
-- This counts the total number of distinct functions X → Y
-- definable by pattern matching, for all Y in the library.

νEliminator : Nat → Library → Nat
νEliminator k lib = sumList (mapList (λ m → m ^ k) (getCardinalities lib))

-- ============================================
-- Definition B: Simplified Enabling Power
-- ============================================

-- Alternative: ν = 2^(constructors) - 1
-- This is simpler and context-independent.
-- It counts maps to Bool minus the trivial constant map.

νExponential : Nat → Nat
νExponential k = (2 ^ k)

-- A variant: just use the constructor count itself
-- This is the minimal ν, assuming each constructor enables
-- at least one new map.

νMinimal : Nat → Nat
νMinimal k = k

-- ============================================
-- Reflection-based ν computation
-- ============================================

-- Get constructor count from a type name (same as κ)
handleDef : Definition → TC Nat
handleDef (data-type pars cs) = returnTC (len cs)
handleDef (record-type c fs)  = returnTC (suc (len fs))
handleDef (function cs)       = returnTC (len cs)
handleDef (data-cons d q)     = returnTC 1
handleDef axiom               = returnTC 0
handleDef prim-fun            = returnTC 0

countCons : Name → TC Nat
countCons n = bindTC (getDefinition n) handleDef

-- Compute ν using Definition A (eliminator count)
-- Requires a library context
computeNuElim : Name → Library → TC Nat
computeNuElim typeName lib =
  bindTC (countCons typeName) (λ k → returnTC (νEliminator k lib))

-- Compute ν using exponential bound (context-free)
computeNuExp : Name → TC Nat
computeNuExp typeName =
  bindTC (countCons typeName) (λ k → returnTC (νExponential k))

-- ============================================
-- Macro for computing ν at compile time
-- ============================================

unifyNat : Term → Nat → TC ⊤
unifyNat hole n = unify hole (lit (nat n))

-- Macro using exponential bound (simpler, no library needed)
ν-exp-macro : Name → Term → TC ⊤
ν-exp-macro x hole = bindTC (computeNuExp x) (unifyNat hole)

macro
  νExp : Name → Term → TC ⊤
  νExp = ν-exp-macro

-- ============================================
-- Test Types (same as Kappa.agda)
-- ============================================

-- Unit type
data ⊤' : Set where
  tt' : ⊤'

-- Boolean type
data Bool' : Set where
  true'  : Bool'
  false' : Bool'

-- Three-element type
data Tri : Set where
  one : Tri
  two : Tri
  three : Tri

-- Four-element type
data Quad : Set where
  q1 : Quad
  q2 : Quad
  q3 : Quad
  q4 : Quad

-- ============================================
-- Test Library for Definition A
-- ============================================

-- A minimal library containing Unit and Bool
minimalLib : Library
minimalLib = mkEntry "Unit" 1 ∷ mkEntry "Bool" 2 ∷ []

-- Library after adding Unit
afterUnit : Library
afterUnit = mkEntry "Unit" 1 ∷ []

-- Library after adding Bool
afterBool : Library
afterBool = mkEntry "Unit" 1 ∷ mkEntry "Bool" 2 ∷ []

-- Library after adding Tri
afterTri : Library
afterTri = mkEntry "Unit" 1 ∷ mkEntry "Bool" 2 ∷ mkEntry "Tri" 3 ∷ []

-- ============================================
-- ν Tests using Definition A
-- ============================================

-- For Unit (k=1) in minimal library {Unit(1), Bool(2)}:
-- ν = 1^1 + 2^1 = 1 + 2 = 3
νUnit-in-minLib : Nat
νUnit-in-minLib = νEliminator 1 minimalLib

test-νUnit-in-minLib : νUnit-in-minLib ≡ 3
test-νUnit-in-minLib = refl

-- For Bool (k=2) in minimal library:
-- ν = 1^2 + 2^2 = 1 + 4 = 5
νBool-in-minLib : Nat
νBool-in-minLib = νEliminator 2 minimalLib

test-νBool-in-minLib : νBool-in-minLib ≡ 5
test-νBool-in-minLib = refl

-- For Tri (k=3) in minimal library:
-- ν = 1^3 + 2^3 = 1 + 8 = 9
νTri-in-minLib : Nat
νTri-in-minLib = νEliminator 3 minimalLib

test-νTri-in-minLib : νTri-in-minLib ≡ 9
test-νTri-in-minLib = refl

-- For Quad (k=4) in minimal library:
-- ν = 1^4 + 2^4 = 1 + 16 = 17
νQuad-in-minLib : Nat
νQuad-in-minLib = νEliminator 4 minimalLib

test-νQuad-in-minLib : νQuad-in-minLib ≡ 17
test-νQuad-in-minLib = refl

-- ============================================
-- ν Tests using Exponential Bound
-- ============================================

νExp-⊤' : Nat
νExp-⊤' = νExp ⊤'

νExp-Bool' : Nat
νExp-Bool' = νExp Bool'

νExp-Tri : Nat
νExp-Tri = νExp Tri

νExp-Quad : Nat
νExp-Quad = νExp Quad

-- Exponential bound: ν = 2^k
-- Unit: 2^1 = 2
test-νExp-⊤' : νExp-⊤' ≡ 2
test-νExp-⊤' = refl

-- Bool: 2^2 = 4
test-νExp-Bool' : νExp-Bool' ≡ 4
test-νExp-Bool' = refl

-- Tri: 2^3 = 8
test-νExp-Tri : νExp-Tri ≡ 8
test-νExp-Tri = refl

-- Quad: 2^4 = 16
test-νExp-Quad : νExp-Quad ≡ 16
test-νExp-Quad = refl

-- ============================================
-- Empirical ν Values from the Genesis Table
-- ============================================

-- These are the values observed/claimed in the paper.
-- Phase 3 attempts to derive these computationally.

-- Genesis Sequence ν values (for validation)
-- n=1: Universe,      ν=1,   κ=2
-- n=2: Unit,          ν=1,   κ=1
-- n=3: Witness,       ν=2,   κ=1
-- n=4: Π/Σ types,     ν=5,   κ=3
-- n=5: Circle,        ν=7,   κ=3
-- n=6: Prop trunc,    ν=8,   κ=3
-- n=7: S²,            ν=10,  κ=3
-- n=8: S³/SU(2),      ν=18,  κ=5
-- n=9: Hopf,          ν=17,  κ=4
-- n=10: Lie groups,   ν=9,   κ=2
-- n=11: Cohesion,     ν=19,  κ=4
-- n=12: Connections,  ν=26,  κ=5
-- n=13: Curvature,    ν=34,  κ=6
-- n=14: Metric,       ν=43,  κ=7
-- n=15: Hilbert,      ν=60,  κ=9
-- n=16: DCT,          ν=150, κ=8

νGenesis : Nat → Nat
νGenesis 1  = 1
νGenesis 2  = 1
νGenesis 3  = 2
νGenesis 4  = 5
νGenesis 5  = 7
νGenesis 6  = 8
νGenesis 7  = 10
νGenesis 8  = 18
νGenesis 9  = 17
νGenesis 10 = 9
νGenesis 11 = 19
νGenesis 12 = 26
νGenesis 13 = 34
νGenesis 14 = 43
νGenesis 15 = 60
νGenesis 16 = 150
νGenesis _  = 0

-- Corresponding κ values from the paper
κGenesis : Nat → Nat
κGenesis 1  = 2
κGenesis 2  = 1
κGenesis 3  = 1
κGenesis 4  = 3
κGenesis 5  = 3
κGenesis 6  = 3
κGenesis 7  = 3
κGenesis 8  = 5
κGenesis 9  = 4
κGenesis 10 = 2
κGenesis 11 = 4
κGenesis 12 = 5
κGenesis 13 = 6
κGenesis 14 = 7
κGenesis 15 = 9
κGenesis 16 = 8
κGenesis _  = 1

-- ============================================
-- Analysis: Comparing Computed vs Genesis ν
-- ============================================

-- For Unit (κ=1) entering empty library:
-- Computed νExp = 2^1 = 2
-- Genesis ν = 1
-- MISMATCH: Our exponential bound is an overestimate

-- For Bool (κ=2) entering library {Unit}:
-- Computed using Definition A: 1^2 = 1
-- Genesis ν for Bool would be κ=2, but genesis table shows...
-- Actually genesis n=2 is Unit, not Bool

-- The Genesis table doesn't directly list Bool as a separate step.
-- Let's check what the table actually claims:

-- Genesis n=2: Unit with ν=1, κ=1
-- If we compute: type with 1 constructor entering empty lib
-- νEliminator with empty lib = 0 (no target types yet)
-- This suggests we need a different interpretation...

-- ============================================
-- Revised Definition: Intrinsic Novelty
-- ============================================

-- Perhaps ν should be intrinsic to the type, not context-dependent.
-- Looking at the genesis values:
--   Unit (κ=1): ν=1
--   Witness (κ=1): ν=2
--   Π/Σ (κ=3): ν=5
--   Circle (κ=3): ν=7
--
-- There's no simple formula here. The relationship is complex.
--
-- One observation: ν often slightly exceeds 2*κ for later structures.
-- This might reflect that each constructor enables multiple novel uses.

-- ============================================
-- Placeholder: ν = constructor count (simplest)
-- ============================================

-- The simplest possible ν: just count constructors.
-- This makes ν = κ, giving ρ = 1 always.
-- Not interesting, but a baseline.

νSimple : Nat → Nat
νSimple k = k

-- ============================================
-- Export for Efficiency module
-- ============================================

-- Re-export νGenesis and κGenesis for use elsewhere
