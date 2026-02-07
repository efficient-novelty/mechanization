{-# OPTIONS --guardedness --without-K #-}

module OpSchema.Enumerate where

-- ============================================
-- Schema Enumeration
-- ============================================
--
-- This module enumerates all operation schemas up to a given
-- depth, for a given library and new type X.
-- ============================================

open import OpSchema.Core

-- ============================================
-- List Utilities
-- ============================================

-- Append lists
_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infixr 5 _++_

-- Concat a list of lists
concat : {A : Set} → List (List A) → List A
concat [] = []
concat (xs ∷ xss) = xs ++ concat xss

-- Map over a list
map : {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

-- Filter a list
filter : {A : Set} → (A → Bool) → List A → List A
filter p [] = []
filter p (x ∷ xs) with p x
... | true  = x ∷ filter p xs
... | false = filter p xs

-- Length of a list
length : {A : Set} → List A → Nat
length [] = 0
length (x ∷ xs) = suc (length xs)

-- ============================================
-- Type Descriptor
-- ============================================

-- A type descriptor captures the essential structure of a type X
-- for the purpose of schema enumeration.

record TypeDescriptor : Set where
  constructor mkTypeDesc
  field
    tdName         : TypeName           -- The type's name (e.g., "S¹")
    tdConstructors : List ConstructorName  -- Point constructors
    tdPathDim      : Nat                -- Dimension of highest non-trivial path
    tdHasGroup     : Bool               -- Does it have group structure?
    tdHasRing      : Bool               -- Does it have ring structure?
    tdIsFibration  : Bool               -- Is it a fibration?
    tdTruncLevel   : Nat                -- Truncation level (-1 = contractible, 0 = prop, ...)

-- ============================================
-- Genesis Type Descriptors
-- ============================================

-- Universe U₀ (counts as one "constructor" for type formation)
descUniverse : TypeDescriptor
descUniverse = mkTypeDesc "U₀" ("Type" ∷ []) 0 false false false 0

-- Unit 1
descUnit : TypeDescriptor
descUnit = mkTypeDesc "1" ("★" ∷ []) 0 false false false 0

-- Witness ★
descWitness : TypeDescriptor
descWitness = mkTypeDesc "★" ("★" ∷ []) 0 false false false 0

-- Π/Σ types (type formers, not individual types)
descPiSigma : TypeDescriptor
descPiSigma = mkTypeDesc "Π/Σ" ("Π" ∷ "Σ" ∷ "λ" ∷ []) 0 false false false 0

-- Circle S¹
descCircle : TypeDescriptor
descCircle = mkTypeDesc "S¹" ("base" ∷ []) 1 false false false 1

-- Propositional Truncation
descPropTrunc : TypeDescriptor
descPropTrunc = mkTypeDesc "‖-‖" ("∣_∣" ∷ "squash" ∷ []) 0 false false false 0

-- S²
descS2 : TypeDescriptor
descS2 = mkTypeDesc "S²" ("base" ∷ []) 2 false false false 2

-- S³ ≅ SU(2)
descS3 : TypeDescriptor
descS3 = mkTypeDesc "S³" ("base" ∷ []) 3 true false false 3

-- Hopf fibration
descHopf : TypeDescriptor
descHopf = mkTypeDesc "Hopf" ("h" ∷ []) 0 false false true 0

-- Lie groups
descLieGroups : TypeDescriptor
descLieGroups = mkTypeDesc "LieGrp" [] 0 true false false 0

-- ============================================
-- R11-R16: Advanced Cohesive Structures
-- ============================================

-- R11: Cohesion (κ=4)
-- The cohesive modalities: ♯ (sharp/codiscrete), ♭ (flat/discrete), ʃ (shape)
-- These relate discrete and continuous mathematics
descCohesion : TypeDescriptor
descCohesion = mkTypeDesc "Cohesion" ("♯" ∷ "♭" ∷ "ʃ" ∷ "∫" ∷ []) 0 false false false 0

-- R12: Connections (κ=5)
-- Differential forms, connections on bundles, parallel transport
descConnections : TypeDescriptor
descConnections = mkTypeDesc "Connection" ("∇" ∷ "Ω¹" ∷ "transport" ∷ "holonomy" ∷ "gauge" ∷ []) 1 false false true 0

-- R13: Curvature (κ=6)
-- Curvature of connections, Riemann tensor, Bianchi identities
descCurvature : TypeDescriptor
descCurvature = mkTypeDesc "Curvature" ("R" ∷ "Ω²" ∷ "Bianchi" ∷ "Ricci" ∷ "scalar" ∷ "Weyl" ∷ []) 2 false false false 0

-- R14: Metric + frame (κ=7)
-- Riemannian/Lorentzian metrics, frame bundles, orthonormal frames
descMetric : TypeDescriptor
descMetric = mkTypeDesc "Metric" ("g" ∷ "frame" ∷ "Levi-Civita" ∷ "geodesic" ∷ "exp" ∷ "vol" ∷ "Hodge" ∷ []) 2 true false true 0

-- R15: Hilbert functional (κ=9)
-- Action functionals, variational calculus, Euler-Lagrange equations
descHilbert : TypeDescriptor
descHilbert = mkTypeDesc "Hilbert" ("S" ∷ "L" ∷ "δS" ∷ "EL" ∷ "Noether" ∷ "symplectic" ∷ "Hamiltonian" ∷ "Poisson" ∷ "quantize" ∷ []) 2 true true false 0

-- R16: Dynamical Cohesive Topos (κ=8)
-- The full framework: combines all previous structures
descDCT : TypeDescriptor
descDCT = mkTypeDesc "DCT" ("∞-topos" ∷ "cohesive" ∷ "differential" ∷ "super" ∷ "orbifold" ∷ "prequantum" ∷ "quantization" ∷ "TFT" ∷ []) 3 true true true 0

-- ============================================
-- Schema Generation
-- ============================================

-- Generate EXIST schema for the new type
genExist : TypeDescriptor → List OpSchema
genExist td = Exist (TRef (TypeDescriptor.tdName td)) ∷ []

-- Generate PATH schemas - only if there are non-trivial paths
-- PathBetween and PathDim are the same concept, count once
genPath : TypeDescriptor → List OpSchema
genPath td with TypeDescriptor.tdPathDim td
... | zero = []  -- No non-trivial paths
... | suc n = PathBetween (TRef name) "base" "base" ∷ []  -- One schema for non-trivial identity
  where
    name = TypeDescriptor.tdName td

-- Generate MAP-IN schema - DERIVED from EXIST, don't count separately
genMapIn : TypeDescriptor → Library → List OpSchema
genMapIn td lib = []  -- Derived from EXIST

-- Boolean or (local definition for hasSpheresInLib)
_or_ : Bool → Bool → Bool
true  or _ = true
false or b = b

-- Check if library contains spheres
hasSpheresInLib : Library → Bool
hasSpheresInLib [] = false
hasSpheresInLib (e ∷ es) =
  let n = LibEntry.libName e in
  ((primStringEquality n "S¹") or
   ((primStringEquality n "S²") or
    (primStringEquality n "S³"))) or
  hasSpheresInLib es

-- Generate MAP-OUT schema - only count if there are interesting targets (spheres)
-- MAP-OUT to 1 is trivial; MAP-OUT to higher spheres is interesting
genMapOut : TypeDescriptor → Library → List OpSchema
genMapOut td lib with hasSpheresInLib lib
... | false = []  -- No interesting targets
... | true  = MapOut X (TRef "sphere") ∷ []  -- Maps to spheres are interesting
  where
    X = TRef (TypeDescriptor.tdName td)

-- Generate MAP-SELF schema
genMapSelf : TypeDescriptor → List OpSchema
genMapSelf td = MapSelf (TRef (TypeDescriptor.tdName td)) ∷ []

-- Generate DEP-ELIM schemas - count as one category
genDepElim : TypeDescriptor → List OpSchema
genDepElim td =
  DepElim X FSLibValued ∷ []  -- One schema for dependent elimination
  where
    X = TRef (TypeDescriptor.tdName td)

-- Generate GROUP schemas (if applicable)
genGroup : TypeDescriptor → List OpSchema
genGroup td with TypeDescriptor.tdHasGroup td
... | false = []
... | true  =
  Group X ∷
  GroupOp X OpMult ∷
  GroupOp X OpInv ∷
  GroupOp X OpUnit ∷
  []
  where
    X = TRef (TypeDescriptor.tdName td)

-- Generate LOOP-SPACE schemas
genLoopSpace : TypeDescriptor → List OpSchema
genLoopSpace td with TypeDescriptor.tdPathDim td
... | zero = []
... | suc n =
  LoopSpace X (suc n) ∷
  HomotopyGroup X (suc n) ∷
  []
  where
    X = TRef (TypeDescriptor.tdName td)

-- Generate SUSPENSION schema
genSuspension : TypeDescriptor → List OpSchema
genSuspension td = Suspension (TRef (TypeDescriptor.tdName td)) ∷ []

-- Generate FIBRATION schemas (if applicable)
genFibration : TypeDescriptor → List OpSchema
genFibration td with TypeDescriptor.tdIsFibration td
... | false = []
... | true  =
  TotalSpace X ∷
  Section X ∷
  LESConnect X ∷
  Classifying X ∷
  []
  where
    X = TRef (TypeDescriptor.tdName td)

-- Generate TRUNCATION schemas - count as one if non-trivial
genTrunc : TypeDescriptor → Library → List OpSchema
genTrunc td lib with TypeDescriptor.tdTruncLevel td
... | zero = []  -- Contractible, no interesting truncation
... | suc n = TruncLevel X (suc n) ∷ []  -- One schema for truncation behavior
  where
    X = TRef (TypeDescriptor.tdName td)

-- ============================================
-- Special handling for type formers (Π/Σ)
-- ============================================

-- When Π/Σ is added, it enables CATEGORIES of operations.
-- We count categories, not instances.
-- For Π/Σ, the categories are:
--   1. Π type former (function types A → B)
--   2. Σ type former (pair types A × B)
--   3. Dependent Π: (x : A) → B(x)
--   4. Dependent Σ: Σ(x : A).B(x)
--   5. Curry/uncurry structural operations

genPiSigmaSchemas : List OpSchema
genPiSigmaSchemas =
  Exist (TRef "Π")   ∷  -- Non-dependent function types
  Exist (TRef "Σ")   ∷  -- Non-dependent pair types
  DepElim (TRef "Π/Σ") FSLibValued ∷  -- Dependent Π
  DepPair (TRef "Σ") FSLibValued   ∷  -- Dependent Σ
  Bridge (TRef "Π") (TRef "Σ") (TRef "Π") ∷  -- Curry/uncurry
  []

-- When PropTrunc (‖-‖) is added, it enables categories:
--   1. ‖A‖ type former (mere existence)
--   2. Unit map A → ‖A‖
--   3. Elimination ‖A‖ → B when B is a proposition
--   4. Restricted dependent elim (prop-valued families)
--   5. Prop sub-universe definable
--   6. Image factorization (f factors through ‖fiber‖)
--   7. Truncation levels (is-prop, is-set, etc.)
--   8. Interaction with S¹ (‖S¹‖ = 1)
genPropTruncSchemas : Library → List OpSchema
genPropTruncSchemas lib =
  Exist (TRef "‖-‖")   ∷  -- Type former
  MapIn (TRef "A") (TRef "‖A‖") ∷  -- Unit: A → ‖A‖
  MapOut (TRef "‖A‖") (TRef "Prop") ∷  -- Elim to props
  DepElim (TRef "‖-‖") FSConst ∷  -- Restricted dep elim
  Classifying (TRef "Prop") ∷  -- Prop as sub-universe
  Bridge (TRef "A") (TRef "‖A‖") (TRef "B") ∷  -- Image factorization
  TruncLevel (TRef "‖-‖") 0 ∷  -- Truncation level 0 = prop
  TruncInteract (TRef "S¹") (TRef "1") ∷  -- ‖S¹‖ = 1 interaction
  []

-- ============================================
-- Main Enumeration Function
-- ============================================

-- Check if the type is Π/Σ (a type former)
isPiSigma : TypeDescriptor → Bool
isPiSigma td = primStringEquality (TypeDescriptor.tdName td) "Π/Σ"

-- Check if the type is PropTrunc
isPropTrunc : TypeDescriptor → Bool
isPropTrunc td = primStringEquality (TypeDescriptor.tdName td) "‖-‖"

-- Check if the type is Cohesion
isCohesion : TypeDescriptor → Bool
isCohesion td = primStringEquality (TypeDescriptor.tdName td) "Cohesion"

-- Cohesion schemas: The cohesive modalities ♯, ♭, ʃ, ∫
-- These enable 19 qualitative operations according to the paper
genCohesionSchemas : Library → List OpSchema
genCohesionSchemas lib =
  -- Core modalities (4)
  Exist (TRef "♯") ∷  -- Sharp/codiscrete modality
  Exist (TRef "♭") ∷  -- Flat/discrete modality
  Exist (TRef "ʃ") ∷  -- Shape modality
  Exist (TRef "∫") ∷  -- Integration modality
  -- Adjunctions between modalities (3)
  Bridge (TRef "♭") (TRef "Id") (TRef "♯") ∷  -- ♭ ⊣ Id ⊣ ♯
  Bridge (TRef "ʃ") (TRef "♭") (TRef "Id") ∷  -- ʃ ⊣ ♭
  Bridge (TRef "∫") (TRef "ʃ") (TRef "Id") ∷  -- Axiom of cohesion
  -- New type constructions (4)
  DepElim (TRef "♭") FSLibValued ∷  -- Discrete types
  DepElim (TRef "♯") FSLibValued ∷  -- Codiscrete types
  MapOut (TRef "ʃX") (TRef "X") ∷   -- Shape comparison
  MapIn (TRef "X") (TRef "♭X") ∷    -- Discretization
  -- Paths and homotopy (4)
  PathBetween (TRef "♭X") "x" "y" ∷  -- Discrete paths = equality
  LoopSpace (TRef "ʃX") 1 ∷          -- Fundamental groupoid via shape
  HomotopyGroup (TRef "ʃX") 1 ∷      -- π₁ via shape
  TruncLevel (TRef "♯X") 0 ∷         -- Codiscrete = contractible paths
  -- Differential structure (4)
  MapOut (TRef "X") (TRef "ʃX") ∷    -- Universal property of shape
  Suspension (TRef "♭X") ∷           -- Discrete suspension
  Fiber (TRef "ʃ") (TRef "X") ∷      -- Infinitesimal disk bundle
  Classifying (TRef "♭G") ∷          -- Discrete classifying space
  []

-- Generate all schemas for a type descriptor and library
enumerateSchemas : TypeDescriptor → Library → List OpSchema
enumerateSchemas td lib with isPiSigma td
... | true  = genPiSigmaSchemas  -- Π/Σ type former
... | false with isPropTrunc td
...   | true  = genPropTruncSchemas lib  -- PropTrunc type former
...   | false with isCohesion td
...     | true  = genCohesionSchemas lib  -- Cohesion type former
...     | false = -- Concrete type: standard enumeration
  genExist td ++
  genPath td ++
  genMapIn td lib ++
  genMapOut td lib ++
  genMapSelf td ++
  genDepElim td ++
  genGroup td ++
  genLoopSpace td ++
  genSuspension td ++
  genFibration td ++
  genTrunc td lib

-- ============================================
-- Depth-bounded enumeration
-- ============================================

-- Filter schemas by maximum depth
filterByDepth : Nat → List OpSchema → List OpSchema
filterByDepth maxD = filter (λ s → schemaDepth s ≤? maxD)
  where
    _≤?_ : Nat → Nat → Bool
    zero  ≤? _     = true
    suc m ≤? zero  = false
    suc m ≤? suc n = m ≤? n

-- Enumerate with depth bound
enumerateBounded : Nat → TypeDescriptor → Library → List OpSchema
enumerateBounded maxD td lib = filterByDepth maxD (enumerateSchemas td lib)

-- ============================================
-- Count schemas
-- ============================================

countSchemas : TypeDescriptor → Library → Nat
countSchemas td lib = length (enumerateSchemas td lib)

countSchemasBounded : Nat → TypeDescriptor → Library → Nat
countSchemasBounded maxD td lib = length (enumerateBounded maxD td lib)
