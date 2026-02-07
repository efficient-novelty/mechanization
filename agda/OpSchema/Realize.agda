{-# OPTIONS --guardedness --without-K #-}

module OpSchema.Realize where

-- ============================================
-- Schema Realizability Checking
-- ============================================
--
-- This module provides (approximate) checks for whether
-- an operation schema is realizable in a given library context.
--
-- IMPORTANT: True realizability is undecidable in general.
-- This module provides conservative approximations.
-- ============================================

open import OpSchema.Core
open import OpSchema.Enumerate

-- ============================================
-- Utility Functions
-- ============================================

-- Check if a name is in the library
inLibrary : TypeName → Library → Bool
inLibrary n [] = false
inLibrary n (e ∷ es) with primStringEquality n (LibEntry.libName e)
... | true  = true
... | false = inLibrary n es

-- Check if any element satisfies predicate
any : {A : Set} → (A → Bool) → List A → Bool
any p [] = false
any p (x ∷ xs) with p x
... | true  = true
... | false = any p xs

-- Boolean and
_&&_ : Bool → Bool → Bool
true  && b = b
false && _ = false

-- Boolean or
_||_ : Bool → Bool → Bool
true  || _ = true
false || b = b

-- Boolean not
not : Bool → Bool
not true  = false
not false = true

-- Check if Π types are available (needed for function types)
hasPi : Library → Bool
hasPi lib = inLibrary "Π/Σ" lib || inLibrary "Π" lib

-- ============================================
-- Realizability Judgments
-- ============================================

-- A type expression is "well-formed" if all its references are in scope
typeWellFormed : TypeExpr → Library → TypeName → Bool
typeWellFormed (TRef n) lib x =
  primStringEquality n x || inLibrary n lib
typeWellFormed (TArrow t₁ t₂) lib x =
  typeWellFormed t₁ lib x && typeWellFormed t₂ lib x
typeWellFormed (TProd t₁ t₂) lib x =
  typeWellFormed t₁ lib x && typeWellFormed t₂ lib x
typeWellFormed (TSigma t₁ t₂) lib x =
  typeWellFormed t₁ lib x && typeWellFormed t₂ lib x
typeWellFormed (TId t) lib x =
  typeWellFormed t lib x
typeWellFormed (TLoop n t) lib x =
  typeWellFormed t lib x
typeWellFormed (TSusp t) lib x =
  typeWellFormed t lib x
typeWellFormed (TTrunc n t) lib x =
  typeWellFormed t lib x

-- Check if a type expression mentions the new type X
mentionsX : TypeExpr → TypeName → Bool
mentionsX (TRef n) x = primStringEquality n x
mentionsX (TArrow t₁ t₂) x = mentionsX t₁ x || mentionsX t₂ x
mentionsX (TProd t₁ t₂) x = mentionsX t₁ x || mentionsX t₂ x
mentionsX (TSigma t₁ t₂) x = mentionsX t₁ x || mentionsX t₂ x
mentionsX (TId t) x = mentionsX t x
mentionsX (TLoop n t) x = mentionsX t x
mentionsX (TSusp t) x = mentionsX t x
mentionsX (TTrunc n t) x = mentionsX t x

-- ============================================
-- Realizability for Each Schema Type
-- ============================================

-- EXIST: Realizable if X is a valid type with at least one constructor
realizeExist : TypeDescriptor → Bool
realizeExist td = not (null (TypeDescriptor.tdConstructors td))
  where
    null : {A : Set} → List A → Bool
    null [] = true
    null (_ ∷ _) = false

-- PATH: Realizable if X has non-trivial paths
realizePath : TypeDescriptor → Nat → Bool
realizePath td dim = TypeDescriptor.tdPathDim td ≥? dim
  where
    _≥?_ : Nat → Nat → Bool
    m ≥? zero = true
    zero ≥? suc n = false
    suc m ≥? suc n = m ≥? n

-- MAP-IN: Realizable if source has inhabitants, X has constructors, AND Π types exist
realizeMapIn : TypeName → Library → Bool
realizeMapIn srcName lib =
  hasPi lib &&
  -- Check if source type exists in library
  any (λ e → primStringEquality srcName (LibEntry.libName e) &&
             (LibEntry.libCardinality e ≥? 1)) lib
  where
    _≥?_ : Nat → Nat → Bool
    m ≥? zero = true
    zero ≥? suc n = false
    suc m ≥? suc n = m ≥? n

-- MAP-OUT: Realizable if target in library AND Π types exist
realizeMapOut : TypeName → Library → Bool
realizeMapOut tgtName lib = hasPi lib && inLibrary tgtName lib

-- MAP-SELF: Realizable if X → X has non-trivial maps AND Π types exist
realizeMapSelf : TypeDescriptor → Library → Bool
realizeMapSelf td lib =
  hasPi lib && ((TypeDescriptor.tdPathDim td ≥? 1) || TypeDescriptor.tdHasGroup td)
  where
    _≥?_ : Nat → Nat → Bool
    m ≥? zero = true
    zero ≥? suc n = false
    suc m ≥? suc n = m ≥? n

-- DEP-ELIM: Realizable if X has an eliminator AND Π types exist
realizeDepElim : TypeDescriptor → Library → Bool
realizeDepElim td lib =
  hasPi lib && not (null (TypeDescriptor.tdConstructors td))
  where
    null : {A : Set} → List A → Bool
    null [] = true
    null (_ ∷ _) = false

-- GROUP: Realizable if td says it has group structure
realizeGroup : TypeDescriptor → Bool
realizeGroup td = TypeDescriptor.tdHasGroup td

-- LOOP-SPACE: Realizable if X has paths of the given dimension
realizeLoopSpace : TypeDescriptor → Nat → Bool
realizeLoopSpace td n = TypeDescriptor.tdPathDim td ≥? n
  where
    _≥?_ : Nat → Nat → Bool
    m ≥? zero = true
    zero ≥? suc n = false
    suc m ≥? suc n = m ≥? n

-- SUSPENSION: Requires Σ types (pushouts need Σ)
realizeSuspension : Library → Bool
realizeSuspension lib = hasPi lib  -- Π/Σ needed for suspension

-- FIBRATION: Realizable if td is a fibration
realizeFibration : TypeDescriptor → Bool
realizeFibration td = TypeDescriptor.tdIsFibration td

-- TRUNCATION: Always realizable if we have truncation in the library
realizeTrunc : Library → Bool
realizeTrunc lib = inLibrary "‖-‖" lib || inLibrary "PropTrunc" lib

-- ============================================
-- Main Realizability Check
-- ============================================

-- Count list length
len' : {A : Set} → List A → Nat
len' [] = 0
len' (_ ∷ xs) = suc (len' xs)

-- Extend library to include X (for realizability checking)
extendLibrary : TypeDescriptor → Library → Library
extendLibrary td lib =
  mkLibEntry
    (TypeDescriptor.tdName td)
    (len' (TypeDescriptor.tdConstructors td))
    (TypeDescriptor.tdConstructors td)
  ∷ lib

-- Check if a schema is realizable given a type descriptor and library
-- NOTE: We check realizability in L ∪ {X}, not just L
isRealizable : OpSchema → TypeDescriptor → Library → Bool
isRealizable (Exist t) td lib = realizeExist td
isRealizable (PathBetween t c₁ c₂) td lib = realizePath td 1
isRealizable (PathDim t n) td lib = realizePath td n
isRealizable (MapIn (TRef srcName) t) td lib = realizeMapIn srcName extLib
  where extLib = extendLibrary td lib
isRealizable (MapIn _ t) td lib = false  -- Non-ref source, complex case
isRealizable (MapOut t (TRef tgtName)) td lib = realizeMapOut tgtName extLib
  where extLib = extendLibrary td lib
isRealizable (MapOut t _) td lib = false  -- Non-ref target, complex case
isRealizable (MapSelf t) td lib = realizeMapSelf td extLib
  where extLib = extendLibrary td lib
isRealizable (Bridge s t u) td lib = hasPi extLib
  where extLib = extendLibrary td lib
isRealizable (DepElim t _) td lib = realizeDepElim td extLib
  where extLib = extendLibrary td lib
isRealizable (DepPair t _) td lib = realizeDepElim td extLib
  where extLib = extendLibrary td lib
isRealizable (Group t) td lib = realizeGroup td
isRealizable (GroupOp t _) td lib = realizeGroup td
isRealizable (Ring t) td lib = TypeDescriptor.tdHasRing td
isRealizable (Module s t) td lib = realizeGroup td
isRealizable (LoopSpace t n) td lib = realizeLoopSpace td n
isRealizable (HomotopyGroup t n) td lib = realizeLoopSpace td n
isRealizable (Suspension t) td lib = realizeSuspension extLib
  where extLib = extendLibrary td lib
isRealizable (HomotopyCalc s t n) td lib = realizeLoopSpace td n
isRealizable (Fiber s t) td lib = realizeFibration td
isRealizable (TotalSpace t) td lib = realizeFibration td
isRealizable (Section t) td lib = realizeFibration td
isRealizable (LESConnect t) td lib = realizeFibration td
isRealizable (Classifying t) td lib = realizeFibration td
isRealizable (CharacteristicClass t _) td lib = realizeFibration td
isRealizable (TruncLevel t n) td lib = realizeTrunc extLib
  where extLib = extendLibrary td lib
isRealizable (TruncInteract s t) td lib = realizeTrunc extLib
  where extLib = extendLibrary td lib

-- ============================================
-- Filter realizable schemas
-- ============================================

filterRealizable : List OpSchema → TypeDescriptor → Library → List OpSchema
filterRealizable schemas td lib = filter (λ s → isRealizable s td lib) schemas
