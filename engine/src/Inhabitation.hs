{-# LANGUAGE DeriveGeneric #-}

-- | Inhabitation heuristics for PEN information-theoretic framework
--
-- This module provides conservative approximations for type inhabitation.
-- The heuristics are deliberately conservative: Unknown is always safe,
-- it just means we undercount ν.

module Inhabitation where

import Types
import Data.Maybe (isJust, fromMaybe)

-- ============================================
-- Inhabitation Results
-- ============================================

-- | Result of inhabitation check
data InhabResult
  = Inhabited Witness    -- ^ Definitely inhabited, with witness
  | NotInhabited Reason  -- ^ Definitely not inhabited
  | Unknown              -- ^ Can't decide
  deriving (Eq, Show)

-- | Witness of inhabitation
data Witness
  = WConstructor String  -- ^ A constructor of the type
  | WUnit                -- ^ tt : 1
  | WRefl                -- ^ refl : x =_A x
  | WConst Witness       -- ^ const w : A -> B when B is inhabited by w
  | WPair Witness Witness -- ^ (a, b) : A × B
  | WInl Witness         -- ^ inl a : A + B
  | WInr Witness         -- ^ inr b : A + B
  | WLoop String         -- ^ The loop constructor of a HIT
  | WNorth               -- ^ north : Susp A
  | WVacuous             -- ^ vacuous function from empty type
  | WPiIntro Witness     -- ^ λx. w : (x:A) → B
  | WSigmaIntro Witness Witness -- ^ (a, b) : (x:A) × B
  deriving (Eq, Show)

-- | Reason for non-inhabitation
data Reason
  = REmpty               -- ^ The empty type
  | RNoConstructors      -- ^ A type with no constructors
  | RStructural String   -- ^ Structural impossibility
  deriving (Eq, Show)

-- | Check if result indicates inhabitation
isInhabited :: InhabResult -> Bool
isInhabited (Inhabited _) = True
isInhabited _ = False

-- ============================================
-- Main Inhabitation Checker
-- ============================================

-- | Check whether a type expression is inhabited in a library
-- This is a heuristic - it's conservative and may return Unknown
-- for types that are actually inhabited.
checkInhab :: TypeExpr -> Library -> InhabResult

-- Rule 1: Unit is always inhabited
checkInhab TUnit _ = Inhabited WUnit

-- Rule 2: Void is never inhabited
checkInhab TVoid _ = NotInhabited REmpty

-- Rule 3: Library types are inhabited if they have constructors
checkInhab (TRef name) lib = case getEntry name lib of
  Just entry
    | leConstructors entry > 0 -> Inhabited (WConstructor name)
    | otherwise -> Unknown  -- Might still be inhabited via other means
  Nothing -> Unknown  -- Type not in library

-- Rule 4: A → B is inhabited if B is inhabited (const function)
-- or if A is empty (vacuous function)
checkInhab (TArrow a b) lib = case checkInhab b lib of
  Inhabited w -> Inhabited (WConst w)
  _ -> case checkInhab a lib of
    NotInhabited _ -> Inhabited WVacuous
    _ -> Unknown

-- Rule 5: A × B is inhabited if both A and B are inhabited
checkInhab (TProd a b) lib = case (checkInhab a lib, checkInhab b lib) of
  (Inhabited wa, Inhabited wb) -> Inhabited (WPair wa wb)
  (NotInhabited r, _) -> NotInhabited r
  (_, NotInhabited r) -> NotInhabited r
  _ -> Unknown

-- Rule 6: A + B is inhabited if either A or B is inhabited
checkInhab (TCoprod a b) lib = case checkInhab a lib of
  Inhabited wa -> Inhabited (WInl wa)
  _ -> case checkInhab b lib of
    Inhabited wb -> Inhabited (WInr wb)
    NotInhabited _ -> case checkInhab a lib of
      NotInhabited _ -> NotInhabited (RStructural "empty coproduct")
      _ -> Unknown
    _ -> Unknown

-- Rule 7: x =_A x (reflexivity) is always inhabited if A is inhabited
checkInhab (TSelfId a) lib = case checkInhab a lib of
  Inhabited _ -> Inhabited WRefl
  _ -> Unknown

-- Rule 8: General identity type a =_A b
-- Conservative: only know it's inhabited if we can prove a ≡ b
checkInhab (TId _ _ _) _ = Unknown

-- Rule 9: Ω(A) is inhabited if A has a non-trivial loop
checkInhab (TOmega (TRef name)) lib = case getEntry name lib of
  Just entry
    | leHasLoop entry -> Inhabited (WLoop name)
    | otherwise -> Unknown  -- Might still have a loop we don't know about
  Nothing -> Unknown
checkInhab (TOmega _) _ = Unknown

-- Rule 10: Susp(A) is always inhabited (has north pole)
checkInhab (TSusp _) _ = Inhabited WNorth

-- Rule 11: Truncation ‖A‖_n is inhabited if A is inhabited
checkInhab (TTrunc _ a) lib = case checkInhab a lib of
  Inhabited w -> Inhabited w  -- |w|_n
  _ -> Unknown

-- Rule 12: Dependent product (x:A) → B
-- Conservative: only inhabited if B doesn't depend on x and B is inhabited
checkInhab (TPi _ _ b) lib = case checkInhab b lib of
  Inhabited w -> Inhabited (WPiIntro w)
  _ -> Unknown

-- Rule 13: Dependent sum (x:A) × B
-- Inhabited if A and B are both inhabited
checkInhab (TSigma _ a b) lib = case (checkInhab a lib, checkInhab b lib) of
  (Inhabited wa, Inhabited wb) -> Inhabited (WSigmaIntro wa wb)
  _ -> Unknown

-- Rule 14: HITs are inhabited if they have point constructors
checkInhab (THIT pts _) _
  | pts > 0 = Inhabited (WConstructor "pt")
  | otherwise = Unknown

-- Rule 15: Fibers - hard in general
checkInhab (TFiber _ _) _ = Unknown

-- Rule 16: Delooping BA - inhabited if it's classifying a group
checkInhab (TDeloop _) _ = Unknown

-- ============================================
-- Extended Inhabitation Checking
-- ============================================

-- | More sophisticated inhabitation check that uses type-specific knowledge
checkInhabExtended :: TypeExpr -> Library -> InhabResult

-- Known homotopy group inhabitants
checkInhabExtended (TOmega (TRef "S1")) _ = Inhabited (WLoop "S1")
checkInhabExtended (TOmega (TOmega (TRef "S2"))) _ = Inhabited (WLoop "S2")
checkInhabExtended (TOmega (TOmega (TOmega (TRef "S3")))) _ = Inhabited (WLoop "S3")

-- Truncated loop spaces give homotopy groups
checkInhabExtended (TTrunc 0 (TOmega (TRef "S1"))) _ =
  Inhabited (WConstructor "Z")  -- π₁(S¹) ≃ ℤ

-- Fall back to basic checker
checkInhabExtended t lib = checkInhab t lib

-- ============================================
-- Novelty Detection
-- ============================================

-- | Check if a type is NEWLY inhabited after adding X to library L
-- Returns True if:
--   1. Type is inhabited in L ∪ {X}
--   2. Type was NOT inhabited in L alone
isNewlyInhabited :: TypeExpr -> LibraryEntry -> Library -> Bool
isNewlyInhabited t newType lib =
  let oldResult = checkInhab t lib
      newLib = newType : lib
      newResult = checkInhab t newLib
  in case (oldResult, newResult) of
    (Inhabited _, _) -> False  -- Already inhabited
    (_, Inhabited _) -> True   -- Newly inhabited
    (_, NotInhabited _) -> False  -- Still not inhabited
    _ -> False  -- Unknown cases: be conservative

-- | More lenient version that counts Unknown -> Inhabited as new
isNewlyInhabitedLenient :: TypeExpr -> LibraryEntry -> Library -> Bool
isNewlyInhabitedLenient t newType lib =
  let oldResult = checkInhab t lib
      newLib = newType : lib
      newResult = checkInhab t newLib
  in case (oldResult, newResult) of
    (Inhabited _, _) -> False  -- Already inhabited
    (_, Inhabited _) -> True   -- Newly inhabited
    _ -> False
