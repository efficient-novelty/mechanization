{-# OPTIONS --cubical --safe --guardedness #-}

module Core.Sequence where

open import Cubical.Foundations.Prelude

open import Core.Nat

-- ============================================
-- Vectors (self-contained)
-- ============================================

data Vec (A : Type) : ℕ → Type where
  []  : Vec A zero
  _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n)

infixr 5 _∷_

-- ============================================
-- Indexed Sequences and Windows
-- ============================================

-- A history of n schemas, each represented by its cardinality
History : ℕ → Type
History n = Vec ℕ n

-- Sum of a vector of naturals
sumVec : {n : ℕ} → Vec ℕ n → ℕ
sumVec [] = 0
sumVec (x ∷ xs) = x + sumVec xs

-- ============================================
-- Building History from Fibonacci
-- ============================================

-- Build a Fibonacci history of length n
-- Each entry is Δᵢ = fib(i-1)
fibHistory : (n : ℕ) → History n
fibHistory zero = []
fibHistory (suc n) = fib n ∷ fibHistory n

-- Verify: sumVec of fibHistory n equals τ n
-- (They're defined the same way, just structured differently)
