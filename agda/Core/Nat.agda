{-# OPTIONS --cubical --safe --guardedness #-}

module Core.Nat where

open import Cubical.Foundations.Prelude

-- ============================================
-- Natural Numbers (self-contained)
-- ============================================

-- Define our own ℕ to avoid Agda 2.8.0 cubical import issues
data ℕ : Type where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

-- Addition
_+_ : ℕ → ℕ → ℕ
zero    + n = n
(suc m) + n = suc (m + n)

infixl 6 _+_

-- Pair type (to avoid Cubical.Data.Sigma import issues)
record _×_ (A B : Type) : Type where
  constructor _,_
  field
    fst : A
    snd : B

open _×_ public
infixr 4 _,_
infixr 2 _×_

-- Basic properties we need for proofs
+-assoc : (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p = refl
+-assoc (suc m) n p = cong suc (+-assoc m n p)

+-zero : (n : ℕ) → n + zero ≡ n
+-zero zero = refl
+-zero (suc n) = cong suc (+-zero n)

+-suc : (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n = refl
+-suc (suc m) n = cong suc (+-suc m n)

+-comm : (m n : ℕ) → m + n ≡ n + m
+-comm zero n = sym (+-zero n)
+-comm (suc m) n = cong suc (+-comm m n) ∙ sym (+-suc n m)

-- ============================================
-- Fibonacci Numbers
-- ============================================

-- The standard Fibonacci function
fib : ℕ → ℕ
fib zero = 1
fib (suc zero) = 1
fib (suc (suc n)) = fib (suc n) + fib n

-- First few values for reference:
-- fib 0 = 1, fib 1 = 1, fib 2 = 2, fib 3 = 3, fib 4 = 5, fib 5 = 8, ...
-- Note: We use fib(0)=1, fib(1)=1 to match Δ₁=1, Δ₂=1 from the paper

-- ============================================
-- Fibonacci Sum Identity
-- ============================================

-- Key identity: Σ fib(i) for i=0..n = fib(n+2) - 1
-- This is the "Golden Schedule" from the paper: τₙ = F_{n+2} - 1

-- Helper: sum of Fibonacci numbers from 0 to n
fibSum : ℕ → ℕ
fibSum zero = fib zero
fibSum (suc n) = fib (suc n) + fibSum n

-- The main identity we need to prove:
-- fibSum n ≡ fib (suc (suc n)) - 1
--
-- However, subtraction in ℕ is tricky (monus).
-- Instead, we prove the equivalent:
-- fibSum n + 1 ≡ fib (suc (suc n))

fibSum-identity : (n : ℕ) → fibSum n + 1 ≡ fib (suc (suc n))
fibSum-identity zero = refl
fibSum-identity (suc n) =
  fibSum (suc n) + 1                           ≡⟨ refl ⟩
  (fib (suc n) + fibSum n) + 1                 ≡⟨ +-assoc (fib (suc n)) (fibSum n) 1 ⟩
  fib (suc n) + (fibSum n + 1)                 ≡⟨ cong (fib (suc n) +_) (fibSum-identity n) ⟩
  fib (suc n) + fib (suc (suc n))              ≡⟨ +-comm (fib (suc n)) (fib (suc (suc n))) ⟩
  fib (suc (suc n)) + fib (suc n)              ≡⟨ refl ⟩
  fib (suc (suc (suc n)))                      ∎

-- ============================================
-- Integration Cost Δ and Realization Time τ
-- ============================================

-- For the paper, we want Δₙ = Fₙ where F is 1-indexed:
-- Δ₁ = 1, Δ₂ = 1, Δ₃ = 2, Δ₄ = 3, ...
--
-- Our fib is 0-indexed, so: Δₙ = fib (n - 1)

-- Integration cost at step n (1-indexed, n ≥ 1)
Δ : ℕ → ℕ
Δ zero = 0       -- undefined for n=0, but we need totality
Δ (suc n) = fib n

-- The recurrence: Δ(n+1) = Δ(n) + Δ(n-1) for n ≥ 2
Δ-recurrence : (n : ℕ) → Δ (suc (suc (suc n))) ≡ Δ (suc (suc n)) + Δ (suc n)
Δ-recurrence n = refl  -- This follows directly from fib's definition

-- Realization time: τₙ = Σᵢ₌₁ⁿ Δᵢ
τ : ℕ → ℕ
τ zero = 0
τ (suc n) = Δ (suc n) + τ n

-- The Golden Schedule: τₙ = F_{n+2} - 1
-- Equivalently: τₙ + 1 = fib (n + 1)  [using our 0-indexed fib]
τ-golden-schedule : (n : ℕ) → τ n + 1 ≡ fib (suc n)
τ-golden-schedule zero = refl
τ-golden-schedule (suc zero) = refl
τ-golden-schedule (suc (suc n)) =
  τ (suc (suc n)) + 1                              ≡⟨ refl ⟩
  (Δ (suc (suc n)) + τ (suc n)) + 1                ≡⟨ refl ⟩
  (fib (suc n) + τ (suc n)) + 1                    ≡⟨ +-assoc (fib (suc n)) (τ (suc n)) 1 ⟩
  fib (suc n) + (τ (suc n) + 1)                    ≡⟨ cong (fib (suc n) +_) (τ-golden-schedule (suc n)) ⟩
  fib (suc n) + fib (suc (suc n))                  ≡⟨ +-comm (fib (suc n)) (fib (suc (suc n))) ⟩
  fib (suc (suc n)) + fib (suc n)                  ≡⟨ refl ⟩
  fib (suc (suc (suc n)))                          ∎
