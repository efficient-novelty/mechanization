{-# OPTIONS --cubical --safe --guardedness #-}

module Core.AffineRecurrence where

open import Cubical.Foundations.Prelude

open import Core.Nat

-- Constant payload contributed by the two inherited layers.
payload2 : ℕ → ℕ
payload2 c = c + c

-- Payload-aware depth-two recurrence.
Delta-affine : ℕ → ℕ → ℕ
Delta-affine c zero = 1
Delta-affine c (suc zero) = 1
Delta-affine c (suc (suc n)) =
  Delta-affine c (suc n) + Delta-affine c n + payload2 c

Delta-affine-step : (c n : ℕ) →
  Delta-affine c (suc (suc n))
    ≡ Delta-affine c (suc n) + Delta-affine c n + payload2 c
Delta-affine-step c n = refl

-- Constant shift that recovers the homogeneous Fibonacci law.
U : ℕ → ℕ → ℕ
U c n = Delta-affine c n + payload2 c

payload-shift :
  (a b s : ℕ) →
  (a + b + s) + s ≡ (a + s) + (b + s)
payload-shift a b s =
  ((a + b) + s) + s           ≡⟨ +-assoc (a + b) s s ⟩
  (a + b) + (s + s)           ≡⟨ +-assoc a b (s + s) ⟩
  a + (b + (s + s))           ≡⟨ cong (a +_) (sym (+-assoc b s s)) ⟩
  a + ((b + s) + s)           ≡⟨ cong (a +_) (cong (_+ s) (+-comm b s)) ⟩
  a + ((s + b) + s)           ≡⟨ cong (a +_) (+-assoc s b s) ⟩
  a + (s + (b + s))           ≡⟨ sym (+-assoc a s (b + s)) ⟩
  (a + s) + (b + s)           ∎

U-is-fibonacci : (c n : ℕ) →
  U c (suc (suc n)) ≡ U c (suc n) + U c n
U-is-fibonacci c n =
  U c (suc (suc n))                                                        ≡⟨ refl ⟩
  (Delta-affine c (suc n) + Delta-affine c n + payload2 c) + payload2 c   ≡⟨ payload-shift (Delta-affine c (suc n)) (Delta-affine c n) (payload2 c) ⟩
  (Delta-affine c (suc n) + payload2 c) + (Delta-affine c n + payload2 c) ≡⟨ refl ⟩
  U c (suc n) + U c n                                                      ∎

payload-free-fibonacci : (n : ℕ) → Delta-affine zero n ≡ fib n
payload-free-fibonacci zero = refl
payload-free-fibonacci (suc zero) = refl
payload-free-fibonacci (suc (suc n)) =
  Delta-affine zero (suc (suc n))   ≡⟨ refl ⟩
  (Delta-affine zero (suc n) + Delta-affine zero n) + zero
                                   ≡⟨ +-zero (Delta-affine zero (suc n) + Delta-affine zero n) ⟩
  Delta-affine zero (suc n) + Delta-affine zero n
                                   ≡⟨ cong₂ _+_ (payload-free-fibonacci (suc n)) (payload-free-fibonacci n) ⟩
  fib (suc n) + fib n              ≡⟨ refl ⟩
  fib (suc (suc n))                ∎

payload-free-Delta : (n : ℕ) → Delta-affine zero n ≡ Δ (suc n)
payload-free-Delta n =
  Delta-affine zero n ≡⟨ payload-free-fibonacci n ⟩
  fib n               ≡⟨ refl ⟩
  Δ (suc n)           ∎
