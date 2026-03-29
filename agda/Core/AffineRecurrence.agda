{-# OPTIONS --cubical --safe --guardedness #-}

module Core.AffineRecurrence where

open import Cubical.Foundations.Prelude

open import Core.Nat

-- Constant payload contributed by the two inherited layers.
payload2 : ℕ → ℕ
payload2 c = c + c

-- Repeated addition, used for the paper-facing closed forms.
scale : ℕ → ℕ → ℕ
scale c zero = zero
scale c (suc n) = c + scale c n

scale-+ : (c a b : ℕ) → scale c (a + b) ≡ scale c a + scale c b
scale-+ c zero b = refl
scale-+ c (suc a) b =
  scale c (suc a + b)                  ≡⟨ refl ⟩
  c + scale c (a + b)                  ≡⟨ cong (c +_) (scale-+ c a b) ⟩
  c + (scale c a + scale c b)          ≡⟨ sym (+-assoc c (scale c a) (scale c b)) ⟩
  (c + scale c a) + scale c b          ≡⟨ refl ⟩
  scale c (suc a) + scale c b          ∎

scale-two : (c : ℕ) → scale c 2 ≡ payload2 c
scale-two c =
  scale c 2         ≡⟨ refl ⟩
  c + (c + zero)    ≡⟨ cong (c +_) (+-zero c) ⟩
  c + c             ∎

scale-three : (c : ℕ) → scale c 3 ≡ c + payload2 c
scale-three c =
  scale c 3                  ≡⟨ refl ⟩
  c + scale c 2              ≡⟨ cong (c +_) (scale-two c) ⟩
  c + payload2 c             ∎

scale-plus-two : (c n : ℕ) → scale c (n + 2) ≡ scale c n + payload2 c
scale-plus-two c n =
  scale c (n + 2)                       ≡⟨ scale-+ c n 2 ⟩
  scale c n + scale c 2                 ≡⟨ cong (scale c n +_) (scale-two c) ⟩
  scale c n + payload2 c                ∎

interchange : (a b c d : ℕ) → (a + b) + (c + d) ≡ (a + c) + (b + d)
interchange a b c d =
  (a + b) + (c + d)                     ≡⟨ sym (+-assoc (a + b) c d) ⟩
  ((a + b) + c) + d                     ≡⟨ cong (_+ d) (+-assoc a b c) ⟩
  (a + (b + c)) + d                     ≡⟨ cong (λ x → (a + x) + d) (+-comm b c) ⟩
  (a + (c + b)) + d                     ≡⟨ cong (_+ d) (sym (+-assoc a c b)) ⟩
  ((a + c) + b) + d                     ≡⟨ +-assoc (a + c) b d ⟩
  (a + c) + (b + d)                     ∎

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

-- Paper-facing bootstrap indexing.
-- Here index n corresponds to paper stage n + 1, so:
--   Delta-bootstrap c 0 = 0   corresponds to paper Delta_1 = 0
--   Delta-bootstrap c 1 = c   corresponds to paper Delta_2 = c

Delta-bootstrap : ℕ → ℕ → ℕ
Delta-bootstrap c zero = zero
Delta-bootstrap c (suc zero) = c
Delta-bootstrap c (suc (suc n)) =
  Delta-bootstrap c (suc n) + Delta-bootstrap c n + payload2 c

Delta-bootstrap-step : (c n : ℕ) →
  Delta-bootstrap c (suc (suc n))
    ≡ Delta-bootstrap c (suc n) + Delta-bootstrap c n + payload2 c
Delta-bootstrap-step c n = refl

U-bootstrap : ℕ → ℕ → ℕ
U-bootstrap c n = Delta-bootstrap c n + payload2 c

U-bootstrap-is-fibonacci : (c n : ℕ) →
  U-bootstrap c (suc (suc n)) ≡ U-bootstrap c (suc n) + U-bootstrap c n
U-bootstrap-is-fibonacci c n =
  U-bootstrap c (suc (suc n))                                                    ≡⟨ refl ⟩
  (Delta-bootstrap c (suc n) + Delta-bootstrap c n + payload2 c) + payload2 c   ≡⟨ payload-shift (Delta-bootstrap c (suc n)) (Delta-bootstrap c n) (payload2 c) ⟩
  (Delta-bootstrap c (suc n) + payload2 c) + (Delta-bootstrap c n + payload2 c) ≡⟨ refl ⟩
  U-bootstrap c (suc n) + U-bootstrap c n                                        ∎

U-bootstrap-closed : (c n : ℕ) →
  U-bootstrap c n ≡ scale c (fib (suc (suc n)))
U-bootstrap-closed c zero =
  U-bootstrap c zero     ≡⟨ refl ⟩
  payload2 c             ≡⟨ sym (scale-two c) ⟩
  scale c 2             ≡⟨ refl ⟩
  scale c (fib 2)        ∎
U-bootstrap-closed c (suc zero) =
  U-bootstrap c 1        ≡⟨ refl ⟩
  c + payload2 c         ≡⟨ sym (scale-three c) ⟩
  scale c 3              ≡⟨ refl ⟩
  scale c (fib 3)        ∎
U-bootstrap-closed c (suc (suc n)) =
  U-bootstrap c (suc (suc n))                                    ≡⟨ U-bootstrap-is-fibonacci c n ⟩
  U-bootstrap c (suc n) + U-bootstrap c n                        ≡⟨ cong₂ _+_ (U-bootstrap-closed c (suc n)) (U-bootstrap-closed c n) ⟩
  scale c (fib (suc (suc (suc n)))) + scale c (fib (suc (suc n))) ≡⟨ sym (scale-+ c (fib (suc (suc (suc n)))) (fib (suc (suc n)))) ⟩
  scale c (fib (suc (suc (suc n))) + fib (suc (suc n)))         ≡⟨ refl ⟩
  scale c (fib (suc (suc (suc (suc n)))))                       ∎

tauShift : ℕ → ℕ
tauShift zero = 5
tauShift (suc n) = tauShift n + 2

tau-bootstrap : ℕ → ℕ → ℕ
tau-bootstrap c zero = Delta-bootstrap c zero
tau-bootstrap c (suc n) = Delta-bootstrap c (suc n) + tau-bootstrap c n

tau-bootstrap-closed : (c n : ℕ) →
  tau-bootstrap c n + scale c (tauShift n) ≡ scale c (fib (suc (suc (suc (suc n)))))
tau-bootstrap-closed c zero = refl
tau-bootstrap-closed c (suc n) =
  tau-bootstrap c (suc n) + scale c (tauShift (suc n))              ≡⟨ refl ⟩
  (Delta-bootstrap c (suc n) + tau-bootstrap c n)
    + scale c (tauShift n + 2)                                      ≡⟨ cong ((Delta-bootstrap c (suc n) + tau-bootstrap c n) +_) (scale-plus-two c (tauShift n)) ⟩
  (Delta-bootstrap c (suc n) + tau-bootstrap c n)
    + (scale c (tauShift n) + payload2 c)                           ≡⟨ cong ((Delta-bootstrap c (suc n) + tau-bootstrap c n) +_) (+-comm (scale c (tauShift n)) (payload2 c)) ⟩
  (Delta-bootstrap c (suc n) + tau-bootstrap c n)
    + (payload2 c + scale c (tauShift n))                           ≡⟨ interchange (Delta-bootstrap c (suc n)) (tau-bootstrap c n) (payload2 c) (scale c (tauShift n)) ⟩
  (Delta-bootstrap c (suc n) + payload2 c)
    + (tau-bootstrap c n + scale c (tauShift n))                    ≡⟨ cong₂ _+_ (U-bootstrap-closed c (suc n)) (tau-bootstrap-closed c n) ⟩
  scale c (fib (suc (suc (suc n)))) + scale c (fib (suc (suc (suc (suc n))))) ≡⟨ sym (scale-+ c (fib (suc (suc (suc n)))) (fib (suc (suc (suc (suc n)))))) ⟩
  scale c (fib (suc (suc (suc n))) + fib (suc (suc (suc (suc n))))) ≡⟨ cong (scale c) (+-comm (fib (suc (suc (suc n)))) (fib (suc (suc (suc (suc n))))) ) ⟩
  scale c (fib (suc (suc (suc (suc n)))) + fib (suc (suc (suc n)))) ≡⟨ refl ⟩
  scale c (fib (suc (suc (suc (suc (suc n))))))                     ∎
