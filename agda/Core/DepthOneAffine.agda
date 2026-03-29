{-# OPTIONS --cubical --safe --guardedness #-}

module Core.DepthOneAffine where

open import Cubical.Foundations.Prelude

open import Core.Nat
open import Core.AffineRecurrence using (scale; scale-+)

-- Paper-facing bootstrap indexing for cor:d1.
-- Here index n corresponds to paper stage n + 1, so:
--   Delta-depth1-bootstrap c 0 = 0   corresponds to paper Delta_1 = 0
--   tau-depth1-bootstrap c 0 = 0     corresponds to paper tau_1 = 0

Delta-depth1-bootstrap : ℕ → ℕ → ℕ
Delta-depth1-bootstrap c zero = zero
Delta-depth1-bootstrap c (suc n) = c + Delta-depth1-bootstrap c n

depth1-affine-growth : (c n : ℕ) →
  Delta-depth1-bootstrap c (suc n) ≡ Delta-depth1-bootstrap c n + c
depth1-affine-growth c n =
  Delta-depth1-bootstrap c (suc n)  ≡⟨ refl ⟩
  c + Delta-depth1-bootstrap c n    ≡⟨ +-comm c (Delta-depth1-bootstrap c n) ⟩
  Delta-depth1-bootstrap c n + c    ∎

Delta-depth1-closed : (c n : ℕ) →
  Delta-depth1-bootstrap c n ≡ scale c n
Delta-depth1-closed c zero = refl
Delta-depth1-closed c (suc n) =
  Delta-depth1-bootstrap c (suc n)  ≡⟨ refl ⟩
  c + Delta-depth1-bootstrap c n    ≡⟨ cong (c +_) (Delta-depth1-closed c n) ⟩
  c + scale c n                     ≡⟨ refl ⟩
  scale c (suc n)                  ∎

-- triangle n = 1 + ... + n.
-- With the same bootstrap shift, this is the subtraction-free form of the
-- paper equation tau_n = c * n * (n - 1) / 2.
triangle : ℕ → ℕ
triangle zero = zero
triangle (suc n) = suc n + triangle n

tau-depth1-bootstrap : ℕ → ℕ → ℕ
tau-depth1-bootstrap c zero = Delta-depth1-bootstrap c zero
tau-depth1-bootstrap c (suc n) =
  Delta-depth1-bootstrap c (suc n) + tau-depth1-bootstrap c n

tau-depth1-closed : (c n : ℕ) →
  tau-depth1-bootstrap c n ≡ scale c (triangle n)
tau-depth1-closed c zero = refl
tau-depth1-closed c (suc n) =
  tau-depth1-bootstrap c (suc n)                         ≡⟨ refl ⟩
  Delta-depth1-bootstrap c (suc n) + tau-depth1-bootstrap c n
                                                        ≡⟨ cong₂ _+_ (Delta-depth1-closed c (suc n)) (tau-depth1-closed c n) ⟩
  scale c (suc n) + scale c (triangle n)               ≡⟨ sym (scale-+ c (suc n) (triangle n)) ⟩
  scale c (suc n + triangle n)                         ≡⟨ refl ⟩
  scale c (triangle (suc n))                           ∎
