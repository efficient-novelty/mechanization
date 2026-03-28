{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.AdjunctionBarrier where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso; isoToPath; transportIsoToPath)
open import Cubical.Data.Empty.Base using (⊥)

¬_ : ∀ {ℓ} → Type ℓ → Type ℓ
¬ A = A → ⊥

BinaryObligation : ∀ {ℓ} {A : Type ℓ} {x y : A} (p q : x ≡ y) → Type ℓ
BinaryObligation p q = p ≡ q

data ⊤ : Type where
  tt : ⊤

data Two : Type where
  left  : Two
  right : Two

Two-code : Two → Type
Two-code left = ⊤
Two-code right = ⊥

left≠right : left ≡ right → ⊥
left≠right p = subst Two-code p tt

right≠left : right ≡ left → ⊥
right≠left p = left≠right (sym p)

swap : Two → Two
swap left = right
swap right = left

swap-iso : Iso Two Two
swap-iso = record
  { fun = swap
  ; inv = swap
  ; rightInv = λ where
      left → refl
      right → refl
  ; leftInv = λ where
      left → refl
      right → refl
  }

swap-path : Two ≡ Two
swap-path = isoToPath swap-iso

swap-path≠refl : swap-path ≡ refl → ⊥
swap-path≠refl α =
  right≠left
    (sym (transportIsoToPath swap-iso left)
      ∙ cong (λ p → transport p left) α
      ∙ transportRefl left)

binary-coherence-nontrivial :
  ¬ isContr (BinaryObligation {A = Type} refl swap-path)
binary-coherence-nontrivial ctr =
  swap-path≠refl (sym (ctr .fst))

depth1-insufficient :
  ¬ ((X Y : Type) (p q : X ≡ Y) → isContr (BinaryObligation p q))
depth1-insufficient collapse =
  binary-coherence-nontrivial (collapse Two Two refl swap-path)
