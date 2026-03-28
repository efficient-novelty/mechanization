{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.Extensional where

open import Cubical.Foundations.Prelude

-- An arity-2 semantic obligation is parallel-path coherence.
BinaryObligation : ∀ {ℓ} {A : Type ℓ} {x y : A} (p q : x ≡ y) → Type ℓ
BinaryObligation p q = p ≡ q

isProp→isContrPath : ∀ {ℓ} {A : Type ℓ} → isProp A → (x y : A) → isContr (x ≡ y)
isProp→isContrPath prop x y =
  prop x y , λ r → isProp→isSet prop x y (prop x y) r

UIP-forces-depth-1 :
  ∀ {ℓ} {A : Type ℓ} →
  isSet A → {x y : A} (p q : x ≡ y) → isContr (BinaryObligation p q)
UIP-forces-depth-1 setA {x} {y} p q =
  isProp→isContrPath (setA x y) p q

history-truncates-to-one :
  ∀ {ℓ} {A : Type ℓ} →
  isSet A → {x y : A} (p q : x ≡ y) → p ≡ q
history-truncates-to-one setA p q =
  UIP-forces-depth-1 setA p q .fst
