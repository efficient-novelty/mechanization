{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.AdjunctionBarrier where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using
  ( Iso
  ; endoIso
  ; isoToPath
  ; transportIsoToPath
  )
open import Cubical.Data.Empty.Base using (⊥)

open import Adjunction.AdjunctionDepth using
  ( Depth2
  ; triangle-L-depth
  ; triangle-R-depth
  )

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

const-left : Two → Two
const-left _ = left

const-right : Two → Two
const-right _ = right

swap-endomap-path : (Two → Two) ≡ (Two → Two)
swap-endomap-path = isoToPath (endoIso swap-iso)

swap-endomap-conjugates-left : Iso.fun (endoIso swap-iso) const-left ≡ const-right
swap-endomap-conjugates-left =
  funExt λ where
    left → refl
    right → refl

swap-endomap-transport : transport swap-endomap-path const-left ≡ const-right
swap-endomap-transport =
  transportIsoToPath (endoIso swap-iso) const-left
    ∙ swap-endomap-conjugates-left

swap-endomap-obligation : Type
swap-endomap-obligation = transport swap-endomap-path const-left ≡ const-left

const-right≠const-left : const-right ≡ const-left → ⊥
const-right≠const-left α = right≠left (cong (λ f → f left) α)

swap-endomap-obligation-impossible : ¬ swap-endomap-obligation
swap-endomap-obligation-impossible α =
  const-right≠const-left (sym swap-endomap-transport ∙ α)

record ExplicitBinarySealingObstruction : Type₁ where
  field
    unaryClauseAtTwo : Two → Two
    transportedUnaryClause : transport swap-endomap-path unaryClauseAtTwo ≡ const-right
    residualBinaryObligation : Type
    residualBinaryObligationUninhabited : ¬ residualBinaryObligation

explicit-binary-sealing-obstruction : ExplicitBinarySealingObstruction
explicit-binary-sealing-obstruction = record
  { unaryClauseAtTwo = const-left
  ; transportedUnaryClause = swap-endomap-transport
  ; residualBinaryObligation = swap-endomap-obligation
  ; residualBinaryObligationUninhabited = swap-endomap-obligation-impossible
  }

record TriangleIdentityCorollary : Type where
  field
    leftTriangleRequiresBinary : triangle-L-depth ≡ Depth2
    rightTriangleRequiresBinary : triangle-R-depth ≡ Depth2

triangle-identity-corollary : TriangleIdentityCorollary
triangle-identity-corollary = record
  { leftTriangleRequiresBinary = refl
  ; rightTriangleRequiresBinary = refl
  }

adjunction-barrier :
  ¬ ((X Y : Type) (p q : X ≡ Y) → isContr (BinaryObligation p q))
adjunction-barrier = depth1-insufficient
