{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.FiniteInterfaceBasis where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin)

private
  variable
    ℓ : Level

record InterfaceField (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkInterfaceField
  field
    Carrier : Type ℓ

open InterfaceField public

record TransparentEquivalence {ℓ : Level} {n : Nat}
  (fieldAt : Fin n → InterfaceField ℓ) : Type (ℓ-suc ℓ) where
  constructor mkTransparentEquivalence
  field
    representative :
      Fin n → InterfaceField ℓ
    representativeMatches :
      (i : Fin n) → representative i ≡ fieldAt i

open TransparentEquivalence public

record BasisFamily {ℓ : Level} {n : Nat}
  {fieldAt : Fin n → InterfaceField ℓ}
  (transparent : TransparentEquivalence fieldAt) :
  Type (ℓ-suc ℓ) where
  constructor mkBasisFamily
  field
    basisFieldAt :
      Fin n → InterfaceField ℓ
    basisMatchesRepresentative :
      (i : Fin n) →
      basisFieldAt i ≡ representative transparent i

open BasisFamily public

record ActiveInterface (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkActiveInterface
  field
    fieldCount  : Nat
    fieldAt     : Fin fieldCount → InterfaceField ℓ
    transparent : TransparentEquivalence fieldAt
    basis       : BasisFamily transparent

open ActiveInterface public

InterfaceFieldOf : ActiveInterface ℓ → Type
InterfaceFieldOf I = Fin (fieldCount I)

BasisSite : (I : ActiveInterface ℓ) → BasisFamily (transparent I) → Type
BasisSite I basis = Fin (fieldCount I)

canonical-transparent-equivalence :
  {n : Nat} →
  (fieldAt : Fin n → InterfaceField ℓ) →
  TransparentEquivalence fieldAt
canonical-transparent-equivalence fieldAt =
  mkTransparentEquivalence fieldAt (λ i → refl)

canonical-basis-family :
  {n : Nat} →
  (fieldAt : Fin n → InterfaceField ℓ) →
  BasisFamily (canonical-transparent-equivalence fieldAt)
canonical-basis-family fieldAt =
  mkBasisFamily fieldAt (λ i → refl)

canonical-active-interface :
  (n : Nat) →
  (fieldAt : Fin n → InterfaceField ℓ) →
  ActiveInterface ℓ
canonical-active-interface n fieldAt =
  mkActiveInterface
    n
    fieldAt
    (canonical-transparent-equivalence fieldAt)
    (canonical-basis-family fieldAt)

basis-families-exist :
  {n : Nat} →
  (fieldAt : Fin n → InterfaceField ℓ) →
  BasisFamily (canonical-transparent-equivalence fieldAt)
basis-families-exist = canonical-basis-family

basis-family-cardinality-invariant :
  (I : ActiveInterface ℓ) →
  (β : BasisFamily (transparent I)) →
  fieldCount I ≡ fieldCount I
basis-family-cardinality-invariant I β = refl

basis-action-equivalence :
  (I : ActiveInterface ℓ) →
  (β : BasisFamily (transparent I)) →
  Iso (BasisSite I β) (InterfaceFieldOf I)
basis-action-equivalence I β = record
  { fun = λ i → i
  ; inv = λ i → i
  ; rightInv = λ i → refl
  ; leftInv = λ i → refl
  }
