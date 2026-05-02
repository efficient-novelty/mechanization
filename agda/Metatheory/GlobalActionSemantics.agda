{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.GlobalActionSemantics where

open import Cubical.Foundations.Prelude

open import Metatheory.FiniteInterfaceBasis

private
  variable
    ℓ ℓA : Level

record WholeActiveInterface (I : ActiveInterface ℓ) : Type where
  constructor whole-active-interface
  field
    includesBasisSite :
      BasisSite I (basis I) → InterfaceFieldOf I
    includesBasisSiteIsIdentity :
      (i : BasisSite I (basis I)) → includesBasisSite i ≡ i

open WholeActiveInterface public

whole-active-interface-scope :
  (I : ActiveInterface ℓ) → WholeActiveInterface I
whole-active-interface-scope I =
  whole-active-interface (λ i → i) (λ i → refl)

record GlobalActionPayload
  (I : ActiveInterface ℓ) (ℓA : Level) :
  Type (ℓ-suc (ℓ-max ℓ ℓA)) where
  constructor mkGlobalActionPayload
  field
    advertisedScope :
      WholeActiveInterface I
    actsOnField :
      InterfaceFieldOf I → Type ℓA
    actionDeterministic :
      (i : InterfaceFieldOf I) →
      (p q : actsOnField i) → p ≡ q

open GlobalActionPayload public

record ActionTotality {I : ActiveInterface ℓ}
  (X : GlobalActionPayload I ℓA) :
  Type (ℓ-max ℓ ℓA) where
  constructor mkActionTotality
  field
    actsOnEveryBasisSite :
      (i : BasisSite I (basis I)) →
      actsOnField X
        (includesBasisSite (advertisedScope X) i)

open ActionTotality public

total-action-on-field :
  {I : ActiveInterface ℓ} →
  {X : GlobalActionPayload I ℓA} →
  ActionTotality X →
  (i : InterfaceFieldOf I) →
  actsOnField X i
total-action-on-field {X = X} total i =
  subst
    (actsOnField X)
    (includesBasisSiteIsIdentity (advertisedScope X) i)
    (actsOnEveryBasisSite total i)
