{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.ActiveBasisContract where

open import Cubical.Foundations.Prelude
open import Cubical.Data.Empty.Base using (⊥)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.FiniteInterfaceBasis
open import Metatheory.GlobalActionSemantics

private
  variable
    ℓ ℓA : Level

record ActiveBasisContract {I : ActiveInterface ℓ}
  (X : GlobalActionPayload I ℓA) :
  Type (ℓ-max ℓ ℓA) where
  constructor mkActiveBasisContract
  field
    coveredBasisSite :
      (i : BasisSite I (basis I)) →
      actsOnField X
        (includesBasisSite (advertisedScope X) i)

open ActiveBasisContract public

record ActiveBasisDensity {I : ActiveInterface ℓ}
  (X : GlobalActionPayload I ℓA) :
  Type (ℓ-max ℓ ℓA) where
  constructor mkActiveBasisDensity
  field
    exactlyOneAction :
      (i : InterfaceFieldOf I) → isContr (actsOnField X i)

open ActiveBasisDensity public

global-action-totality-implies-active-basis-contract :
  {I : ActiveInterface ℓ} →
  {X : GlobalActionPayload I ℓA} →
  ActionTotality X →
  ActiveBasisContract X
global-action-totality-implies-active-basis-contract total =
  mkActiveBasisContract (actsOnEveryBasisSite total)

active-basis-contract-entails-density :
  {I : ActiveInterface ℓ} →
  {X : GlobalActionPayload I ℓA} →
  ActiveBasisContract X →
  ActiveBasisDensity X
active-basis-contract-entails-density {I = I} {X = X} contract =
  mkActiveBasisDensity
    (λ i →
      total i
      , λ p → actionDeterministic X i (total i) p)
  where
    total : (i : InterfaceFieldOf I) → actsOnField X i
    total i =
      subst
        (actsOnField X)
        (includesBasisSiteIsIdentity (advertisedScope X) i)
        (coveredBasisSite contract i)

data WindowShape : Type where
  depth-one-collapse : WindowShape
  depth-two-window   : WindowShape
  depth-three-window : WindowShape

data GrowthShape : Type where
  no-growth-law : GrowthShape
  fibonacci-law : GrowthShape

record CoverageModel : Type₁ where
  constructor mkCoverageModel
  field
    Field :
      Type
    Action :
      Field → Type
    covered :
      (i : Field) → Action i
    deterministic :
      (i : Field) → (p q : Action i) → p ≡ q
    windowShape :
      WindowShape
    growthShape :
      GrowthShape

open CoverageModel public

data Unit : Type where
  tt : Unit

unit-is-prop : (p q : Unit) → p ≡ q
unit-is-prop tt tt = refl

unitCoverageModel : WindowShape → GrowthShape → CoverageModel
unitCoverageModel window growth =
  mkCoverageModel
    Unit
    (λ i → Unit)
    (λ i → tt)
    (λ i → unit-is-prop)
    window
    growth

coverage-with-depth-one-collapse : CoverageModel
coverage-with-depth-one-collapse =
  unitCoverageModel depth-one-collapse no-growth-law

coverage-with-artificial-depth-three-window : CoverageModel
coverage-with-artificial-depth-three-window =
  unitCoverageModel depth-three-window no-growth-law

WindowShape-code : WindowShape → Type
WindowShape-code depth-one-collapse = Unit
WindowShape-code depth-two-window = ⊥
WindowShape-code depth-three-window = Unit

GrowthShape-code : GrowthShape → Type
GrowthShape-code no-growth-law = Unit
GrowthShape-code fibonacci-law = ⊥

coverage-alone-does-not-imply-depth-two-window :
  windowShape coverage-with-depth-one-collapse ≡ depth-two-window → ⊥
coverage-alone-does-not-imply-depth-two-window p =
  subst WindowShape-code p tt

coverage-alone-does-not-imply-fibonacci :
  growthShape coverage-with-artificial-depth-three-window ≡ fibonacci-law → ⊥
coverage-alone-does-not-imply-fibonacci p =
  subst GrowthShape-code p tt
