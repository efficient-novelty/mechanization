{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.CanonicityDensity where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin)
open import Metatheory.InterfaceCalculus using (LibraryState)

private
  variable
    ℓI ℓG ℓX : Level

composeIso :
  {ℓA ℓB ℓC : Level}
  {A : Type ℓA} {B : Type ℓB} {C : Type ℓC} →
  Iso A B → Iso B C → Iso A C
composeIso α β = record
  { fun = λ a → Iso.fun β (Iso.fun α a)
  ; inv = λ c → Iso.inv α (Iso.inv β c)
  ; rightInv = λ c →
      cong (Iso.fun β) (Iso.rightInv α (Iso.inv β c))
      ∙ Iso.rightInv β c
  ; leftInv = λ a →
      cong (Iso.inv α) (Iso.leftInv β (Iso.fun α a))
      ∙ Iso.leftInv α a
  }

record HistoricalInterface {ℓI ℓG : Level}
  (B : LibraryState ℓI) :
  Type (ℓ-suc (ℓ-max ℓI ℓG)) where
  constructor mkHistoricalInterface
  field
    Generator          : Type ℓG
    generatorCount     : Nat
    countingNormalForm : Iso Generator (Fin generatorCount)

open HistoricalInterface public

record FullyCoupledFoundation {ℓI ℓG : Level}
  (B : LibraryState ℓI) :
  Type (ℓ-suc (ℓ-max ℓI ℓG)) where
  constructor mkFullyCoupledFoundation
  field
    activeHistoricalInterface :
      HistoricalInterface {ℓI = ℓI} {ℓG = ℓG} B

open FullyCoupledFoundation public

record FoundationalCoreExtension
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  (F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓG ℓX))) where
  constructor mkFoundationalCoreExtension
  field
    PrimitiveInteraction :
      HistoricalInterface.Generator (activeHistoricalInterface F) →
      Type ℓX

open FoundationalCoreExtension public

primitive-interaction-site :
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B} →
  (X : FoundationalCoreExtension {ℓX = ℓX} F) →
  Type (ℓ-max ℓG ℓX)
primitive-interaction-site {F = F} X =
  Σ[ g ∈ HistoricalInterface.Generator (activeHistoricalInterface F) ]
    PrimitiveInteraction X g

record NativeCanonicityPreservingTotality
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  (X : FoundationalCoreExtension {ℓX = ℓX} F) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓG ℓX))) where
  field
    total-native-action :
      (g : HistoricalInterface.Generator (activeHistoricalInterface F)) →
      PrimitiveInteraction X g
    single-native-clause :
      (g : HistoricalInterface.Generator (activeHistoricalInterface F)) →
      (p q : PrimitiveInteraction X g) →
      p ≡ q

open NativeCanonicityPreservingTotality public

record PromotedOperationalExhaustiveness
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  (X : FoundationalCoreExtension {ℓX = ℓX} F) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓG ℓX))) where
  field
    exhaustive-promoted-action :
      (g : HistoricalInterface.Generator (activeHistoricalInterface F)) →
      PrimitiveInteraction X g
    single-promoted-witness :
      (g : HistoricalInterface.Generator (activeHistoricalInterface F)) →
      (p q : PrimitiveInteraction X g) →
      p ≡ q

open PromotedOperationalExhaustiveness public

record MaximalInterfaceDensity
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  (X : FoundationalCoreExtension {ℓX = ℓX} F) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓG ℓX))) where
  field
    exactly-one-primitive-datum :
      (g : HistoricalInterface.Generator (activeHistoricalInterface F)) →
      isContr (PrimitiveInteraction X g)

open MaximalInterfaceDensity public

native-totality-forces-maximal-interface-density :
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  {X : FoundationalCoreExtension {ℓX = ℓX} F} →
  NativeCanonicityPreservingTotality X →
  MaximalInterfaceDensity X
native-totality-forces-maximal-interface-density admissible = record
  { exactly-one-primitive-datum = λ g →
      total-native-action admissible g
    , λ p →
        single-native-clause admissible g
          (total-native-action admissible g) p
  }

promoted-exhaustiveness-forces-maximal-interface-density :
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  {X : FoundationalCoreExtension {ℓX = ℓX} F} →
  PromotedOperationalExhaustiveness X →
  MaximalInterfaceDensity X
promoted-exhaustiveness-forces-maximal-interface-density admissible = record
  { exactly-one-primitive-datum = λ g →
      exhaustive-promoted-action admissible g
    , λ p →
        single-promoted-witness admissible g
          (exhaustive-promoted-action admissible g) p
  }

data GlobalAdmissibilityDiscipline
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  (X : FoundationalCoreExtension {ℓX = ℓX} F) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓG ℓX))) where
  native-global-admissibility :
    NativeCanonicityPreservingTotality X →
    GlobalAdmissibilityDiscipline X
  promoted-global-admissibility :
    PromotedOperationalExhaustiveness X →
    GlobalAdmissibilityDiscipline X

interaction-site-collapse :
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  {X : FoundationalCoreExtension {ℓX = ℓX} F} →
  MaximalInterfaceDensity X →
  Iso (primitive-interaction-site X)
      (HistoricalInterface.Generator (activeHistoricalInterface F))
interaction-site-collapse {F = F} density = record
  { fun = fst
  ; inv = λ g →
      g , fst (exactly-one-primitive-datum density g)
  ; rightInv = λ g → refl
  ; leftInv = λ where
      (g , p) →
        cong
          (λ q → g , q)
          (snd (exactly-one-primitive-datum density g) p)
  }

primitive-interaction-counting-normal-form :
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  {X : FoundationalCoreExtension {ℓX = ℓX} F} →
  MaximalInterfaceDensity X →
  Iso (primitive-interaction-site X)
      (Fin (HistoricalInterface.generatorCount (activeHistoricalInterface F)))
primitive-interaction-counting-normal-form {F = F} density =
  composeIso
    (interaction-site-collapse density)
    (HistoricalInterface.countingNormalForm (activeHistoricalInterface F))

record CanonicityDensityTheorem
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  (X : FoundationalCoreExtension {ℓX = ℓX} F) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓG ℓX))) where
  field
    maximal-density :
      MaximalInterfaceDensity X
    primitive-site-equivalence :
      Iso (primitive-interaction-site X)
          (HistoricalInterface.Generator (activeHistoricalInterface F))
    primitive-counting :
      Iso (primitive-interaction-site X)
          (Fin (HistoricalInterface.generatorCount (activeHistoricalInterface F)))

open CanonicityDensityTheorem public

canonicity-density-theorem :
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  {X : FoundationalCoreExtension {ℓX = ℓX} F} →
  MaximalInterfaceDensity X →
  CanonicityDensityTheorem X
canonicity-density-theorem density = record
  { maximal-density = density
  ; primitive-site-equivalence = interaction-site-collapse density
  ; primitive-counting = primitive-interaction-counting-normal-form density
  }

global-admissibility-forces-maximal-interface-density :
  {ℓI ℓG ℓX : Level}
  {B : LibraryState ℓI}
  {F : FullyCoupledFoundation {ℓI = ℓI} {ℓG = ℓG} B}
  {X : FoundationalCoreExtension {ℓX = ℓX} F} →
  GlobalAdmissibilityDiscipline X →
  CanonicityDensityTheorem X
global-admissibility-forces-maximal-interface-density
  (native-global-admissibility admissible) =
  canonicity-density-theorem
    (native-totality-forces-maximal-interface-density admissible)
global-admissibility-forces-maximal-interface-density
  (promoted-global-admissibility admissible) =
  canonicity-density-theorem
    (promoted-exhaustiveness-forces-maximal-interface-density admissible)
