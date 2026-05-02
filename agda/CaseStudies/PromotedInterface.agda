{-# OPTIONS --cubical --safe --guardedness #-}

module CaseStudies.PromotedInterface where

open import Cubical.Foundations.Prelude
open import Agda.Primitive using (lzero)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin)
open import Metatheory.InterfaceCalculus
  using ( LibraryState
        ; TransparentDevelopment
        ; mkLibraryState
        ; mkTransparentDevelopment
        )
open import Metatheory.FiniteInterfaceBasis
open import Metatheory.GlobalActionSemantics
open import Metatheory.ActiveBasisContract
  using ( ActiveBasisContract
        ; ActiveBasisDensity
        ; coverage-alone-does-not-imply-depth-two-window
        ; coverage-alone-does-not-imply-fibonacci
        ; global-action-totality-implies-active-basis-contract
        ; active-basis-contract-entails-density
        )
open import Metatheory.SparseDependencyRecurrence
  using (TransparentZeroFootprint; transparent-growth-zero-footprint)
open import CaseStudies.Common

promoted-field : Fin two -> InterfaceField lzero
promoted-field i = mkInterfaceField Unit

promoted-interface : ActiveInterface lzero
promoted-interface =
  canonical-active-interface two promoted-field

promoted-action : GlobalActionPayload promoted-interface lzero
promoted-action =
  mkGlobalActionPayload
    (whole-active-interface-scope promoted-interface)
    (λ i -> Unit)
    (λ i -> unit-is-prop)

promoted-totality : ActionTotality promoted-action
promoted-totality = mkActionTotality (λ i -> tt)

promoted-active-basis-contract : ActiveBasisContract promoted-action
promoted-active-basis-contract =
  global-action-totality-implies-active-basis-contract promoted-totality

promoted-active-basis-density : ActiveBasisDensity promoted-action
promoted-active-basis-density =
  active-basis-contract-entails-density promoted-active-basis-contract

promoted-interface-summary : CaseStudySummary
promoted-interface-summary =
  mkCaseStudySummary
    one
    two
    two
    zero
    zero
    zero
    Unit
    two
    promoted-active-basis

transparent-lemma-state : LibraryState lzero
transparent-lemma-state = mkLibraryState Unit

transparent-lemma-extension :
  TransparentDevelopment {ℓU = lzero} transparent-lemma-state
transparent-lemma-extension =
  mkTransparentDevelopment Unit (λ symbol -> symbol)

transparent-lemma-zero-footprint :
  TransparentZeroFootprint transparent-lemma-extension
transparent-lemma-zero-footprint =
  transparent-growth-zero-footprint transparent-lemma-extension

transparent-lemma-summary : CaseStudySummary
transparent-lemma-summary =
  mkCaseStudySummary
    zero
    zero
    zero
    zero
    zero
    zero
    Unit
    zero
    transparent-zero

coverage-does-not-prove-window =
  coverage-alone-does-not-imply-depth-two-window

coverage-does-not-prove-fibonacci =
  coverage-alone-does-not-imply-fibonacci
