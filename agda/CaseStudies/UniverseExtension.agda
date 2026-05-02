{-# OPTIONS --cubical --safe --guardedness #-}

module CaseStudies.UniverseExtension where

open import Cubical.Foundations.Prelude
open import Agda.Primitive using (lzero)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin)
open import Metatheory.FiniteInterfaceBasis
open import Metatheory.GlobalActionSemantics
open import Metatheory.ActiveBasisContract
  using ( ActiveBasisContract
        ; ActiveBasisDensity
        ; global-action-totality-implies-active-basis-contract
        ; active-basis-contract-entails-density
        )
open import Metatheory.FullCouplingEnvelope
open import CaseStudies.Common

universe-interface-field : Fin two -> InterfaceField lzero
universe-interface-field i = mkInterfaceField Unit

universe-active-interface : ActiveInterface lzero
universe-active-interface =
  canonical-active-interface two universe-interface-field

universe-action-payload :
  GlobalActionPayload universe-active-interface lzero
universe-action-payload =
  mkGlobalActionPayload
    (whole-active-interface-scope universe-active-interface)
    (λ i -> Unit)
    (λ i -> unit-is-prop)

universe-action-totality : ActionTotality universe-action-payload
universe-action-totality = mkActionTotality (λ i -> tt)

universe-active-basis-contract :
  ActiveBasisContract universe-action-payload
universe-active-basis-contract =
  global-action-totality-implies-active-basis-contract
    universe-action-totality

universe-active-basis-density :
  ActiveBasisDensity universe-action-payload
universe-active-basis-density =
  active-basis-contract-entails-density
    universe-active-basis-contract

universe-layer-cost : Fin two -> Nat
universe-layer-cost i = one

universe-full-envelope : FullCouplingEnvelope two
universe-full-envelope = full-coupling-envelope two

universe-full-recurrence =
  full-coupling-specializes-sparse-recurrence
    two
    universe-layer-cost
    one

universe-depth-two-affine-law =
  full-coupling-depth-two-affine-law one

universe-extension-summary : CaseStudySummary
universe-extension-summary =
  mkCaseStudySummary
    one
    two
    one
    one
    one
    one
    Unit
    two
    full-coupling

universe-refactored-presentation-same-mu : Nat
universe-refactored-presentation-same-mu =
  expectedMuContribution universe-extension-summary
