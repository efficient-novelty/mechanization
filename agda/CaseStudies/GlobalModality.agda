{-# OPTIONS --cubical --safe --guardedness #-}

module CaseStudies.GlobalModality where

open import Cubical.Foundations.Prelude
open import Agda.Primitive using (lzero)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin)
open import Metatheory.FiniteInterfaceBasis
open import Metatheory.GlobalActionSemantics
open import Metatheory.ActiveBasisContract
  using ( ActiveBasisContract
        ; global-action-totality-implies-active-basis-contract
        )
open import Metatheory.FullCouplingEnvelope
open import CaseStudies.Common

global-modality-field : Fin three -> InterfaceField lzero
global-modality-field i = mkInterfaceField Unit

global-modality-interface : ActiveInterface lzero
global-modality-interface =
  canonical-active-interface three global-modality-field

global-modality-action :
  GlobalActionPayload global-modality-interface lzero
global-modality-action =
  mkGlobalActionPayload
    (whole-active-interface-scope global-modality-interface)
    (λ i -> Unit)
    (λ i -> unit-is-prop)

global-modality-totality : ActionTotality global-modality-action
global-modality-totality = mkActionTotality (λ i -> tt)

global-modality-active-basis-contract :
  ActiveBasisContract global-modality-action
global-modality-active-basis-contract =
  global-action-totality-implies-active-basis-contract
    global-modality-totality

global-modality-layer-cost : Fin three -> Nat
global-modality-layer-cost i = one

global-modality-envelope : FullCouplingEnvelope three
global-modality-envelope = full-coupling-envelope three

global-modality-recurrence =
  full-coupling-specializes-sparse-recurrence
    three
    global-modality-layer-cost
    one

global-modality-summary : CaseStudySummary
global-modality-summary =
  mkCaseStudySummary
    one
    three
    one
    two
    one
    one
    Unit
    three
    full-coupling
