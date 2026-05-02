{-# OPTIONS --cubical --safe --guardedness #-}

module Test.ActiveBasisExamples where

open import Metatheory.FiniteInterfaceBasis
open import Metatheory.GlobalActionSemantics
open import Metatheory.ActiveBasisContract

finite-interface-basis-surface = ActiveInterface
finite-interface-field-surface = InterfaceField
transparent-equivalence-surface = TransparentEquivalence
basis-family-surface = BasisFamily
basis-families-exist-theorem = basis-families-exist
basis-family-cardinality-theorem =
  basis-family-cardinality-invariant
basis-action-equivalence-theorem = basis-action-equivalence
global-action-payload-surface = GlobalActionPayload
action-totality-surface = ActionTotality
global-action-totality-theorem =
  global-action-totality-implies-active-basis-contract
active-basis-density-theorem =
  active-basis-contract-entails-density
coverage-depth-two-noncircularity-example =
  coverage-alone-does-not-imply-depth-two-window
coverage-fibonacci-noncircularity-example =
  coverage-alone-does-not-imply-fibonacci
