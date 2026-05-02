{-# OPTIONS --cubical --safe --guardedness #-}

module CaseStudies.Common where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)

data Unit : Type where
  tt : Unit

unit-is-prop : (p q : Unit) -> p ≡ q
unit-is-prop tt tt = refl

one : Nat
one = suc zero

two : Nat
two = suc one

three : Nat
three = suc two

data RecurrenceClassification : Type where
  transparent-zero : RecurrenceClassification
  sparse-local : RecurrenceClassification
  promoted-active-basis : RecurrenceClassification
  full-coupling : RecurrenceClassification
  no-recurrence-law : RecurrenceClassification

record CaseStudySummary : Type₁ where
  constructor mkCaseStudySummary
  field
    payloadFields : Nat
    activeInterfaceFootprint : Nat
    unaryTraceObligations : Nat
    binaryTraceObligations : Nat
    higherHornObligations : Nat
    derivedHigherHornObligations : Nat
    activeBasisTotality : Type
    expectedMuContribution : Nat
    recurrenceClassification : RecurrenceClassification

open CaseStudySummary public
