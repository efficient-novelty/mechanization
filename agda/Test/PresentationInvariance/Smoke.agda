{-# OPTIONS --cubical --safe --guardedness #-}

module Test.PresentationInvariance.Smoke where

open import Metatheory.CanonicalTelescope
open import Metatheory.TraceCostNormalForm

canonical-telescope-surface = CanonicalTelescope
canonical-field-index-surface = FieldIndex
canonical-cardinality-available = canonical-telescope-cardinality
canonical-field-lookup-available = canonical-telescope-field

trace-cost-field-surface = TraceCostField
trace-cost-subtelescope-surface = TraceCostSubtelescope
trace-cost-normal-form-surface = CanonicalTraceCostNormalForm
trace-cardinality-available = trace-cost-normal-form-cardinality
primitive-subtelescope-available = primitive-trace-subtelescope
derived-subtelescope-available = derived-trace-subtelescope
mu-normal-form-available = mu-of-trace-cost-normal-form

