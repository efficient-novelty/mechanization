{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.CanonicalTelescope where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin)

private
  variable
    ℓ : Level

-- A finite, canonical telescope surface.  The telescope is deliberately
-- syntactic: fields are addressed by the local Fin index used throughout the
-- coherence-depth metatheory.
record CanonicalTelescope (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkCanonicalTelescope
  field
    fieldCount : Nat
    fieldAt    : Fin fieldCount → Type ℓ

open CanonicalTelescope public

FieldIndex : CanonicalTelescope ℓ → Type
FieldIndex T = Fin (fieldCount T)

canonical-telescope-cardinality :
  CanonicalTelescope ℓ → Nat
canonical-telescope-cardinality T = fieldCount T

canonical-telescope-field :
  (T : CanonicalTelescope ℓ) →
  FieldIndex T → Type ℓ
canonical-telescope-field T i = fieldAt T i

