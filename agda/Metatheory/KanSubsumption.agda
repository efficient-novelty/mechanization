{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.KanSubsumption where

open import Cubical.Foundations.Prelude

private
  variable
    ℓ : Level
    A : Type ℓ
    φ : I

-- This module isolates the algorithmic content of arity-3 subsumption.
-- It does not claim that the full space of 3-cell fillers is contractible.
-- Instead it packages exactly the open-box data consumed by cubical
-- composition. When A is instantiated by a square type, this is the
-- missing-face problem for an open 3-box.
Arity3-Obligation :
  {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Type ℓ
Arity3-Obligation {A = A} _ _ = A

arity3-obligation-syntactically-derivable :
  {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Arity3-Obligation u u0
arity3-obligation-syntactically-derivable u u0 =
  hcomp u (outS u0)

history-beyond-two-algorithmically-subsumed :
  {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Arity3-Obligation u u0
history-beyond-two-algorithmically-subsumed =
  arity3-obligation-syntactically-derivable

arity3-open-box-hfilled :
  {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  I → A
arity3-open-box-hfilled u u0 =
  hfill u u0
