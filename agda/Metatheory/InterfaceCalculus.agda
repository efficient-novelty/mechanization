{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.InterfaceCalculus where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)
open import Cubical.Data.Sum.Base using (_⊎_)

open import Core.Nat using (ℕ; _+_)
open import Core.Nat using (zero)
open import Metatheory.Obligations using (Fin)
open import Metatheory.Obligations using (PrimitiveCost; derived; costOf)

private
  variable
    ℓ ℓI ℓP ℓT ℓU : Level

-- A lightweight theorem-facing interface calculus for distinguishing
-- transparent user elaboration from genuinely sealed exported layers.
record LibraryState (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkLibraryState
  field
    ActiveInterface : Type ℓ

open LibraryState public

-- Transparent developments elaborate entirely into the existing active
-- interface and do not create a new opaque export boundary.
record TransparentDevelopment {ℓI ℓU : Level}
  (B : LibraryState ℓI) : Type (ℓ-suc (ℓ-max ℓI ℓU)) where
  constructor mkTransparentDevelopment
  field
    UserSymbol           : Type ℓU
    definitionalExpansion : UserSymbol → ActiveInterface B

open TransparentDevelopment public

-- Sealed layers are exported as public payload together with resolved trace.
record SealedLayer {ℓI ℓP ℓT : Level}
  (B : LibraryState ℓI) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓP ℓT))) where
  constructor mkSealedLayer
  field
    CorePayload  : Type ℓP
    ResolvedTrace : Type ℓT

open SealedLayer public

sealed-public-interface :
  {B : LibraryState ℓI} →
  SealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} B →
  Type (ℓ-max ℓP ℓT)
sealed-public-interface L = CorePayload L ⊎ ResolvedTrace L

after-transparent :
  {ℓU : Level} →
  {B : LibraryState ℓI} →
  TransparentDevelopment {ℓU = ℓU} B →
  LibraryState ℓI
after-transparent {B = B} U = B

transparent-integration-latency :
  {ℓU : Level} →
  {B : LibraryState ℓI} →
  TransparentDevelopment {ℓU = ℓU} B →
  PrimitiveCost
transparent-integration-latency U = derived

transparent-growth-keeps-library-state :
  {ℓU : Level} →
  {B : LibraryState ℓI} →
  (U : TransparentDevelopment {ℓU = ℓU} B) →
  after-transparent U ≡ B
transparent-growth-keeps-library-state U = refl

transparent-definitions-preserve-active-interface :
  {ℓU : Level} →
  {B : LibraryState ℓI} →
  (U : TransparentDevelopment {ℓU = ℓU} B) →
  ActiveInterface (after-transparent U) ≡ ActiveInterface B
transparent-definitions-preserve-active-interface U = refl

transparent-definitions-have-zero-integration-latency :
  {ℓU : Level} →
  {B : LibraryState ℓI} →
  (U : TransparentDevelopment {ℓU = ℓU} B) →
  costOf (transparent-integration-latency U) ≡ zero
transparent-definitions-have-zero-integration-latency U = refl

record TransparentOutsideRecurrence {ℓI ℓU : Level}
  (B : LibraryState ℓI)
  (U : TransparentDevelopment {ℓU = ℓU} B) :
  Type (ℓ-suc (ℓ-max ℓI ℓU)) where
  field
    ordinary-growth-inside-state :
      after-transparent U ≡ B
    unchanged-active-interface :
      ActiveInterface (after-transparent U) ≡ ActiveInterface B
    zero-integration-latency :
      costOf (transparent-integration-latency U) ≡ zero

open TransparentOutsideRecurrence public

transparent-user-level-code-lies-outside-the-recurrence :
  {ℓU : Level} →
  {B : LibraryState ℓI} →
  (U : TransparentDevelopment {ℓU = ℓU} B) →
  TransparentOutsideRecurrence B U
transparent-user-level-code-lies-outside-the-recurrence U = record
  { ordinary-growth-inside-state =
      transparent-growth-keeps-library-state U
  ; unchanged-active-interface =
      transparent-definitions-preserve-active-interface U
  ; zero-integration-latency =
      transparent-definitions-have-zero-integration-latency U
  }

-- A counted presentation of a sealed export boundary. The counts are the
-- paper's κ_k and Δ_k, while the isomorphisms witness counting normal forms.
record ExplicitSealedLayer {ℓI ℓP ℓT : Level}
  (B : LibraryState ℓI) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓP ℓT))) where
  constructor mkExplicitSealedLayer
  field
    layer            : SealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} B
    corePayloadSize  : ℕ
    coherenceCost    : ℕ
    coreCounting     : Iso (CorePayload layer) (Fin corePayloadSize)
    traceCounting    : Iso (ResolvedTrace layer) (Fin coherenceCost)

open ExplicitSealedLayer public

explicit-sealed-public-interface :
  {B : LibraryState ℓI} →
  ExplicitSealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} B →
  Type (ℓ-max ℓP ℓT)
explicit-sealed-public-interface E = sealed-public-interface (layer E)

explicit-sealed-public-size :
  {B : LibraryState ℓI} →
  ExplicitSealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} B →
  ℕ
explicit-sealed-public-size E = corePayloadSize E + coherenceCost E
