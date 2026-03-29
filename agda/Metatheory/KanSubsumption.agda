{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.KanSubsumption where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)

open import Core.Nat renaming (ℕ to Nat)
open import Core.Sequence using (Vec; []; _∷_)
open import Metatheory.Obligations
  using ( HistoricalSupport
        ; mkSupport
        ; PrimitiveCost
        ; derived
        ; requiresPrimitive
        ; ObligationLanguage
        ; Fin
        ; fzero
        ; fsuc
        ; mapVec
        )

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

record HornExtensionFiber {ℓ : Level} {A : Type ℓ} {φ : I}
  (u : I → Partial φ A) (u0 : A [ φ ↦ u i0 ]) : Type ℓ where
  constructor mkHornExtensionFiber
  field
    missingFace : Arity3-Obligation u u0
    filler      : I → A

open HornExtensionFiber public

canonical-horn-extension :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  HornExtensionFiber u u0
canonical-horn-extension u u0 =
  mkHornExtensionFiber
    (arity3-obligation-syntactically-derivable u u0)
    (arity3-open-box-hfilled u u0)

data HornCandidate : Type where
  horn-candidate : HornCandidate

data StructuralObligation {ℓ : Level} {A : Type ℓ} {φ : I}
  (u : I → Partial φ A) (u0 : A [ φ ↦ u i0 ]) : Nat → Type ℓ where
  depth2-boundary :
    StructuralObligation u u0 (suc (suc zero))
  extend-remote-layer :
    {k : Nat} →
    (boundary : StructuralObligation u u0 (suc (suc k))) →
    HornExtensionFiber u u0 →
    StructuralObligation u u0 (suc (suc (suc k)))

record HornReductionView {ℓ : Level} {A : Type ℓ} {φ : I}
  (u : I → Partial φ A) (u0 : A [ φ ↦ u i0 ]) (offset : Nat) : Type ℓ where
  constructor mkHornReductionView
  field
    boundary       : StructuralObligation u u0 (suc (suc offset))
    additionalData : HornExtensionFiber u u0

open HornReductionView public

structural-integration-horn-reduction :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  Iso (StructuralObligation u u0 (suc (suc (suc offset))))
      (HornReductionView u u0 offset)
structural-integration-horn-reduction u u0 offset = record
  { fun = λ where
      (extend-remote-layer boundary fiber) →
        mkHornReductionView boundary fiber
  ; inv = λ where
      (mkHornReductionView boundary fiber) →
        extend-remote-layer boundary fiber
  ; rightInv = λ where
      (mkHornReductionView boundary fiber) → refl
  ; leftInv = λ where
      (extend-remote-layer boundary fiber) → refl
  }

allFin : (n : Nat) → Vec (Fin n) n
allFin zero = []
allFin (suc n) = fzero ∷ mapVec fsuc (allFin n)

structural-support :
  {ℓ : Level} {A : Type ℓ} {φ : I}
  {u : I → Partial φ A} {u0 : A [ φ ↦ u i0 ]} {k : Nat} →
  StructuralObligation u u0 k → HistoricalSupport k
structural-support {k = suc (suc zero)} depth2-boundary =
  mkSupport (suc (suc zero)) (allFin (suc (suc zero)))
structural-support {k = suc (suc (suc k))} (extend-remote-layer _ _) =
  mkSupport (suc (suc (suc k))) (allFin (suc (suc (suc k))))

structural-primitive-cost :
  {ℓ : Level} {A : Type ℓ} {φ : I}
  {u : I → Partial φ A} {u0 : A [ φ ↦ u i0 ]} {k : Nat} →
  StructuralObligation u u0 k → PrimitiveCost
structural-primitive-cost depth2-boundary = requiresPrimitive
structural-primitive-cost (extend-remote-layer _ _) = derived

structural-weaken :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (k : Nat) →
  StructuralObligation u u0 k →
  StructuralObligation u u0 (suc k)
structural-weaken u u0 zero ()
structural-weaken u u0 (suc zero) ()
structural-weaken u u0 (suc (suc k)) o =
  extend-remote-layer o (canonical-horn-extension u u0)

structural-horn-language :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  ObligationLanguage ℓ-zero ℓ
structural-horn-language u u0 = record
  { Candidate = HornCandidate
  ; O = λ _ k → StructuralObligation u u0 k
  ; weaken = λ k o → structural-weaken u u0 k o
  ; Supp = structural-support
  ; primitiveCost = structural-primitive-cost
  }

remote-layer-obligation-derived :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  (view : HornReductionView u u0 offset) →
  ObligationLanguage.primitiveCost (structural-horn-language u u0)
    {X = horn-candidate}
    {k = suc (suc (suc offset))}
    (Iso.inv (structural-integration-horn-reduction u u0 offset) view)
    ≡ derived
remote-layer-obligation-derived u u0 offset view = refl
