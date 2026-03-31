{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.UpperBound where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)
open import Cubical.Data.Sigma.Base using (Σ; _,_)

open import Core.Nat using (zero; suc; _+_) renaming (ℕ to Nat)
open import Metatheory.Obligations using (StabilizesAt)
open import Metatheory.KanSubsumption
  using ( StructuralObligation
        ; HornCandidate
        ; horn-candidate
        ; HornExtensionWitness
        ; HornExtensionFiber
        ; hornWitness
        ; mkHornExtensionFiber
        ; depth2-boundary
        ; extend-remote-layer
        ; horn-extension-fiber-contractible
        ; TelescopicTraceChain
        ; no-remote-traces
        ; extend-trace-chain
        ; structural-weaken
        ; structural-horn-language
        )

ContractibleRemoteFactor :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  Nat → Type ℓ
ContractibleRemoteFactor u u0 offset =
  TelescopicTraceChain u u0 offset

snoc-remote-factor :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  {u : I → Partial φ A} →
  {u0 : A [ φ ↦ u i0 ]} →
  {offset : Nat} →
  HornExtensionFiber u u0 →
  Σ (StructuralObligation u u0 2)
    (λ _ → ContractibleRemoteFactor u u0 offset) →
  Σ (StructuralObligation u u0 2)
    (λ _ → ContractibleRemoteFactor u u0 (suc offset))
snoc-remote-factor fiber (boundary , traceChain) =
  boundary , extend-trace-chain fiber traceChain

factor-through-depth-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  StructuralObligation u u0 (2 + offset) →
  Σ (StructuralObligation u u0 2)
    (λ _ → ContractibleRemoteFactor u u0 offset)
factor-through-depth-two u u0 zero boundary =
  boundary , no-remote-traces
factor-through-depth-two u u0 (suc offset) (extend-remote-layer boundary fiber) =
  snoc-remote-factor fiber (factor-through-depth-two u u0 offset boundary)

rebuild-from-depth-two-factor :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  Σ (StructuralObligation u u0 2)
    (λ _ → ContractibleRemoteFactor u u0 offset) →
  StructuralObligation u u0 (2 + offset)
rebuild-from-depth-two-factor u u0 zero (boundary , no-remote-traces) =
  boundary
rebuild-from-depth-two-factor u u0 (suc offset)
  (boundary , extend-trace-chain fiber traceChain) =
  extend-remote-layer
    (rebuild-from-depth-two-factor u u0 offset (boundary , traceChain))
    fiber

rebuild-after-factor-through-depth-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  (o : StructuralObligation u u0 (2 + offset)) →
  rebuild-from-depth-two-factor u u0 offset
    (factor-through-depth-two u u0 offset o)
    ≡ o
rebuild-after-factor-through-depth-two u u0 zero depth2-boundary =
  refl
rebuild-after-factor-through-depth-two u u0 (suc offset)
  (extend-remote-layer boundary fiber) =
  cong (λ b → extend-remote-layer b fiber)
    (rebuild-after-factor-through-depth-two u u0 offset boundary)

factor-through-depth-two-after-rebuild :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  (factor :
    Σ (StructuralObligation u u0 2)
      (λ _ → ContractibleRemoteFactor u u0 offset)) →
  factor-through-depth-two u u0 offset
    (rebuild-from-depth-two-factor u u0 offset factor)
    ≡ factor
factor-through-depth-two-after-rebuild u u0 zero
  (boundary , no-remote-traces) =
  refl
factor-through-depth-two-after-rebuild u u0 (suc offset)
  (boundary , extend-trace-chain fiber traceChain) =
  cong (snoc-remote-factor fiber)
    (factor-through-depth-two-after-rebuild u u0 offset
      (boundary , traceChain))

structural-obligation-contractible-factorization :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  Iso (StructuralObligation u u0 (2 + offset))
      (Σ (StructuralObligation u u0 2)
        (λ _ → ContractibleRemoteFactor u u0 offset))
structural-obligation-contractible-factorization u u0 offset = record
  { fun = factor-through-depth-two u u0 offset
  ; inv = rebuild-from-depth-two-factor u u0 offset
  ; rightInv = factor-through-depth-two-after-rebuild u u0 offset
  ; leftInv = rebuild-after-factor-through-depth-two u u0 offset
  }

contractible-remote-factor-center :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  ContractibleRemoteFactor u u0 offset
contractible-remote-factor-center u u0 zero =
  no-remote-traces
contractible-remote-factor-center u u0 (suc offset) =
  extend-trace-chain
    (horn-extension-fiber-contractible u u0 .fst)
    (contractible-remote-factor-center u u0 offset)

contractible-remote-factor-contractible :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  isContr (ContractibleRemoteFactor u u0 offset)
contractible-remote-factor-contractible u u0 zero =
  no-remote-traces , λ where
    no-remote-traces → refl
contractible-remote-factor-contractible u u0 (suc offset)
  with horn-extension-fiber-contractible u u0
     | contractible-remote-factor-contractible u u0 offset
... | centerFiber , contractFiber
    | centerChain , contractChain =
  extend-trace-chain centerFiber centerChain , λ where
    (extend-trace-chain fiber traceChain) →
      cong₂ extend-trace-chain
        (contractFiber fiber)
        (contractChain traceChain)

collapse-to-depth-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  StructuralObligation u u0 (2 + offset) →
  StructuralObligation u u0 2
collapse-to-depth-two u u0 zero boundary =
  boundary
collapse-to-depth-two u u0 (suc offset) (extend-remote-layer boundary _) =
  collapse-to-depth-two u u0 offset boundary

extend-from-depth-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  StructuralObligation u u0 2 →
  StructuralObligation u u0 (2 + offset)
extend-from-depth-two u u0 zero boundary =
  boundary
extend-from-depth-two u u0 (suc offset) boundary =
  structural-weaken u u0 (2 + offset)
    (extend-from-depth-two u u0 offset boundary)

collapse-after-extend :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  (boundary : StructuralObligation u u0 2) →
  collapse-to-depth-two u u0 offset
    (extend-from-depth-two u u0 offset boundary)
    ≡ boundary
collapse-after-extend u u0 zero boundary =
  refl
collapse-after-extend u u0 (suc offset) boundary =
  collapse-after-extend u u0 offset boundary

extend-after-collapse :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  (o : StructuralObligation u u0 (2 + offset)) →
  extend-from-depth-two u u0 offset
    (collapse-to-depth-two u u0 offset o)
    ≡ o
extend-after-collapse u u0 zero depth2-boundary =
  refl
extend-after-collapse u u0 (suc offset)
  (extend-remote-layer boundary (mkHornExtensionFiber hornWitness)) =
  cong
    (λ b → extend-remote-layer b (mkHornExtensionFiber hornWitness))
    (extend-after-collapse u u0 offset boundary)

structural-obligation-set-equivalence :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  (offset : Nat) →
  Iso (StructuralObligation u u0 (2 + offset))
      (StructuralObligation u u0 2)
structural-obligation-set-equivalence u u0 offset = record
  { fun = collapse-to-depth-two u u0 offset
  ; inv = extend-from-depth-two u u0 offset
  ; rightInv = collapse-after-extend u u0 offset
  ; leftInv = extend-after-collapse u u0 offset
  }

structural-stabilizes-at-two :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  StabilizesAt (structural-horn-language u u0) 2
structural-stabilizes-at-two u u0 = record
  { stabilize = λ where
      horn-candidate offset →
        structural-obligation-set-equivalence u u0 offset
  }
