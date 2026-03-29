{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.TracePrinciple where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)
open import Cubical.Data.Sum.Base using (_⊎_; inl; inr)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations
  using (Fin; fzero; fsuc; injectFinLeft)
open import Metatheory.InterfaceCalculus

composeIso :
  {ℓA ℓB ℓC : Level}
  {A : Type ℓA} {B : Type ℓB} {C : Type ℓC} →
  Iso A B → Iso B C → Iso A C
composeIso α β = record
  { fun = λ a → Iso.fun β (Iso.fun α a)
  ; inv = λ c → Iso.inv α (Iso.inv β c)
  ; rightInv = λ c →
      cong (Iso.fun β) (Iso.rightInv α (Iso.inv β c))
      ∙ Iso.rightInv β c
  ; leftInv = λ a →
      cong (Iso.inv α) (Iso.leftInv β (Iso.fun α a))
      ∙ Iso.leftInv α a
  }

sumIso :
  {ℓA ℓB ℓC ℓD : Level}
  {A : Type ℓA} {B : Type ℓB} {C : Type ℓC} {D : Type ℓD} →
  Iso A B → Iso C D → Iso (A ⊎ C) (B ⊎ D)
sumIso α β = record
  { fun = λ where
      (inl a) → inl (Iso.fun α a)
      (inr c) → inr (Iso.fun β c)
  ; inv = λ where
      (inl b) → inl (Iso.inv α b)
      (inr d) → inr (Iso.inv β d)
  ; rightInv = λ where
      (inl b) → cong inl (Iso.rightInv α b)
      (inr d) → cong inr (Iso.rightInv β d)
  ; leftInv = λ where
      (inl a) → cong inl (Iso.leftInv α a)
      (inr c) → cong inr (Iso.leftInv β c)
  }

injectFinRight : (left right : Nat) → Fin right → Fin (left + right)
injectFinRight zero right j = j
injectFinRight (suc left) right j = fsuc (injectFinRight left right j)

mergeFinSum : (left right : Nat) → Fin left ⊎ Fin right → Fin (left + right)
mergeFinSum left right (inl i) = injectFinLeft left right i
mergeFinSum left right (inr j) = injectFinRight left right j

bumpSplit : {left right : Nat} → Fin left ⊎ Fin right → Fin (suc left) ⊎ Fin right
bumpSplit (inl i) = inl (fsuc i)
bumpSplit (inr j) = inr j

splitFinSum : (left right : Nat) → Fin (left + right) → Fin left ⊎ Fin right
splitFinSum zero right i = inr i
splitFinSum (suc left) right fzero = inl fzero
splitFinSum (suc left) right (fsuc i) = bumpSplit (splitFinSum left right i)

split-inject-left :
  (left right : Nat) (i : Fin left) →
  splitFinSum left right (injectFinLeft left right i) ≡ inl i
split-inject-left zero right ()
split-inject-left (suc left) right fzero = refl
split-inject-left (suc left) right (fsuc i)
  = cong bumpSplit (split-inject-left left right i)

split-inject-right :
  (left right : Nat) (j : Fin right) →
  splitFinSum left right (injectFinRight left right j) ≡ inr j
split-inject-right zero right j = refl
split-inject-right (suc left) right j
  = cong bumpSplit (split-inject-right left right j)

split-after-merge :
  (left right : Nat) (s : Fin left ⊎ Fin right) →
  splitFinSum left right (mergeFinSum left right s) ≡ s
split-after-merge left right (inl i) = split-inject-left left right i
split-after-merge left right (inr j) = split-inject-right left right j

merge-bumpSplit :
  (left right : Nat) (s : Fin left ⊎ Fin right) →
  mergeFinSum (suc left) right (bumpSplit s) ≡ fsuc (mergeFinSum left right s)
merge-bumpSplit left right (inl i) = refl
merge-bumpSplit left right (inr j) = refl

merge-after-split :
  (left right : Nat) (i : Fin (left + right)) →
  mergeFinSum left right (splitFinSum left right i) ≡ i
merge-after-split zero right i = refl
merge-after-split (suc left) right fzero = refl
merge-after-split (suc left) right (fsuc i) =
  merge-bumpSplit left right (splitFinSum left right i)
  ∙ cong fsuc (merge-after-split left right i)

fin-sum-iso : (left right : Nat) → Iso (Fin left ⊎ Fin right) (Fin (left + right))
fin-sum-iso left right = record
  { fun = mergeFinSum left right
  ; inv = splitFinSum left right
  ; rightInv = merge-after-split left right
  ; leftInv = split-after-merge left right
  }

record IntegrationTracePrinciple
  {ℓI ℓP ℓT : Level}
  {B : LibraryState ℓI}
  (E : ExplicitSealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} B) :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓP ℓT))) where
  field
    canonical-decomposition :
      Iso (explicit-sealed-public-interface E)
          (CorePayload (ExplicitSealedLayer.layer E)
           ⊎ ResolvedTrace (ExplicitSealedLayer.layer E))
    core-cardinality :
      Iso (CorePayload (ExplicitSealedLayer.layer E))
          (Fin (corePayloadSize E))
    trace-cardinality :
      Iso (ResolvedTrace (ExplicitSealedLayer.layer E))
          (Fin (coherenceCost E))
    public-cardinality :
      Iso (explicit-sealed-public-interface E)
          (Fin (explicit-sealed-public-size E))

open IntegrationTracePrinciple public

public-counting-normal-form :
  {ℓI ℓP ℓT : Level}
  {B : LibraryState ℓI} →
  (E : ExplicitSealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} B) →
  Iso (explicit-sealed-public-interface E)
      (Fin (explicit-sealed-public-size E))
public-counting-normal-form E =
  composeIso
    (sumIso (coreCounting E) (traceCounting E))
    (fin-sum-iso (corePayloadSize E) (coherenceCost E))

integration-trace-principle :
  {ℓI ℓP ℓT : Level}
  {B : LibraryState ℓI} →
  (E : ExplicitSealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} B) →
  IntegrationTracePrinciple E
integration-trace-principle E = record
  { canonical-decomposition = record
      { fun = λ s → s
      ; inv = λ s → s
      ; rightInv = λ s → refl
      ; leftInv = λ s → refl
      }
  ; core-cardinality = coreCounting E
  ; trace-cardinality = traceCounting E
  ; public-cardinality = public-counting-normal-form E
  }
