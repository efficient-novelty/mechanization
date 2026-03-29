{-# OPTIONS --cubical --safe --guardedness #-}

module Geometry.Clutching where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Equiv using (_≃_; idEquiv; equivFun)
open import Cubical.Foundations.Isomorphism using (Iso; isoToPath; transportIsoToPath)
open import Cubical.Foundations.Univalence using (ua)
open import Cubical.Data.Sigma.Base using (Σ; _,_)
open import Cubical.HITs.S1.Base
  using (S¹; base; loop; rotLoopEquiv)
open import Cubical.HITs.Susp.Base using (Susp; north; south; merid)

record CircleClutchingBoundary (North South : Type) : Type₁ where
  field
    meridianDatum : S¹ → North ≃ South

open CircleClutchingBoundary public

clutching-family :
  {North South : Type} →
  CircleClutchingBoundary North South →
  Susp S¹ →
  Type
clutching-family {North = North} {South = South} B north = North
clutching-family {North = North} {South = South} B south = South
clutching-family B (merid a i) = ua (meridianDatum B a) i

hopf-unary-clutching-datum : S¹ ≃ S¹
hopf-unary-clutching-datum = rotLoopEquiv i0

HopfMeridianDatum : Type
HopfMeridianDatum = S¹ → S¹ ≃ S¹

hopf-meridian-equivalence : HopfMeridianDatum
hopf-meridian-equivalence base = hopf-unary-clutching-datum
hopf-meridian-equivalence (loop i) = rotLoopEquiv i

hopf-binary-clutching-datum :
  hopf-unary-clutching-datum ≡ hopf-unary-clutching-datum
hopf-binary-clutching-datum = cong hopf-meridian-equivalence loop

hopf-clutching-boundary : CircleClutchingBoundary S¹ S¹
hopf-clutching-boundary = record
  { meridianDatum = hopf-meridian-equivalence
  }

HopfClutchingFamily : Susp S¹ → Type
HopfClutchingFamily = clutching-family hopf-clutching-boundary

HopfClutchingTotalSpace : Type
HopfClutchingTotalSpace = Σ (Susp S¹) HopfClutchingFamily

data ⊥ : Type where

private
  data ⊤ : Type where
    tt : ⊤

  data Marker : Type where
    left : Marker
    right : Marker

  Marker-code : Marker → Type
  Marker-code left = ⊤
  Marker-code right = ⊥

  right≠left : right ≡ left → ⊥
  right≠left p = subst Marker-code (sym p) tt

  swap-marker : Marker → Marker
  swap-marker left = right
  swap-marker right = left

  swap-marker-iso : Iso Marker Marker
  swap-marker-iso = record
    { fun = swap-marker
    ; inv = swap-marker
    ; rightInv = λ where
        left → refl
        right → refl
    ; leftInv = λ where
        left → refl
        right → refl
    }

  swap-marker-path : Marker ≡ Marker
  swap-marker-path = isoToPath swap-marker-iso

  transport-swap-marker-left : transport swap-marker-path left ≡ right
  transport-swap-marker-left = transportIsoToPath swap-marker-iso left

  marker-helix : S¹ → Type
  marker-helix base = Marker
  marker-helix (loop i) = swap-marker-path i

loop-nontrivial : loop ≡ refl → ⊥
loop-nontrivial p =
  right≠left
    (sym transport-swap-marker-left
      ∙ cong (λ q → transport q left) (cong (cong marker-helix) p))

hopf-evaluated-loop :
  (λ i → equivFun (hopf-binary-clutching-datum i) base) ≡ loop
hopf-evaluated-loop = refl

constant-evaluated-loop :
  (λ i → equivFun (refl {x = hopf-unary-clutching-datum} i) base) ≡ refl
constant-evaluated-loop = refl

hopf-binary-clutching-nontrivial :
  hopf-binary-clutching-datum ≡ refl →
  ⊥
hopf-binary-clutching-nontrivial α =
  loop-nontrivial
    (sym hopf-evaluated-loop
      ∙ cong (λ f → λ i → equivFun (f i) base)
          α
      ∙ constant-evaluated-loop)

data ClutchingHornExtensionWitness : Type where
  hornWitness : ClutchingHornExtensionWitness

record ClutchingHornExtensionFiber {ℓ : Level} {A : Type ℓ} {φ : I}
  (u : I → Partial φ A) (u0 : A [ φ ↦ u i0 ]) : Type ℓ where
  constructor mkClutchingHornExtensionFiber
  field
    witness : ClutchingHornExtensionWitness

  missingFace : A
  missingFace = hcomp u (outS u0)

  filler : I → A
  filler = hfill u u0

open ClutchingHornExtensionFiber public

canonical-clutching-horn-extension :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  ClutchingHornExtensionFiber u u0
canonical-clutching-horn-extension u u0 =
  mkClutchingHornExtensionFiber hornWitness

clutching-horn-extension-fiber-contractible :
  {ℓ : Level} {A : Type ℓ} {φ : I} →
  (u : I → Partial φ A) →
  (u0 : A [ φ ↦ u i0 ]) →
  isContr (ClutchingHornExtensionFiber u u0)
clutching-horn-extension-fiber-contractible u u0 =
  canonical-clutching-horn-extension u u0 , λ where
    (mkClutchingHornExtensionFiber hornWitness) → refl

record ClutchingFamilyTheorem : Type₁ where
  field
    clutchingFamily : Susp S¹ → Type
    familyAtNorth : clutchingFamily north ≡ S¹
    familyAtSouth : clutchingFamily south ≡ S¹
    unaryClutchingDatum : S¹ ≃ S¹
    meridianClutchingDatum : S¹ → S¹ ≃ S¹
    meridianAtBase : meridianClutchingDatum base ≡ unaryClutchingDatum
    binaryClutchingDatum : unaryClutchingDatum ≡ unaryClutchingDatum
    binaryClutchingNontrivial : binaryClutchingDatum ≡ refl → ⊥

open ClutchingFamilyTheorem public

clutching-family-theorem : ClutchingFamilyTheorem
clutching-family-theorem = record
  { clutchingFamily = HopfClutchingFamily
  ; familyAtNorth = refl
  ; familyAtSouth = refl
  ; unaryClutchingDatum = hopf-unary-clutching-datum
  ; meridianClutchingDatum = hopf-meridian-equivalence
  ; meridianAtBase = refl
  ; binaryClutchingDatum = hopf-binary-clutching-datum
  ; binaryClutchingNontrivial = hopf-binary-clutching-nontrivial
  }
