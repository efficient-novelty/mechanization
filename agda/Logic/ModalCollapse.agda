{-# OPTIONS --cubical --guardedness #-}

module Logic.ModalCollapse where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Equiv using (_≃_)
open import Cubical.Foundations.Univalence using (ua)
open import Cubical.Data.Empty.Base using (⊥)

-- Artifact A: Euclidean modal collapse vs Lorentzian separation
--
-- This module intentionally isolates the logical shape of the argument:
--   1) A Euclidean isotropy witness provides an equivalence between
--      Flow-vectors and Shape-vectors.
--   2) Univalence bridges this equivalence into a path (type equality),
--      i.e. modal collapse.
--   3) A Lorentzian null-cone separation axiom forbids such a path.

record TangentSpace : Type₁ where
  field
    Vec      : Type
    IsFlow   : Vec → Type
    IsShape  : Vec → Type

open TangentSpace public

record ModalLayer (T : TangentSpace) : Type₁ where
  field
    FlowTy   : Type
    ShapeTy  : Type

open ModalLayer public

-- Geometric assumptions packaged as logical witnesses.
record GeometryAssumptions (T : TangentSpace) (M : ModalLayer T) : Type₁ where
  field
    -- In Euclidean geometry, isotropy can identify Flow and Shape directions.
    euclidean-isotropy : FlowTy M ≃ ShapeTy M

    -- In Lorentzian geometry, null-cone separation blocks identification.
    lorentzian-separation : (FlowTy M ≡ ShapeTy M) → ⊥

open GeometryAssumptions public

-- Univalence bridge: equivalence induces type equality.
univalence-bridge
  : {A B : Type}
  → (A ≃ B)
  → (A ≡ B)
univalence-bridge = ua

-- Euclidean modal collapse theorem:
-- isotropy + univalence implies Flow and Shape collapse.
euclidean-modal-collapse
  : {T : TangentSpace}
  → (M : ModalLayer T)
  → (G : GeometryAssumptions T M)
  → FlowTy M ≡ ShapeTy M
euclidean-modal-collapse M G =
  univalence-bridge (euclidean-isotropy G)

-- Lorentzian modal separation theorem:
-- null-cone barrier refutes modal collapse.
lorentzian-no-collapse
  : {T : TangentSpace}
  → (M : ModalLayer T)
  → (G : GeometryAssumptions T M)
  → (FlowTy M ≡ ShapeTy M)
  → ⊥
lorentzian-no-collapse M G p = lorentzian-separation G p

-- Combined consistency statement:
-- If Euclidean collapse is derivable while Lorentzian separation holds,
-- then assuming both signatures at once is contradictory.
modal-signature-incompatibility
  : {T : TangentSpace}
  → (M : ModalLayer T)
  → (G : GeometryAssumptions T M)
  → ⊥
modal-signature-incompatibility M G =
  lorentzian-no-collapse M G (euclidean-modal-collapse M G)
