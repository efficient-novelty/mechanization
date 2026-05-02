{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.SurfaceToHornImage where

open import Cubical.Foundations.Prelude
open import Cubical.Data.Sum.Base using (_⊎_; inl; inr)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.InterfaceCalculus using (LibraryState)
open import Metatheory.Obligations
  using ( Fin
        ; HistoricalSupport
        ; PrimitiveCost
        ; derived
        )
open import Metatheory.TraceCostNormalForm
open import Metatheory.RawStructuralSyntax
open import Metatheory.RawStructuralTyping
open import Metatheory.SurfaceNormalizationBridge

private
  variable
    ℓ : Level
    B : LibraryState ℓ

data HornImageKind {ℓ : Level}
  (c : RawStructuralClause ℓ) : Type (ℓ-suc ℓ) where
  action-image :
    UnaryTraceSupport c →
    HornImageKind c
  comparison-image :
    BinaryTraceSupport c →
    HornImageKind c
  packaged-horn-image :
    HigherBoundarySupport c →
    HornImageKind c

record SurfaceHornImage {ℓ : Level}
  {c : RawStructuralClause ℓ}
  (role : TypedStructuralRole c) : Type (ℓ-suc ℓ) where
  constructor mkSurfaceHornImage
  field
    normalTrace :
      TraceCostField twoLayerDepth
    imageKind :
      HornImageKind c
    supportMatchesNormalizer :
      support normalTrace ≡ supportForRole role
    arityMatchesNormalizer :
      arity normalTrace ≡ HistoricalSupport.arity (support normalTrace)
    primitiveCostMatchesNormalizer :
      primitiveCost normalTrace ≡ primitiveCostForRole role

open SurfaceHornImage public

horn-image-kind-for-role :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  HornImageKind c
horn-image-kind-for-role (unary-action-role p s) =
  action-image (act-clause-has-unary-support p s)
horn-image-kind-for-role (binary-comparison-role p left right) =
  comparison-image (cmp-clause-has-binary-support p left right)
horn-image-kind-for-role (horn-boundary-role b package) =
  packaged-horn-image (horn-clause-has-higher-boundary-support b package)

surface-to-horn-normal-form :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  SurfaceHornImage role
surface-to-horn-normal-form role =
  mkSurfaceHornImage
    (traceCostFieldForRole role)
    (horn-image-kind-for-role role)
    refl
    (arityMatchesSupport (traceCostFieldForRole role))
    refl

surface-to-horn-preserves-support :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  support (normalTrace (surface-to-horn-normal-form role))
    ≡
  supportForRole role
surface-to-horn-preserves-support role =
  supportMatchesNormalizer (surface-to-horn-normal-form role)

surface-to-horn-preserves-arity :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  arity (normalTrace (surface-to-horn-normal-form role))
    ≡
  HistoricalSupport.arity
    (support (normalTrace (surface-to-horn-normal-form role)))
surface-to-horn-preserves-arity role =
  arityMatchesNormalizer (surface-to-horn-normal-form role)

surface-to-horn-preserves-primitive-cost :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  primitiveCost (normalTrace (surface-to-horn-normal-form role))
    ≡
  primitiveCostForRole role
surface-to-horn-preserves-primitive-cost role =
  primitiveCostMatchesNormalizer (surface-to-horn-normal-form role)

higher-structural-fields-derived :
  (b : RawBoundary ℓ) →
  (package : PackagedHornBoundary b) →
  primitiveCost
    (normalTrace
      (surface-to-horn-normal-form (horn-boundary-role b package)))
    ≡
  derived
higher-structural-fields-derived b package = refl

higher-raw-structural-traces-derived :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  (naked-higher-face-rejected-or-packaged role ≡ rejected-as-structural)
    ⊎
  (primitiveCost (normalTrace (surface-to-horn-normal-form role)) ≡ derived)
higher-raw-structural-traces-derived (unary-action-role p s) =
  inl refl
higher-raw-structural-traces-derived
  (binary-comparison-role p left right) =
  inl refl
higher-raw-structural-traces-derived (horn-boundary-role b package) =
  inr refl

raw-syntax-no-naked-higher-structural-projections :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  HigherFaceDisposition ℓ
raw-syntax-no-naked-higher-structural-projections =
  naked-higher-face-rejected-or-packaged

horn-image-complete-for-structural-clauses :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  (admissible : AdmissibleRawExtension B e) →
  (i : Fin
       (RawTelescope.fieldCount
         (raw-extension-structural-clauses e))) →
  SurfaceHornImage
    (StructuralClausesWellTyped.structuralClauseTyped
      (AdmissibleRawExtension.structuralWellTyped admissible)
      i)
horn-image-complete-for-structural-clauses B e admissible i =
  surface-to-horn-normal-form
    (StructuralClausesWellTyped.structuralClauseTyped
      (AdmissibleRawExtension.structuralWellTyped admissible)
      i)

record RawStructuralHornNormalization
  (B : LibraryState ℓ)
  (e : RawExtension ℓ)
  (admissible : AdmissibleRawExtension B e) :
  Type (ℓ-suc (ℓ-suc ℓ)) where
  constructor mkRawStructuralHornNormalization
  field
    normalizedSignature :
      CanonicalNormalizedSignature ℓ
    structuralClauseHornImage :
      (i : Fin
           (RawTelescope.fieldCount
             (raw-extension-structural-clauses e))) →
      SurfaceHornImage
        (StructuralClausesWellTyped.structuralClauseTyped
          (AdmissibleRawExtension.structuralWellTyped admissible)
          i)
    traceImageMatchesBridge :
      (i : Fin
           (RawTelescope.fieldCount
             (raw-extension-structural-clauses e))) →
      normalizedTraceField e admissible i
        ≡
      normalTrace (structuralClauseHornImage i)

open RawStructuralHornNormalization public

trace-image-matches-role :
  {c : RawStructuralClause ℓ} →
  (role : TypedStructuralRole c) →
  traceCostFieldForRole role
    ≡
  normalTrace (surface-to-horn-normal-form role)
trace-image-matches-role (unary-action-role p s) = refl
trace-image-matches-role (binary-comparison-role p left right) = refl
trace-image-matches-role (horn-boundary-role b package) = refl

raw-structural-normalizes-to-horn :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  (admissible : AdmissibleRawExtension B e) →
  RawStructuralHornNormalization B e admissible
raw-structural-normalizes-to-horn B e admissible =
  mkRawStructuralHornNormalization
    (normalizeRawExtension B e admissible)
    (horn-image-complete-for-structural-clauses B e admissible)
    (λ i →
      trace-image-matches-role
        (StructuralClausesWellTyped.structuralClauseTyped
          (AdmissibleRawExtension.structuralWellTyped admissible)
          i))
