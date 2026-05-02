{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.RawStructuralTyping where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.InterfaceCalculus using (LibraryState)
open import Metatheory.Obligations using (Fin)
open import Metatheory.RawStructuralSyntax

private
  variable
    ℓ : Level

data PayloadTypingWitness : Type where
  payload-typed : PayloadTypingWitness

record PackagedHornBoundary (b : RawBoundary ℓ) : Type (ℓ-suc ℓ) where
  constructor mkPackagedHornBoundary
  field
    typedSupport :
      Metatheory.RawStructuralSyntax.RawBoundary.boundarySupport b
      ≡ Metatheory.RawStructuralSyntax.RawBoundary.boundarySupport b

open PackagedHornBoundary public

data TypedStructuralRole {ℓ : Level} :
  RawStructuralClause ℓ → Type (ℓ-suc ℓ) where
  unary-action-role :
    (p : NewPayloadRef) →
    (s : BasisSite) →
    TypedStructuralRole (act p s)
  binary-comparison-role :
    (p : NewPayloadRef) →
    (left right : BasisSite) →
    TypedStructuralRole (cmp p left right)
  horn-boundary-role :
    (b : RawBoundary ℓ) →
    PackagedHornBoundary b →
    TypedStructuralRole (horn b)

record UnaryTraceSupport {ℓ : Level}
  (c : RawStructuralClause ℓ) : Type (ℓ-suc ℓ) where
  constructor mkUnaryTraceSupport
  field
    unaryPayload : NewPayloadRef
    unarySite    : BasisSite
    unaryClause  : c ≡ act unaryPayload unarySite

open UnaryTraceSupport public

record BinaryTraceSupport {ℓ : Level}
  (c : RawStructuralClause ℓ) : Type (ℓ-suc ℓ) where
  constructor mkBinaryTraceSupport
  field
    binaryPayload : NewPayloadRef
    leftSite      : BasisSite
    rightSite     : BasisSite
    binaryClause  : c ≡ cmp binaryPayload leftSite rightSite

open BinaryTraceSupport public

record HigherBoundarySupport {ℓ : Level}
  (c : RawStructuralClause ℓ) : Type (ℓ-suc ℓ) where
  constructor mkHigherBoundarySupport
  field
    packagedBoundary : RawBoundary ℓ
    boundaryPackage  : PackagedHornBoundary packagedBoundary
    higherClause     : c ≡ horn packagedBoundary

open HigherBoundarySupport public

data AlgebraicNotStructuralTrace {ℓ : Level} :
  AlgebraicPayloadField ℓ → Type (ℓ-suc ℓ) where
  algebraic-payload-field :
    (a : AlgebraicPayloadField ℓ) →
    AlgebraicNotStructuralTrace a

record AlgebraicFieldClassification {ℓ : Level}
  (a : AlgebraicPayloadField ℓ) : Type (ℓ-suc ℓ) where
  constructor mkAlgebraicFieldClassification
  field
    payloadCarrier      : Type ℓ
    carrierMatches      : payloadCarrier ≡ algebraicCarrier a
    notStructuralTrace  : AlgebraicNotStructuralTrace a

open AlgebraicFieldClassification public

data HigherFaceDisposition (ℓ : Level) : Type (ℓ-suc ℓ) where
  rejected-as-structural :
    HigherFaceDisposition ℓ
  packaged-as-horn :
    (b : RawBoundary ℓ) →
    PackagedHornBoundary b →
    HigherFaceDisposition ℓ

record PayloadWellTyped
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (ℓ-suc ℓ) where
  constructor mkPayloadWellTyped
  field
    payloadFieldTyped :
      (i : Fin (Metatheory.RawStructuralSyntax.RawTelescope.fieldCount
             (raw-extension-payload-fields e))) →
      PayloadTypingWitness

open PayloadWellTyped public

record StructuralClausesWellTyped
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (ℓ-suc ℓ) where
  constructor mkStructuralClausesWellTyped
  field
    structuralClauseTyped :
      (i : Fin (Metatheory.RawStructuralSyntax.RawTelescope.fieldCount
             (raw-extension-structural-clauses e))) →
      TypedStructuralRole
        (Metatheory.RawStructuralSyntax.RawTelescope.fieldAt
          (raw-extension-structural-clauses e) i)

open StructuralClausesWellTyped public

record AlgebraicWellTyped
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (ℓ-suc ℓ) where
  constructor mkAlgebraicWellTyped
  field
    algebraicFieldTyped :
      (i : Fin (Metatheory.RawStructuralSyntax.RawTelescope.fieldCount
             (raw-extension-algebraic-fields e))) →
      AlgebraicFieldClassification
        (Metatheory.RawStructuralSyntax.RawTelescope.fieldAt
          (raw-extension-algebraic-fields e) i)

open AlgebraicWellTyped public

record SealingDerivation
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (ℓ-suc ℓ) where
  constructor mkSealingDerivation
  field
    payloadSealedWithTrace :
      Type ℓ

open SealingDerivation public

record OpacityRespected
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (ℓ-suc ℓ) where
  constructor mkOpacityRespected
  field
    hiddenPayloadNotExportedAsTrace :
      Type ℓ

open OpacityRespected public

record ExportPolicySound
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (ℓ-suc ℓ) where
  constructor mkExportPolicySound
  field
    exportsOnlySealedPayloadAndTrace :
      Type ℓ

open ExportPolicySound public

record AdmissibleRawExtension
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (ℓ-suc ℓ) where
  constructor mkAdmissibleRawExtension
  field
    payloadWellTyped    : PayloadWellTyped B e
    structuralWellTyped : StructuralClausesWellTyped B e
    algebraicWellTyped  : AlgebraicWellTyped B e
    sealingDerivation   : SealingDerivation B e
    opacityRespected    : OpacityRespected B e
    exportPolicySound   : ExportPolicySound B e

open AdmissibleRawExtension public

act-clause-has-unary-support :
  {ℓ : Level} →
  (p : NewPayloadRef) →
  (s : BasisSite) →
  UnaryTraceSupport (act {ℓ = ℓ} p s)
act-clause-has-unary-support p s =
  mkUnaryTraceSupport p s refl

cmp-clause-has-binary-support :
  {ℓ : Level} →
  (p : NewPayloadRef) →
  (left right : BasisSite) →
  BinaryTraceSupport (cmp {ℓ = ℓ} p left right)
cmp-clause-has-binary-support p left right =
  mkBinaryTraceSupport p left right refl

horn-clause-has-higher-boundary-support :
  {ℓ : Level} →
  (b : RawBoundary ℓ) →
  PackagedHornBoundary b →
  HigherBoundarySupport (horn b)
horn-clause-has-higher-boundary-support b package =
  mkHigherBoundarySupport b package refl

algebraic-field-is-payload-not-structural-trace :
  {ℓ : Level} →
  (a : AlgebraicPayloadField ℓ) →
  AlgebraicFieldClassification a
algebraic-field-is-payload-not-structural-trace a =
  mkAlgebraicFieldClassification
    (algebraicCarrier a)
    refl
    (algebraic-payload-field a)

naked-higher-face-rejected-or-packaged :
  {ℓ : Level} →
  {c : RawStructuralClause ℓ} →
  TypedStructuralRole c →
  HigherFaceDisposition ℓ
naked-higher-face-rejected-or-packaged (unary-action-role p s) =
  rejected-as-structural
naked-higher-face-rejected-or-packaged (binary-comparison-role p left right) =
  rejected-as-structural
naked-higher-face-rejected-or-packaged (horn-boundary-role b package) =
  packaged-as-horn b package
