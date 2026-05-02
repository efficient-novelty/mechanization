{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.RawStructuralSyntax where

open import Cubical.Foundations.Prelude

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin; HistoricalSupport)

private
  variable
    ℓ : Level

-- Raw references used by the fixed extension calculus.  These are not an
-- encoding of arbitrary Agda names; they are the small address language used
-- by the paper's surface bridge.
record LayerRef : Type where
  constructor mkLayerRef
  field
    layerDistance : Nat

open LayerRef public

record NewPayloadRef : Type where
  constructor mkNewPayloadRef
  field
    payloadIndex : Nat

open NewPayloadRef public

record BasisSite : Type where
  constructor mkBasisSite
  field
    basisLayer : LayerRef
    basisIndex : Nat

open BasisSite public

data ExportDecision : Type where
  exported hidden transparentAlias : ExportDecision

record ExportPolicy : Type where
  constructor mkExportPolicy
  field
    payloadExport    : ExportDecision
    traceExport      : ExportDecision
    algebraicExport  : ExportDecision

open ExportPolicy public

record PayloadField (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkPayloadField
  field
    payloadName    : Nat
    payloadCarrier : Type ℓ

open PayloadField public

data AlgebraicOperationKind : Type where
  ordinaryPayloadOperation higherUserOperation : AlgebraicOperationKind

record AlgebraicPayloadField (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkAlgebraicPayloadField
  field
    algebraicName    : Nat
    algebraicCarrier : Type ℓ
    algebraicArity   : Nat
    operationKind    : AlgebraicOperationKind

open AlgebraicPayloadField public

record RawBoundary (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkRawBoundary
  field
    historyDepth    : Nat
    boundarySupport : HistoricalSupport historyDepth
    boundaryCarrier : Type ℓ
    fillerCarrier   : Type ℓ

open RawBoundary public

data RawStructuralClause (ℓ : Level) : Type (ℓ-suc ℓ) where
  act  : NewPayloadRef → BasisSite → RawStructuralClause ℓ
  cmp  : NewPayloadRef → BasisSite → BasisSite → RawStructuralClause ℓ
  horn : RawBoundary ℓ → RawStructuralClause ℓ

record RawTelescope {ℓ : Level} (A : Type ℓ) : Type ℓ where
  constructor mkRawTelescope
  field
    fieldCount : Nat
    fieldAt    : Fin fieldCount → A

open RawTelescope public

record RawExtension (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkRawExtension
  field
    payload      : RawTelescope (PayloadField ℓ)
    structural   : RawTelescope (RawStructuralClause ℓ)
    algebraic    : RawTelescope (AlgebraicPayloadField ℓ)
    exportPolicy : ExportPolicy

open RawExtension public

raw-extension-payload-fields :
  RawExtension ℓ → RawTelescope (PayloadField ℓ)
raw-extension-payload-fields e = payload e

raw-extension-structural-clauses :
  RawExtension ℓ → RawTelescope (RawStructuralClause ℓ)
raw-extension-structural-clauses e = structural e

raw-extension-algebraic-fields :
  RawExtension ℓ → RawTelescope (AlgebraicPayloadField ℓ)
raw-extension-algebraic-fields e = algebraic e
