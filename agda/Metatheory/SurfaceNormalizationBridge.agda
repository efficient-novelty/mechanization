{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.SurfaceNormalizationBridge where

open import Agda.Primitive using (lsuc)
open import Cubical.Foundations.Prelude
open import Cubical.Data.Sum.Base using (_⊎_; inl; inr)

open import Core.Nat renaming (ℕ to Nat)
open import Core.Sequence using ([]; _∷_)
open import Metatheory.Obligations
  using ( Fin
        ; fzero
        ; fsuc
        ; HistoricalSupport
        ; mkSupport
        ; PrimitiveCost
        ; derived
        ; requiresPrimitive
        )
open import Metatheory.CanonicalTelescope
open import Metatheory.TraceCostNormalForm
open import Metatheory.PresentationEquivalence
  using (PresentationEquivalent)
open import Metatheory.MuInvariance
  using (mu-invariant-under-presentation-equivalence)
open import Metatheory.InterfaceCalculus
  using (LibraryState; ActiveInterface)
open import Metatheory.TracePrinciple
  using (splitFinSum)
open import Metatheory.RawStructuralSyntax
open import Metatheory.RawStructuralTyping

private
  variable
    ℓ : Level
    B : LibraryState ℓ

twoLayerDepth : Nat
twoLayerDepth = suc (suc zero)

unaryNormalizedSupport : HistoricalSupport twoLayerDepth
unaryNormalizedSupport =
  mkSupport (suc zero) (fzero ∷ [])

binaryNormalizedSupport : HistoricalSupport twoLayerDepth
binaryNormalizedSupport =
  mkSupport (suc (suc zero)) (fzero ∷ fsuc fzero ∷ [])

derivedHornNormalizedSupport : HistoricalSupport twoLayerDepth
derivedHornNormalizedSupport =
  mkSupport zero []

record CanonicalNormalizedSignature (ℓ : Level) :
  Type (lsuc (lsuc ℓ)) where
  constructor mkCanonicalNormalizedSignature
  field
    payloadFields : CanonicalTelescope ℓ
    traceFields   : CanonicalTraceCostNormalForm (lsuc ℓ) twoLayerDepth

open CanonicalNormalizedSignature public

record RawExtensionCandidate
  (B : LibraryState ℓ) (e : RawExtension ℓ) : Type (lsuc ℓ) where
  constructor mkRawExtensionCandidate
  field
    activeInterface : Type ℓ
    admissible      : AdmissibleRawExtension B e

open RawExtensionCandidate public

payload-plus-algebraic-count : RawExtension ℓ → Nat
payload-plus-algebraic-count e =
  RawTelescope.fieldCount (raw-extension-payload-fields e) +
  RawTelescope.fieldCount (raw-extension-algebraic-fields e)

normalizedPayloadCarrier :
  (e : RawExtension ℓ) →
  Fin (payload-plus-algebraic-count e) →
  Type ℓ
normalizedPayloadCarrier e i
  with splitFinSum
    (RawTelescope.fieldCount (raw-extension-payload-fields e))
    (RawTelescope.fieldCount (raw-extension-algebraic-fields e))
    i
... | inl p =
  payloadCarrier
    (RawTelescope.fieldAt (raw-extension-payload-fields e) p)
... | inr a =
  algebraicCarrier
    (RawTelescope.fieldAt (raw-extension-algebraic-fields e) a)

normalizedPayloadTelescope :
  RawExtension ℓ →
  CanonicalTelescope ℓ
normalizedPayloadTelescope e =
  mkCanonicalTelescope
    (payload-plus-algebraic-count e)
    (normalizedPayloadCarrier e)

normalizedTraceTelescope :
  (e : RawExtension ℓ) →
  AdmissibleRawExtension B e →
  CanonicalTelescope (lsuc ℓ)
normalizedTraceTelescope e admissible =
  mkCanonicalTelescope
    (RawTelescope.fieldCount (raw-extension-structural-clauses e))
    (λ i →
      TypedStructuralRole
        (RawTelescope.fieldAt (raw-extension-structural-clauses e) i))

supportForRole :
  {c : RawStructuralClause ℓ} →
  TypedStructuralRole c →
  HistoricalSupport twoLayerDepth
supportForRole (unary-action-role p s) =
  unaryNormalizedSupport
supportForRole (binary-comparison-role p left right) =
  binaryNormalizedSupport
supportForRole (horn-boundary-role b package) =
  derivedHornNormalizedSupport

primitiveCostForRole :
  {c : RawStructuralClause ℓ} →
  TypedStructuralRole c →
  PrimitiveCost
primitiveCostForRole (unary-action-role p s) =
  requiresPrimitive
primitiveCostForRole (binary-comparison-role p left right) =
  requiresPrimitive
primitiveCostForRole (horn-boundary-role b package) =
  derived

traceCostFieldForRole :
  {c : RawStructuralClause ℓ} →
  TypedStructuralRole c →
  TraceCostField twoLayerDepth
traceCostFieldForRole role =
  mkTraceCostField
    (supportForRole role)
    (HistoricalSupport.arity (supportForRole role))
    refl
    (primitiveCostForRole role)

normalizedTraceField :
  (e : RawExtension ℓ) →
  (admissible : AdmissibleRawExtension B e) →
  FieldIndex (normalizedTraceTelescope e admissible) →
  TraceCostField twoLayerDepth
normalizedTraceField e admissible i =
  traceCostFieldForRole
    (StructuralClausesWellTyped.structuralClauseTyped
      (AdmissibleRawExtension.structuralWellTyped admissible)
      i)

normalizedTraceNormalForm :
  (e : RawExtension ℓ) →
  AdmissibleRawExtension B e →
  CanonicalTraceCostNormalForm (lsuc ℓ) twoLayerDepth
normalizedTraceNormalForm e admissible =
  mkCanonicalTraceCostNormalForm
    (normalizedTraceTelescope e admissible)
    (normalizedTraceField e admissible)
    zero
    (λ ())
    (λ ())
    zero
    (λ ())
    (λ ())

normalizeRawExtension :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  AdmissibleRawExtension B e →
  CanonicalNormalizedSignature ℓ
normalizeRawExtension B e admissible =
  mkCanonicalNormalizedSignature
    (normalizedPayloadTelescope e)
    (normalizedTraceNormalForm e admissible)

raw-extension-elaborates-to-candidate :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  AdmissibleRawExtension B e →
  RawExtensionCandidate B e
raw-extension-elaborates-to-candidate B e admissible =
  mkRawExtensionCandidate (ActiveInterface B) admissible

raw-extension-normalizes-to-canonical-signature :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  AdmissibleRawExtension B e →
  CanonicalNormalizedSignature ℓ
raw-extension-normalizes-to-canonical-signature =
  normalizeRawExtension

raw-trace-normalizes-to-canonical-signature :
  {c : RawStructuralClause ℓ} →
  TypedStructuralRole c →
  TraceCostField twoLayerDepth
raw-trace-normalizes-to-canonical-signature =
  traceCostFieldForRole

normalize-preserves-support :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  (admissible : AdmissibleRawExtension B e) →
  (i : FieldIndex
       (traceTelescope
         (traceFields (normalizeRawExtension B e admissible)))) →
  support
    (traceField (traceFields (normalizeRawExtension B e admissible)) i)
  ≡
  support
    (normalizedTraceField e admissible i)
normalize-preserves-support B e admissible i = refl

normalize-preserves-arity :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  (admissible : AdmissibleRawExtension B e) →
  (i : FieldIndex
       (traceTelescope
         (traceFields (normalizeRawExtension B e admissible)))) →
  arity
    (traceField (traceFields (normalizeRawExtension B e admissible)) i)
  ≡
  HistoricalSupport.arity
    (support
      (traceField (traceFields (normalizeRawExtension B e admissible)) i))
normalize-preserves-arity B e admissible i =
  arityMatchesSupport
    (traceField (traceFields (normalizeRawExtension B e admissible)) i)

normalize-preserves-primitive-cost :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  (admissible : AdmissibleRawExtension B e) →
  (i : FieldIndex
       (traceTelescope
         (traceFields (normalizeRawExtension B e admissible)))) →
  primitiveCost
    (traceField (traceFields (normalizeRawExtension B e admissible)) i)
  ≡
  primitiveCost
    (normalizedTraceField e admissible i)
normalize-preserves-primitive-cost B e admissible i = refl

normalization-respects-presentation-equivalence :
  {Γ Δ : CanonicalTraceCostNormalForm (lsuc ℓ) twoLayerDepth} →
  PresentationEquivalent Γ Δ →
  mu-of-trace-cost-normal-form Γ ≡ mu-of-trace-cost-normal-form Δ
normalization-respects-presentation-equivalence =
  mu-invariant-under-presentation-equivalence

record NormalizedSignatureMatchesCountedInterface
  (B : LibraryState ℓ)
  (e : RawExtension ℓ)
  (admissible : AdmissibleRawExtension B e) :
  Type where
  constructor mkNormalizedSignatureMatchesCountedInterface
  field
    payloadCountMatches :
      canonical-telescope-cardinality
        (payloadFields (normalizeRawExtension B e admissible))
      ≡
      payload-plus-algebraic-count e
    traceCountMatches :
      trace-cost-normal-form-cardinality
        (traceFields (normalizeRawExtension B e admissible))
      ≡
      RawTelescope.fieldCount (raw-extension-structural-clauses e)

open NormalizedSignatureMatchesCountedInterface public

normalized-signature-matches-counted-interface :
  (B : LibraryState ℓ) →
  (e : RawExtension ℓ) →
  (admissible : AdmissibleRawExtension B e) →
  NormalizedSignatureMatchesCountedInterface B e admissible
normalized-signature-matches-counted-interface B e admissible =
  mkNormalizedSignatureMatchesCountedInterface refl refl
