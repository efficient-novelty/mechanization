{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.ComputationalReplacement where

open import Agda.Primitive using (lsuc; _⊔_)
open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations
  using ( Not
        ; HistoricalSupport
        ; PrimitiveCost
        ; derived
        ; requiresPrimitive
        ; requiresPrimitive≠derived
        ; ObligationLanguage
        ; PrimitiveEliminatesAbove
        )

private
  variable
    lC lO lN lP : Level

idIso : {A : Type lC} → Iso A A
idIso = record
  { fun = λ x → x
  ; inv = λ x → x
  ; rightInv = λ x → refl
  ; leftInv = λ x → refl
  }

composeIso :
  {A : Type lC} {B : Type lO} {C : Type lN} →
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

inverseIso :
  {A : Type lC} {B : Type lO} →
  Iso A B → Iso B A
inverseIso α = record
  { fun = Iso.inv α
  ; inv = Iso.fun α
  ; rightInv = Iso.leftInv α
  ; leftInv = Iso.rightInv α
  }

record CanonicalTraceSignature
  {lC lO lN : Level}
  (L : ObligationLanguage lC lO)
  (X : ObligationLanguage.Candidate L)
  (k : Nat) : Type (lsuc ((lN ⊔ lC) ⊔ lO)) where
  open ObligationLanguage L renaming
    ( O to OL
    ; Supp to SuppL
    ; primitiveCost to primitiveCostL
    )

  field
    AtomicTraceField : Type lN
    normalizeTraceField : Iso AtomicTraceField (OL X k)

  normalizedSupport : AtomicTraceField → HistoricalSupport k
  normalizedSupport φ =
    SuppL (Iso.fun normalizeTraceField φ)

  normalizedPrimitiveCost : AtomicTraceField → PrimitiveCost
  normalizedPrimitiveCost φ =
    primitiveCostL (Iso.fun normalizeTraceField φ)

open CanonicalTraceSignature public

record TracePresentation
  {lC lO lN lP : Level}
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) :
  Type (lsuc (((lC ⊔ lO) ⊔ lN) ⊔ lP)) where
  field
    PresentedTraceField : Type lP
    normalizePresentedTraceField :
      Iso PresentedTraceField (AtomicTraceField S)

open TracePresentation public

presentedHistoricalSupport :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  (P : TracePresentation {lP = lP} S) →
  PresentedTraceField P → HistoricalSupport k
presentedHistoricalSupport S P φ =
  normalizedSupport S (Iso.fun (normalizePresentedTraceField P) φ)

presentedPrimitiveCost :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  (P : TracePresentation {lP = lP} S) →
  PresentedTraceField P → PrimitiveCost
presentedPrimitiveCost S P φ =
  normalizedPrimitiveCost S (Iso.fun (normalizePresentedTraceField P) φ)

presentation-equivalence :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  {S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k} →
  (P : TracePresentation {lP = lP} S) →
  (Q : TracePresentation {lP = lP} S) →
  Iso (PresentedTraceField P) (PresentedTraceField Q)
presentation-equivalence P Q =
  composeIso
    (normalizePresentedTraceField P)
    (inverseIso (normalizePresentedTraceField Q))

normalization-commutes :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  {S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k} →
  (P : TracePresentation {lP = lP} S) →
  (Q : TracePresentation {lP = lP} S) →
  (φ : PresentedTraceField P) →
  Iso.fun (normalizePresentedTraceField Q)
    (Iso.fun (presentation-equivalence P Q) φ)
    ≡ Iso.fun (normalizePresentedTraceField P) φ
normalization-commutes P Q φ =
  Iso.rightInv (normalizePresentedTraceField Q)
    (Iso.fun (normalizePresentedTraceField P) φ)

presented-historical-support-correspondence :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  (P : TracePresentation {lP = lP} S) →
  (Q : TracePresentation {lP = lP} S) →
  (φ : PresentedTraceField P) →
  presentedHistoricalSupport S Q
    (Iso.fun (presentation-equivalence P Q) φ)
    ≡ presentedHistoricalSupport S P φ
presented-historical-support-correspondence S P Q φ =
  cong (normalizedSupport S) (normalization-commutes P Q φ)

presented-primitive-cost-correspondence :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  (P : TracePresentation {lP = lP} S) →
  (Q : TracePresentation {lP = lP} S) →
  (φ : PresentedTraceField P) →
  presentedPrimitiveCost S Q
    (Iso.fun (presentation-equivalence P Q) φ)
    ≡ presentedPrimitiveCost S P φ
presented-primitive-cost-correspondence S P Q φ =
  cong (normalizedPrimitiveCost S) (normalization-commutes P Q φ)

InCanonicalMinimalSignature :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat} →
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  AtomicTraceField S → Type _
InCanonicalMinimalSignature S φ =
  normalizedPrimitiveCost S φ ≡ requiresPrimitive

InPresentedMinimalSignature :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  (P : TracePresentation {lP = lP} S) →
  PresentedTraceField P → Type _
InPresentedMinimalSignature S P φ =
  presentedPrimitiveCost S P φ ≡ requiresPrimitive

derived-field-does-not-survive-minimal-signature :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat} →
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  (φ : AtomicTraceField S) →
  normalizedPrimitiveCost S φ ≡ derived →
  Not (InCanonicalMinimalSignature S φ)
derived-field-does-not-survive-minimal-signature S φ derivedWitness primitiveWitness =
  requiresPrimitive≠derived (sym primitiveWitness ∙ derivedWitness)

presented-derived-field-does-not-survive-minimal-signature :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  {S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k} →
  (P : TracePresentation {lP = lP} S) →
  (φ : PresentedTraceField P) →
  presentedPrimitiveCost S P φ ≡ derived →
  Not (InPresentedMinimalSignature S P φ)
presented-derived-field-does-not-survive-minimal-signature P φ derivedWitness primitiveWitness =
  requiresPrimitive≠derived (sym primitiveWitness ∙ derivedWitness)

record ComputationalReplacementResult
  {lC lO lN lP : Level}
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k)
  (P : TracePresentation {lP = lP} S)
  (Q : TracePresentation {lP = lP} S)
  (φ : PresentedTraceField P)
  : Type (lsuc (((lC ⊔ lO) ⊔ lN) ⊔ lP)) where
  field
    equivalent-presentation :
      Iso (PresentedTraceField P) (PresentedTraceField Q)
    replaced-field-derived :
      presentedPrimitiveCost S P φ ≡ derived
    replaced-field-eliminated :
      Not (InPresentedMinimalSignature S P φ)
    irreducible-fields-correspond :
      (ψ : PresentedTraceField P) →
      InPresentedMinimalSignature S P ψ →
      InPresentedMinimalSignature S Q
        (Iso.fun equivalent-presentation ψ)

open ComputationalReplacementResult public

computational-replacement-preserves-canonical-presentation :
  {L : ObligationLanguage lC lO}
  {X : ObligationLanguage.Candidate L}
  {k : Nat}
  (S : CanonicalTraceSignature {lC = lC} {lO = lO} {lN = lN} L X k) →
  (P : TracePresentation {lP = lP} S) →
  (Q : TracePresentation {lP = lP} S) →
  (φ : PresentedTraceField P) →
  presentedPrimitiveCost S P φ ≡ derived →
  ComputationalReplacementResult S P Q φ
computational-replacement-preserves-canonical-presentation S P Q φ derivedWitness = record
  { equivalent-presentation = presentation-equivalence P Q
  ; replaced-field-derived = derivedWitness
  ; replaced-field-eliminated =
      presented-derived-field-does-not-survive-minimal-signature
        P φ derivedWitness
  ; irreducible-fields-correspond = λ ψ primitiveWitness →
      presented-primitive-cost-correspondence S P Q ψ
      ∙ primitiveWitness
  }

module _ {lC lO : Level} {L : ObligationLanguage lC lO} where

  open ObligationLanguage L renaming
    ( Candidate to CandidateL
    ; O to OL
    ; primitiveCost to primitiveCostL
    )

  canonical-obligation-signature :
    (X : CandidateL) →
    (k : Nat) →
    CanonicalTraceSignature L X k
  canonical-obligation-signature X k = record
    { AtomicTraceField = OL X k
    ; normalizeTraceField = idIso
    }

  higher-arity-fields-become-derived :
    {d : Nat} →
    (elimination : PrimitiveEliminatesAbove L d) →
    (X : CandidateL) →
    (offset : Nat) →
    (φ : AtomicTraceField (canonical-obligation-signature X (d + suc offset))) →
    normalizedPrimitiveCost
      (canonical-obligation-signature X (d + suc offset)) φ
      ≡ derived
  higher-arity-fields-become-derived {d = d} elimination X offset φ =
    PrimitiveEliminatesAbove.eliminate elimination X offset φ

  higher-arity-fields-disappear-from-minimal-signature :
    {d : Nat} →
    (elimination : PrimitiveEliminatesAbove L d) →
    (X : CandidateL) →
    (offset : Nat) →
    (φ : AtomicTraceField (canonical-obligation-signature X (d + suc offset))) →
    Not
      (InCanonicalMinimalSignature
        (canonical-obligation-signature X (d + suc offset)) φ)
  higher-arity-fields-disappear-from-minimal-signature
    {d = d} elimination X offset φ =
    derived-field-does-not-survive-minimal-signature
      (canonical-obligation-signature X (d + suc offset))
      φ
      (higher-arity-fields-become-derived elimination X offset φ)

  higher-arity-presented-fields-disappear-from-minimal-signature :
    {lP : Level} →
    {d : Nat} →
    (elimination : PrimitiveEliminatesAbove L d) →
    (X : CandidateL) →
    (offset : Nat) →
    (P : TracePresentation {lP = lP}
           (canonical-obligation-signature X (d + suc offset))) →
    (φ : PresentedTraceField P) →
    Not
      (InPresentedMinimalSignature
        (canonical-obligation-signature X (d + suc offset)) P φ)
  higher-arity-presented-fields-disappear-from-minimal-signature
    {d = d} elimination X offset P φ =
    presented-derived-field-does-not-survive-minimal-signature
      P
      φ
      (higher-arity-fields-become-derived elimination X offset
        (Iso.fun (normalizePresentedTraceField P) φ))

  higher-arity-computational-replacement :
    {lP : Level} →
    {d : Nat} →
    (elimination : PrimitiveEliminatesAbove L d) →
    (X : CandidateL) →
    (offset : Nat) →
    (P Q : TracePresentation {lP = lP}
             (canonical-obligation-signature X (d + suc offset))) →
    (φ : PresentedTraceField P) →
    ComputationalReplacementResult
      (canonical-obligation-signature X (d + suc offset)) P Q φ
  higher-arity-computational-replacement
    {d = d} elimination X offset P Q φ =
    computational-replacement-preserves-canonical-presentation
      (canonical-obligation-signature X (d + suc offset))
      P Q φ
      (higher-arity-fields-become-derived elimination X offset
        (Iso.fun (normalizePresentedTraceField P) φ))
