{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.Refactoring where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations using (Fin; HistoricalSupport)

private
  variable
    ℓ ℓ' ℓN ℓP ℓO : Level
    k : Nat

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

inverseIso :
  {ℓA ℓB : Level}
  {A : Type ℓA} {B : Type ℓB} →
  Iso A B → Iso B A
inverseIso α = record
  { fun = Iso.inv α
  ; inv = Iso.fun α
  ; rightInv = Iso.leftInv α
  ; leftInv = Iso.rightInv α
  }

-- A canonical telescope / counting normal form for atomic payload schemas.
record PayloadNormalForm (ℓ : Level) : Type (ℓ-suc ℓ) where
  constructor mkPayloadNormalForm
  field
    AtomicPayloadSchema       : Type ℓ
    payloadCount              : Nat
    payloadCountingNormalForm :
      Iso AtomicPayloadSchema (Fin payloadCount)

open PayloadNormalForm public

-- A concrete presentation whose source telescope normalizes to the same
-- canonical payload schema set.
record PayloadPresentation {ℓN ℓP : Level}
  (N : PayloadNormalForm ℓN) : Type (ℓ-suc (ℓ-max ℓN ℓP)) where
  constructor mkPayloadPresentation
  field
    PresentedPayload : Type ℓP
    normalizePayload :
      Iso PresentedPayload (AtomicPayloadSchema N)

open PayloadPresentation public

atomic-payload-bijection :
  {ℓP ℓP' : Level} →
  {N : PayloadNormalForm ℓN} →
  (P : PayloadPresentation {ℓP = ℓP} N) →
  (Q : PayloadPresentation {ℓP = ℓP'} N) →
  Iso (PresentedPayload P) (PresentedPayload Q)
atomic-payload-bijection P Q =
  composeIso (normalizePayload P) (inverseIso (normalizePayload Q))

payload-counting-normal-form :
  {ℓP : Level} →
  {N : PayloadNormalForm ℓN} →
  (P : PayloadPresentation {ℓP = ℓP} N) →
  Iso (PresentedPayload P) (Fin (payloadCount N))
payload-counting-normal-form {N = N} P =
  composeIso (normalizePayload P) (payloadCountingNormalForm N)

kappa :
  {ℓP : Level} →
  {N : PayloadNormalForm ℓN} →
  PayloadPresentation {ℓP = ℓP} N → Nat
kappa {N = N} P = payloadCount N

kappa-invariant-under-refactoring :
  {ℓP ℓP' : Level} →
  {N : PayloadNormalForm ℓN} →
  (P : PayloadPresentation {ℓP = ℓP} N) →
  (Q : PayloadPresentation {ℓP = ℓP'} N) →
  kappa P ≡ kappa Q
kappa-invariant-under-refactoring P Q = refl

-- A canonical normal form for atomic obligations together with their
-- normalized historical supports.
record ObligationNormalForm (ℓ : Level) (k : Nat) : Type (ℓ-suc ℓ) where
  constructor mkObligationNormalForm
  field
    AtomicObligationSchema       : Type ℓ
    normalizedSupport            :
      AtomicObligationSchema → HistoricalSupport k
    obligationCount              : Nat
    obligationCountingNormalForm :
      Iso AtomicObligationSchema (Fin obligationCount)

open ObligationNormalForm public

-- Two source presentations of the same sealing judgment are related when they
-- both normalize to the same obligation normal form and support map.
record ObligationPresentation {ℓN ℓO : Level} {k : Nat}
  (N : ObligationNormalForm ℓN k) : Type (ℓ-suc (ℓ-max ℓN ℓO)) where
  constructor mkObligationPresentation
  field
    PresentedObligation : Type ℓO
    presentedSupport    :
      PresentedObligation → HistoricalSupport k
    normalizeObligation :
      Iso PresentedObligation (AtomicObligationSchema N)
    supportRespectsNormalization :
      (o : PresentedObligation) →
      presentedSupport o
        ≡ normalizedSupport N (Iso.fun normalizeObligation o)

open ObligationPresentation public

atomic-obligation-bijection :
  {ℓO ℓO' : Level} →
  {N : ObligationNormalForm ℓN k} →
  (P : ObligationPresentation {ℓO = ℓO} N) →
  (Q : ObligationPresentation {ℓO = ℓO'} N) →
  Iso (PresentedObligation P) (PresentedObligation Q)
atomic-obligation-bijection P Q =
  composeIso (normalizeObligation P) (inverseIso (normalizeObligation Q))

obligation-counting-normal-form :
  {ℓO : Level} →
  {N : ObligationNormalForm ℓN k} →
  (P : ObligationPresentation {ℓO = ℓO} N) →
  Iso (PresentedObligation P) (Fin (obligationCount N))
obligation-counting-normal-form {N = N} P =
  composeIso (normalizeObligation P) (obligationCountingNormalForm N)

Delta :
  {ℓO : Level} →
  {N : ObligationNormalForm ℓN k} →
  ObligationPresentation {ℓO = ℓO} N → Nat
Delta {N = N} P = obligationCount N

Delta-invariant-under-refactoring :
  {ℓO ℓO' : Level} →
  {N : ObligationNormalForm ℓN k} →
  (P : ObligationPresentation {ℓO = ℓO} N) →
  (Q : ObligationPresentation {ℓO = ℓO'} N) →
  Delta P ≡ Delta Q
Delta-invariant-under-refactoring P Q = refl

historicalArity :
  {ℓO : Level} →
  {N : ObligationNormalForm ℓN k} →
  (P : ObligationPresentation {ℓO = ℓO} N) →
  PresentedObligation P → Nat
historicalArity P o = HistoricalSupport.arity (presentedSupport P o)

normalization-commutes :
  {ℓO ℓO' : Level} →
  {N : ObligationNormalForm ℓN k} →
  (P : ObligationPresentation {ℓO = ℓO} N) →
  (Q : ObligationPresentation {ℓO = ℓO'} N) →
  (o : PresentedObligation P) →
  Iso.fun (normalizeObligation Q)
    (Iso.fun (atomic-obligation-bijection P Q) o)
    ≡ Iso.fun (normalizeObligation P) o
normalization-commutes P Q o =
  Iso.rightInv (normalizeObligation Q)
    (Iso.fun (normalizeObligation P) o)

historical-support-correspondence :
  {ℓO ℓO' : Level} →
  {N : ObligationNormalForm ℓN k} →
  (P : ObligationPresentation {ℓO = ℓO} N) →
  (Q : ObligationPresentation {ℓO = ℓO'} N) →
  (o : PresentedObligation P) →
  presentedSupport Q
    (Iso.fun (atomic-obligation-bijection P Q) o)
    ≡ presentedSupport P o
historical-support-correspondence {N = N} P Q o =
  presentedSupport Q
    (Iso.fun (atomic-obligation-bijection P Q) o)
    ≡⟨ supportRespectsNormalization Q
         (Iso.fun (atomic-obligation-bijection P Q) o) ⟩
  normalizedSupport N
    (Iso.fun (normalizeObligation Q)
      (Iso.fun (atomic-obligation-bijection P Q) o))
    ≡⟨ cong (normalizedSupport N) (normalization-commutes P Q o) ⟩
  normalizedSupport N (Iso.fun (normalizeObligation P) o)
    ≡⟨ sym (supportRespectsNormalization P o) ⟩
  presentedSupport P o ∎

historical-arity-invariant-under-refactoring :
  {ℓO ℓO' : Level} →
  {N : ObligationNormalForm ℓN k} →
  (P : ObligationPresentation {ℓO = ℓO} N) →
  (Q : ObligationPresentation {ℓO = ℓO'} N) →
  (o : PresentedObligation P) →
  historicalArity Q (Iso.fun (atomic-obligation-bijection P Q) o)
    ≡ historicalArity P o
historical-arity-invariant-under-refactoring P Q o =
  cong HistoricalSupport.arity
    (historical-support-correspondence P Q o)

record RefactoringInvariance
  {ℓPN ℓON ℓP₁ ℓP₂ ℓO₁ ℓO₂ : Level}
  {k : Nat}
  {payloadNF : PayloadNormalForm ℓPN}
  {obligationNF : ObligationNormalForm ℓON k}
  (payload₁ : PayloadPresentation {ℓN = ℓPN} {ℓP = ℓP₁} payloadNF)
  (payload₂ : PayloadPresentation {ℓN = ℓPN} {ℓP = ℓP₂} payloadNF)
  (obligation₁ : ObligationPresentation {ℓN = ℓON} {ℓO = ℓO₁} obligationNF)
  (obligation₂ : ObligationPresentation {ℓN = ℓON} {ℓO = ℓO₂} obligationNF)
  : Type (ℓ-suc (ℓ-max (ℓ-max ℓPN ℓON)
                        (ℓ-max (ℓ-max ℓP₁ ℓP₂) (ℓ-max ℓO₁ ℓO₂)))) where
  field
    atomic-payloads-correspond :
      Iso (PresentedPayload payload₁) (PresentedPayload payload₂)
    atomic-obligations-correspond :
      Iso (PresentedObligation obligation₁)
          (PresentedObligation obligation₂)
    induced-historical-supports-correspond :
      (o : PresentedObligation obligation₁) →
      presentedSupport obligation₂
        (Iso.fun atomic-obligations-correspond o)
        ≡ presentedSupport obligation₁ o
    kappa-invariant :
      kappa payload₁ ≡ kappa payload₂
    Delta-invariant :
      Delta obligation₁ ≡ Delta obligation₂
    historical-arity-invariant :
      (o : PresentedObligation obligation₁) →
      historicalArity obligation₂
        (Iso.fun atomic-obligations-correspond o)
        ≡ historicalArity obligation₁ o

open RefactoringInvariance public

refactoring-invariance :
  {ℓPN ℓON ℓP₁ ℓP₂ ℓO₁ ℓO₂ : Level}
  {k : Nat}
  {payloadNF : PayloadNormalForm ℓPN}
  {obligationNF : ObligationNormalForm ℓON k}
  (payload₁ : PayloadPresentation {ℓN = ℓPN} {ℓP = ℓP₁} payloadNF)
  (payload₂ : PayloadPresentation {ℓN = ℓPN} {ℓP = ℓP₂} payloadNF)
  (obligation₁ : ObligationPresentation {ℓN = ℓON} {ℓO = ℓO₁} obligationNF)
  (obligation₂ : ObligationPresentation {ℓN = ℓON} {ℓO = ℓO₂} obligationNF) →
  RefactoringInvariance payload₁ payload₂ obligation₁ obligation₂
refactoring-invariance payload₁ payload₂ obligation₁ obligation₂ = record
  { atomic-payloads-correspond =
      atomic-payload-bijection payload₁ payload₂
  ; atomic-obligations-correspond =
      atomic-obligation-bijection obligation₁ obligation₂
  ; induced-historical-supports-correspond =
      historical-support-correspondence obligation₁ obligation₂
  ; kappa-invariant =
      kappa-invariant-under-refactoring payload₁ payload₂
  ; Delta-invariant =
      Delta-invariant-under-refactoring obligation₁ obligation₂
  ; historical-arity-invariant =
      historical-arity-invariant-under-refactoring obligation₁ obligation₂
  }
