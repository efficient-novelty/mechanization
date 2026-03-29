{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.Obligations where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)
open import Cubical.Data.Empty.Base using (⊥)

open import Core.Nat
open import Core.Sequence using (Vec; []; _∷_)

private
  variable
    ℓ ℓ' ℓC ℓO : Level

infix 4 _≤_ _<_

Not : ∀ {ℓ} → Type ℓ → Type ℓ
Not A = A → ⊥

data _≤_ : ℕ → ℕ → Type where
  z≤n : {n : ℕ} → zero ≤ n
  s≤s : {m n : ℕ} → m ≤ n → suc m ≤ suc n

_<_ : ℕ → ℕ → Type
m < n = suc m ≤ n

-- Finite depth indices for the most recent k historical layers.
data Fin : ℕ → Type where
  fzero : {n : ℕ} → Fin (suc n)
  fsuc  : {n : ℕ} → Fin n → Fin (suc n)

mapVec : {A B : Type} {n : ℕ} →
  (A → B) → Vec A n → Vec B n
mapVec f [] = []
mapVec f (x ∷ xs) = f x ∷ mapVec f xs

injectFinLeft : (w offset : ℕ) → Fin w → Fin (w + offset)
injectFinLeft zero offset ()
injectFinLeft (suc w) offset fzero = fzero
injectFinLeft (suc w) offset (fsuc i) = fsuc (injectFinLeft w offset i)

-- Historical support is recorded by the list of distinct layer distances
-- that still appear irreducibly after normalization.
record HistoricalSupport (k : ℕ) : Type where
  constructor mkSupport
  field
    arity  : ℕ
    layers : Vec (Fin k) arity

open HistoricalSupport public

data PrimitiveCost : Type where
  derived           : PrimitiveCost
  requiresPrimitive : PrimitiveCost

costOf : PrimitiveCost → ℕ
costOf derived = zero
costOf requiresPrimitive = suc zero

-- A theorem-facing interface for the paper's obligation language.
record ObligationLanguage (ℓC ℓO : Level) : Type (ℓ-suc (ℓ-max ℓC ℓO)) where
  field
    Candidate     : Type ℓC
    O             : Candidate → ℕ → Type ℓO
    weaken        : {X : Candidate} (k : ℕ) → O X k → O X (suc k)
    Supp          : {X : Candidate} {k : ℕ} → O X k → HistoricalSupport k
    primitiveCost : {X : Candidate} {k : ℕ} → O X k → PrimitiveCost

open ObligationLanguage public

module _ {ℓC ℓO} (L : ObligationLanguage ℓC ℓO) where

  open ObligationLanguage L renaming
    ( Candidate to CandidateL
    ; O to OL
    ; weaken to weakenL
    ; Supp to SuppL
    ; primitiveCost to primitiveCostL
    )

  a : {X : CandidateL} {k : ℕ} → OL X k → ℕ
  a o = HistoricalSupport.arity (SuppL o)

  δ : {X : CandidateL} {k : ℕ} → OL X k → ℕ
  δ o = costOf (primitiveCostL o)

  Irreducible : {X : CandidateL} {k : ℕ} → OL X k → Type
  Irreducible o = δ o ≡ suc zero

  Derived : {X : CandidateL} {k : ℕ} → OL X k → Type
  Derived o = δ o ≡ zero

record StabilizesAt {ℓC ℓO : Level}
  (L : ObligationLanguage ℓC ℓO) (d : ℕ) : Type (ℓ-max ℓC ℓO) where
  open ObligationLanguage L renaming
    ( Candidate to CandidateL
    ; O to OL
    )

  field
    stabilize : (X : CandidateL) (offset : ℕ) → Iso (OL X (d + offset)) (OL X d)

record HasCoherenceDepth {ℓC ℓO : Level}
  (L : ObligationLanguage ℓC ℓO) (d : ℕ) : Type (ℓ-max ℓC ℓO) where
  field
    stabilizesAt : StabilizesAt L d
    leastDepth   : (d' : ℕ) → d' < d → Not (StabilizesAt L d')

-- A support factors through the recent w layers when every referenced layer in
-- a deeper history is already the image of one of those w recent positions.
record FactorsThroughWindow {offset : ℕ}
  (w : ℕ) (S : HistoricalSupport (w + offset)) : Type where
  field
    localLayers   : Vec (Fin w) (HistoricalSupport.arity S)
    factorization :
      mapVec (injectFinLeft w offset) localLayers ≡ HistoricalSupport.layers S

record ChronologicalWindowAt {ℓC ℓO : Level}
  (L : ObligationLanguage ℓC ℓO) (w : ℕ) : Type (ℓ-max ℓC ℓO) where
  open ObligationLanguage L renaming
    ( Candidate to CandidateL
    ; O to OL
    ; Supp to SuppL
    ; primitiveCost to primitiveCostL
    )

  field
    primitive-support-factors :
      (X : CandidateL) (offset : ℕ) →
      (o : OL X (w + offset)) →
      costOf (primitiveCostL o) ≡ suc zero →
      FactorsThroughWindow {offset = offset} w (SuppL o)

record HasChronologicalWindowSize {ℓC ℓO : Level}
  (L : ObligationLanguage ℓC ℓO) (w : ℕ) : Type (ℓ-max ℓC ℓO) where
  field
    windowAt    : ChronologicalWindowAt L w
    leastWindow : (w' : ℕ) → w' < w → Not (ChronologicalWindowAt L w')
