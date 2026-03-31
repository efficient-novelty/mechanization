{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.TwoLTTFoundations where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)
open import Cubical.Data.Empty.Base using (⊥)

open import Core.Nat renaming (ℕ to Nat)
open import Core.Sequence using (Vec; []; _∷_)
open import Metatheory.Obligations
  using ( Not
        ; Fin
        ; fzero
        ; fsuc
        ; HistoricalSupport
        ; PrimitiveCost
        ; derived
        ; requiresPrimitive
        ; costOf
        ; mapVec
        ; injectFinLeft
        ; ObligationLanguage
        ; PrimitiveEliminatesAbove
        ; HasPrimitiveDepth
        ; FactorsThroughWindow
        ; ChronologicalWindowAt
        ; HasChronologicalWindowSize
        ; StabilizesAt
        ; HasCoherenceDepth
        ; _<_
        ; s≤s
        )
open import Metatheory.TwoDFoundations
  using ( PrimitiveWindow2DFoundation
        ; FullyCoupled2DFoundation
        ; primitive-depth-two-law-for-2d-foundations
        ; depth-two-law-for-2d-foundations
        ; chronological-window-size-two-for-2d-foundations
        )

private
  variable
    l : Level

data NoObligation : Type where

data One : Type where
  one : One

data TwoLevelCandidate : Type where
  strictified-fibrant-extension : TwoLevelCandidate

data BinaryTraceSchema : Type where
  fibrant-binary-comparison : BinaryTraceSchema

TwoLevelObligation : Nat → Type
TwoLevelObligation zero = NoObligation
TwoLevelObligation (suc zero) = NoObligation
TwoLevelObligation (suc (suc k)) = BinaryTraceSchema

idIso : {A : Type l} → Iso A A
idIso = record
  { fun = λ x → x
  ; inv = λ x → x
  ; rightInv = λ x → refl
  ; leftInv = λ x → refl
  }

two-level-weaken :
  (k : Nat) →
  TwoLevelObligation k →
  TwoLevelObligation (suc k)
two-level-weaken zero ()
two-level-weaken (suc zero) ()
two-level-weaken (suc (suc k)) o = o

two-level-support :
  {k : Nat} →
  TwoLevelObligation k →
  HistoricalSupport k
two-level-support {zero} ()
two-level-support {suc zero} ()
two-level-support {suc (suc k)} _ = record
  { arity = suc (suc zero)
  ; layers = fzero ∷ fsuc fzero ∷ []
  }

two-level-primitive-cost :
  {k : Nat} →
  TwoLevelObligation k →
  PrimitiveCost
two-level-primitive-cost {zero} ()
two-level-primitive-cost {suc zero} ()
two-level-primitive-cost {suc (suc zero)} _ = requiresPrimitive
two-level-primitive-cost {suc (suc (suc k))} _ = derived

two-level-language : ObligationLanguage _ _
two-level-language = record
  { Candidate = TwoLevelCandidate
  ; O = λ _ k → TwoLevelObligation k
  ; weaken = two-level-weaken
  ; Supp = two-level-support
  ; primitiveCost = two-level-primitive-cost
  }

two-level-binary-factors-through-two :
  FactorsThroughWindow {offset = zero}
    (suc (suc zero))
    (two-level-support {k = suc (suc zero)} fibrant-binary-comparison)
two-level-binary-factors-through-two = record
  { localLayers = fzero ∷ fsuc fzero ∷ []
  ; factorization = refl
  }

higher-arity-cost-is-not-primitive :
  (offset : Nat) →
  Not
    (costOf
      (two-level-primitive-cost
        {k = suc (suc (suc offset))}
        fibrant-binary-comparison)
      ≡ suc zero)
higher-arity-cost-is-not-primitive offset primitiveWitness =
  zero-is-not-suc
    (sym higher-arity-cost-is-zero ∙ primitiveWitness)
  where
    higher-arity-cost-is-zero :
      costOf
        (two-level-primitive-cost
          {k = suc (suc (suc offset))}
          fibrant-binary-comparison)
      ≡ zero
    higher-arity-cost-is-zero = refl

    nat-zero-code : Nat → Type
    nat-zero-code zero = One
    nat-zero-code (suc n) = NoObligation

    zero-is-not-suc : {n : Nat} → zero ≡ suc n → ⊥
    zero-is-not-suc p with subst nat-zero-code p one
    ... | ()

two-level-no-zero-layer-factorization :
  (localLayers : Vec (Fin zero) (suc (suc zero))) →
  ⊥
two-level-no-zero-layer-factorization (() ∷ _)

two-level-second :
  {A : Type} {n : Nat} →
  Vec A (suc (suc n)) →
  A
two-level-second (_ ∷ y ∷ _) = y

two-level-fin-zero-code : Fin (suc (suc zero)) → Type
two-level-fin-zero-code fzero = One
two-level-fin-zero-code (fsuc fzero) = NoObligation

two-level-fzero-is-not-fsuc :
  fzero ≡ fsuc fzero →
  ⊥
two-level-fzero-is-not-fsuc p with subst two-level-fin-zero-code p one
... | ()

two-level-no-one-layer-factorization :
  (localLayers : Vec (Fin (suc zero)) (suc (suc zero))) →
  mapVec (injectFinLeft (suc zero) (suc zero)) localLayers ≡
    (fzero ∷ fsuc fzero ∷ []) →
  ⊥
two-level-no-one-layer-factorization (fzero ∷ fzero ∷ []) q =
  two-level-fzero-is-not-fsuc (cong two-level-second q)

module _ where

  open ObligationLanguage two-level-language

  two-level-not-less-than-two :
    (n : Nat) →
    Not ((suc (suc n)) < (suc (suc zero)))
  two-level-not-less-than-two n (s≤s (s≤s ()))

  two-level-binary-not-unary :
    Not (Iso (O strictified-fibrant-extension (suc (suc zero)))
             (O strictified-fibrant-extension (suc zero)))
  two-level-binary-not-unary alpha with Iso.fun alpha fibrant-binary-comparison
  ... | ()

  two-level-eliminates-above-two :
    PrimitiveEliminatesAbove two-level-language (suc (suc zero))
  two-level-eliminates-above-two = record
    { eliminate = λ where
        strictified-fibrant-extension offset fibrant-binary-comparison → refl
    }

  two-level-stabilizes-at-two :
    StabilizesAt two-level-language (suc (suc zero))
  two-level-stabilizes-at-two = record
    { stabilize = λ strictified-fibrant-extension offset → idIso }

  two-level-primitive-support-factors :
    (offset : Nat) →
    (o : TwoLevelObligation (suc (suc zero) + offset)) →
    costOf
      (two-level-primitive-cost {k = suc (suc zero) + offset} o)
      ≡ suc zero →
    FactorsThroughWindow {offset = offset} (suc (suc zero))
      (two-level-support {k = suc (suc zero) + offset} o)
  two-level-primitive-support-factors
    zero
    fibrant-binary-comparison
    refl =
    two-level-binary-factors-through-two
  two-level-primitive-support-factors
    (suc offset)
    fibrant-binary-comparison
    primitiveWitness
    with higher-arity-cost-is-not-primitive offset primitiveWitness
  ... | ()

  two-level-chronological-window-at-two :
    ChronologicalWindowAt two-level-language (suc (suc zero))
  two-level-chronological-window-at-two = record
    { primitive-support-factors = λ where
        strictified-fibrant-extension offset o costProof →
          two-level-primitive-support-factors offset o costProof
    }

  zero-window-impossible :
    Not (ChronologicalWindowAt two-level-language zero)
  zero-window-impossible window =
    two-level-no-zero-layer-factorization (FactorsThroughWindow.localLayers factors)
    where
      factors =
        ChronologicalWindowAt.primitive-support-factors window
          strictified-fibrant-extension
          (suc (suc zero))
          fibrant-binary-comparison
          refl

  one-window-impossible :
    Not (ChronologicalWindowAt two-level-language (suc zero))
  one-window-impossible window =
    two-level-no-one-layer-factorization
      (FactorsThroughWindow.localLayers factors)
      (FactorsThroughWindow.factorization factors)
    where
      factors =
        ChronologicalWindowAt.primitive-support-factors window
          strictified-fibrant-extension
          (suc zero)
          fibrant-binary-comparison
          refl

two-level-chronological-window-size-two :
  HasChronologicalWindowSize two-level-language (suc (suc zero))
two-level-chronological-window-size-two = record
  { windowAt = two-level-chronological-window-at-two
  ; leastWindow = λ where
      zero _ → zero-window-impossible
      (suc zero) _ → one-window-impossible
      (suc (suc d)) lt → λ _ → two-level-not-less-than-two d lt
  }

two-level-primitive-window-2d-foundation :
  PrimitiveWindow2DFoundation two-level-language
two-level-primitive-window-2d-foundation = record
  { primitiveBinaryWitness = strictified-fibrant-extension
  ; primitiveBinaryObligation = fibrant-binary-comparison
  ; primitiveBinaryCost = refl
  ; eliminatesAboveTwo = two-level-eliminates-above-two
  ; twoLayerWindow = two-level-chronological-window-size-two
  }

two-level-2d-foundation :
  FullyCoupled2DFoundation two-level-language
two-level-2d-foundation = record
  { primitiveWindowFoundation = two-level-primitive-window-2d-foundation
  ; binaryWitness = strictified-fibrant-extension
  ; binaryNotUnary = two-level-binary-not-unary
  ; stabilizesAtTwo = two-level-stabilizes-at-two
  }

two-level-primitive-depth-two-law-for-2d-foundations :
  HasPrimitiveDepth two-level-language (suc (suc zero))
two-level-primitive-depth-two-law-for-2d-foundations =
  primitive-depth-two-law-for-2d-foundations
    two-level-primitive-window-2d-foundation

two-level-depth-two-law-for-2d-foundations :
  HasCoherenceDepth two-level-language (suc (suc zero))
two-level-depth-two-law-for-2d-foundations =
  depth-two-law-for-2d-foundations two-level-2d-foundation

two-level-window-size-two-for-2d-foundations :
  HasChronologicalWindowSize two-level-language (suc (suc zero))
two-level-window-size-two-for-2d-foundations =
  chronological-window-size-two-for-2d-foundations
    two-level-primitive-window-2d-foundation
