{-# OPTIONS --cubical --safe --guardedness #-}

module Metatheory.UniversalRecurrence where

open import Cubical.Foundations.Prelude
open import Cubical.Foundations.Isomorphism using (Iso)
open import Cubical.Data.Sum.Base using (_⊎_)

open import Core.Nat renaming (ℕ to Nat)
open import Metatheory.Obligations
  using (Fin; ObligationLanguage; HasCoherenceDepth; HasChronologicalWindowSize)
open import Metatheory.InterfaceCalculus
  using ( LibraryState
        ; ExplicitSealedLayer
        ; corePayloadSize
        ; coherenceCost
        ; explicit-sealed-public-interface
        ; explicit-sealed-public-size
        )
open import Metatheory.TracePrinciple
  using ( composeIso
        ; sumIso
        ; fin-sum-iso
        ; IntegrationTracePrinciple
        ; public-counting-normal-form
        ; integration-trace-principle
        )

private
  variable
    ℓC ℓO ℓI ℓP ℓT : Level
    ℓA : Level

data Vec (A : Type ℓA) : Nat → Type ℓA where
  [] : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

infixr 5 _∷_

data Empty {ℓ : Level} : Type ℓ where

record CountedHistoricalLayer {ℓI ℓP ℓT : Level} :
  Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓP ℓT))) where
  constructor mkCountedHistoricalLayer
  field
    baseState :
      LibraryState ℓI
    explicitLayer :
      ExplicitSealedLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} baseState

open CountedHistoricalLayer public

HistoricalWindow :
  {ℓI ℓP ℓT : Level} →
  Nat → Type (ℓ-suc (ℓ-max ℓI (ℓ-max ℓP ℓT)))
HistoricalWindow {ℓI} {ℓP} {ℓT} depth =
  Vec (CountedHistoricalLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT}) depth

layer-public-interface :
  CountedHistoricalLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} →
  Type (ℓ-max ℓP ℓT)
layer-public-interface H = explicit-sealed-public-interface (explicitLayer H)

layer-public-size :
  CountedHistoricalLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} → Nat
layer-public-size H = explicit-sealed-public-size (explicitLayer H)

layer-integration-latency :
  CountedHistoricalLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} → Nat
layer-integration-latency H = coherenceCost (explicitLayer H)

layer-core-payload :
  CountedHistoricalLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} → Nat
layer-core-payload H = corePayloadSize (explicitLayer H)

layer-trace-principle :
  (H : CountedHistoricalLayer {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT}) →
  IntegrationTracePrinciple (explicitLayer H)
layer-trace-principle H = integration-trace-principle (explicitLayer H)

historical-interface :
  {depth : Nat} →
  HistoricalWindow {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} depth →
  Type (ℓ-max ℓP ℓT)
historical-interface [] = Empty
historical-interface (H ∷ window) =
  layer-public-interface H ⊎ historical-interface window

historical-interface-size :
  {depth : Nat} →
  HistoricalWindow {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} depth →
  Nat
historical-interface-size [] = zero
historical-interface-size (H ∷ window) =
  layer-public-size H + historical-interface-size window

historical-interface-counting-normal-form :
  {depth : Nat} →
  (window : HistoricalWindow {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} depth) →
  Iso (historical-interface window)
      (Fin (historical-interface-size window))
historical-interface-counting-normal-form [] =
  record
    { fun = λ ()
    ; inv = λ ()
    ; rightInv = λ ()
    ; leftInv = λ ()
    }
historical-interface-counting-normal-form (H ∷ window) =
  composeIso
    (sumIso
      (public-counting-normal-form (explicitLayer H))
      (historical-interface-counting-normal-form window))
    (fin-sum-iso (layer-public-size H) (historical-interface-size window))

window-affine-sum :
  {depth : Nat} →
  HistoricalWindow {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} depth →
  Nat
window-affine-sum [] = zero
window-affine-sum (H ∷ window) =
  (layer-integration-latency H + layer-core-payload H) + window-affine-sum window

historical-interface-size-as-affine-sum :
  {depth : Nat} →
  (window : HistoricalWindow {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} depth) →
  historical-interface-size window ≡ window-affine-sum window
historical-interface-size-as-affine-sum [] = refl
historical-interface-size-as-affine-sum (H ∷ window) =
  historical-interface-size (H ∷ window)                           ≡⟨ refl ⟩
  (layer-core-payload H + layer-integration-latency H)
    + historical-interface-size window                             ≡⟨ cong (_+ historical-interface-size window)
                                                                      (+-comm (layer-core-payload H)
                                                                        (layer-integration-latency H)) ⟩
  (layer-integration-latency H + layer-core-payload H)
    + historical-interface-size window                             ≡⟨ cong ((layer-integration-latency H + layer-core-payload H) +_)
                                                                      (historical-interface-size-as-affine-sum window) ⟩
  (layer-integration-latency H + layer-core-payload H)
    + window-affine-sum window                                     ∎

-- This is the minimal interface needed by the recurrence package itself:
-- a genuine chronological window together with the explicit counted exports
-- inhabiting that window at the current stage.
record WindowedRecurrenceContext
  {ℓC ℓO ℓI ℓP ℓT : Level}
  (L : ObligationLanguage ℓC ℓO) (d : Nat) :
  Type (ℓ-suc (ℓ-max (ℓ-max ℓC ℓO) (ℓ-max ℓI (ℓ-max ℓP ℓT)))) where
  constructor mkWindowedRecurrenceContext
  field
    chronologicalWindow :
      HasChronologicalWindowSize L d
    recentLayers :
      HistoricalWindow {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} d

open WindowedRecurrenceContext public

-- Exact depth can still be tracked separately when a later theorem needs it,
-- but the recurrence law itself only consumes the windowed subpackage above.
record ChronologicalRecurrenceContext
  {ℓC ℓO ℓI ℓP ℓT : Level}
  (L : ObligationLanguage ℓC ℓO) (d : Nat) :
  Type (ℓ-suc (ℓ-max (ℓ-max ℓC ℓO) (ℓ-max ℓI (ℓ-max ℓP ℓT)))) where
  constructor mkChronologicalRecurrenceContext
  field
    coherenceDepth :
      HasCoherenceDepth L d
    windowedContext :
      WindowedRecurrenceContext
        {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d

open ChronologicalRecurrenceContext public

windowed-recurrence-context :
  {L : ObligationLanguage ℓC ℓO} {d : Nat} →
  ChronologicalRecurrenceContext
    {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d →
  WindowedRecurrenceContext
    {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d
windowed-recurrence-context = windowedContext

active-historical-interface :
  {L : ObligationLanguage ℓC ℓO} {d : Nat} →
  WindowedRecurrenceContext
    {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d →
  Type (ℓ-max ℓP ℓT)
active-historical-interface C = historical-interface (recentLayers C)

next-integration-latency :
  {L : ObligationLanguage ℓC ℓO} {d : Nat} →
  WindowedRecurrenceContext
    {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d →
  Nat
next-integration-latency C = historical-interface-size (recentLayers C)

recent-layer-affine-sum :
  {L : ObligationLanguage ℓC ℓO} {d : Nat} →
  WindowedRecurrenceContext
    {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d →
  Nat
recent-layer-affine-sum C = window-affine-sum (recentLayers C)

record UniversalAffineRecurrence
  {ℓC ℓO ℓI ℓP ℓT : Level}
  {L : ObligationLanguage ℓC ℓO} {d : Nat}
  (C : WindowedRecurrenceContext
         {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d) :
  Type (ℓ-suc (ℓ-max (ℓ-max ℓC ℓO) (ℓ-max ℓI (ℓ-max ℓP ℓT)))) where
  field
    active-historical-interface-counting :
      Iso (active-historical-interface C)
          (Fin (next-integration-latency C))
    integration-latency-step :
      next-integration-latency C ≡ recent-layer-affine-sum C

open UniversalAffineRecurrence public

universal-affine-recurrence :
  {L : ObligationLanguage ℓC ℓO} {d : Nat} →
  (C : WindowedRecurrenceContext
         {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d) →
  UniversalAffineRecurrence C
universal-affine-recurrence C = record
  { active-historical-interface-counting =
      historical-interface-counting-normal-form (recentLayers C)
  ; integration-latency-step =
      historical-interface-size-as-affine-sum (recentLayers C)
  }

universal-affine-recurrence-from-coherence :
  {L : ObligationLanguage ℓC ℓO} {d : Nat} →
  (C : ChronologicalRecurrenceContext
         {ℓC = ℓC} {ℓO = ℓO} {ℓI = ℓI} {ℓP = ℓP} {ℓT = ℓT} L d) →
  UniversalAffineRecurrence (windowed-recurrence-context C)
universal-affine-recurrence-from-coherence C =
  universal-affine-recurrence (windowed-recurrence-context C)
