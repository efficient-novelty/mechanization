{-# OPTIONS --cubical --safe --guardedness #-}

module Test.MetatheorySmoke where

open import PEN

affine-shift-available = U-is-fibonacci
affine-bootstrap-available = tau-bootstrap-closed
affine-depth1-available = tau-depth1-closed

obligation-language-surface = ObligationLanguage
historical-support-surface = HistoricalSupport
positive-arity-surface = Positive
coherence-cell-shape-surface = CoherenceCellShape
arity-dimension-theorem = historical-arity-forces-cell-dimension
obligation-arity-theorem = irreducible-obligation-requires-cell
coherence-depth-surface = HasCoherenceDepth
chronological-window-surface = HasChronologicalWindowSize

extensional-available = history-truncates-to-one

horn-fiber-surface = HornExtensionFiber
horn-language-surface = structural-horn-language
horn-reduction-available = structural-integration-horn-reduction
horn-derived-cost-available = remote-layer-obligation-derived

upper-bound-available = history-beyond-two-algorithmically-subsumed
lower-bound-available = depth1-insufficient
adjunction-package-available = explicit-binary-sealing-obstruction
adjunction-triangle-available = triangle-identity-corollary
adjunction-barrier-available = adjunction-barrier
