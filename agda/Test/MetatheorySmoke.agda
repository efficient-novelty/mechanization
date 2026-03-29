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
telescopic-trace-surface = TelescopicTraceChain
telescopic-view-surface = TelescopicSubsumptionView
telescopic-lemma-available = telescopic-subsumption
telescopic-derived-available = telescopic-remote-comparison-derived
upper-bound-equivalence-available = structural-obligation-set-equivalence
upper-bound-theorem-available = structural-stabilizes-at-two
one-layer-window-insufficient-available = one-layer-window-insufficient
chrono-window-theorem-available = two-layer-chronological-window
markov-blanket-theorem-available = chronological-markov-blanket
exact-depth-zero-impossible = stabilization-at-zero-impossible
exact-depth-one-impossible = stabilization-at-one-impossible
exact-depth-theorem-available = cubical-coherence-depth-exactly-two
exact-window-theorem-available = cubical-chronological-window-size-exactly-two
exact-depth-binary-obstruction = cubical-binary-sealing-obstruction
exact-depth-triangle-corollary = cubical-triangle-identity-corollary

upper-bound-available = history-beyond-two-algorithmically-subsumed
lower-bound-available = depth1-insufficient
adjunction-package-available = explicit-binary-sealing-obstruction
adjunction-triangle-available = triangle-identity-corollary
adjunction-barrier-available = adjunction-barrier
