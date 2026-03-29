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
library-state-surface = LibraryState
transparent-development-surface = TransparentDevelopment
sealed-layer-surface = SealedLayer
explicit-sealed-layer-surface = ExplicitSealedLayer
payload-normal-form-surface = PayloadNormalForm
payload-presentation-surface = PayloadPresentation
obligation-normal-form-surface = ObligationNormalForm
obligation-presentation-surface = ObligationPresentation
higher-interface-surface = HistoricalInterface
fully-coupled-foundation-surface = FullyCoupledFoundation
foundational-core-extension-surface = FoundationalCoreExtension
transparent-state-theorem = transparent-growth-keeps-library-state
transparent-interface-theorem = transparent-definitions-preserve-active-interface
transparent-latency-theorem = transparent-definitions-have-zero-integration-latency
transparent-outside-recurrence-theorem =
  transparent-user-level-code-lies-outside-the-recurrence
payload-refactoring-bijection-available = atomic-payload-bijection
payload-counting-normal-form-available = payload-counting-normal-form
payload-kappa-invariant-available = kappa-invariant-under-refactoring
obligation-refactoring-bijection-available = atomic-obligation-bijection
obligation-counting-normal-form-available = obligation-counting-normal-form
support-refactoring-available = historical-support-correspondence
obligation-arity-invariant-available =
  historical-arity-invariant-under-refactoring
refactoring-package-surface = RefactoringInvariance
refactoring-theorem-available = refactoring-invariance
native-totality-surface = NativeCanonicityPreservingTotality
promoted-exhaustiveness-surface = PromotedOperationalExhaustiveness
maximal-density-surface = MaximalInterfaceDensity
canonicity-package-surface = CanonicityDensityTheorem
canonicity-counting-normal-form = primitive-interaction-counting-normal-form
canonicity-theorem-available =
  global-admissibility-forces-maximal-interface-density
trace-package-surface = IntegrationTracePrinciple
trace-public-counting-normal-form = public-counting-normal-form
trace-theorem-available = integration-trace-principle
counted-historical-layer-surface = CountedHistoricalLayer
historical-window-surface = HistoricalWindow
historical-interface-counting-available =
  historical-interface-counting-normal-form
chronological-recurrence-context-surface = ChronologicalRecurrenceContext
universal-recurrence-package-surface = UniversalAffineRecurrence
universal-recurrence-theorem-available = universal-affine-recurrence

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
two-d-foundation-surface = FullyCoupled2DFoundation
two-d-foundation-law-available = depth-two-law-for-2d-foundations
two-d-foundation-window-available = chronological-window-size-two-for-2d-foundations
two-d-foundation-payload-law = constant-payload-depth-two-law
cubical-two-d-foundation-available = cubical-2d-foundation
cubical-two-d-law-available = cubical-depth-two-law-for-2d-foundations
cubical-two-d-window-available = cubical-chronological-window-size-two-for-2d-foundations

upper-bound-available = history-beyond-two-algorithmically-subsumed
lower-bound-available = depth1-insufficient
adjunction-package-available = explicit-binary-sealing-obstruction
adjunction-triangle-available = triangle-identity-corollary
adjunction-barrier-available = adjunction-barrier
