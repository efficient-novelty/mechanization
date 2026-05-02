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
primitive-elimination-surface = PrimitiveEliminatesAbove
primitive-depth-surface = HasPrimitiveDepth
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
canonical-trace-signature-surface = CanonicalTraceSignature
trace-presentation-surface = TracePresentation
trace-presentation-equivalence-available = presentation-equivalence
computational-replacement-package-surface = ComputationalReplacementResult
computational-replacement-theorem-available =
  computational-replacement-preserves-canonical-presentation
canonical-obligation-signature-available = canonical-obligation-signature
higher-arity-derived-available = higher-arity-fields-become-derived
higher-arity-minimal-elimination-available =
  higher-arity-fields-disappear-from-minimal-signature
higher-arity-presented-minimal-elimination-available =
  higher-arity-presented-fields-disappear-from-minimal-signature
higher-arity-computational-replacement-available =
  higher-arity-computational-replacement
canonical-telescope-surface = CanonicalTelescope
canonical-telescope-cardinality-available =
  canonical-telescope-cardinality
canonical-trace-cost-normal-form-surface =
  CanonicalTraceCostNormalForm
trace-cost-cardinality-available =
  trace-cost-normal-form-cardinality
primitive-trace-subtelescope-available =
  primitive-trace-subtelescope
derived-trace-subtelescope-available =
  derived-trace-subtelescope
mu-normal-form-available = mu-of-trace-cost-normal-form
presentation-step-surface = PresentationStep
presentation-equivalence-surface = PresentationEquivalent
presentation-step-support-theorem =
  presentation-step-preserves-trace-support
presentation-step-primitive-cost-theorem =
  presentation-step-preserves-primitive-cost
presentation-equivalence-trace-fields-theorem =
  presentation-equivalence-preserves-trace-fields
presentation-equivalence-primitive-cost-theorem =
  presentation-equivalence-preserves-primitive-cost
transparently-generated-surface = TransparentlyGenerated
requires-primitive-surface = RequiresPrimitive
mu-presentation-step-theorem = mu-preserved-by-presentation-step
mu-presentation-equivalence-theorem =
  mu-invariant-under-presentation-equivalence
derived-deletion-mu-theorem = derived-field-deletion-preserves-mu
primitive-essential-theorem = requires-primitive-field-essential
computational-replacement-mu-theorem =
  computational-replacement-preserves-mu
raw-layer-ref-surface = LayerRef
raw-payload-ref-surface = NewPayloadRef
raw-basis-site-surface = RawBasisSite
raw-export-policy-surface = ExportPolicy
raw-payload-field-surface = PayloadField
raw-algebraic-payload-field-surface = AlgebraicPayloadField
raw-boundary-surface = RawBoundary
raw-structural-clause-surface = RawStructuralClause
raw-telescope-surface = RawTelescope
raw-extension-surface = RawExtension
raw-payload-fields-projection = raw-extension-payload-fields
raw-structural-clauses-projection =
  raw-extension-structural-clauses
raw-algebraic-fields-projection = raw-extension-algebraic-fields
typed-structural-role-surface = TypedStructuralRole
admissible-raw-extension-surface = AdmissibleRawExtension
act-unary-support-theorem = act-clause-has-unary-support
cmp-binary-support-theorem = cmp-clause-has-binary-support
horn-higher-boundary-support-theorem =
  horn-clause-has-higher-boundary-support
algebraic-payload-not-structural-theorem =
  algebraic-field-is-payload-not-structural-trace
naked-higher-face-boundary-theorem =
  naked-higher-face-rejected-or-packaged
canonical-normalized-signature-surface =
  CanonicalNormalizedSignature
raw-extension-candidate-surface = RawExtensionCandidate
normalize-raw-extension-theorem = normalizeRawExtension
raw-extension-candidate-theorem =
  raw-extension-elaborates-to-candidate
raw-extension-normalization-theorem =
  raw-extension-normalizes-to-canonical-signature
raw-trace-normalization-theorem =
  raw-trace-normalizes-to-canonical-signature
normalization-support-theorem = normalize-preserves-support
normalization-arity-theorem = normalize-preserves-arity
normalization-primitive-cost-theorem =
  normalize-preserves-primitive-cost
normalization-presentation-theorem =
  normalization-respects-presentation-equivalence
normalization-counted-interface-theorem =
  normalized-signature-matches-counted-interface
surface-horn-image-surface = SurfaceHornImage
surface-horn-normal-form-theorem =
  surface-to-horn-normal-form
surface-horn-support-theorem =
  surface-to-horn-preserves-support
surface-horn-arity-theorem = surface-to-horn-preserves-arity
surface-horn-primitive-cost-theorem =
  surface-to-horn-preserves-primitive-cost
higher-structural-derived-theorem =
  higher-structural-fields-derived
higher-raw-structural-derived-theorem =
  higher-raw-structural-traces-derived
raw-no-naked-higher-structural-theorem =
  raw-syntax-no-naked-higher-structural-projections
horn-image-completeness-theorem =
  horn-image-complete-for-structural-clauses
raw-structural-horn-normalization-surface =
  RawStructuralHornNormalization
raw-structural-normalizes-to-horn-theorem =
  raw-structural-normalizes-to-horn
counted-historical-layer-surface = CountedHistoricalLayer
historical-window-surface = HistoricalWindow
historical-interface-counting-available =
  historical-interface-counting-normal-form
windowed-recurrence-context-surface = WindowedRecurrenceContext
chronological-recurrence-context-surface = ChronologicalRecurrenceContext
universal-recurrence-package-surface = UniversalAffineRecurrence
universal-recurrence-theorem-available = universal-affine-recurrence
universal-recurrence-from-coherence-available =
  universal-affine-recurrence-from-coherence
finite-active-interface-surface = FiniteActiveInterface
finite-interface-field-surface = InterfaceField
transparent-equivalence-surface = TransparentEquivalence
basis-family-surface = BasisFamily
basis-families-exist-theorem = basis-families-exist
basis-family-cardinality-theorem =
  basis-family-cardinality-invariant
basis-action-equivalence-theorem = basis-action-equivalence
whole-active-interface-surface = WholeActiveInterface
global-action-payload-surface = GlobalActionPayload
action-totality-surface = ActionTotality
active-basis-contract-surface = ActiveBasisContract
active-basis-density-surface = ActiveBasisDensity
global-action-totality-theorem =
  global-action-totality-implies-active-basis-contract
active-basis-density-theorem =
  active-basis-contract-entails-density
coverage-depth-window-noncircularity-example =
  coverage-alone-does-not-imply-depth-two-window
coverage-fibonacci-noncircularity-example =
  coverage-alone-does-not-imply-fibonacci
coupling-footprint-surface = CouplingFootprint
sparse-windowed-context-surface = SparseWindowedContext
sparse-windowed-recurrence-surface = SparseWindowedRecurrence
sparse-windowed-recurrence-theorem =
  sparse-windowed-recurrence
transparent-zero-footprint-surface = TransparentZeroFootprint
transparent-growth-zero-footprint-theorem =
  transparent-growth-zero-footprint
orthogonal-zero-or-sparse-surface = ZeroOrSparseFootprint
orthogonal-extension-zero-or-sparse-theorem =
  orthogonal-extension-zero-or-sparse
orthogonal-extension-below-full-envelope-theorem =
  orthogonal-extension-below-full-envelope
full-coupling-envelope-surface = FullCouplingEnvelope
full-coupling-envelope-theorem = full-coupling-envelope
full-coupling-specializes-sparse-recurrence-theorem =
  full-coupling-specializes-sparse-recurrence
full-coupling-depth-two-affine-law-theorem =
  full-coupling-depth-two-affine-law

extensional-available = history-truncates-to-one

horn-fiber-surface = HornExtensionFiber
horn-language-surface = structural-horn-language
horn-reduction-available = structural-integration-horn-reduction
horn-derived-cost-available = remote-layer-obligation-derived
telescopic-trace-surface = TelescopicTraceChain
telescopic-view-surface = TelescopicSubsumptionView
telescopic-lemma-available = telescopic-subsumption
telescopic-derived-available = telescopic-remote-comparison-derived
contractible-remote-factor-surface = ContractibleRemoteFactor
contractible-factorization-available =
  structural-obligation-contractible-factorization
contractible-remote-factor-available =
  contractible-remote-factor-contractible
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
primitive-two-d-foundation-surface = PrimitiveWindow2DFoundation
primitive-two-d-law-available = primitive-depth-two-law-for-2d-foundations
two-d-foundation-surface = FullyCoupled2DFoundation
two-d-foundation-law-available = depth-two-law-for-2d-foundations
two-d-foundation-window-available = chronological-window-size-two-for-2d-foundations
two-d-foundation-payload-law = constant-payload-depth-two-law
cubical-primitive-two-d-foundation-available = cubical-primitive-window-2d-foundation
cubical-primitive-two-d-law-available =
  cubical-primitive-depth-two-law-for-2d-foundations
cubical-two-d-foundation-available = cubical-2d-foundation
cubical-two-d-law-available = cubical-depth-two-law-for-2d-foundations
cubical-two-d-window-available = cubical-chronological-window-size-two-for-2d-foundations
two-level-language-surface = two-level-language
two-level-primitive-two-d-foundation-available =
  two-level-primitive-window-2d-foundation
two-level-primitive-two-d-law-available =
  two-level-primitive-depth-two-law-for-2d-foundations
two-level-two-d-foundation-available = two-level-2d-foundation
two-level-two-d-law-available = two-level-depth-two-law-for-2d-foundations
two-level-two-d-window-available = two-level-window-size-two-for-2d-foundations

upper-bound-available = history-beyond-two-algorithmically-subsumed
lower-bound-available = depth1-insufficient
adjunction-package-available = explicit-binary-sealing-obstruction
adjunction-triangle-available = triangle-identity-corollary
adjunction-barrier-available = adjunction-barrier
