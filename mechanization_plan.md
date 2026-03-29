# Mechanization Plan For `1_coherence_depth.tex`

Date: 2026-03-29

This file lists the statements in `1_coherence_depth.tex` that still need
exact mechanization in Agda, ordered from easier proof-engineering work to
harder work.

The ordering is by estimated implementation difficulty, not by dependency
order. Several later items depend on extending the now-explicit interface
calculus further.

## Baseline Already Covered

These are not part of the remaining backlog below.

- `thm:extensional` already has an exact theorem-facing counterpart in
  `agda/Metatheory/Extensional.agda` via `UIP-forces-depth-1` and
  `history-truncates-to-one`.
- `cor:d1` already has an exact theorem-facing counterpart in
  `agda/Core/DepthOneAffine.agda` via `depth1-affine-growth`,
  `Delta-depth1-closed`, and `tau-depth1-closed`. The Agda statements use the
  same bootstrap indexing / subtraction-free natural-number equivalent of the
  paper formulas.
- `thm:adjunction` already has an exact theorem-facing counterpart in
  `agda/Metatheory/AdjunctionBarrier.agda` via
  `explicit-binary-sealing-obstruction`, `triangle-identity-corollary`, and
  `adjunction-barrier`.
- The paper's obligation-language definitions now have a theorem-facing
  counterpart in `agda/Metatheory/Obligations.agda` via
  `HistoricalSupport`, `PrimitiveCost`, `ObligationLanguage`,
  `StabilizesAt`, `HasCoherenceDepth`, `FactorsThroughWindow`, and
  `HasChronologicalWindowSize`.
- `lem:horn-reduction` already has an exact theorem-facing counterpart in
  `agda/Metatheory/KanSubsumption.agda` via `HornExtensionFiber`,
  `structural-horn-language`, `structural-integration-horn-reduction`, and
  `remote-layer-obligation-derived`, built on the computational witnesses
  `arity3-obligation-syntactically-derivable`,
  `history-beyond-two-algorithmically-subsumed`, and
  `arity3-open-box-hfilled`.
- `lem:telescopic` now has an exact theorem-facing counterpart in
  `agda/Metatheory/KanSubsumption.agda` via `TelescopicTraceChain`,
  `TelescopicSubsumptionView`, `telescopic-subsumption`, and
  `telescopic-remote-comparison-derived`, built on the computational witnesses
  `history-beyond-two-algorithmically-subsumed` and
  `arity3-open-box-hfilled`.
- `thm:upper` now has an exact theorem-facing counterpart in
  `agda/Metatheory/UpperBound.agda` via
  `structural-obligation-set-equivalence` and
  `structural-stabilizes-at-two`, built on the canonical horn-extension
  fiber exposed from `agda/Metatheory/KanSubsumption.agda`.
- `cor:chrono-window` now has an exact theorem-facing counterpart in
  `agda/Metatheory/ChronologicalWindow.agda` via
  `primitive-obligations-factor-through-last-two`,
  `one-layer-window-insufficient`, `two-layer-chronological-window`, and
  `chronological-markov-blanket`, built on the horn-reduction,
  telescopic-subsumption, and upper-bound packages.
- `cor:d2` now has an exact theorem-facing counterpart in
  `agda/Metatheory/ExactDepth.agda` via
  `structural-coherence-depth-exactly-two`,
  `structural-chronological-window-size-exactly-two`,
  `cubical-coherence-depth-exactly-two`, and
  `cubical-chronological-window-size-exactly-two`, with the adjunction lower
  bound threaded through the same surface as
  `cubical-binary-sealing-obstruction` and
  `cubical-triangle-identity-corollary`.
- `thm:2d-foundations` now has an exact theorem-facing counterpart in
  `agda/Metatheory/TwoDFoundations.agda` via
  `FullyCoupled2DFoundation`, `depth-two-law-for-2d-foundations`,
  `chronological-window-size-two-for-2d-foundations`,
  `constant-payload-depth-two-law`, and the cubical instantiations
  `cubical-2d-foundation`,
  `cubical-depth-two-law-for-2d-foundations`, and
  `cubical-chronological-window-size-two-for-2d-foundations`.
- `lem:arity-dimension` now has an exact theorem-facing counterpart in
  `agda/Metatheory/Obligations.agda` via `Positive`,
  `CoherenceCellShape`, `historical-arity-forces-cell-dimension`, and
  `irreducible-obligation-requires-cell`.
- `prop:transparent` now has an exact theorem-facing counterpart in
  `agda/Metatheory/InterfaceCalculus.agda` via `LibraryState`,
  `TransparentDevelopment`, `transparent-growth-keeps-library-state`,
  `transparent-definitions-preserve-active-interface`,
  `transparent-definitions-have-zero-integration-latency`, and
  `transparent-user-level-code-lies-outside-the-recurrence`.
- `cor:refactoring` now has an exact theorem-facing counterpart in
  `agda/Metatheory/Refactoring.agda` via `PayloadNormalForm`,
  `PayloadPresentation`, `ObligationNormalForm`, `ObligationPresentation`,
  `historical-support-correspondence`, and `refactoring-invariance`, which
  quotient admissible rebundling/currying/transparent-normalization
  presentations through a shared counting normal form and expose the induced
  bijections on atomic payloads, atomic obligations, and historical arities.
- `thm:canonicity` now has an exact theorem-facing counterpart in
  `agda/Metatheory/CanonicityDensity.agda` via `HistoricalInterface`,
  `FullyCoupledFoundation`, `FoundationalCoreExtension`,
  `NativeCanonicityPreservingTotality`,
  `PromotedOperationalExhaustiveness`, `MaximalInterfaceDensity`,
  `CanonicityDensityTheorem`,
  `primitive-interaction-counting-normal-form`, and
  `global-admissibility-forces-maximal-interface-density`.
- `thm:trace` now has an exact theorem-facing counterpart across
  `agda/Metatheory/InterfaceCalculus.agda` and
  `agda/Metatheory/TracePrinciple.agda` via the explicit counted sealed-layer
  surface `ExplicitSealedLayer`,
  `explicit-sealed-public-interface`,
  `explicit-sealed-public-size`, and the theorem-facing package
  `IntegrationTracePrinciple`, `public-counting-normal-form`, and
  `integration-trace-principle`.
- `thm:recurrence` now has an exact theorem-facing counterpart in
  `agda/Metatheory/UniversalRecurrence.agda` via
  `CountedHistoricalLayer`, `HistoricalWindow`, `historical-interface`,
  `historical-interface-counting-normal-form`,
  `ChronologicalRecurrenceContext`, `UniversalAffineRecurrence`, and
  `universal-affine-recurrence`, built on the explicit counted sealed-layer
  surface from `agda/Metatheory/InterfaceCalculus.agda` and the per-layer
  trace package from `agda/Metatheory/TracePrinciple.agda`.
- The recurrence/shift fragment of `cor:fibonacci` is present in
  `agda/Core/AffineRecurrence.agda`, and the full bootstrap-indexed paper
  corollary is now present there via `Delta-bootstrap`, `U-bootstrap-closed`,
  and `tau-bootstrap-closed`. The Agda statements use the subtraction-free
  natural-number equivalent of the paper formulas.
- `thm:clutching` now has an exact theorem-facing counterpart in
  `agda/Geometry/Clutching.agda` via `CircleClutchingBoundary`,
  `clutching-family`, `HopfClutchingFamily`,
  `hopf-binary-clutching-datum`, `hopf-binary-clutching-nontrivial`,
  `ClutchingHornExtensionFiber`,
  `clutching-horn-extension-fiber-contractible`, and
  `clutching-family-theorem`, with a dedicated regression import in
  `agda/Test/ClutchingSmoke.agda`. Because the repository's theorem package
  still uses `agda/Core/Nat.agda` while the exact clutching development uses
  the Cubical library's standard HIT/suspension stack, this topological
  package is currently checked as a standalone geometry artifact rather than
  re-exported through `agda/PEN.agda`.
- The final paper-level mechanization-claim cleanup is now reflected directly
  in `1_coherence_depth.tex`: the abstract, mechanization section, and
  conclusion now give an explicit paper-to-Agda theorem map, state the trust
  boundary (no theorem-facing postulates; Agda plus the declared Cubical
  library and the repository-local arithmetic surface), and distinguish the
  exact cubical metatheory from the distilled sealed-interface/counting
  calculus layered on top of it.

## Remaining Statements, Easy To Hard

No remaining paper-facing theorem/prose gaps are currently tracked here.

## Practical Dependency Notes

- The completed arity/dimension and horn-reduction/telescopic packages are the first
  theorem-facing results built directly on the new obligation-language
  surface.
- The exact depth-two corollary is now the core theorem-facing wrapper tying
  the upper-bound, chronological-window, and lower-bound packages together.
- The canonicity, trace, and universal-recurrence packages now expose a
  theorem-facing historical interface and counted sealed-layer window surface
  for later refactoring work.
- The refactoring package now builds on that explicit interface-calculus
  surface and extends it from counted-window recurrence to refactoring
  invariance.
- The clutching family is now a completed standalone geometry milestone.
- The paper prose now points the cited theorem package directly at exact Agda
  theorem names instead of only at computational ingredients or scaffolding.

## Suggested Execution Order

All items in the current paper-facing mechanization plan are complete.
