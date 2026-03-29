# Mechanization Plan For `1_coherence_depth.tex`

Date: 2026-03-29

This file lists the statements in `1_coherence_depth.tex` that still need
exact mechanization in Agda, ordered from easier proof-engineering work to
harder work.

The ordering is by estimated implementation difficulty, not by dependency
order. Several later items depend on formalizing the paper's interface
calculus more explicitly.

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
- `lem:arity-dimension` now has an exact theorem-facing counterpart in
  `agda/Metatheory/Obligations.agda` via `Positive`,
  `CoherenceCellShape`, `historical-arity-forces-cell-dimension`, and
  `irreducible-obligation-requires-cell`.
- The recurrence/shift fragment of `cor:fibonacci` is present in
  `agda/Core/AffineRecurrence.agda`, and the full bootstrap-indexed paper
  corollary is now present there via `Delta-bootstrap`, `U-bootstrap-closed`,
  and `tau-bootstrap-closed`. The Agda statements use the subtraction-free
  natural-number equivalent of the paper formulas.

## Remaining Statements, Easy To Hard

1. `cor:chrono-window` (`1_coherence_depth.tex`, label `cor:chrono-window`)

   Status: missing as a theorem.
   The paper claims a genuine chronological Markov blanket of size two. The
   current Agda artifact only contains the computational ingredient; it does
   not yet define and prove the full chronological-window corollary.
   Suggested target: a small corollary module built on the completed
   horn-reduction, telescopic, and upper-bound baselines.

2. `cor:d2` (`1_coherence_depth.tex`, label `cor:d2`)

   Status: missing as a theorem.
   The paper's "coherence depth is exactly 2" statement is currently only a
   prose composition of the upper-bound and lower-bound packages. There is no
    single checked Agda theorem with that conclusion.
   Suggested target: add an exact-depth corollary module after Item 1 and
   the completed lower-bound package for `thm:adjunction`.

3. `thm:2d-foundations` (`1_coherence_depth.tex`, label `thm:2d-foundations`)

    Status: missing.
    The abstract universality theorem for all fully coupled 2D foundations is
    not yet formalized. Once exact depth, horn-subsumption, and recurrence are
    explicit, this should be a relatively short wrapper theorem.
    Suggested target: a new abstract theorem module reusing the completed
    arity/dimension and upper-bound packages together with Items 1 and 2.

4. `prop:transparent` (`1_coherence_depth.tex`, label `prop:transparent`)

    Status: missing.
    The paper distinguishes transparent user-level elaboration from sealed
    layer creation, but the Agda artifact does not yet formalize that
    distinction as a theorem about unchanged exported interfaces and zero
    integration latency.
    Suggested target: a lightweight interface-calculus module building on
    `agda/Metatheory/Obligations.agda`.

5. `thm:trace` (`1_coherence_depth.tex`, label `thm:trace`)

    Status: missing.
    The current barrier modules illustrate the idea that resolved obligations
    survive as opaque trace fields, but there is no exact theorem proving the
    decomposition `S(L_k) ~= S_core(L_k) disjoint-union S_tr(L_k)` with the
    stated cardinality equations.
    Suggested target: add a theorem-facing trace module built over an explicit
    sealed-record interface calculus.

6. `thm:canonicity` (`1_coherence_depth.tex`, label `thm:canonicity`)

    Status: missing.
    This is the first genuinely heavy semantic theorem still absent from the
    artifact. The paper appeals to canonicity-preserving totality for native
    extensions and operational exhaustiveness for promoted interface packages,
    but neither side is yet formalized in Agda.
    Suggested target: a dedicated module such as
    `agda/Metatheory/CanonicityDensity.agda`.

7. `thm:recurrence` (`1_coherence_depth.tex`, label `thm:recurrence`)

    Status: missing as stated.
    The current code proves the depth-two constant-payload specialization, not
    the paper's universal depth-`d` affine law
    `Delta_{n+1} = sum_j (Delta_{n-j} + kappa_{n-j})`.
    Suggested target: prove it after Item 6 by making the historical
    interface `I_n^(d)` an explicit finite tagged coproduct over the new
    obligation-language surface.

8. `cor:refactoring` (`1_coherence_depth.tex`, label `cor:refactoring`)

    Status: missing.
    The paper claims invariance under canonical telescope isomorphism,
    rebundling, currying, and transparent normalization. The current code uses
    this principle informally, but does not yet mechanize the quotienting or
    the induced bijections on atomic payloads and obligations.
    Suggested target: a separate normalization/refactoring module after the
    interface calculus from Item 6 is explicit.

9. `thm:clutching` (`1_coherence_depth.tex`, label `thm:clutching`)

    Status: missing.
    This is the hardest remaining statement. The paper's topological exact
    family is only echoed by exploratory files such as
    `agda/Experiments/HopfTrace.agda`, which uses postulates and is not part of
    the theorem-facing safe artifact. An exact mechanization needs an actual
    cubical HIT/bundle development rather than a distilled interface sketch.
    Suggested target: a dedicated `agda/Metatheory/Clutching.agda` or
    `agda/Geometry/Clutching.agda`.

10. Conclusion-level mechanization claim cleanup

    Status: still needed after the theorem work above.
    Once Items 1-9 are completed, the prose in the abstract, mechanization
    section, and conclusion should be tightened so that every cited paper
    theorem points to an exact Agda theorem rather than to a computational
    ingredient or explanatory scaffold.
    Suggested target: `1_coherence_depth.tex`.

## Practical Dependency Notes

- The completed arity/dimension and horn-reduction/telescopic packages are the first
  theorem-facing results built directly on the new obligation-language
  surface.
- Items 1-2 are the remaining core "exact depth theorem package" work.
- Items 4-8 require making the current distilled interface calculus explicit
  enough to support exact paper-level statements.
- Item 9 is the main topological formalization project and should be treated
  as its own milestone.

## Suggested Execution Order

If the goal is fastest improvement to theorem-to-code fidelity, a good
implementation order is:

1. Items 1-2 (chronological window through exact-depth package)
2. Item 3 (abstract 2D foundations wrapper)
3. Items 4-7 (transparent/interface calculus through universal recurrence)
4. Item 8 (refactoring invariance)
5. Item 9 (clutching family)
