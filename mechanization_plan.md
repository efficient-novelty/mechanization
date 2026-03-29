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
- `lem:arity-dimension` now has an exact theorem-facing counterpart in
  `agda/Metatheory/Obligations.agda` via `Positive`,
  `CoherenceCellShape`, `historical-arity-forces-cell-dimension`, and
  `irreducible-obligation-requires-cell`.
- The computational core of `lem:telescopic` is present in
  `agda/Metatheory/KanSubsumption.agda`, but the current Agda code still
  stops short of the paper's exact iterated remote-binary statement.
- The recurrence/shift fragment of `cor:fibonacci` is present in
  `agda/Core/AffineRecurrence.agda`, and the full bootstrap-indexed paper
  corollary is now present there via `Delta-bootstrap`, `U-bootstrap-closed`,
  and `tau-bootstrap-closed`. The Agda statements use the subtraction-free
  natural-number equivalent of the paper formulas.

## Remaining Statements, Easy To Hard

1. `thm:upper` (`1_coherence_depth.tex`, label `thm:upper`)

   Status: partial.
   The current code proves the computational ingredient for the arity-3 case,
   but it does not yet prove the exact paper statement
   `O^(k)(X) ~= O^(2)(X)` for all `k >= 2`.
   Suggested target: either extend `agda/Metatheory/KanSubsumption.agda` or
   add a separate `agda/Metatheory/UpperBound.agda`.

2. `lem:telescopic` (`1_coherence_depth.tex`, label `lem:telescopic`)

   Status: partial.
   The paper-facing Agda name
   `history-beyond-two-algorithmically-subsumed` captures the intended
   mechanism, but not yet the exact iterated statement about all remote
   binary comparisons generated from adjacent exported traces.
   Suggested target: strengthen the current Kan-subsumption module once the
   obligation/interface definitions are explicit.

3. `cor:chrono-window` (`1_coherence_depth.tex`, label `cor:chrono-window`)

   Status: missing as a theorem.
   The paper claims a genuine chronological Markov blanket of size two. The
   current Agda artifact only contains the computational ingredient; it does
   not yet define and prove the full chronological-window corollary.
   Suggested target: a small corollary module built on Items 1 and 2
   together with the completed horn-reduction baseline.

4. `cor:d2` (`1_coherence_depth.tex`, label `cor:d2`)

    Status: missing as a theorem.
    The paper's "coherence depth is exactly 2" statement is currently only a
    prose composition of the upper-bound and lower-bound packages. There is no
    single checked Agda theorem with that conclusion.
   Suggested target: add an exact-depth corollary module after Items 1-3 and
   the completed lower-bound package for `thm:adjunction`.

5. `thm:2d-foundations` (`1_coherence_depth.tex`, label `thm:2d-foundations`)

    Status: missing.
    The abstract universality theorem for all fully coupled 2D foundations is
    not yet formalized. Once exact depth, horn-subsumption, and recurrence are
    explicit, this should be a relatively short wrapper theorem.
    Suggested target: a new abstract theorem module reusing the completed
    arity/dimension package together with Items 1, 2, and 4.

6. `prop:transparent` (`1_coherence_depth.tex`, label `prop:transparent`)

    Status: missing.
    The paper distinguishes transparent user-level elaboration from sealed
    layer creation, but the Agda artifact does not yet formalize that
    distinction as a theorem about unchanged exported interfaces and zero
    integration latency.
    Suggested target: a lightweight interface-calculus module building on
    `agda/Metatheory/Obligations.agda`.

7. `thm:trace` (`1_coherence_depth.tex`, label `thm:trace`)

    Status: missing.
    The current barrier modules illustrate the idea that resolved obligations
    survive as opaque trace fields, but there is no exact theorem proving the
    decomposition `S(L_k) ~= S_core(L_k) disjoint-union S_tr(L_k)` with the
    stated cardinality equations.
    Suggested target: add a theorem-facing trace module built over an explicit
    sealed-record interface calculus.

8. `thm:canonicity` (`1_coherence_depth.tex`, label `thm:canonicity`)

    Status: missing.
    This is the first genuinely heavy semantic theorem still absent from the
    artifact. The paper appeals to canonicity-preserving totality for native
    extensions and operational exhaustiveness for promoted interface packages,
    but neither side is yet formalized in Agda.
    Suggested target: a dedicated module such as
    `agda/Metatheory/CanonicityDensity.agda`.

9. `thm:recurrence` (`1_coherence_depth.tex`, label `thm:recurrence`)

    Status: missing as stated.
    The current code proves the depth-two constant-payload specialization, not
    the paper's universal depth-`d` affine law
    `Delta_{n+1} = sum_j (Delta_{n-j} + kappa_{n-j})`.
    Suggested target: prove it after Item 8 by making the historical
    interface `I_n^(d)` an explicit finite tagged coproduct over the new
    obligation-language surface.

10. `cor:refactoring` (`1_coherence_depth.tex`, label `cor:refactoring`)

    Status: missing.
    The paper claims invariance under canonical telescope isomorphism,
    rebundling, currying, and transparent normalization. The current code uses
    this principle informally, but does not yet mechanize the quotienting or
    the induced bijections on atomic payloads and obligations.
    Suggested target: a separate normalization/refactoring module after the
    interface calculus from Item 7 is explicit.

11. `thm:clutching` (`1_coherence_depth.tex`, label `thm:clutching`)

    Status: missing.
    This is the hardest remaining statement. The paper's topological exact
    family is only echoed by exploratory files such as
    `agda/Experiments/HopfTrace.agda`, which uses postulates and is not part of
    the theorem-facing safe artifact. An exact mechanization needs an actual
    cubical HIT/bundle development rather than a distilled interface sketch.
    Suggested target: a dedicated `agda/Metatheory/Clutching.agda` or
    `agda/Geometry/Clutching.agda`.

12. Conclusion-level mechanization claim cleanup

    Status: still needed after the theorem work above.
    Once Items 1-11 are completed, the prose in the abstract, mechanization
    section, and conclusion should be tightened so that every cited paper
    theorem points to an exact Agda theorem rather than to a computational
    ingredient or explanatory scaffold.
    Suggested target: `1_coherence_depth.tex`.

## Practical Dependency Notes

- The completed arity/dimension and horn-reduction packages are the first
  theorem-facing results built directly on the new obligation-language
  surface.
- Items 1-4 are the remaining core "exact depth theorem package" work.
- Items 6-10 require making the current distilled interface calculus explicit
  enough to support exact paper-level statements.
- Item 11 is the main topological formalization project and should be treated
  as its own milestone.

## Suggested Execution Order

If the goal is fastest improvement to theorem-to-code fidelity, a good
implementation order is:

1. Items 1-4 (upper bound through exact-depth package)
2. Item 5 (abstract 2D foundations wrapper)
3. Items 6-9 (transparent/interface calculus through universal recurrence)
4. Item 10 (refactoring invariance)
5. Item 11 (clutching family)
