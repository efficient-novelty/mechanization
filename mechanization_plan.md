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
- The computational core of `lem:horn-reduction` / `lem:telescopic` is
  present in `agda/Metatheory/KanSubsumption.agda`, but the current Agda code
  stops short of the paper's full normalized-obligation statements.
- The recurrence/shift fragment of `cor:fibonacci` is present in
  `agda/Core/AffineRecurrence.agda`, and the full bootstrap-indexed paper
  corollary is now present there via `Delta-bootstrap`, `U-bootstrap-closed`,
  and `tau-bootstrap-closed`. The Agda statements use the subtraction-free
  natural-number equivalent of the paper formulas.

## Remaining Statements, Easy To Hard

1. Formalize the paper's obligation-language definitions

   Status: missing.
   Several later theorems still rely on prose definitions rather than checked
   Agda objects: normalized obligation sets `O^(k)(X)`, historical support
   `Supp(o)`, arity `a(o)`, primitive cost `delta(o)`, coherence depth, and
   chronological window size.
   Suggested target: add a new theorem-facing module such as
   `agda/Metatheory/Obligations.agda`.

2. `lem:arity-dimension` (`1_coherence_depth.tex`, label `lem:arity-dimension`)

   Status: missing.
   The paper claims that historical arity `k` forces a `k`-dimensional
   coherence cell. The current artifact uses this dictionary informally, but
   does not yet derive it from a formal notion of support/arity.
   Suggested target: build on the obligation-language module from Item 1.

3. `lem:horn-reduction` (`1_coherence_depth.tex`, label `lem:horn-reduction`)

   Status: partial.
   `agda/Metatheory/KanSubsumption.agda` packages the open-box interface used
   by `hcomp` and `hfill`, but it does not yet restate the paper theorem in
   terms of normalized sealing obligations against remote layers.
   Suggested target: strengthen `agda/Metatheory/KanSubsumption.agda` after
   Item 1.

4. `thm:upper` (`1_coherence_depth.tex`, label `thm:upper`)

   Status: partial.
   The current code proves the computational ingredient for the arity-3 case,
   but it does not yet prove the exact paper statement
   `O^(k)(X) ~= O^(2)(X)` for all `k >= 2`.
   Suggested target: either extend `agda/Metatheory/KanSubsumption.agda` or
   add a separate `agda/Metatheory/UpperBound.agda`.

5. `lem:telescopic` (`1_coherence_depth.tex`, label `lem:telescopic`)

   Status: partial.
   The paper-facing Agda name
   `history-beyond-two-algorithmically-subsumed` captures the intended
   mechanism, but not yet the exact iterated statement about all remote
   binary comparisons generated from adjacent exported traces.
   Suggested target: strengthen the current Kan-subsumption module once the
   obligation/interface definitions are explicit.

6. `cor:chrono-window` (`1_coherence_depth.tex`, label `cor:chrono-window`)

   Status: missing as a theorem.
   The paper claims a genuine chronological Markov blanket of size two. The
   current Agda artifact only contains the computational ingredient; it does
   not yet define and prove the full chronological-window corollary.
   Suggested target: a small corollary module built on Items 1, 4, and 5.

7. `cor:d2` (`1_coherence_depth.tex`, label `cor:d2`)

    Status: missing as a theorem.
    The paper's "coherence depth is exactly 2" statement is currently only a
    prose composition of the upper-bound and lower-bound packages. There is no
    single checked Agda theorem with that conclusion.
   Suggested target: add an exact-depth corollary module after Items 4, 5,
   and the completed lower-bound package for `thm:adjunction`.

8. `thm:2d-foundations` (`1_coherence_depth.tex`, label `thm:2d-foundations`)

    Status: missing.
    The abstract universality theorem for all fully coupled 2D foundations is
    not yet formalized. Once exact depth, horn-subsumption, and recurrence are
    explicit, this should be a relatively short wrapper theorem.
    Suggested target: a new abstract theorem module reusing the exact-depth
    interface from Items 1, 4, 5, and 7.

9. `prop:transparent` (`1_coherence_depth.tex`, label `prop:transparent`)

    Status: missing.
    The paper distinguishes transparent user-level elaboration from sealed
    layer creation, but the Agda artifact does not yet formalize that
    distinction as a theorem about unchanged exported interfaces and zero
    integration latency.
    Suggested target: a lightweight interface-calculus module, probably after
    Item 1.

10. `thm:trace` (`1_coherence_depth.tex`, label `thm:trace`)

    Status: missing.
    The current barrier modules illustrate the idea that resolved obligations
    survive as opaque trace fields, but there is no exact theorem proving the
    decomposition `S(L_k) ~= S_core(L_k) disjoint-union S_tr(L_k)` with the
    stated cardinality equations.
    Suggested target: add a theorem-facing trace module built over an explicit
    sealed-record interface calculus.

11. `thm:canonicity` (`1_coherence_depth.tex`, label `thm:canonicity`)

    Status: missing.
    This is the first genuinely heavy semantic theorem still absent from the
    artifact. The paper appeals to canonicity-preserving totality for native
    extensions and operational exhaustiveness for promoted interface packages,
    but neither side is yet formalized in Agda.
    Suggested target: a dedicated module such as
    `agda/Metatheory/CanonicityDensity.agda`.

12. `thm:recurrence` (`1_coherence_depth.tex`, label `thm:recurrence`)

    Status: missing as stated.
    The current code proves the depth-two constant-payload specialization, not
    the paper's universal depth-`d` affine law
    `Delta_{n+1} = sum_j (Delta_{n-j} + kappa_{n-j})`.
    Suggested target: prove it after Items 1 and 10 by making the historical
    interface `I_n^(d)` an explicit finite tagged coproduct.

13. `cor:refactoring` (`1_coherence_depth.tex`, label `cor:refactoring`)

    Status: missing.
    The paper claims invariance under canonical telescope isomorphism,
    rebundling, currying, and transparent normalization. The current code uses
    this principle informally, but does not yet mechanize the quotienting or
    the induced bijections on atomic payloads and obligations.
    Suggested target: a separate normalization/refactoring module after the
    interface calculus from Item 2 is explicit.

14. `thm:clutching` (`1_coherence_depth.tex`, label `thm:clutching`)

    Status: missing.
    This is the hardest remaining statement. The paper's topological exact
    family is only echoed by exploratory files such as
    `agda/Experiments/HopfTrace.agda`, which uses postulates and is not part of
    the theorem-facing safe artifact. An exact mechanization needs an actual
    cubical HIT/bundle development rather than a distilled interface sketch.
    Suggested target: a dedicated `agda/Metatheory/Clutching.agda` or
    `agda/Geometry/Clutching.agda`.

15. Conclusion-level mechanization claim cleanup

    Status: still needed after the theorem work above.
    Once Items 1-15 are completed, the prose in the abstract, mechanization
    section, and conclusion should be tightened so that every cited paper
    theorem points to an exact Agda theorem rather than to a computational
    ingredient or explanatory scaffold.
    Suggested target: `1_coherence_depth.tex`.

## Practical Dependency Notes

- Item 1 is now the main semantic bottleneck for the remaining exact-depth
  package.
- Items 1-7 are the core "exact depth theorem package" work.
- Items 9-13 require making the current distilled interface calculus explicit
  enough to support exact paper-level statements.
- Item 14 is the main topological formalization project and should be treated
  as its own milestone.

## Suggested Execution Order

If the goal is fastest improvement to theorem-to-code fidelity, a good
implementation order is:

1. Item 1 (obligation-language definitions)
2. Items 2-7 (arity/dimension through exact-depth package)
3. Items 10-12 (trace, canonicity, universal recurrence)
4. Item 13 (refactoring invariance)
5. Item 14 (clutching family)
