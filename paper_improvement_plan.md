# Paper Improvement Plan for `1_coherence_depth.tex`

## Strategic target

The paper should end in a position where:

1. The fixed fully coupled cubical extension calculus over a chosen CCHM-style core remains the unquestionable main theorem object.
2. Exact obligation stabilization, minimal-signature eliminability, and chronological factorization are explicitly separated and never conflated.
3. The abstract "2D foundations" wrapper is stated by explicit hypotheses, not by broad rhetoric about all univalent or all cubical systems.
4. The mechanization section is maximally honest about what is formalized, what is formalized only abstractly, and what still uses a paper-level bridge.
5. Any broader claim beyond the chosen cubical calculus is either fully instantiated or explicitly marked as conjecture / open problem / future work.

## Recommended end-state claim package

This is the safest high-ambition end-state:

1. For the fixed cubical extension calculus:
   - exact obligation depth `d_obl = 2`;
   - minimal-signature depth `d_mu = 2`;
   - a genuine two-layer chronological window for primitive trace data;
   - the affine/Fibonacci recurrence for `\mu`.
2. For the abstract wrapper:
   - a theorem about `d_mu` and the `\mu`-recurrence under explicit hypotheses;
   - a separate exact-depth theorem only when exact obligation stabilization is also assumed or proved.
3. For HoTT / 2LTT:
   - no theorem for plain Book HoTT unless a genuine upper-bound argument is written;
   - a theorem for a suitable 2LTT-style extension discipline only if that discipline is actually specified and checked.
4. For cubical generalization:
   - not "any cubical type theory";
   - only foundations satisfying an explicit checklist: computational univalence, a uniform horn-computation principle strong enough for the elimination theorem, and a sealing/normalization discipline validating the bridge.

## If time is tight

Treat everything in "Ambitious research goals" as optional unless it can be completed cleanly.

## Remaining execution order

1. Optional ambitious generalizations.

## Ambitious research goals

These should only enter the paper if they can be completed cleanly.

### A. Prove and mechanize the contractible-factor decomposition

Target:

- prove
  `\Ocal^{(k)}(X) \simeq \Ocal^{(2)}(X) \times C_k(X)`
  with `C_k(X)` canonically contractible;
- then explain exactly how normalized obligation quotients identify the extra factor.

Why this matters:

- it would let the paper retain a stronger exact-depth narrative while still being precise about minimal signatures.

### B. Mechanize the bridge theorem from cubical synthesis to `\mu`-minimal elimination

Target:

- mechanize the judgmental/presentation theorem that replaces a primitive field by a synthesized cubical term and shows the field disappears from canonical normalized public form.

Why this matters:

- it would remove the main remaining paper-level trust boundary.

### C. Refactor the abstract Agda layer to distinguish exact depth from minimal-signature depth

Target:

- add a weaker abstract interface for `2D foundations` that speaks directly about primitive-cost elimination and chronological windows, not only about exact `StabilizesAt`.

Why this matters:

- it would make the generalization layer stronger and more widely applicable, while still remaining formalized.

### D. Build a real 2LTT instantiation

Target:

- write down a concrete 2LTT-style extension discipline and prove it satisfies the revised abstract `2D foundation` hypotheses.

Why this matters:

- this is the cleanest way to keep the paper pointed toward generalization beyond the chosen cubical calculus without overclaiming about plain HoTT.

### E. Construct a toy separation example for `d_obl` versus `d_mu`

Target:

- build a small example in which higher obligations remain present or contractible while contributing nothing to `\mu`, or vice versa if that is the direction that genuinely occurs.

Why this matters:

- it would justify the invariant split conceptually and preempt reviewer resistance to the new terminology.

### F. General abstract horn-computational foundation interface

Target:

- formulate one abstract package of hypotheses strong enough for the cubical upper bound and the bridge theorem;
- instantiate it only for systems actually checked.

Why this matters:

- it would push the paper toward the broadest defensible generalization.

## Final verification checklist

Before calling the revision done, run one last audit for these phrases and their local logical role:

- `higher-elim`
- `upper`
- `stabilizes`
- `contractible`
- `derived`
- `eliminable`
- `presentation-equivalent`
- `any cubical`
- `HoTT`
- `2LTT`
- `formalized`

Every occurrence should now be unambiguous, locally justified, and consistent with the revised theorem architecture.
