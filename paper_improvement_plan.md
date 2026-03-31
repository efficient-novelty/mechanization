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

## Execution order

1. Add the remaining source-justification scaffolding.
2. Optional ambitious generalizations.

## Detailed task map by reviewer objection

### 1. Repair the upper-bound notion mismatch

Goal: make exact obligation stabilization and minimal-signature elimination impossible to confuse.

Task 1.1. Decide whether to reconnect the two depths.

- Safe default:
  - keep `d_obl` and `d_mu` separate in the abstract theory.
- Stronger option:
  - prove a cubical decomposition theorem of the form
    `\Ocal^{(k)}(X) \simeq \Ocal^{(2)}(X) \times C_k(X)`
    with `C_k(X)` canonically contractible.
  - then explain exactly what quotient or normalization identifies the contractible factor and why that permits a corollary relating `d_obl` and `d_mu`.
- Done when:
  - the paper either has two clearly separated invariants or a fully proved theorem relating them.

Task 1.2. Move the abstract recurrence theorem onto the weaker notion unless the stronger bridge is proved.

- Best safe formulation:
  - the abstract `2D foundations` wrapper should be a theorem about `d_mu = 2` plus a two-layer chronological window, not automatically a theorem about exact `d_obl = 2`.
- Only keep exact-depth rhetoric in the abstract wrapper if exact stabilization is explicitly one of the hypotheses.

### 7. Replace "any cubical theory" with explicit sufficient hypotheses

Goal: generalize as far as possible without pretending all cubical variants instantiate the same proof.

Task 7.3. If time allows, restate the cubical theorem once for an abstract horn-computational foundation interface and instantiate the chosen cubical calculus as the main example.

- If this is not completed:
  - keep the chosen CCHM-style cubical core as the only theorem-level cubical instantiation.

Done when:

- a reviewer must attack a specific missing hypothesis rather than a vague universality claim.

## Cross-cutting exposition tasks

### 10. Add a compact taxonomy of notions and never blur them again

Near `rem:present-vs-cost`, add a compact table or short list distinguishing:

- present obligation type;
- contractible obligation type;
- transparently derived / reconstructible field;
- eliminated from a `\mu`-minimal normalized signature;
- absent from the obligation set.

Then enforce this vocabulary in:

- `thm:higher-elim`;
- `thm:upper`;
- `lem:telescopic`;
- `thm:2d-foundations`;
- mechanization section;
- conclusion.

### 11. Add a claim dependency map

Add one figure or one compact table showing:

- exact `\Ocal`-stabilization;
- minimal-signature elimination;
- recent-history factorization;
- lower-bound binary obstruction;
- recurrence theorem;
- mechanization status.

This should make the theorem architecture visually obvious and reviewer-proof.

### 12. Add a hostile-reviewer pass

Do a final pass with these challenge prompts:

- Does this statement depend on a chosen basis?
- Is this an exact `\Ocal` claim or only a `\mu` claim?
- Where is factorization completeness proved?
- What is fully formalized and what still uses the paper-level bridge?
- Does this generalization really apply to plain HoTT or to arbitrary cubical variants?
- Is this lower-bound evidence or upper-bound evidence?

Delete or rewrite any sentence that cannot answer one of those questions locally.

### 13. Tighten source usage and citations

Use the cited sources only for the claims they actually support:

- Cubical Agda documentation:
  - use for the claim that the implementation is a variation of CCHM with built-in `hcomp` and `transp`.
- HoTT book:
  - use for the limitation of path induction on loop-indexed families with fixed endpoints.
- 2LTT paper:
  - use for the claim that 2LTT is a stronger coherence / metatheoretic framework, not a synonym for plain HoTT.

Do not cite any of these sources for a stronger theorem than they actually give.

## Concrete section-by-section patch list

### Core definition and theorem edits

1. Rework the abstract 2D theorem to use the correct hypotheses and depth notion.

### Mechanization alignment edits

1. Check `agda/Metatheory/TwoDFoundations.agda` against the paper's revised abstract theorem.
2. Check `agda/Metatheory/UpperBound.agda` against the paper's exact-depth statements.

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
