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

The minimum safe submission path is:

1. Split the invariant or prove a decomposition theorem connecting the two notions.
2. Make the basis layer canonical or explicitly prove basis existence and canonical bijection.
3. Strengthen the recent-history factorization argument with an explicit exported-structure theorem.
4. Rewrite the eliminability argument as a judgmental/presentation theorem, not just a contractibility argument.
5. Downgrade the mechanization claims to the exact trusted boundary.
6. Remove all theorem-level claims about plain Book HoTT and about arbitrary cubical theories.
7. Fix the `thm:2d-foundations` witness bug.

Treat everything in "Ambitious research goals" as optional unless it can be completed cleanly.

## Execution order

1. Claim freeze and theorem statement triage.
2. Invariant split and basis repair.
3. Export-completeness and bridge-replacement proofs.
4. Abstract 2D wrapper rewrite.
5. Mechanization status rewrite.
6. Optional ambitious generalizations.

## Detailed task map by reviewer objection

### 1. Repair the upper-bound notion mismatch

Goal: make exact obligation stabilization and minimal-signature elimination impossible to confuse.

Task 1.1. Audit all places where `thm:higher-elim` and `thm:upper` are currently treated as interchangeable.

- Targets:
  - abstract;
  - introduction bullet list;
  - the paragraph after `def:depth`;
  - `thm:higher-elim`;
  - `thm:upper`;
  - `cor:chrono-window`;
  - `thm:2d-foundations`;
  - mechanization bullet list;
  - `rem:mech-scope`;
  - conclusion.
- Rewrite rule:
  - `thm:upper` handles exact `\Ocal`-stabilization;
  - `thm:higher-elim` handles disappearance from `\mu`-minimal normalized public signatures;
  - `lem:telescopic` handles chronology.
- Done when:
  - no sentence infers exact `\Ocal^{(k)}(X) \simeq \Ocal^{(2)}(X)` from eliminability in `\mu`-minimal signatures.

Task 1.2. Split the invariant.

- Recommended move:
  - rename current Definition 4.2 to `obligation depth` `d_obl`;
  - add `minimal-signature depth` `d_mu`.
- Suggested definition of `d_mu`:
  - the least `d` such that every admissible declaration is presentation-equivalent to one whose canonical normalized public signature contains no primitive historical trace field of arity greater than `d`.
- Required companion remark:
  - `d_mu` does not by itself yield a chronological window; that still requires recent-history factorization.
- Done when:
  - the recurrence theorem and abstract 2D wrapper say exactly which depth notion they use.

Task 1.3. Decide whether to reconnect the two depths.

- Safe default:
  - keep `d_obl` and `d_mu` separate in the abstract theory.
- Stronger option:
  - prove a cubical decomposition theorem of the form
    `\Ocal^{(k)}(X) \simeq \Ocal^{(2)}(X) \times C_k(X)`
    with `C_k(X)` canonically contractible.
  - then explain exactly what quotient or normalization identifies the contractible factor and why that permits a corollary relating `d_obl` and `d_mu`.
- Done when:
  - the paper either has two clearly separated invariants or a fully proved theorem relating them.

Task 1.4. Move the abstract recurrence theorem onto the weaker notion unless the stronger bridge is proved.

- Best safe formulation:
  - the abstract `2D foundations` wrapper should be a theorem about `d_mu = 2` plus a two-layer chronological window, not automatically a theorem about exact `d_obl = 2`.
- Only keep exact-depth rhetoric in the abstract wrapper if exact stabilization is explicitly one of the hypotheses.

### 2. Make the basis layer canonical

Goal: remove dependence on an implicit basis choice from `def:basis-sites`, `thm:coverage`, and `thm:canonicity`.

Task 2.1. Preferred redesign: replace the chosen minimal subset by a canonical quotient notion of basis site.

- Define basis sites as equivalence classes of atomic schemas under the transparent-generation relation already named in `def:basis-sites`.
- Use chosen representatives only for exposition, not inside theorem statements.
- This makes cardinality and invariance structural rather than postulated.

Task 2.2. If the subset-based definition is retained, add an explicit theorem immediately after `def:basis-sites`.

- Theorem shape:
  - every normalized public signature admits a finite basis;
  - any two bases are canonically bijective.
- What must be proved:
  - existence of a finite minimal generating family;
  - invariance of its cardinality;
  - compatibility with elaboration and normalization;
  - stability under presentation equivalence.

Task 2.3. Add bridge corollaries making basis cardinality presentation-invariant.

- Show that `|S_bas(L)|` is independent of presentation-equivalent choices.
- Show that `|I_{n,bas}^{(k)}|` is well-defined for historical interfaces.
- Update the proofs of:
  - `thm:coverage`;
  - `prop:basis-minimality`;
  - `thm:canonicity`;
  - `cor:mu-delta`.

Task 2.4. Check whether the existing Agda basis abstractions can be aligned with the paper's new definition.

- Touchpoints:
  - `agda/Metatheory/CanonicityDensity.agda`;
  - `agda/ObligationGraph/Interface.agda`.
- Only claim a basis theorem as "formalized" if the formal object really matches the new paper definition.

Done when:

- a reviewer cannot say "your recurrence depends on a non-canonical basis choice."

### 3. Strengthen the exported structure used by recent-history factorization

Goal: make `lem:telescopic` depend on an explicit theorem, not on an implicit reading of the trace principle.

Task 3.1. Introduce a definition of `factorization-complete trace basis`.

- This exported structure should include:
  - adjacent unary bridges;
  - binary comparison traces for the needed composites;
  - whatever degenerate or structural faces are required to form the next open `3`-box;
  - stability under normalization and presentation equivalence.

Task 3.2. Add a theorem after `thm:trace`.

- Preferred theorem:
  - every sealed layer exports a factorization-complete trace basis.
- Fallback if not derivable from the current trace principle:
  - make factorization completeness an explicit admissibility requirement of the extension calculus.
- If the fallback is used:
  - narrow the scope claims accordingly and say so clearly.

Task 3.3. Rewrite `lem:telescopic` to cite the new completeness theorem explicitly.

- The first nonadjacent step `X -> L_n -> L_{n-1} -> L_{n-2}` should name the exact exported fields used for each face.
- The inductive step should reference the theorem, not merely "the trace principle".

Task 3.4. Align the paper proof with the existing mechanized telescopic view.

- Touchpoints:
  - `agda/Metatheory/KanSubsumption.agda`;
  - `agda/Metatheory/ChronologicalWindow.agda`.
- Make clear whether the Agda side already packages the needed exported structure or whether the paper is reading more into it than is formally present.

Done when:

- the first and inductive factorization steps each point to explicit exported data, and no implicit sufficiency assumption remains.

### 4. Restate the trusted boundary honestly

Goal: align the paper's prose with the actual formal status.

Task 4.1. Rewrite the abstract, introduction, mechanization section, and conclusion so they say exactly this:

- cubical lower and upper metatheory are formalized;
- the recurrence law is formalized abstractly;
- the bridge from those results to `\mu`-minimal normalized signatures remains at paper level unless it is separately mechanized.

Task 4.2. Add a theorem-status table.

- Suggested columns:
  - paper result / label;
  - formal status;
  - Agda module(s);
  - whether a paper-level bridge is still required.
- Suggested status vocabulary:
  - `fully formalized`;
  - `formalized abstractly, read through bridge`;
  - `paper-level only`.

Task 4.3. Rewrite the mechanization bullet list and `rem:mech-scope`.

- Do not say `thm:higher-elim`, `lem:telescopic`, `thm:2d-foundations`, or the `\mu`-based recurrence are formalized simpliciter if their surface-syntactic form still depends on the bridge.
- Instead say:
  - the Agda development formalizes the structural horn language / obligation-level theorem;
  - the paper reads those results through `sec:bridge`.

Task 4.4. Recheck every "main claims are formalized" sentence.

- If a sentence can be read as "the syntactic theorem about minimal opaque cost is fully mechanized", rewrite it.

Done when:

- a hostile reviewer would agree that the mechanization claims are conservative rather than inflated.

### 5. Upgrade the eliminability argument from "contractible fiber" to a judgmental/presentation theorem

Goal: prove that cubically synthesized witnesses can actually replace primitive fields in the extension language without leaving residue in canonical normalized public form.

Task 5.1. Add an explicit bridge theorem near `thm:adequacy` / `prop:canonical-presentation`.

- Statement template:
  - if a public trace field is definitionally synthesized from lower-arity boundary data by the ambient cubical operations, then replacing the primitive clause by the synthesized term preserves the sealed public typing judgment up to presentation equivalence, and the field disappears from the canonical normalized trace signature.
- Possible theorem names:
  - `thm:computational-replacement`;
  - `prop:kan-synthesis-preserves-presentation`;
  - `cor:derived-fields-drop-from-normal-form`.

Task 5.2. Prove the ingredients separately if needed.

- Replacement/substitution preserves elaborated well-typed sealed export.
- Presentation equivalence respects that replacement.
- Counting normalization removes fields that are transparently reconstructible from the remaining boundary data.
- No new irreducible public fields are introduced by the replacement.

Task 5.3. Rewrite `thm:higher-elim`.

- Required proof shape:
  - horn reduction;
  - cubical synthesis of the missing face/filler;
  - computational replacement theorem;
  - canonical normalized presentation.
- Explicitly avoid the jump:
  - "the fiber is contractible, therefore the field disappears from `N_tr(e)`".

Task 5.4. Decide whether the bridge theorem can be mechanized now.

- If yes:
  - put it on the high-priority formalization queue.
- If no:
  - identify it as the main remaining paper-level bridge lemma and state that openly.

Touchpoints:

- `agda/Metatheory/KanSubsumption.agda`;
- `agda/Metatheory/UpperBound.agda`;
- `agda/Metatheory/InterfaceCalculus.agda`;
- bridge theorems in `sec:bridge`.

Done when:

- the eliminability claim is a theorem about the extension language, not merely an existence statement in homotopy semantics.

### 6. Recast the HoTT discussion so it is unassailable

Goal: remove all theorem-level overclaim about plain Book HoTT.

Task 6.1. Audit the paper for any sentence implying that iterated `J` in plain HoTT gives the same upper-bound mechanism as cubical filling.

Task 6.2. Replace those passages with one of the following, in order of safety.

- preferred:
  - an explicit open problem;
- acceptable:
  - a conjecture;
- only if actually developed:
  - a theorem about HoTT embedded in a suitable 2LTT-style extension discipline.

Task 6.3. Add a short explanatory remark near the generalization section.

- State that path induction in the HoTT book does not in general collapse loop-indexed families with fixed endpoints to the reflexivity case.
- Therefore the paper does not claim the cubical upper-bound mechanism for plain Book HoTT.

Task 6.4. Keep 2LTT language narrow unless fully instantiated.

- Safe wording:
  - `2LTT offers a plausible strictification environment in which an analogue of the depth-two law may be formulated.`
- Do not write:
  - `Book HoTT is exactly 2D`;
  - `2LTT rescues the HoTT theorem`;
  - or anything equivalent unless a real instance has been built.

Done when:

- the HoTT text reads as ambitious but mathematically cautious.

### 7. Replace "any cubical theory" with explicit sufficient hypotheses

Goal: generalize as far as possible without pretending all cubical variants instantiate the same proof.

Task 7.1. Write a short hypothesis checklist theorem or remark for cubical-like instantiations.

- Required ingredients:
  - computational univalence;
  - a uniform horn composition/filling operation or equivalent;
  - enough judgmental computation to synthesize the missing face and filler used by `thm:higher-elim`;
  - a sealing discipline whose normalized exports satisfy the basis and factorization-complete trace theorems;
  - a bridge theorem connecting derived witnesses to elimination from canonical normalized public signatures.

Task 7.2. Rewrite every "any cubical type theory" sentence to refer to this checklist instead.

Task 7.3. Add a short comparison table.

- Suggested rows:
  - chosen CCHM-style cubical core / Cubical Agda variation;
  - other cubical variants;
  - suitable 2LTT-style extension discipline;
  - plain Book HoTT.
- Suggested columns:
  - lower bound;
  - upper bound;
  - bridge to `\mu`;
  - claimed in this paper?

Task 7.4. If time allows, restate the cubical theorem once for an abstract horn-computational foundation interface and instantiate the chosen cubical calculus as the main example.

- If this is not completed:
  - keep the chosen CCHM-style cubical core as the only theorem-level cubical instantiation.

Done when:

- a reviewer must attack a specific missing hypothesis rather than a vague universality claim.

### 8. Separate the roles of univalence and cubical computation

Goal: ensure the paper credits the lower and upper bounds to the right mechanisms.

Task 8.1. Rewrite the introduction and conclusion slogans.

- Univalence / swap / clutching support the lower bound `d >= 2`.
- Cubical Kan computation plus the bridge theorem support the upper bound and eliminability.
- Recent-history factorization depends on exported trace completeness, not on univalence alone.

Task 8.2. Add a short remark near Theorem B.

- State plainly:
  - univalence gives the obstruction to collapse below `2`;
  - it does not by itself remove primitive arity-`3` trace fields from minimal signatures.

Task 8.3. Audit the abstract and conclusion for any sentence that moves directly from "types are weak infinity-groupoids" to the upper bound.

Done when:

- no sentence suggests that univalence or weak infinity-groupoid semantics alone yields the upper bound.

### 9. Fix the logical bug in the abstract 2D theorem

Goal: make the lower-bound witness notion internally coherent.

Task 9.1. Change Property `(1)` of `thm:2d-foundations` from `sealed candidate` to `candidate`.

Task 9.2. Recheck the proof and surrounding prose.

- The lower-bound witness should always be a candidate that induces a genuine binary obligation before sealing.
- Once sealed, the obligation has already been discharged.

Task 9.3. If the theorem is split into exact and `\mu`-minimal versions, ensure each theorem uses the correct witness notion.

Done when:

- `thm:2d-foundations` is internally coherent without any ambiguity about whether the witness still carries obligations.

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

### Immediate textual edits

1. Fix the theorem statement and proof of `thm:2d-foundations`.
2. Fix all "sealed candidate" language in lower-bound contexts.
3. Rewrite mechanization claims in the abstract, introduction, mechanization section, and conclusion.
4. Remove any "plain HoTT has depth two" wording.
5. Remove any "any cubical theory" wording.

### Core definition and theorem edits

1. Add `d_obl` / `d_mu` split, or prove a theorem relating them.
2. Replace or justify the basis definition.
3. Add factorization-complete trace basis theorem.
4. Add computational replacement bridge theorem.
5. Rework `thm:higher-elim` around the new bridge theorem.
6. Rework the abstract 2D theorem to use the correct hypotheses and depth notion.

### Mechanization alignment edits

1. Check `agda/Metatheory/TwoDFoundations.agda` against the paper's revised abstract theorem.
2. Check `agda/Metatheory/UpperBound.agda` against the paper's exact-depth statements.
3. Check `agda/Metatheory/KanSubsumption.agda` and `agda/Metatheory/ChronologicalWindow.agda` against the paper's recent-history factorization statement.
4. Check `agda/Metatheory/CanonicityDensity.agda` against the paper's revised basis theory.
5. Update the theorem-status table accordingly.

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

## Submission gate

Do not submit until all of the following are true:

1. The paper no longer conflates exact obligation stabilization with minimal-signature elimination.
2. The basis layer is canonical or explicitly proved choice-invariant.
3. `lem:telescopic` cites an explicit factorization-complete export theorem or admissibility axiom.
4. `thm:higher-elim` is proved via a judgmental/presentation bridge theorem, not by contractibility rhetoric alone.
5. The mechanization section clearly says what is formalized and what remains at paper level.
6. No theorem-level claim remains about plain Book HoTT unless a real proof is supplied.
7. No theorem-level claim remains about arbitrary cubical theories without an explicit hypothesis checklist.
8. `thm:2d-foundations` Property `(1)` is fixed and the theorem's hypotheses match the paper's actual proof obligations.
9. The abstract, introduction, mechanization section, and conclusion all tell the same truth about scope and status.

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
- `sealed candidate`
- `formalized`

Every occurrence should now be unambiguous, locally justified, and consistent with the revised theorem architecture.
