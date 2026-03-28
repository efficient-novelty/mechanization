# Coherence Depth Mechanization Plan

## Objective

Turn the current Agda artifact from

- "depth-two recurrence follows once `Saturation.Axiom` is assumed"

into

- "Cubical Agda proves the exact depth theorem package that explains why the depth-two window exists, why UIP collapses it to one, and why payload-aware counting still yields shifted Fibonacci growth."

This plan is written against the current repository at `C:\DEV\pen`

## Current Baseline

The repo already has a solid Phase-1 counting artifact:

- `agda/Core/Nat.agda` defines `fib`, `Δ`, `τ`, and the normalized recurrence.
- `agda/ObligationGraph/Recurrence.agda` packages the Fibonacci and stagnation statements.
- `agda/Saturation/Axiom.agda` makes the depth-two window an explicit assumption.
- `agda/Saturation/Decomposition.agda` and the barrier files check the sealed-interface discipline concretely.
- `agda/Adjunction/AdjunctionDepth.agda` and `agda/Adjunction/TriangleIdentity.agda` already contain useful lower-bound scaffolding.
- `1_coherence_depth.tex` explicitly says the present mechanization does **not yet** prove the depth-two theorem package itself.

So the work is not greenfield. The real task is to replace the current "axiom + counting" center of gravity with a theorem-driven metatheory layer, while preserving the recurrence code that already works.

## Target End State

When this plan is complete, the repository should support the following claims mechanically:

1. `Theorem A`: assuming UIP / `isSet`, binary coherence spaces collapse, so the coherence depth is `1`.
2. `Theorem B.1`: cubical Kan filling uniformly derives witnesses for arity-3 open-box obligations, so history beyond two layers is telescopically subsumed without implying uniqueness of the full filler space.
3. `Theorem B.2`: univalence prevents a global depth-one collapse, so arity-2 obligations remain genuinely independent in general.
4. `Affine recurrence`: once constant per-layer payload is accounted for explicitly, the cost law is affine and becomes homogeneous Fibonacci after a constant shift.
5. The paper can replace the current scope limitation in Section 5 with a completed theorem-package statement.

## Repository Changes

### New modules

Add:

- `agda/Core/AffineRecurrence.agda`
- `agda/Metatheory/Extensional.agda`
- `agda/Metatheory/KanSubsumption.agda`
- `agda/Metatheory/AdjunctionBarrier.agda`

### Existing modules to update

Update:

- `agda/PEN.agda` to re-export the new theorem package
- `agda/README.md` to describe the metatheory layer
- `agda/progress_tracking.md` to replace the old "Phase 1 complete / future work" story with the new milestones
- `1_coherence_depth.tex` to remove the current mechanization scope caveat and describe the completed formalization

### Existing modules to reuse rather than discard

Keep and mine for reusable material:

- `agda/Adjunction/AdjunctionDepth.agda`
- `agda/Adjunction/TriangleIdentity.agda`
- `agda/Experiments/DepthThreeAttempt.agda`
- `agda/Saturation/AbstractionBarrier.agda`
- `agda/Saturation/AbstractionBarrier9.agda`

The new `Metatheory` modules should become the theorem-facing API. Existing `Adjunction/` and `Saturation/` experiments can remain as supporting development material unless cleanup becomes worthwhile later.

## Guardrails

These constraints should shape every implementation step:

- Do **not** build a full syntactic embedding of type theory. The plan is semantic: model integration cost via contractibility of obligation spaces.
- Stay compatible with the existing Agda setup. Continue avoiding `Cubical.Data.*` imports that trigger the repository's Agda 2.8.0 cubical compatibility issue.
- Reuse `Core/Nat.agda` arithmetic infrastructure where possible. Extend it only when the affine proof needs additional lemmas.
- Treat the current recurrence modules as stable. The new theorem package should explain them, not break them.
- Separate "proof of exact depth two" from "paper rhetoric about exact depth two." The paper should only be tightened after the Agda modules type-check together.

## Semantic Dictionary

This is the key modeling move and should be stated early in the code and the paper.

- `cost = 0` means the relevant semantic obligation space is `isContr`
- `cost > 0` means the obligation space is not contractible, so new independent data must be supplied

This dictionary avoids a large syntactic mechanization while still matching the paper's notion of independent coherence burden.

## Work Plan

## Phase 0: Align the Artifact

### Goal

Prepare the repo so the new modules can be added without destabilizing the current artifact.

### Tasks

- Create the new `agda/Metatheory/` directory.
- Decide whether `Adjunction/` remains as an internal helper namespace or whether selected definitions are moved into `Metatheory/AdjunctionBarrier.agda`.
- Audit the imports needed for `isContr`, `isSet`, equivalences, univalence, `hcomp`, and path composition, and confirm they can be obtained from the Cubical foundations modules already used in the project.
- If the lower-bound proof needs a local boolean type, add a tiny self-contained support module rather than importing `Cubical.Data.Bool`.

### Done criteria

- New directory exists.
- Import plan is known.
- There is a clear decision about whether `Adjunction/*` is reused by import or by copy-and-refactor.

## Phase 1: Fix the Payload Bookkeeping

### Goal

Replace the normalized recurrence story with the payload-aware affine one, then recover Fibonacci after a constant shift.

### Deliverable

- `agda/Core/AffineRecurrence.agda`

### Tasks

- Define `Δ-affine : ℕ → ℕ → ℕ` with payload parameter `c`.
- Encode the intended base cases exactly as used in the paper.
- Define the shifted sequence `U c n = Δ-affine c n + (c + c)`.
- Prove `U-is-fibonacci`.
- Prove the closed-form identification with the existing Fibonacci normalization if useful for downstream reuse.
- Decide whether `ObligationGraph/Recurrence.agda` should re-export a payload-free corollary of the affine theorem or remain unchanged.

### Acceptance criteria

- The new module type-checks on its own.
- The proof does not smuggle the homogeneous recurrence into the definition; it is recovered by arithmetic.
- The module makes the paper's "payload accounting flaw" disappear cleanly.

### Notes

This phase should happen first. It upgrades the recurrence statement without waiting on the topology.

## Phase 2: Build the Metatheory Interface

### Goal

Create a minimal shared vocabulary for the three depth theorems.

### Deliverable

- Common obligation-space definitions inside `agda/Metatheory/`

### Tasks

- Decide whether to introduce a tiny shared helper file later or to keep each theorem module self-contained. Start self-contained unless duplication becomes painful.
- Define the notion of an obligation space in semantic terms: a type whose contractibility represents "no new primitive coherence burden."
- Write short module-level comments connecting the Agda object to the paper's notion of arity and chronological lookback.
- Keep the API theorem-oriented. Avoid introducing speculative abstractions not needed for the three target theorems.

### Acceptance criteria

- Every metatheory module can state its main theorem using the same cost/contractibility language.
- There is no dependence on the counting modules at this layer except possibly `Core/Nat.agda` for indexing conventions or comments.

## Phase 3: Mechanize Theorem A (`d = 1` under UIP)

### Goal

Show that extensional settings collapse binary coherence, yielding depth one.

### Deliverable

- `agda/Metatheory/Extensional.agda`

### Tasks

- Import the h-level machinery needed for `isSet`.
- State the main theorem in the strongest clean form that the paper uses:
  - either `UIP-forces-depth-1 : isSet A → ... → isContr (p ≡ q)`
  - or an equivalent statement phrased as contractibility of all arity-2 obligation spaces
- Prove the theorem directly from the UIP / set-level assumption.
- Add one short corollary or comment that translates the Agda result back into "history truncates to one layer."

### Acceptance criteria

- The theorem is genuinely semantic, not just a restatement of prose.
- The module is simple enough to cite in the paper as the exact mechanization of the extensional collapse claim.

### Likely reuse

- `Cubical.Foundations.HLevels`

## Phase 4: Mechanize Theorem B.1 (`d ≤ 2` from cubical filling)

### Goal

Prove the dimensional upper bound as a contractibility statement about 3-dimensional fillers.

### Deliverable

- `agda/Metatheory/KanSubsumption.agda`

### Tasks

- Choose a boundary model that is small enough to prove against but rich enough to represent the paper's telescopic subsumption argument.
- Define an `Arity3-Obligation` type representing the open 3-box determined by adjacent unary and binary boundary data.
- Construct a canonical filler using cubical composition.
- Prove uniqueness of fillers, ideally via cubical filling machinery rather than ad hoc path algebra.
- State the final theorem as a contractibility result and add a corollary phrased as "history beyond two layers contributes no new primitive data."
- Cross-check the theorem statement against the prose around the current telescopic lemma in `1_coherence_depth.tex`.

### Acceptance criteria

- The module uses actual cubical geometry, not a combinatorial surrogate.
- The final theorem can replace the role currently played by `Saturation.Axiom` in the paper's explanation of why depth two is the right chronological window.
- The proof explains why higher-arity obligations are semantically free rather than merely uncounted.

### Technical caution

This is the hardest module. Expect one or two iterations on the boundary encoding before the proof shape settles.

## Phase 5: Mechanize Theorem B.2 (`d ≥ 2` from univalence)

### Goal

Show that depth one cannot hold in general in the univalent setting because binary coherence spaces do not collapse globally.

### Deliverable

- `agda/Metatheory/AdjunctionBarrier.agda`

### Tasks

- Reuse the existing adjunction-depth modules for intuition, examples, or helper definitions where that saves time.
- Decide on the cleanest contradiction argument:
  - a direct univalence-based path-space separation argument
  - or an adjunction-flavored wrapper whose final contradiction is still powered by univalence
- If using `Bool`, define a local boolean type if the existing environment still cannot import one safely.
- State a theorem of the form
  - `depth1-insufficient : ¬ (∀ ... → isContr (p ≡ q))`
- Carry the contradiction through explicitly so the proof cannot be read as a mere heuristic.
- Add a short explanation tying the theorem back to triangle identities and independent binary coherence data.

### Acceptance criteria

- The result shows a genuine lower bound, not just a classification of existing examples.
- The theorem cleanly separates the univalent case from the UIP case proved in `Extensional.agda`.

### Recommended proof posture

Keep the final theorem univalence-centric and use adjunctions as motivation or corollary. That gives the paper a sharper formal story and reduces the risk of proving only an example-specific lower bound.

## Phase 6: Connect the Metatheory Back to the Existing Artifact

### Goal

Make the new theorem package visible from the top-level Agda artifact and explain how it refines, rather than replaces, the current recurrence files.

### Tasks

- Re-export the new modules from `agda/PEN.agda`.
- Update `agda/README.md` so the repository structure includes `Metatheory/` and `Core/AffineRecurrence.agda`.
- Update `agda/progress_tracking.md` with the new milestones and status.
- Decide what to do with `Saturation/Axiom.agda`:
  - keep it as a focused abstraction of the old combinatorial model
  - or annotate it as superseded by theorem-level metatheory for the main paper claims
- Add one lightweight regression module if needed, for example a `Test/MetatheorySmoke.agda` that imports the theorem package and top-level `PEN.agda`.

### Acceptance criteria

- A reader opening `PEN.agda` can discover the theorem package immediately.
- The repository tells a coherent story: arithmetic consequences plus foundational causes.

## Phase 7: Update the Paper

### Goal

Rewrite the paper so its formalization claims match the new artifact exactly.

### Primary target

- `C:\DEV\pen\1_coherence_depth.tex`

### Paper edits

- Update the abstract sentence that currently describes only a focused mechanization of the abstraction barrier and induced recurrence.
- Update the introduction and contribution list so the mechanization now covers both consequences and causes.
- Revise Section 5 from "focused recurrence-and-sealing fragment" to "completed theorem package plus recurrence."
- Replace the current mechanization scope remark with a theorem-backed description of what is now formalized.
- Update the conclusion so the "remaining work is clear" paragraph no longer describes the depth-two theorem package as future work.

### Text that should likely be rewritten

At minimum, revisit the passages around these themes:

- the abstract discussion of mechanization
- the contribution bullet list near the start
- Section 5 beginning at `\section{Cubical Agda mechanization}`
- `\begin{rem}[Scope of the current mechanization]`
- the final paragraph of the conclusion

### Acceptance criteria

- The paper no longer claims the depth-two theorem package is unformalized.
- Every mechanization claim in prose points to a real module that type-checks.
- The paper distinguishes clearly between the affine recurrence result and the exact-depth metatheory result.

## Suggested Section-5 Replacement

Once the Agda work is complete, Section 5 should say something close to:

> We formalize both the combinatorial consequences and the foundational causes of the coherence window in Cubical Agda. `Metatheory/KanSubsumption.agda` proves that cubical composition renders arity-3 semantic obligations contractible, so historical layers at depth at least three contribute no new primitive coherence data. `Metatheory/AdjunctionBarrier.agda` proves that arity-2 obligations do not collapse under univalence, forcing the lower bound `d ≥ 2`, while `Metatheory/Extensional.agda` shows that UIP collapses the same obligations and yields `d = 1` in the extensional case. On the recurrence side, `Core/AffineRecurrence.agda` proves that explicit payload accounting yields an affine depth-two law whose constant shift recovers the homogeneous Fibonacci recurrence.

This should be treated as the draft target, not copied blindly. Final wording should match theorems actually present in the code.

## Verification Checklist

Type-check these modules individually first:

- `agda/Core/AffineRecurrence.agda`
- `agda/Metatheory/Extensional.agda`
- `agda/Metatheory/KanSubsumption.agda`
- `agda/Metatheory/AdjunctionBarrier.agda`

Then check the integrated artifact:

- `agda/PEN.agda`
- any smoke-test module added during Phase 6

Then rebuild the paper:

- compile `1_coherence_depth.tex`
- verify the mechanization section, introduction, and conclusion are internally consistent

## Risks and How to Handle Them

### Risk 1: The cubical 3-box encoding becomes too heavy

Mitigation:

- use the smallest boundary object that still captures the telescopic claim
- prove contractibility for that object first
- generalize only if the paper truly needs the stronger statement

### Risk 2: The lower-bound proof via univalence depends on unavailable library pieces

Mitigation:

- keep the contradiction argument minimal
- introduce small local support types if needed
- treat existing `Adjunction/*` modules as scaffolding, not as the final theorem statement

### Risk 3: The new metatheory and the old counting artifact drift apart conceptually

Mitigation:

- explicitly document that `Saturation/Axiom.agda` is the old modeling surface
- explicitly document that `Metatheory/*` explains why the depth-two and depth-one windows are mathematically forced

### Risk 4: The paper overstates the formalization before the code is stable

Mitigation:

- do the paper rewrite last
- require successful type-checking of the new modules before tightening prose

## Recommended Execution Order

1. Add `Core/AffineRecurrence.agda`.
2. Add `Metatheory/Extensional.agda`.
3. Prototype `Metatheory/AdjunctionBarrier.agda` enough to nail the lower-bound strategy.
4. Build `Metatheory/KanSubsumption.agda` as the main hard theorem.
5. Re-export and document the new modules.
6. Rewrite `1_coherence_depth.tex`.

This order gives quick wins early, settles the easy theorem first, de-risks the lower bound, and leaves the hardest geometric module with the rest of the infrastructure already in place.

## Definition of Done

The plan is complete when all of the following are true:

- `PEN.agda` re-exports a theorem package that covers the extensional collapse, the cubical upper bound, the univalent lower bound, and the affine recurrence.
- `Saturation.Axiom` is no longer the central justification for the paper's main theorem.
- `1_coherence_depth.tex` describes the mechanization as proving the depth theorem package rather than merely validating a focused recurrence model.
- The paper's depth-one and depth-two claims each point to a concrete Agda module.
- The recurrence discussion uses the payload-aware affine law instead of the old normalized shorthand alone.
