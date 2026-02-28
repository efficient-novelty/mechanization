# Path A1 Plan: Derive `\mathbb{R}` Internally via Arithmetic Tower (and Update `pen_unified.tex`)

## Objective
Implement **Workstream A / Path A1** rigorously: make the Step 13 scalar codomain (`\R`) derivable from internal univalent foundations by explicitly introducing and cost-accounting the arithmetic tower
\[
\mathbb N \to \mathbb Z \to \mathbb Q \to \mathbb R
\]
instead of silently assuming reals.

This plan includes both:
1. **theoretical/mechanization tasks** needed for internal derivability claims, and
2. **eventual manuscript updates** to `pen_unified.tex` so the argument is reviewer-proof.

---

## Why Path A1 is necessary (current gap)
The paper currently uses `g : TX \otimes_s TX \to \R` in Step 13 while also emphasizing empty-library synthesis and rejecting `\N` on efficiency grounds at Step 5.
Without explicit arithmetic imports, this leaves an exposed dependency gap: Cauchy/Dedekind reals require countable arithmetic substrate.

Path A1 closes the gap by **explicitly reintroducing arithmetic only as needed for scalar construction**, then charging this in `\kappa` and rechecking Step 13 margins.

---

## Deliverable Set

### D1. Formal dependency specification
A precise dependency DAG for scalar construction with the minimal interfaces required at each node:
- `\N` interface (formation/constructors/recursor + set-truncation assumptions if needed)
- `\Z` interface (quotient of `\N \times \N` or equivalent)
- `\Q` interface (fractions over `\Z` with nonzero denominator + equivalence)
- `\R` interface (choose one primary route: Cauchy completion **or** Dedekind cuts)

### D2. PEN cost model integration
A policy for where arithmetic/completion costs are charged in PEN:
- separate additive component: `\kappa_{\mathrm{scalar}}`
- explicit relation to Step 13 clause accounting:
  \[
  \kappa'_{13} = \kappa_{13,\mathrm{metric\ API}} + \kappa_{\mathrm{scalar}}
  \]
- recalculate `\rho'_{13}` and margin against Step 13 bar.

### D3. Manuscript updates (`pen_unified.tex`)
Add/modify text in main sections + appendix so every use of `\R` has explicit provenance and cost treatment.

### D4. Evidence appendix artifacts
- scalar dependency DAG figure/table
- sensitivity table for Step 13 under scalar-cost variants
- short proof sketch of internal constructibility assumptions

---

## Proposed construction strategy (strict Path A1)

## 1) Choose a primary `\R` construction
Pick one as canonical in paper text (recommended: **Cauchy completion** for operational readability), while noting equivalence to Dedekind form in a remark.

### Recommended choice: Cauchy completion
- Better aligns with computational/approximation language in synthesis contexts.
- Easier to communicate as “completion of rational approximants”.

### Alternate retained in appendix
- Dedekind cuts for order-theoretic rigor and cross-check equivalence.

**Decision gate A1:** lock one primary route before editing Step 13 prose.

### A1 resolution (completed)
- **Primary route locked:** Cauchy completion of `\Q`.
- **Decision record:** see `decision_gate_A1.md` for criteria, scoring matrix, and recommendation rationale.
- **Dedekind status:** retained as appendix-level equivalence cross-check, not the primary manuscript route.

---

## 2) Define minimal prerequisite interfaces (not full arithmetic encyclopedia)
To avoid reviewer concern that we imported “all of arithmetic,” constrain imports to minimal interfaces sufficient for completion.

### `\N` minimal import bundle
- type + zero + successor + recursor (or equivalent induction principle)
- set-level status assumptions needed by chosen formalization

### `\Z` minimal import bundle
- constructor/equivalence from `\N` pairs
- ring operations sufficient to define rationals

### `\Q` minimal import bundle
- fraction representation, equivalence relation, field operations/order as needed for completion

### `\R` from completion bundle
- Cauchy approximants + equivalence + completeness witness
- embedding `\iota : \Q \hookrightarrow \R`

**Decision gate A2:** verify each imported item is truly required by chosen completion route.

### A2 resolution (completed)
- **Minimal interface validated:** only REQ dependencies for Cauchy completion are retained.
- **Decision record:** see `decision_gate_A2.md` for required/derivable/optional audit tables.
- **Accounting implication:** `\kappa_{\mathrm{scalar}}` will be computed on the A2-minimal bundle only.

---

## 3) Integrate into PEN accounting
Introduce explicit scalar accounting rather than implicit availability.

### Cost accounting template
Define in text:
- `\kappa_{\mathrm{metric}}` = current Step 13 API clauses (presently 7)
- `\kappa_{\mathrm{scalar}}` = scalar tower + completion clauses
- `\kappa'_{13} = \kappa_{\mathrm{metric}} + \kappa_{\mathrm{scalar}}`
- `\rho'_{13} = \nu_{13} / \kappa'_{13}` (or updated `\nu` if scalar interactions are credited)

### Reporting policy (revised)
1. **Canonical mode (V1):** strict first-use full-charge at Step 13.
2. **Non-canonical comparison (V2):** amortized ledger may be shown only as a contrast lane, never as normative scoring.

Report V2 only as a rejected-policy sensitivity lane.

**Decision gate A3:** set the single normative accounting mode.

### A3 resolution (completed, revised)
- **Normative mode selected:** strict first-use full-charge (V1).
- **Rejected mode:** amortized infrastructure ledger (V2) is non-canonical.
- **Decision record:** see `decision_gate_A3.md`.

---

## 4) Recompute Step 13 viability
Run an explicit sensitivity analysis:
- baseline (current implicit scalar)
- V1 strict first-use full-charge
- V2 non-canonical comparison lane
- V3 strict full-charge with mechanically computed interaction-novelty

For each scenario report:
- `\kappa`
- `\nu` (unchanged vs adjusted if new interaction schemas are counted)
- `\rho`
- margin against bar

If Step 13 fails under conservative charging, explicitly discuss consequences:
- possible sequence shift,
- delayed metric emergence,
- or revised step-local accounting policy.

**Decision gate A4:** confirm whether PEN’s core narrative survives strict accounting unchanged.

### A4 resolution (completed, revised)
- **Outcome:** under strict V1 with bar 5.99, Step 13 fails at baseline unless emergent `\Delta\nu` is added mechanically.
- **Recommendation:** keep strict V1 and recover by (i) mechanical V3 interaction-novelty, or (ii) lower-`\kappa_{\mathrm{scalar}}` construction route.
- **Decision record:** see `decision_gate_A4.md`.

---

## `pen_unified.tex` update plan (eventual edits)

## Section-level edit map

### E1. Foundations/assumptions section (early in paper)
Add a short subsection:
- “Scalar provenance policy”
- rule: no scalar object may be used without explicit construction or axiom/cost
- tie directly to empty-library premise

### E2. Rejected-candidate discussion around `\N`
Refine wording to distinguish:
- “`\N` rejected as a *selected step* under efficiency objective”
- from “`\N` unavailable forever.”

Add one sentence: arithmetic objects may still be imported later if explicitly required and costed for dependent structures.

### E3. Step 13 section
Before or inside metric axioms, add:
- scalar provenance statement referencing new subsection/appendix
- explicit declaration of which `\R` construction is used (Cauchy or Dedekind)
- reference to scalar-cost accounting equation and sensitivity table

### E4. Section 7 (computational verification)
Add a short implementation note:
- whether scalar tower is represented as explicit prerequisites/API package in engine experiments
- if not yet mechanized, clearly mark as manuscript-level accounting extension pending implementation

### E5. Appendix (new dedicated subsection)
“Scalar Construction in UF and PEN Cost Accounting”:
- proof sketch of `\N\to\Z\to\Q\to\R`
- minimal interface list
- accounting formulas
- sensitivity table

### E6. MBTT audit appendix (if needed)
Add note on representation of scalar imports in MBTT AST accounting or explain why current MBTT table excludes this and where adjustment is documented.

---

## Proposed equations / notation to standardize
- `\kappa_{\mathrm{scalar}}` : scalar import burden
- `\kappa'_{13}` : adjusted Step 13 effort
- `\rho'_{13}` : adjusted Step 13 efficiency
- optional split: `\kappa_{\mathrm{arith}} + \kappa_{\mathrm{completion}}`

Keep notation consistent between main text and appendix tables.

---

## Reviewer-risk mitigation checklist

1. **No hidden `\R`:** every `\R` occurrence resolves to explicit construction reference.
2. **No contradiction with Step 5:** clarify “not selected” vs “forbidden forever.”
3. **No accounting opacity:** show conservative + amortized scenarios.
4. **No overclaim of full mechanization:** clearly separate proven/mechanized vs planned accounting layer.
5. **No hand-wavy completion:** name exact real-construction route and prerequisites.

---

## Implementation phases

## Phase P1 — Pre-edit design lock
- choose Cauchy vs Dedekind primary route
- freeze minimal interfaces and symbols
- draft dependency DAG + accounting formulas

**Output:** short design memo merged with this plan.

## Phase P2 — Quantitative impact pass
- estimate `\kappa_{\mathrm{scalar}}` under chosen granularity
- compute Step 13 sensitivity table (conservative/amortized)
- evaluate whether sequence claims need caveats

**Output:** scalar sensitivity table artifact.

## Phase P3 — Manuscript surgery (`pen_unified.tex`)
- apply E1–E6 edits
- add cross-references and labels
- ensure narrative consistency with existing Step 5 and Step 13 language

**Output:** paper diff ready for review.

## Phase P4 — Consistency audit
- search for all `\R` uses and verify provenance reference
- check no sentence implies free scalar import
- verify formulas and table numbers compile and match

**Output:** checklist with pass/fail items.

---

## Concrete acceptance criteria for Path A1

Path A1 is complete only if all are true:
1. `pen_unified.tex` includes explicit scalar provenance policy tied to empty-library premise.
2. Step 13 references a specific internal `\R` construction route.
3. Scalar dependency chain `\N\to\Z\to\Q\to\R` is documented in appendix/main text.
4. `\kappa_{\mathrm{scalar}}` is explicitly introduced and numerically propagated to Step 13 sensitivity.
5. At least one transparent statement addresses how Step 5 rejection of `\N` coexists with later explicit scalar import.
6. All `\R` appearances in relevant sections resolve via cross-reference to provenance/cost discussion.

---

## Suggested commit sequence for execution (future)
1. `path-a1-design-lock-scalar-provenance`
2. `path-a1-add-scalar-kappa-and-step13-sensitivity`
3. `path-a1-update-step5-step13-narrative-consistency`
4. `path-a1-appendix-proof-sketch-and-dag`
5. `path-a1-crossref-and-final-claim-audit`

---

## Notes on scope
This document is a planning artifact. It does **not** by itself change `pen_unified.tex`; it defines the exact work needed so the eventual paper updates are rigorous, auditable, and reviewer-resilient.
