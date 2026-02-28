# Decision Gate A1 Resolution

## Scope
This note closes **Decision gate A1** from `path_A1.md`: select and lock the primary internal construction route for `\mathbb{R}` under Path A1 before manuscript edits to `pen_unified.tex`.

---

## Inputs considered
1. Current PEN manuscript constraints (empty-library framing; Step 5 rejection narrative for `\N`; Step 13 metric uses `\R`).
2. Path A1 requirement: explicit scalar provenance and cost accounting.
3. Reviewer-facing goals: minimal hidden assumptions, reproducible dependency chain, clean accounting language.

---

## Candidate routes

## Route C — Cauchy completion of `\mathbb{Q}`
Construct `\mathbb{R}` as equivalence classes of Cauchy approximations/sequences over rationals, with completion witness and embedding `\iota : \mathbb{Q} \hookrightarrow \mathbb{R}`.

## Route D — Dedekind cuts in `\mathbb{Q}`
Construct `\mathbb{R}` as lower cuts (or equivalent cut presentation) satisfying order/roundedness/locatedness conditions.

---

## Evaluation criteria and scoring rubric
Scores are 1–5 (higher is better for Path A1 execution readiness).

1. **Dependency transparency for PEN readers**
2. **Compatibility with computational/synthesis narrative**
3. **Cost-accounting tractability for `\kappa_{\mathrm{scalar}}`**
4. **Appendix explainability under page pressure**
5. **Risk of semantic ambiguity in main text**

| Criterion | Cauchy (Route C) | Dedekind (Route D) | Notes |
|---|---:|---:|---|
| Dependency transparency | 5 | 4 | Cauchy chain maps directly to `\N\to\Z\to\Q` + completion operation. |
| Computational narrative fit | 5 | 3 | Cauchy aligns naturally with approximation/completion language. |
| `\kappa` accounting tractability | 4 | 4 | Both are countable interface bundles; Cauchy slightly easier to package operationally. |
| Appendix explainability | 4 | 3 | Cauchy usually shorter to explain to mixed logic/physics audience. |
| Main-text ambiguity risk | 4 | 3 | Dedekind cut conditions can invite side debates over order axioms/locatedness details. |
| **Total** | **22/25** | **17/25** | Route C preferred. |

---

## Gate A1 decision
**Decision:** Lock **Cauchy completion** as the **primary route** for Path A1.

**Status:** ✅ Gate A1 complete.

---

## Rationale (reviewer-facing)
1. It preserves strict internal derivation (`\N\to\Z\to\Q\to\R`) while keeping the narrative concise.
2. It integrates cleanly with PEN’s mechanical-accounting story by treating completion as an explicit importable interface bundle.
3. It reduces exposition risk in the main text; full equivalence-to-Dedekind can be stated in appendix without making Dedekind the headline construction.

---

## What is locked vs deferred

## Locked now
- Primary route: **Cauchy completion**.
- Mandatory dependency chain for provenance: `\N\to\Z\to\Q\to\R`.
- Requirement that scalar costs be explicit (`\kappa_{\mathrm{scalar}}`).

## Deferred to next gates
- Exact clause granularity for `\kappa_{\mathrm{scalar}}` (A2/A3).
- Normative accounting mode in main table (conservative vs amortized) (A3).
- Numerical Step 13 margin impact (A4).

---

## Required follow-up edits implied by A1 (not executed here)
1. In `pen_unified.tex`, Step 13 scalar codomain discussion must explicitly reference Cauchy-based provenance.
2. Add appendix subsection describing Cauchy construction assumptions and interface bundle.
3. Add brief remark that Dedekind route is equivalent and retained as secondary/validation perspective.

---

## Recommendation
Proceed with **Cauchy completion as primary** in all forthcoming manuscript edits and accounting tables; keep Dedekind as a short appendix cross-check only.
