# Decision Gate A3 Resolution

## Scope
This note closes **Decision gate A3** from `path_A1.md`: choose the normative accounting mode for scalar-import costs (`\kappa_{\mathrm{scalar}}`) in the main PEN tables, while defining the required sensitivity companion mode.

---

## Inputs
- A1 fixed primary route: Cauchy completion (`\Q \leadsto \R`).
- A2 fixed dependency set: REQ-only minimal scalar bundle `\mathcal I_{\mathrm{scalar}}^{\min}`.
- Step-13 baseline values in manuscript: `\nu_{13}=46`, `\kappa_{13}=7`. 

---

## Candidate accounting modes

### Mode C (Conservative full-charge at Step 13)
Charge all scalar-import clauses at the first metric use:
\[
\kappa'_{13}=\kappa_{13}+\kappa_{\mathrm{scalar}}.
\]

### Mode A (Amortized shared-infrastructure)
Treat scalar tower as shared imported infrastructure (first-use charged in a separate infrastructure ledger), keeping the canonical metric API row comparable:
\[
\kappa_{13}^{\text{table}}=\kappa_{13},\qquad
\kappa_{\mathrm{infra}}\leftarrow \kappa_{\mathrm{scalar}}.
\]
A mandatory sensitivity table still reports full-charge effects.

---

## Evaluation criteria
1. **Comparability with existing genesis rows** (avoid mixing local API cost and global infrastructure cost in one row).
2. **Reviewer transparency** (no hidden charges; full-charge scenario must still be visible).
3. **Stability of narrative across steps** (shared scalar use in Steps 13 and 14 should not be double-charged ad hoc).
4. **Mechanical reproducibility** (clear deterministic rule for future rows).

---

## Analysis

### Criterion 1: comparability
- Mode C distorts row-level interpretation by combining metric-local API and global arithmetic tower into one step-local denominator.
- Mode A preserves table semantics: row cost remains the local API, while infrastructure is tracked in explicit side ledger.

### Criterion 2: transparency
- Mode A only passes transparency requirements if full-charge sensitivity is published side-by-side.
- Mode C is transparent but can obscure API efficiency by conflating layers.

### Criterion 3: cross-step stability
- Scalar infrastructure is reused in Step 14 (`\R` appears in Hilbert clauses), so charging entire scalar stack solely to Step 13 is arbitrary.
- Mode A gives cleaner reuse semantics.

### Criterion 4: reproducibility
- Mode A with a formal rule (“shared infrastructure charged once in infra ledger + mandatory sensitivity table”) is deterministic.

---

## Gate A3 decision
**Decision:** ✅ **A3 complete**.

**Normative mode for main manuscript tables:** **Mode A (Amortized shared-infrastructure)**.

**Required companion disclosure:** publish **Mode C full-charge sensitivity** in appendix/main sensitivity table.

---

## Decision rule to lock in text
Use the following policy sentence in future manuscript edits:

> Shared prerequisite towers (e.g., scalar construction) are charged in a dedicated infrastructure ledger and not repeatedly folded into each local API row; however, full first-use charging is reported in sensitivity analysis for audit transparency.

---

## Recommendation
Proceed to A4 with **Mode A normative + Mode C sensitivity**. This gives the best balance of comparability and reviewer-auditable transparency.
