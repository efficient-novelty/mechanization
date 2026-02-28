# Decision Gate A4 Resolution

## Scope
This note closes **Decision gate A4** from `path_A1.md`: determine whether PEN’s Step-13 narrative survives scalar-provenance charging, and state the required manuscript recommendation.

---

## Known baseline quantities (from current manuscript)
- Step 13 metric: `\nu_{13}=46`, `\kappa_{13}=7` (thus baseline `\rho_{13}=46/7\approx 6.57`).
- Reported Step-13 bar appears in two places with two values:
  - `\mathrm{Bar}_{13}=5.99` (rejected-candidate table context),
  - `\mathrm{Bar}_{13}=6.58` (Step-13 local discussion).

Because the manuscript currently contains both, A4 uses a **robust interval check** against both bars.

---

## Threshold math
With scalar charging at Step 13:
\[
\rho'_{13}=\frac{46}{7+\kappa_{\mathrm{scalar}}}.
\]

### Pass condition under bar 5.99
\[
\frac{46}{7+\kappa_{\mathrm{scalar}}}>5.99
\Rightarrow 7+\kappa_{\mathrm{scalar}}<\frac{46}{5.99}\approx 7.68
\Rightarrow \kappa_{\mathrm{scalar}}\le 0.
\]

### Pass condition under bar 6.58
\[
\frac{46}{7+\kappa_{\mathrm{scalar}}}>6.58
\Rightarrow 7+\kappa_{\mathrm{scalar}}<\frac{46}{6.58}\approx 6.99
\Rightarrow \kappa_{\mathrm{scalar}}<0.
\]

So for either bar, any nonzero scalar-charge on Step 13 causes failure unless `\nu_{13}` is increased materially.

---

## Scenario table (A4)

| Scenario | Assumption | `\kappa'_{13}` | `\rho'_{13}` | Outcome vs 5.99 | Outcome vs 6.58 |
|---|---|---:|---:|---|---|
| Baseline current | implicit free scalar | 7 | 6.57 | Pass | Borderline/fail by local 6.58 text |
| Full-charge minimal | `\kappa_{\mathrm{scalar}}=1` (theoretical lower bound) | 8 | 5.75 | Fail | Fail |
| Full-charge realistic | `\kappa_{\mathrm{scalar}}\ge 2` | `\ge 9` | `\le 5.11` | Fail | Fail |
| Amortized normative | scalar in infra ledger, row unchanged | 7 (row) | 6.57 (row) | Pass | Borderline/fail by local 6.58 text |

Key point: under strict full-charge, Step 13 fails immediately for any nonzero scalar burden.

---

## Gate A4 decision
**Decision:** ✅ **A4 complete**.

**Finding:** PEN’s Step-13 narrative does **not** survive unchanged under strict conservative first-use charging.

---

## Required recommendations

1. **Adopt A3 normative policy (amortized infrastructure ledger)** for main sequence reporting.
2. **Publish full-charge sensitivity explicitly** and state that strict first-use charging would defer/reorder Step 13.
3. **Resolve bar inconsistency (`5.99` vs `6.58`)** in `pen_unified.tex` before final claims.
4. **Add contingency sentence**: if conservative charging is enforced, metric emergence shifts unless additional novelty credit for scalar interface interactions is formally justified.

---

## Final recommendation
For reviewer robustness, keep the canonical sequence under amortized shared-infrastructure accounting, but explicitly disclose that under conservative first-use charging Step 13 is not selected at the current `\nu=46` and would require either:
- a different charging convention, or
- a formally justified increase to counted novelty tied to scalar-interface interactions.
