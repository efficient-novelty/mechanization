# Plan: Mechanically Find Missing Interaction Schemas under Strict V1

## Objective
Retain **strict V1 first-use full-charge** as canonical policy and treat the Step-13 mismatch as a discovery target by mechanically identifying additional interaction schemas (`\Delta\nu`) required to clear the Step-13 bar.

This plan defines a reproducible workflow to:
1. compute the exact novelty deficit under strict V1,
2. enumerate candidate interaction families mechanically,
3. validate which are genuinely new inhabited schemas,
4. produce an auditable `\Delta\nu` evidence package suitable for manuscript integration.

---

## Inputs and fixed policy assumptions
- Canonical accounting: **V1 strict first-use full-charge**.
- Step-13 bar: **5.99**.
- Baseline metric values: `\nu_0 = 46`, `\kappa_0 = 7`.
- Scalar burden parameter: `\kappa_{\mathrm{scalar}}` from selected scalar route.
- Pass condition:
  \[
  \nu_0 + \Delta\nu \ge 5.99\,(\kappa_0 + \kappa_{\mathrm{scalar}}).
  \]
- Minimal required novelty increment:
  \[
  \Delta\nu_{\min}=\left\lceil 5.99\,(7+\kappa_{\mathrm{scalar}})-46\right\rceil.
  \]

---

## Success criteria
1. A mechanically generated list of **new** interaction schemas at Step 13 (no hand-assigned credits).
2. Each schema includes machine-checkable witness data (type signature, inhabitation witness or derivation trace, and novelty classification).
3. A deduplicated `\Delta\nu` total with clear mapping to the counting regime used by PEN.
4. A pass/fail verdict against the strict V1 bar for at least one scalar route candidate.

---

## Work packages

## WP1 — Freeze baseline and deficit targets
1. Fix scalar route candidate(s):
   - Cauchy route (current default), and optionally alternate low-`\kappa` route candidates.
2. For each candidate route, fix `\kappa_{\mathrm{scalar}}` value(s).
3. Compute `\Delta\nu_{\min}` target table.

**Deliverable:** `runs/missing_schemas/targets.csv` with columns:
`route,kappa_scalar,kappa_full,nu_base,bar,delta_nu_min`.

---

## WP2 — Build the combined Step-13 experiment library
Construct explicit experimental library states:
- `L_base`: pre-Step-13 library (through Step 12).
- `L_metric`: `L_base +` metric API entry.
- `L_scalar`: `L_base +` scalar bundle route.
- `L_combined`: `L_base +` metric + scalar bundle.

Purpose: measure interaction novelty as
\[
\Delta\nu_{\interaction} = \nu(L_{combined}) - \nu(L_{metric}) - \nu(L_{scalar}) + \nu(L_{base})
\]
under the selected novelty counting semantics.

**Deliverable:** run manifest documenting exact AST/spec entries added to each library variant.

---

## WP3 — Mechanical schema enumeration and witness extraction
Use existing engine capabilities (uniform-nu / typed synthesis / inhabitation deltas) to extract candidate schemas from `L_combined` that are absent in the comparison baselines.

### Required outputs per schema
- `schema_id`
- originating constructor family (modal, mapping, scalar-field, etc.)
- concrete type expression
- witness form (inhabitant/eliminator/computation trace)
- novelty bucket (`\nu_G`, `\nu_C`, `\nu_H`, or uniform-nu category)
- appears in: `L_metric`, `L_scalar`, `L_combined` (boolean indicators)

**Deliverable:** `runs/missing_schemas/schemas_raw.csv`.

---

## WP4 — Novelty deduplication and canonicalization
Apply deterministic dedupe rules to avoid overcounting:
1. alpha-equivalent schema collapse,
2. definitional-equality collapse,
3. path-equivalence class collapse when appropriate to counting regime.

Track both raw and canonical counts.

**Deliverable:**
- `runs/missing_schemas/schemas_canonical.csv`
- `runs/missing_schemas/dedupe_report.md` (raw count, canonical count, ratio, examples).

---

## WP5 — Compute mechanical `\Delta\nu` and pass/fail verdict
For each route/`\kappa_{\mathrm{scalar}}`:
1. compute canonical `\Delta\nu_mech`,
2. compute `\nu_{new} = 46 + \Delta\nu_mech`,
3. compute `\rho_{new} = \nu_{new}/(7+\kappa_{\mathrm{scalar}})`,
4. compare against bar 5.99.

Include confidence bands if counting regime has ambiguity classes.

**Deliverable:** `runs/missing_schemas/verdicts.csv` and `runs/missing_schemas/summary.md`.

---

## WP6 — Cross-check against false-positive pathways
To avoid accidental inflation:
1. run scalar-only and metric-only controls,
2. ensure counted schemas are genuinely interaction-generated,
3. verify no schema is already derivable from `L_base` via previously counted rules.

**Deliverable:** control comparison table and rejection list for invalid credits.

---

## WP7 — Route comparison and decision output
Compare at least these paths under identical methodology:
1. Cauchy route (current A1/A2),
2. at least one low-`\kappa` alternative (e.g., topological compression or synthetic continuum insertion candidate).

For each route report:
- `\kappa_{\mathrm{scalar}}`
- `\Delta\nu_{\min}`
- `\Delta\nu_{\mech}`
- pass/fail margin
- complexity/risk notes.

**Deliverable:** route ranking recommendation for manuscript integration.

---

## Evidence schema (for reproducibility)
Each run should emit:
- `manifest.json` (inputs, commit SHA, mode flags, route, bars)
- `targets.csv`
- `schemas_raw.csv`
- `schemas_canonical.csv`
- `verdicts.csv`
- `summary.md`

All files under `runs/missing_schemas/<run_id>/`.

---

## Quality gates

### Gate Q1 — Mechanical integrity
No manual schema additions allowed. Any schema without machine witness is excluded.

### Gate Q2 — Counting integrity
Canonical dedupe pass completed; raw/canonical delta documented.

### Gate Q3 — Policy integrity
All verdicts use strict V1 denominator `7+\kappa_{\mathrm{scalar}}` and bar 5.99.

### Gate Q4 — Comparative integrity
At least one alternate route compared to Cauchy under same pipeline.

---

## Manuscript-facing outputs (when ready)
1. New subsection: “Mechanical discovery of missing interaction schemas under strict V1”.
2. Table: per-route `\kappa`, `\Delta\nu_{min}`, `\Delta\nu_{mech}`, final `\rho`, bar margin.
3. Appendix artifact index pointing to run manifests and schema evidence files.

---

## Immediate implementation steps
1. Create `runs/missing_schemas/` scaffold + target table generator.
2. Add an engine-side extraction command (or script wrapper) that emits raw interaction schema candidates for `L_base/L_metric/L_scalar/L_combined`.
3. Implement canonical dedupe pass and verdict calculator.
4. Run first full Cauchy-route experiment and produce `summary.md` with pass/fail.
5. Run one alternate low-`\kappa` route using identical pipeline for comparison.

---

## Final decision criterion
Workstream A is resolved only when at least one strict-V1 route provides:
\[
\Delta\nu_{\mech} \ge \Delta\nu_{\min}
\]
with full reproducible evidence, or else the sequence is revised and documented as such.
