# Mechanical Missing-Schema Search — Step-wise Findings, Discussion, and Conclusion (Engine-native traces)

## Run context
- Plan executed: `find_missing_schemas.md`.
- Execution pipeline: `scripts/run_missing_schemas_pipeline.py`.
- Run artifact root: `runs/missing_schemas/20260301T072134Z/`.
- Policy used: strict V1 first-use full-charge.
- Step-13 bar used: 5.99.
- **Witness source:** engine-native `computeNativeNu` trace stream (`nnTrace`, `node=...|ctor=...`) from `engine/src/MBTTNu.hs`.

---

## WP1 — Baseline and deficit targets
### What was done
- Fixed three scalar-route candidates and charged `\kappa_{\mathrm{scalar}}` values:
  - `cauchy_minimal` (`\kappa_{\mathrm{scalar}}=4`),
  - `topological_arithmetic` (`\kappa_{\mathrm{scalar}}=3`),
  - `synthetic_continuum` (`\kappa_{\mathrm{scalar}}=2`).
- Computed strict V1 deficit targets:
  \[
  \Delta\nu_{\min}=\left\lceil 5.99\,(7+\kappa_{\mathrm{scalar}})-46\right\rceil.
  \]

### Findings
- `cauchy_minimal`: `\Delta\nu_{\min}=20`.
- `topological_arithmetic`: `\Delta\nu_{\min}=14`.
- `synthetic_continuum`: `\Delta\nu_{\min}=8`.

(Recorded in `targets.csv`.)

---

## WP2 — Combined library states
### What was done
- Built base library through Step 12 using `referenceTelescope` and `computeNativeNu` history accumulation.
- Evaluated three telescope states per route with engine-native evaluation:
  - `L_scalar` (route scalar telescope),
  - `L_metric` (Step 13 reference telescope),
  - `L_combined` (scalar entries prepended to metric entries).

### Findings
- Route metadata and execution context captured in `manifest.json`.
- Engine-native stdout/stderr captured for audit (`native_stdout.log`, `native_stderr.log`).

---

## WP3 — Mechanical schema enumeration and witness extraction
### What was done
- Parsed only engine-native node trace lines from `nnTrace` (`node=...|ctor=...`).
- Converted each node trace into schema signatures (`Ctor@Path`) and emitted witness rows.
- Marked schema membership against `L_metric`, `L_scalar`, and `L_combined` sets.

### Findings
- All schema witnesses are now grounded in the PEN evaluator trace contract, not synthetic placeholders.
- Route-level raw traces were generated and persisted to `schemas_raw.csv`.

---

## WP4 — Deduplication and canonicalization
### What was done
- Canonicalized by lowercase trace signature key.
- Collapsed duplicates per route for interaction-new schema sets.

### Findings
- Global counts:
  - raw rows total = 126,
  - canonical rows total = 75.
- Route-level dedupe ratios in verdicts remained `1.0` for interaction-new subsets.

(Recorded in `schemas_canonical.csv` and `dedupe_report.md`.)

---

## WP5 — Mechanical `\Delta\nu` and strict-V1 verdicts
### What was done
- Used engine-native totals to compute interaction novelty via inclusion–exclusion:
  \[
  \Delta\nu_{\mech}=\nu(L_{combined})-\nu(L_{metric})-\nu(L_{scalar}).
  \]
- Then computed strict-V1 post-charge efficiency:
  \[
  \rho_{new}=\frac{46+\Delta\nu_{\mech}}{7+\kappa_{\mathrm{scalar}}}
  \]
  and compared against 5.99.

### Findings
- `cauchy_minimal`: `\nu_scalar=6`, `\nu_metric=46`, `\nu_combined=53`, `\Delta\nu_{\mech}=1`, `\rho_{new}=4.272727`, **fail**.
- `topological_arithmetic`: `\nu_scalar=11`, `\nu_metric=46`, `\nu_combined=51`, `\Delta\nu_{\mech}=-6`, `\rho_{new}=4.0`, **fail**.
- `synthetic_continuum`: `\nu_scalar=4`, `\nu_metric=46`, `\nu_combined=50`, `\Delta\nu_{\mech}=0`, `\rho_{new}=5.111111`, **fail**.

(Recorded in `verdicts.csv`.)

---

## WP6 — False-positive controls
### What was done
- Counted node-trace cardinalities for scalar-only, metric-only, and combined sets per route.
- Enforced interaction-new gating (`combined \ setminus (metric ∪ scalar)`).

### Findings
- Example control counts:
  - `cauchy_minimal`: metric=31, scalar=14, combined=45.
  - `topological_arithmetic`: metric=31, scalar=9, combined=40.
  - `synthetic_continuum`: metric=31, scalar=10, combined=41.

(Recorded in `controls.csv`.)

---

## WP7 — Route comparison and recommendation
### What was done
- Compared all routes under identical strict-V1 denominator and bar.
- Ranked by margin to bar.

### Findings
- All routes **failed** in the engine-native trace-grounded run.
- Margins:
  - `synthetic_continuum`: `-0.878889` (best, but fail),
  - `cauchy_minimal`: `-1.717273`,
  - `topological_arithmetic`: `-1.99`.

(Recorded in `summary.md` and `verdicts.csv`.)

---

## Discussion
1. Replacing symbolic witnesses with engine-native trace evidence materially changed conclusions: the prior symbolic pass result did not hold under native `computeNativeNu` traces.
2. The measured interaction lift is currently too small (or negative) to recover Step 13 under strict V1 in tested routes.
3. This suggests either:
   - route telescopes need richer, evaluator-recognized interaction structure, or
   - the current composition model (`scalar ⊕ metric`) is not yet capturing the intended cross-couplings used by native novelty counting.
4. The current best route by margin is synthetic continuum, but still below bar.

---

## Conclusion
- All work packages were executed with engine-native derivation/inhabitation traces as requested.
- Under strict V1 and bar 5.99, none of the tested routes cleared the Step-13 bar in this engine-grounded run.
- Therefore, the mismatch remains an open discovery target; next work should focus on constructing route candidates whose AST interactions are recognized by `computeNativeNu` as genuine additional novelty.

