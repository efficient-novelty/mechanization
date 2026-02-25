# PEN Engine Issue Queue

This queue translates current architecture findings and roadmap notes into executable work items with clear acceptance criteria.

## Priority Definitions

- **P0**: Blocks scientific validity or introduces claim-risk in current paper narrative.
- **P1**: Improves discovery quality, runtime, and reproducibility but does not block core claim.
- **P2**: Scale-up, polish, and long-horizon verification tasks.

---

## Completed (reference only)

P0 (all), P1 (all), P1.5-P1.8, P2.1-P2.3 done. Engine discovers 15/15 canonical
structures in correct order. 12/15 exact ν match, 15/15 exact κ match. Total ν=359
(paper 356), total κ=64 (paper 64). 46 acceptance tests (A-H). Exclusion contract
printed in every structural run. Agda bridge generates 15 deterministic stub files.

**Step A (publication truth mode)**: DONE. MCTS paper-value leak fixed, structural
mode is fully paper-independent end-to-end. Claim profile prints at run end.

**Step H (exclusion contract)**: DONE. Formal "what PEN does not derive" contract
printed at end of structural runs. 4 acceptance tests (H1-H4) validate no empirical
physics constants in selection path. CI updated to 46 tests.

---

## ~~P1.6 — Paper table generation~~ (DONE)

`scripts/gen_latex_table.sh` generates full §1 table from engine structural mode.
One command, zero hand-maintained values.

---

## ~~P1.7 — Ablation mode~~ (DONE)

Two ablation modes: `--no-canonical-priority` and `--max-rho`. Three-way comparison
documented in physics_creation.md. Key result: canonical priority is structurally
necessary (1/15 and 2/15 exact match in ablation modes vs 12/15 normal).

---

## ~~P1.8 — Acceptance test suite~~ (DONE)

46 tests in `cabal run acceptance`: bootstrap bar sensitivity (A), Pi ν>0 (B),
Trunc anti-explosion (C), DCT meta-theorems (D), all 15 κ values (E), full
sequence golden test (F), all 15 canonical names (G), exclusion contract (H). All pass.

---

## ~~P2.1 — Agda Rosetta bridge~~ (DONE)

`cabal run agda-bridge` generates 15 Cubical Agda postulate stubs from discovered
telescopes. Each stub has correct library imports (via Lib references), de Bruijn
variable resolution, and MBTT provenance comments. Output is deterministic
(`--check` verifies). Supports `--step N`, `--output-dir DIR`, `--stdout` flags.
Files written to `agda/bridge/PEN/Genesis/`.

---

## ~~P2.2 — CI pipeline~~ (DONE)

`.github/workflows/pen-engine.yml` — GitHub Actions workflow that runs on push/PR
to engine/: build all, acceptance tests (46), structural discovery (15 steps,
correct names, κ=64), paper-calibrated (ν=356, κ=64). Uploads CSV artifacts.

---

## ~~P2.3 — Public benchmark pack~~ (DONE)

`scripts/benchmark.sh` — one-command runner, 4 profiles (paper replay, structural
discovery, d-window sweep, 46 acceptance tests). Outputs REPORT.txt with pass/fail,
git hash, and all CSV/log artifacts. All 4 profiles pass.

---

## All items complete

The issue queue is fully resolved. All P0, P1, and P2 tasks are done.

### Summary of deliverables

| Item | Status | Key artifact |
|------|--------|-------------|
| P0 (core claims) | DONE | 15/15 structural, 12/15 exact ν match |
| P1.6 (LaTeX table) | DONE | `scripts/gen_latex_table.sh` |
| P1.7 (Ablation) | DONE | `--no-canonical-priority`, `--max-rho` |
| P1.8 (Acceptance) | DONE | 46 tests, `cabal run acceptance` |
| Step H (Exclusion) | DONE | Exclusion contract in claim profile |
| P2.1 (Agda bridge) | DONE | `cabal run agda-bridge`, 15 stubs |
| P2.2 (CI) | DONE | `.github/workflows/pen-engine.yml` |
| P2.3 (Benchmarks) | DONE | `scripts/benchmark.sh` |
