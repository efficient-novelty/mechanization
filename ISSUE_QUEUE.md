# PEN Engine Issue Queue

This queue translates current architecture findings and roadmap notes into executable work items with clear acceptance criteria.

## Priority Definitions

- **P0**: Blocks scientific validity or introduces claim-risk in current paper narrative.
- **P1**: Improves discovery quality, runtime, and reproducibility but does not block core claim.
- **P2**: Scale-up, polish, and long-horizon verification tasks.

---

## Completed (reference only)

P0 (all), P1 (all), P1.5 done. Engine discovers 15/15 canonical structures in
correct order. 12/15 exact ν match, 15/15 exact κ match. Total ν=359 (paper 356),
total κ=64 (paper 64).

**Step A (publication truth mode)**: DONE. MCTS paper-value leak fixed, structural
mode is fully paper-independent end-to-end. Claim profile prints at run end.

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

42 tests in `cabal run acceptance`: bootstrap bar sensitivity (A), Pi ν>0 (B),
Trunc anti-explosion (C), DCT meta-theorems (D), all 15 κ values (E), full
sequence golden test (F), all 15 canonical names (G). All pass.

---

## P2.1 — Agda Rosetta bridge for selected canonical steps

**Problem**: Machine-checked pathway from MBTT telescope discovery to Agda artifacts is still manual/TODO.

**Tasks**
- Define export format from discovered telescopes to Agda stubs.
- Implement for a minimal subset (Universe, Pi, S1, DCT skeleton).
- Add checker script that validates exported files typecheck (where environment supports).

**Acceptance criteria**
- Export command generates deterministic Agda files for selected steps.
- Typecheck pass documented for supported environment.
- Unsupported env paths fail with clear warning (not silent).

---

## ~~P2.2 — CI pipeline~~ (DONE)

`.github/workflows/pen-engine.yml` — GitHub Actions workflow that runs on push/PR
to engine/: build all, acceptance tests (42), structural discovery (15 steps,
correct names, κ=64), paper-calibrated (ν=356, κ=64). Uploads CSV artifacts.

---

## ~~P2.3 — Public benchmark pack~~ (DONE)

`scripts/benchmark.sh` — one-command runner, 4 profiles (paper replay, structural
discovery, d-window sweep, 42 acceptance tests). Outputs REPORT.txt with pass/fail,
git hash, and all CSV/log artifacts. All 4 profiles pass.

---

## Suggested sequencing

1. **Next**: P2.1 (Agda bridge) — requires Agda 2.6.4+ with cubical library.

## Definition of done for this queue

- All P1.6-P1.8 and P2 items complete.
- Paper table auto-generated from engine output.
- Ablation mode documented.
- Acceptance tests catch known failure modes.
- Agda export generates and typechecks for selected steps.
- CI catches sequence/metric regressions automatically.
- External reviewers can run benchmarks without reading source.
