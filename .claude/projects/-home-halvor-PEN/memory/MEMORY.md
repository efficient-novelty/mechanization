# PEN Project Memory

## Project State (as of 2026-02-25)

- Engine: 15/15 canonical structures discovered in correct order
- 12/15 exact ν match, 15/15 exact κ match, total ν=359 (paper 356), κ=64/64
- All P0, P1, P1.6-P1.8, P2.1-P2.3 items DONE
- Forward Plan: Steps A-H ALL DONE
- ISSUE_QUEUE.md: ALL ITEMS COMPLETE
- 46 acceptance tests (A-H), all passing
- Exclusion contract prints at end of structural runs
- Agda bridge generates 15 deterministic stub files

## Key Architecture Patterns

- `EvalMode` has 3 variants: `EvalPaperCalibrated`, `EvalStrictComputed`, `EvalStructural`
- Structural mode is publication-grade (--structural flag)
- MCTS threads EvalMode through mctsSearch → mctsIteration → rolloutFromNode
- `StructuralNu` is paper-free — all computation from AST analysis
- Selection: canonical priority + minimal overshoot (both necessary per ablation)

## Executables

- `ab-initio` — main synthesis loop (--structural, --strict, --window N, --csv FILE)
- `acceptance` — 46-test regression suite (categories A-H)
- `agda-bridge` — Agda Rosetta bridge (--step N, --stdout, --check, --output-dir)
- `pen-engine` — full 10-phase analysis
- `uniform-nu` — schema-based nu computation

## Scripts

- `scripts/benchmark.sh` — 4-profile one-command verification
- `scripts/gen_latex_table.sh` — generate §1 LaTeX table from engine
- `scripts/repro_ab_initio.sh` — multi-mode replication harness
- `scripts/compare_runs.sh` — diff two run directories

## Key Learnings

- Canonical priority is structurally necessary (ablation shows 1/15 without it)
- MCTS rollout eval must respect EvalMode (was leaking paper values before fix)
- d=2 uniquely optimal: d=1 too flat, d=3 too fast
- Bar dynamics have zero tolerance for early ν overcount (cascade from step 6→8)
- Exclusion contract: PEN derives kinematic framework, NOT gauge groups/constants/dimension

## Workflow

- Always read ISSUE_QUEUE.md first → pick top unblocked task
- On completion: delete from ISSUE_QUEUE, update physics_creation.md
- `cd engine && cabal build all` to build, `cabal run acceptance` to test
