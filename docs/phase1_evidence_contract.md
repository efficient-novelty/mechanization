# Phase 1 Evidence Contract and CI Artifact Policy

## Purpose

Define what counts as acceptable evidence for Phase 1 MBTT-first claims, while supporting resource-bounded local development and full-budget CI verification.

## Evidence lanes

### Lane A — Core invariants (required)
- Command: `cabal run acceptance-core`
- Scope: Suites A–I (bootstrap, κ consistency, structural invariants, exclusion contract, MBTT-first contract canary checks)
- Gate: must pass 100%

### Lane B — MBTT bounded smoke (required on PR)
- Command: `cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates 50`
- Scope: MBTT enumerator sanity checks (J-lane under bounded budgets)
- Gate: must pass 100%

### Lane C — MBTT full-budget (required for strong autonomy evidence)
- Command: `cabal run acceptance-mbtt`
- Scope: Full J-lane including heavy reference-recovery checks
- Gate: must pass 100% on a sufficiently provisioned runner

### Lane D — MBTT-first ab-initio replay (required for discovery evidence)
- Command (full replay): `cabal run ab-initio -- --structural --mbtt-first --csv <run.csv> [--mbtt-max-candidates N]`
- Command (shadow replay): `cabal run ab-initio -- --structural --phase1-shadow --csv <shadow.csv> [--mbtt-max-candidates N]`
- Scope: end-to-end discovery trajectory + score outputs (full) and bounded first-stage evidence (shadow)
- Gate: run completes and emits replayable stepwise artifacts

### Lane E1 — MBTT shadow ladder telemetry (required on PR/main)
- Command: `TIMEOUT_S=45 MAX_CANDS=20 STEPS="1 2 3" engine/scripts/run_phase1_shadow_ladder.sh runs/phase1_ci/<run-id>/ladder`
- Scope: hardware-capability telemetry for bounded shadow replays, recorded as `ladder_status.csv` (pass/fail and rows emitted per horizon)
- Gate: command must complete and emit `ladder/ladder_status.csv`; failures in higher horizons are allowed but must be visible in artifact status rows

### Lane E2 — MBTT shadow ladder gate to step 6 (required on main for Phase-1 exit)
- Command: `TIMEOUT_S=90 MAX_CANDS=20 STEPS="1 2 3 4 5 6" REQUIRE_SUCCESS_THROUGH=6 engine/scripts/run_phase1_shadow_ladder.sh runs/phase1_ci/<run-id>/ladder-main`
- Scope: proves bounded MBTT shadow replay can complete each horizon through step 6 on provisioned runner
- Gate: command must exit 0 and emit `ladder-main/ladder_gate.txt` with `pass`; failures block main-branch strong Phase-1 evidence claims

## Reproducibility metadata requirements

Each CI run that claims Phase 1 evidence must include:
- commit SHA
- CI run id / attempt
- exact commands and flags used per lane
- toolchain versions (`ghc`, `cabal`)
- runner profile (OS + runner class)

## Artifact policy (`runs/phase1_*`)

Store artifacts under a run-scoped folder:
- `runs/phase1_ci/<run-id>/acceptance-core.log`
- `runs/phase1_ci/<run-id>/acceptance-mbtt-fast.log`
- `runs/phase1_ci/<run-id>/acceptance-mbtt-full.log` (if full lane executed)
- `runs/phase1_ci/<run-id>/abinitio_mbtt_structural.csv`
- `runs/phase1_ci/<run-id>/manifest.json`
- `runs/phase1_ci/<run-id>/ladder/ladder_status.csv`
- `runs/phase1_ci/<run-id>/ladder-main/ladder_status.csv` and `ladder-main/ladder_gate.txt` (main branch gate lane)

Retention defaults:
- PR runs: 14 days
- main branch runs: 30 days

## Evidence verification gate

CI must run `engine/scripts/verify_phase1_evidence.sh runs/phase1_ci/<run-id> <pr|main>` before artifact upload.
This enforces lane-specific required files and verifies that main-branch ladder gate emits `ladder-main/ladder_gate.txt` with `pass`.

## Replay contract

A third party should be able to replay evidence with only:
1. the commit SHA,
2. the commands listed in `manifest.json`,
3. the recorded artifacts in `runs/phase1_ci/<run-id>/`.

## Claim boundary

Phase 1 evidence demonstrates MBTT-first enumeration viability and bounded/full-lane reproducibility.
Strong autonomy claims remain conditioned on closing known Phase 3 evaluator name-dependence gaps documented in the roadmap.

### Local evidence helper (bundle mode)
- Script: `engine/scripts/run_phase1_evidence_bundle.sh`
- Purpose: produce a single local run directory that mirrors CI-required PR lanes (`acceptance-core`, bounded `acceptance-mbtt`, shadow replay, ladder telemetry) with `lane_status.csv` + `manifest.json`.
- Optional main-gate mode: set `RUN_MAIN_GATES=1` to additionally run full MBTT acceptance, full MBTT replay, and strict ladder gate (`REQUIRE_SUCCESS_THROUGH`).

### Local evidence helper (ladder mode)
- Script: `engine/scripts/run_phase1_shadow_ladder.sh`
- Purpose: run bounded shadow replays for a step ladder (e.g. 1..6) with per-step timeout and produce `ladder_status.csv` to show which horizons complete on current hardware.
- Optional strict mode: set `REQUIRE_SUCCESS_THROUGH=<N>` to require all steps `1..N` to succeed with at least one emitted CSV row; script exits non-zero and writes `ladder_gate.txt=fail` on violation.
