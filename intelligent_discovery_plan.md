# Intelligent Discovery Plan (Updated 2026-03-07)

## Goal

Make discovery behave like obligation-driven mechanized mathematics, not brute-force syntax farming, while preserving strict autonomy constraints:

1. No paper-value substitution in strict mode.
2. No step-index/template injection.
3. No canonical-name unlocking in search/ranking critical paths.

## Current Verified Checkpoint

Command used:

```powershell
cabal run exe:ab-initio -- --strict --phase1-shadow --max-steps 7 --prefix-report prefix_final_check.csv +RTS -N -RTS
```

Observed sequence:

1. Universe
2. Unit
3. Witness
4. Pi
5. S1
6. Trunc
7. S2

Status:

1. Step ordering (1-7) now matches the intended prefix.
2. Representation quality is still below target at steps 6-7:
   1. Step 6 currently tends toward `nu=6, kappa=2` (expected profile closer to `nu=8, kappa=3`).
   2. Step 7 currently tends toward `nu=8, kappa=2` (expected profile closer to `nu=10, kappa=3`).

## Key Learnings Kept

1. The main failure mode at step 6 was not only ranking: valid truncation states were being dropped when closed but non-terminal and stalled.
2. Bridge-first HIT hedging (when loops exist but truncation is absent) is high-impact and structurally justified.
3. Depth-2 novelty at step-6 HIT progression inflated scores and distorted selection; depth-1 is safer in this region.
4. Floating path-only candidates must be rejected early; they create degenerate high-rho shortcuts.
5. Step-7 drift was often tie behavior after truncation entered the library; late tie-breaks must prefer non-redundant representational progress.
6. Large debt-semantic changes can regress earlier bootstrap steps; maintain strict prefix regression checks after each change.

## What Is No Longer Priority

1. Broad architecture rewrites before stabilizing step-6/7 quality.
2. Deep MCTS-first changes for claim-grade path.
3. Legacy rescue-style fallback growth.
4. Expanding historical phase logs in this file.

## Next Execution Roadmap

## Phase A: Lock Baseline and Regression Harness (Immediate)

Objective: Prevent regressions while iterating representation quality.

Tasks:

1. Keep one canonical strict-prefix command and record outputs in CI artifacts.
2. Add assertions for expected discovered names at steps 1-7 in strict shadow mode.
3. Keep 30-second runtime cap per test run and use `+RTS -N`.

Acceptance:

1. Steps 1-7 names remain stable on each commit.
2. Prefix run stays below 30s on this machine profile.

## Phase B: Step-6 Representation Quality (Trunc) (High Priority)

Objective: Raise step-6 truncation from shortcut form to structurally richer construction without target injection.

Tasks:

1. Strengthen structural completeness preferences for truncation-like candidates:
   1. favor candidates that discharge bridge debt with explicit intro/interaction structure,
   2. disfavor minimal unary-trunc-only forms once richer bridge forms are viable.
2. Improve action/debt scoring so truncation acts as bridge infrastructure, not final shortcut.
3. Keep all scoring structural (capabilities/obligations/proof debt), no name hints.

Acceptance:

1. Step 6 remains `Trunc`.
2. Step-6 `kappa` and `nu` move toward expected profile (target band: `kappa >= 3`, higher `nu` than current).

## Phase C: Step-7 Representation Quality (S2) (High Priority)

Objective: Preserve S2 at step 7 while eliminating low-effort residual candidate wins.

Tasks:

1. Tighten lift-quality constraints for post-trunc HIT progression:
   1. require explicit geometric progression evidence rather than residual bridge reuse.
2. Refine tie-breaks among bar-clearers to reward non-redundant structural lift.
3. Keep floating-path and redundant-trunc protections in place.

Acceptance:

1. Step 7 remains `S2` across repeated runs.
2. Step-7 representation trends toward `kappa >= 3` and improved `nu`.

## Phase D: Extend Prefix to Step 8-9 Under Runtime Discipline

Objective: After step-6/7 quality stabilizes, extend reliable prefix discovery.

Tasks:

1. Run strict shadow to steps 8-9 with 30-second per-run guard.
2. Use diagnostics-first tuning (near misses, sigma prunes, dominance prunes).
3. Prioritize pruning/retrieval quality over widening enumeration caps.

Acceptance:

1. Stable 1-9 naming prefix in strict mode.
2. Runtime remains within practical limits (first target: stable under current per-run cap; then optimize toward sub-60s aggregate benchmark).

## Phase E: Claim-Grade Guards and Invariance

Objective: Make autonomy claims auditable.

Tasks:

1. CI leakage guards:
   1. forbidden source/symbol checks,
   2. strict-module import fences.
2. Runtime invariance tests:
   1. name scrambling,
   2. premise order perturbation,
   3. macro-id shuffle.

Acceptance:

1. Strict selection/output invariant up to canonical quotient under perturbations.
2. CI blocks any regression in leakage constraints.

## Active Metrics

1. Prefix correctness by step.
2. Step-6 and step-7 `nu/kappa` quality.
3. Raw vs viable candidate density.
4. Sigma and dominance prune ratios.
5. Near-miss top reasons.
6. Wall-clock runtime and memory peak.
