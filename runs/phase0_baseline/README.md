# Phase 0 Baseline Snapshot

**Date:** 2026-02-25
**Git:** 8e2f548 (main)
**Engine:** pen-engine 0.1.0.0, GHC 9.6.7
**Purpose:** Pre-MBTT-first reference for regression comparison

## Summary

| Profile | Result |
|---------|--------|
| Paper-Calibrated Replay | PASS — 15/15 steps, ν=356, κ=64 |
| Structural Discovery (d=2) | PASS — 15/15 steps, 12/15 exact, ν=359, κ=64 |
| Coherence Window Sweep | PASS — d=2 (ν=359) > d=1 (347) > d=3 (195) |
| Acceptance Tests | PASS — 46/46 |

## Artifacts

| File | Description |
|------|-------------|
| `REPORT.txt` | Full benchmark report |
| `structural_d2.csv` | Primary structural mode (publication-grade) |
| `structural_d1.csv` | d=1 extensional stress test |
| `structural_d3.csv` | d=3 tribonacci stress test |
| `paper_calibrated.csv` | Paper-calibrated replay |
| `ablation_no_canonical.csv` | Ablation: canonical priority disabled |
| `ablation_max_rho.csv` | Ablation: pure max-ρ selection |
| `acceptance.log` | Full acceptance suite output |
| `*.log` | Full console output for each mode |

## CSV Schema

```
step,name,nu,kappa,rho,bar,delta,source,candidates,k_desugar,k_entry,k_bitcost
```

- `step`: Generative sequence index (1–15)
- `name`: Discovered structure name
- `nu`: Structural novelty (ν)
- `kappa`: Desugared complexity (κ)
- `rho`: Efficiency score (ν/κ)
- `bar`: Selection bar at step n
- `delta`: Integration latency (Fibonacci for d=2)
- `source`: Discovery source (ENUM, REF, MCTS)
- `candidates`: Number of candidates evaluated
- `k_desugar`, `k_entry`, `k_bitcost`: Alternative κ measures

## Reproducing

```bash
./scripts/benchmark.sh phase0_baseline
```

## Key Invariants (to hold across MBTT-first migration)

1. 15/15 structures discovered in correct order
2. All 15 canonical names recognized
3. Total κ = 64 (structural mode)
4. d=2 uniquely optimal in window sweep
5. Acceptance suite 46/46
