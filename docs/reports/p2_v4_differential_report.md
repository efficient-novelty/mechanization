# Phase 2 V4 Differential Report (Canonical Quotient OFF vs ON)

- date_utc: 2026-02-28
- script: `engine/scripts/run_phase2_canonical_differential.sh`
- run_dir: `runs/phase2_differential/p2v4_medium`
- command: `engine/scripts/run_phase2_canonical_differential.sh runs/phase2_differential/p2v4_medium`

## Configuration

- lib_step: 3
- max_bit_budget: 16
- max_entries: 2
- max_ast_depth: 2
- max_candidates: 120
- target_reduction_pct: 40.00%

## Results

- raw_candidates (OFF baseline): 120
- canonical_candidates (ON): 62
- reduction_pct: 48.33%
- meets_target: true

## Canonical key sample (ON frontier)

- 68a9ccd7
- e3226337
- 87da2df9
- f6496ac0
- 2247716d

## Artifact pointers

- `runs/phase2_differential/p2v4_medium/frontier_counts.csv`
- `runs/phase2_differential/p2v4_medium/report.md`
