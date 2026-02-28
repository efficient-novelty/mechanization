# Phase 2 V5 Quality Parity Report

- date_utc: 2026-02-28
- script: `engine/scripts/run_phase2_quality_parity.sh`
- run_dir: `runs/phase2_parity/p2v5`
- command: `engine/scripts/run_phase2_quality_parity.sh runs/phase2_parity/p2v5`

## Configuration

- max_steps: 2
- max_candidates: 20
- mode: `--structural --phase1-shadow`
- differential switch: `--no-canonical-quotient` (OFF baseline)

## Results

- parity_on_off: true
- golden_prefix_ok (steps 1..2): true

### Per-step parity

| step | on_name | on_nu | on_kappa | off_name | off_nu | off_kappa | parity |
|---:|---|---:|---:|---|---:|---:|:---:|
| 1 | Universe | 1 | 2 | Universe | 1 | 2 | yes |
| 2 | Unit | 1 | 1 | Unit | 1 | 1 | yes |

## Artifact pointers

- `runs/phase2_parity/p2v5/p2_v5_on.csv`
- `runs/phase2_parity/p2v5/p2_v5_off.csv`
- `runs/phase2_parity/p2v5/report.md`
