# Phase 4 V2 κ-First Scoring Report

Date: 2026-02-28  
Scope: Complete P4-WP2 by implementing κ-first optimizer ranking in MBTT-first mode.

## Change implemented

- Updated `engine/src/RunAbInitio.hs` so MBTT-first runs explicitly advertise a κ-first objective in startup output.
- Updated candidate selection ordering (non-`--max-rho` path):
  - **Before:** canonical tier -> minimal overshoot `(ρ - bar)` -> κ -> name
  - **After (MBTT-first):** canonical tier -> **κ-first** -> minimal overshoot -> name
- Updated canonical-key quotient representative ranking:
  - **Before:** higher ρ -> lower κ -> source rank
  - **After (MBTT-first):** **lower κ** -> higher ρ -> source rank
- Preserved existing behavior for explicit max-ρ ablation (`--max-rho`), and preserved bar-viability filtering (`ρ >= bar` for steps > 2).

## Implementation touchpoints

- `engine/src/RunAbInitio.hs`
  - selection objective in step loop (`selectBest` ordering)
  - quotient representative selection (`quotientCandidates` / `betterCandidate`)
  - run banner messaging for MBTT-first objective

## Validation commands

```bash
engine/scripts/check_no_conflict_markers.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
cd engine && cabal build ab-initio
```

## Observed outcome

- Conflict marker guard: OK.
- Workflow consistency guard: OK.
- Evidence tooling self-check: OK.
- `cabal` build command could not run in this environment (`cabal: command not found`), so compile verification remains for CI/toolchain-enabled environments.

## Conclusion

P4-WP2 is complete at the code level: MBTT-first optimizer ranking now uses κ-first objective while retaining existing bar gating and ablation controls.
