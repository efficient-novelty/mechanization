# Phase 7 V3 CI Migration Report

Date: 2026-02-28  
Scope: Complete P7-WP3 by hardening CI/acceptance migration so MBTT-first default lanes are primary while rollback remains deterministic and testable.

## Changes implemented

- Added a required rollback smoke lane in CI:
  - `Lane D1-rollback — legacy-generator shadow smoke (required on PR/main)`
  - command path: `cabal run ab-initio -- --structural --legacy-generator --phase1-shadow ...`
- Kept MBTT-first lanes as primary required discovery lanes (`Lane D1`, `Lane D2`, ladder lanes) while adding rollback parity coverage.
- Updated workflow manifest generation to include rollback lane metadata (`abinitio_legacy_shadow`) in run-scoped evidence manifests.
- Added `engine/scripts/check_phase7_ci_migration.sh` to enforce:
  - workflow includes rollback lane + migration guard step,
  - roadmap marks P7-WP3 complete,
  - this report artifact exists and carries completion markers.
- Updated workflow consistency checker to require both:
  - `Check Phase-7 CI migration + rollback lanes`
  - `Lane D1-rollback — legacy-generator shadow smoke (required on PR/main)`

## Validation commands

```bash
engine/scripts/check_phase7_default_mode.sh
engine/scripts/check_phase7_ci_migration.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/check_no_conflict_markers.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Observed outcomes

- Phase-7 default mode guard: OK.
- Phase-7 CI migration guard: OK.
- Workflow consistency guard: OK.
- Conflict marker check: OK.
- Evidence tooling self-check: OK.

## Conclusion

P7-WP3 is complete: CI now treats MBTT-first as the canonical default lane set while preserving a deterministic legacy-generator shadow smoke rollback check with explicit guard enforcement.
