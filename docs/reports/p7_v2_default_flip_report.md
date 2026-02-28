# Phase 7 V2 Default Flip Report

Date: 2026-02-28  
Scope: Complete P7-WP2 by flipping runtime default behavior to MBTT-first while preserving rollback fallback guardrails.

## Changes implemented

- Updated `RunAbInitio` configuration parsing to make MBTT-first the default path when no explicit fallback is requested.
- Added explicit rollback switch parsing:
  - `--legacy-generator` forces legacy template-first generator behavior.
- Added Phase-7 deprecation signaling in runtime output when legacy fallback is selected:
  - warns that `--legacy-generator` is deprecated and intended only for rollback.
- Added `cfgLegacyGenerator` configuration field so fallback mode is explicit and auditable.
- Added guard script `engine/scripts/check_phase7_default_mode.sh` to enforce:
  - default/fallback parse wiring in `RunAbInitio.hs`,
  - roadmap completion marker for P7-WP2,
  - presence/content of this report.
- Wired the new guard into CI workflow and workflow-consistency checks.

## Validation commands

```bash
engine/scripts/check_phase7_default_mode.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/check_no_conflict_markers.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Observed outcomes

- Phase-7 default-mode guard: OK.
- Workflow consistency guard: OK.
- Conflict marker check: OK.
- Evidence tooling self-check: OK.

## Conclusion

P7-WP2 is complete: MBTT-first is now the default runtime path, while `--legacy-generator` remains available as an explicit deprecated rollback fallback with CI-enforced contract checks.
