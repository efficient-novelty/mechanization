# Phase 4 V3 Telemetry Rollout Report

Date: 2026-02-28  
Scope: Complete P4-WP3 by rolling out Phase-4 κ telemetry fields and schema enforcement.

## Changes implemented

- Extended `RunAbInitio` CSV schema with required Phase-4 columns:
  - `canonical_key`
  - `bit_kappa`
  - `ast_nodes`
  - `decoded_name?`
- Added AST node counting for MBTT telescopes in `RunAbInitio` (`teleAstNodes`/`exprNodeCount`) and used it to populate `ast_nodes`.
- Updated evidence verification (`engine/scripts/verify_phase1_evidence.sh`) to fail when required Phase-4 telemetry columns are missing in ab-initio CSV artifacts.
- Updated evidence summary (`engine/scripts/summarize_phase1_evidence.sh`) to surface κ telemetry values in summary output.
- Updated fixture self-check (`engine/scripts/test_phase1_evidence_tools.sh`) to include the new CSV columns.
- Updated evidence contract documentation (`docs/phase1_evidence_contract.md`) to state Phase-4 telemetry column requirements.

## Validation commands

```bash
engine/scripts/check_no_conflict_markers.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Observed outcomes

- Conflict marker guard: OK.
- Workflow consistency guard: OK.
- Evidence tooling self-check: OK with updated fixture data containing required Phase-4 telemetry columns.

## Conclusion

P4-WP3 is complete for the current repository state: Phase-4 telemetry columns are emitted, contract-checked, and visible in summaries.
