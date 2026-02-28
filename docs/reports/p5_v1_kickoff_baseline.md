# Phase 5 V1 Kickoff Baseline

Date: 2026-02-28  
Scope: Kick off Phase 5 (post-hoc semantic decoding) with explicit boundaries, package sequencing, and acceptance bars.

## Why this kickoff exists

Phase 4 is closed with κ-first optimization and telemetry contracts. Phase 5 introduces semantic decoding as a **reporting-only** layer and must preserve selection non-interference.

## Baseline at kickoff

- Phase 4 closeout artifacts are present (`p4_v2`, `p4_v3`, `p4_v4` reports).
- No dedicated `MBTTDecode` module has landed yet.
- Evidence tooling is already available to enforce schema/contract checks and will be extended for decode fields in later packages.

## One-shot package plan (Phase 5)

1. **P5-WP1 (this report):** kickoff baseline and package decomposition.
2. **P5-WP2:** add decoder API boundary (`MBTTDecode`) and starter fixtures/tests.
3. **P5-WP3:** integrate decode metadata into reporting surfaces only.
4. **P5-WP4:** run decoder non-interference sign-off and flip phase status to complete.

## Acceptance bars for follow-on packages

- **P5-WP2 bar:** deterministic decode API compiles and fixture tests pass.
- **P5-WP3 bar:** decoded metadata appears in reports/CSV without changing winner selection path.
- **P5-WP4 bar:** decoder-on/off comparison confirms identical winners under matched run configs.

## Validation commands run for kickoff update

```bash
engine/scripts/check_no_conflict_markers.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Conclusion

Phase 5 is now formally in progress with a one-shot execution plan and explicit non-interference boundary.
