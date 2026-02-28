# Phase 7 V1 Kickoff Baseline

Date: 2026-02-28  
Scope: Kick off Phase 7 (migration, hardening, cleanup) with explicit MBTT-first default migration packages and search-space evidence obligations.

## Why this kickoff exists

Phase 6 closed the discovery-vs-verification split with bridge determinism and sign-off guards. Phase 7 now shifts to operational migration: MBTT-first must become the default path with rollback safety, and pruning telemetry must be published to defend against combinatorial-explosion critiques.

## Baseline at kickoff

- Phase 6 closeout artifacts are present (`p6_v2`, `p6_v3`, `p6_v4` reports).
- CI already contains MBTT/Phase guardrails, fixture/schema checks, and evidence bundle verification gates.
- MBTT-first integration exists, but default-mode migration and explicit legacy deprecation contracts are not yet closed.

## One-shot package plan (Phase 7)

1. **P7-WP1 (this report):** kickoff baseline + migration package decomposition.
2. **P7-WP2:** flip runtime default to MBTT-first with explicit `--legacy-generator` fallback/deprecation policy.
3. **P7-WP3:** migrate CI/acceptance so MBTT-first default lanes are canonical while rollback checks remain deterministic.
4. **P7-WP4:** publish search-space reduction telemetry/sign-off (typed-validity rates, prune factors, compositional narrowing evidence).

## Acceptance bars for follow-on packages

- **P7-WP2 bar:** MBTT-first is default with explicit rollback path and deprecation messaging for legacy generator mode.
- **P7-WP3 bar:** CI and acceptance suites run green under default MBTT-first mode with rollback smoke checks.
- **P7-WP4 bar:** reviewer-facing telemetry report quantifies pruning and composition effects against raw bitstring-space attacks.

## Validation commands run for kickoff update

```bash
engine/scripts/check_no_conflict_markers.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Conclusion

Phase 7 is now formally in progress with a concrete migration plan focused on default-path hardening, rollback safety, and evidence-backed search-space reduction claims.
