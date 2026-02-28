# Phase 4 V1 Kickoff Baseline

Date: 2026-02-28  
Scope: Kick off Phase 4 (MBTT κ-primary optimization) with explicit baseline, work packages, and acceptance bars.

## Why this kickoff exists

Phase 3 is now closed with native-ν contracts and CI evidence lanes. Phase 4 now becomes the active optimization stage, where MBTT-first selection should prioritize bit-length complexity (`κ`) while preserving novelty/bar constraints.

## Baseline at kickoff

- Phase 3 closure evidence is present (`p3_exit_audit.md` + P3 reports).
- Current roadmap Phase 4 goals are defined, but implementation is not yet wired as κ-primary by default in scoring.
- Existing evidence tooling can already capture run-scoped artifacts and reports, which Phase 4 will reuse for additional κ telemetry.

## One-shot package plan (Phase 4)

1. **P4-WP1 (this report):** kickoff baseline and package decomposition.
2. **P4-WP2:** implement κ-first scoring in `RunAbInitio`/scoring bridge while preserving bar constraints.
3. **P4-WP3:** telemetry/schema rollout for required Phase-4 fields (`bit_kappa`, `ast_nodes`, `canonical_key`, `decoded_name?`).
4. **P4-WP4:** regression + ablation sign-off and phase completion flip.

## Acceptance bars for next packages

- **P4-WP2 bar:** deterministic bounded replay produces κ-first ordering and does not break acceptance suites.
- **P4-WP3 bar:** schema/verifier checks fail if required κ telemetry fields are missing.
- **P4-WP4 bar:** quality sign-off report demonstrates stable step growth and expected target behavior under κ-first scoring.

## Suggested execution commands for follow-on packages

```bash
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
engine/scripts/check_phase3_native_nu_fixtures.sh
```

(Additional Phase-4-specific scripts/lanes will be added by P4-WP2/P4-WP3 as implementation lands.)

## Conclusion

Phase 4 is now formally started with a defined one-shot plan and artifact expectations. The next concrete implementation target is P4-WP2 (κ-first scoring path).
