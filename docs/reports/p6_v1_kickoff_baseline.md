# Phase 6 V1 Kickoff Baseline

Date: 2026-02-28  
Scope: Kick off Phase 6 (Agda alignment and formal contracts) with explicit verification-split objectives and one-shot package sequencing.

## Why this kickoff exists

Phase 5 closed decoder reporting with non-interference guards. Phase 6 must now harden the independent-verification shield: Haskell discovery outputs should be checkable via Agda-facing bridge artifacts.

## Baseline at kickoff

- Phase 5 closure artifacts are present (`p5_v2`, `p5_v3`, `p5_v4` reports).
- Agda bridge exists, but payload schema for full anonymous AST + ν-claim verification is not yet complete.
- Workflow guardrails and evidence tooling are in place and can be reused for bridge-schema hardening.

## One-shot package plan (Phase 6)

1. **P6-WP1 (this report):** kickoff baseline + package decomposition.
2. **P6-WP2:** implement bridge payload/schema extensions for canonical metadata + anonymous AST + ν claims.
3. **P6-WP3:** add Agda harness checks/proof skeleton validations for updated payloads.
4. **P6-WP4:** complete discovery-vs-verification split sign-off and phase closure.

## Acceptance bars for follow-on packages

- **P6-WP2 bar:** deterministic bridge schema emitted and validated by reproducible checks.
- **P6-WP3 bar:** Agda-side tests validate updated payload schema and contract assumptions.
- **P6-WP4 bar:** independent verification pathway documented and green in CI.

## Validation commands run for kickoff update

```bash
engine/scripts/check_no_conflict_markers.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Conclusion

Phase 6 is now formally in progress with a concrete execution plan centered on independent Agda verification of Haskell discovery claims.
