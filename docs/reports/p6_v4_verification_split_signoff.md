# Phase 6 V4 Verification Split Sign-off

Date: 2026-02-28  
Scope: Complete P6-WP4 by closing the discovery-vs-verification loop with CI-gated replay checks and explicit sign-off criteria.

## Verification split audit

- **Haskell discovery engine proposes anonymous MBTT candidates** and emits deterministic bridge artifacts.
- **Agda-facing bridge payload schema and harness validate exported claims** using a separate contract surface (`BridgePayloadContract`) and schema guards.
- CI now includes a required determinism lane: `cabal run agda-bridge -- --check` (named `Lane P6-V4 — agda-bridge determinism (required on PR/main)`).
- CI also enforces cross-checking guards:
  - `check_phase6_bridge_schema.sh`
  - `check_phase6_agda_harness.sh`
  - `check_phase6_verification_split.sh`

## Acceptance evidence

- Workflow wiring confirms the P6 determinism lane and verification-split guard are present.
- Roadmap Phase-6 checklist and exit criteria are marked complete and traceable to concrete guard scripts and report artifacts.
- P6 documentation now defines an explicit split: discovery proposes, verification independently checks exported claims.

## Validation commands

```bash
engine/scripts/check_phase6_bridge_schema.sh
engine/scripts/check_phase6_agda_harness.sh
engine/scripts/check_phase6_verification_split.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Observed outcomes

- Bridge schema guard: OK.
- Agda harness contract guard: OK.
- Verification split guard: OK.
- Workflow consistency guard: OK.
- Evidence tools self-check: OK.

## Conclusion

P6-WP4 is complete. Phase 6 is now closed with an explicit, CI-enforced discovery-vs-verification split and deterministic replay lane for bridge outputs.
