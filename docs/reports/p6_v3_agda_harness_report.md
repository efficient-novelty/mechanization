# Phase 6 V3 Agda Harness Report

Date: 2026-02-28  
Scope: Complete P6-WP3 by adding Agda-side payload contract harness and CI checks.

## Changes implemented

- Added `agda/Test/BridgePayloadContract.agda` as the Agda-side contract surface for bridge payload validation.
- Introduced Agda records that mirror bridge payload schema:
  - `NuClaim` (`nu-g`, `nu-h`, `nu-c`, `nu-total`)
  - `BridgePayload` (`step`, `name`, `canonical-key`, `kappa-bit`, `kappa-desugared`, `anonymous-ast`, `nu-claim`)
- Added explicit proof-obligation skeletons as Agda postulates:
  - `CanonicalKeySound`
  - `NuClaimWellFormed`
  - `DecodeNonInterference`
- Added `ContractWitness` to keep the obligation bundle type-checked as a stable harness interface.
- Added guard script `engine/scripts/check_phase6_agda_harness.sh` to validate that:
  - Agda harness module contains required schema/obligation tokens.
  - Harness field names stay aligned with bridge JSON exporter keys from `engine/src/AgdaExport.hs`.
- Wired the new harness check into CI workflow and workflow-consistency guard.

## Validation commands

```bash
engine/scripts/check_phase6_bridge_schema.sh
engine/scripts/check_phase6_agda_harness.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Observed outcomes

- Bridge schema guard: OK.
- Agda harness contract guard: OK.
- Workflow consistency guard: OK.
- Evidence tooling self-check: OK.

## Conclusion

P6-WP3 is complete: Agda now has an explicit bridge payload contract harness with proof-obligation skeletons, and CI enforces that harness/schema coupling remains intact while Phase-6 proceeds toward independent verification sign-off.
