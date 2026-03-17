# Evidence and Invariants

## Why This File Matters

The rewrite should not only port code. It should port the claim boundary.

This repo already contains hard-won invariants around:

- name independence
- canonicalization stability
- reporting non-interference
- Agda verification split
- pruning and quotienting evidence

These should become first-class tests in Rust.

## Core Invariants to Preserve

### Name-free hot path

Search and evaluation should not depend on semantic labels or target names.

Evidence:

- `docs/reports/p3_v4_name_independence_report.md`

Key hardening already done:

- step-15 temporal capability is now detected structurally from `Next` and `Eventually`, not from `name == "DCT"`.

### Canonicalization is weak but stable

The current canonicalization contract is intentionally conservative and should remain deterministic.

Evidence:

- `docs/adr/0002-mbtt-canonicalization-contract.md`
- `docs/reports/p2_v4_differential_report.md`

Observed evidence:

- canonical quotienting reduced a bounded frontier by `48.33%`, above the repo's target threshold.

### Native `nu` invariance

The evaluator-facing native `nu` API should preserve output under:

- alpha-equivalent renaming
- canonical-expression rewrites

Evidence:

- `docs/adr/0003-native-nu-trace-contract.md`
- `docs/reports/p3_v3_invariance_report.md`
- `docs/reports/p3_exit_audit.md`

### Reporting must be non-interfering

Post-hoc decoding and reporting are allowed only if they cannot affect selection.

Evidence:

- `engine/src/MBTTDecode.hs`
- `docs/reports/p5_v3_reporting_integration_report.md`
- `docs/reports/p5_v4_non_interference_signoff.md`

### Discovery and verification must be split

Agda export and checking belong on a separate contract surface.

Evidence:

- `docs/reports/p6_v2_bridge_schema_report.md`
- `docs/reports/p6_v3_agda_harness_report.md`
- `docs/reports/p6_v4_verification_split_signoff.md`

## Baseline and Ablation Lessons

### Phase 0 baseline

`runs/phase0_baseline/README.md` records the pre-MBTT-first snapshot:

- structural discovery: 15 out of 15
- 12 out of 15 exact
- `d = 2` beats `d = 1` and `d = 3`

This is useful as historical evidence, but the new rewrite should not lock itself to every old number in those scripts because some later strict behavior changed.

### Review hardening ablations

`runs/review_hardening/ablation_report.md` is the most useful operational evidence file.

Key takeaways:

- greedy `max-rho` diverges at step 6
- linear topological bonus fails by step 7
- no topological dimension bonus fails by step 5
- baseline strict lane uses atomic MBTT only for steps 1 to 3 and molecular candidates from step 4 onward

This means the selection objective and topological accounting are not incidental details. They materially change the recovered sequence.

## Evidence Lanes Worth Recreating in Rust

The current repo's `docs/phase1_evidence_contract.md` divides evidence into:

- core invariants
- bounded MBTT smoke
- full MBTT acceptance
- ab-initio replay
- ladder and prefix regression gates

A Rust rewrite should have equivalent lanes:

1. core property and invariant tests
2. bounded search smoke tests
3. deterministic replay tests
4. checkpoint and resume tests
5. Agda export and verification smoke tests

## Good Rust Test Targets

Port or recreate checks for:

- exact atom schema freeze
- reference telescope corpus stability
- canonical key idempotence
- name-free structural capability detection
- native `nu` trace-key presence
- SCC minimality on packed bundles
- deterministic selection under repeated replay
- decoder non-interference
- bridge payload schema conformance

## What the New Repo Should Prove Early

1. Same input config gives same selected step sequence.
2. Checkpoint and resume do not alter outcomes.
3. Post-hoc reporting cannot affect hot-path ranking.
4. Agda export is deterministic from accepted steps.
5. Atomic-only search either recovers steps or fails with inspectable frontier evidence.

## Primary Source Files

- `docs/phase1_evidence_contract.md`
- `docs/adr/0002-mbtt-canonicalization-contract.md`
- `docs/adr/0003-native-nu-trace-contract.md`
- `docs/reports/p2_v4_differential_report.md`
- `docs/reports/p3_v3_invariance_report.md`
- `docs/reports/p3_v4_name_independence_report.md`
- `docs/reports/p5_v4_non_interference_signoff.md`
- `docs/reports/p6_v4_verification_split_signoff.md`
- `docs/reports/p7_v4_pruning_signoff.md`
- `runs/review_hardening/ablation_report.md`
