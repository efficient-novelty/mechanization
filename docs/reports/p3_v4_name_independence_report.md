# Phase 3 V4 Name-Independence Report

Date: 2026-02-28  
Scope: Close the I1 canary gap by removing label-sensitive temporal capability gating from native-ν evaluation path.

## Change implemented

- Replaced name-gated temporal capability assignment in `telescopeToCandidate`:
  - **Before:** `leHasTemporalOps = True` only when `name == "DCT"`.
  - **After:** `leHasTemporalOps = True` when telescope structure contains both `Next` and `Eventually` forms.

This removes the step-15 DCT name dependency for native-ν/structural scoring under scrambled library names.

## Validation target

1. Scrambling library names does not change structural/native ν across steps 1..15.
2. Existing MBTT acceptance remains green.

## Acceptance tests exercised

- `I1. [C1] StructuralNu name-free for steps 1-15` (updated from prior 1-14 canary form)
- MBTT suite including `J8`..`J13` to ensure native-ν parity/trace/invariance checks remain valid.

Commands run:

```bash
cd engine && cabal run acceptance-core
cd engine && cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates 80
```

Observed outcome:

- `I1` PASS with full 1..15 requirement.
- MBTT suite summary: `12 passed, 0 failed`.

## Conclusion

P3-V4 is satisfied for the current implementation:

- Temporal capability detection in native-ν path is now structure-driven, not label-driven.
- The prior I1 step-15 name-sensitivity canary gap is closed.
