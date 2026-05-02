# Next Step Plan: Canonical Telescope And Trace Cost Normal Forms

## Goal

Implement Phase 1 of `mechanization_plan.md`: the finite canonical telescope
and trace-cost normal-form layer needed by both the raw surface bridge and
future `mu` invariance.

## Files To Create

- `agda/Metatheory/CanonicalTelescope.agda`
- `agda/Metatheory/TraceCostNormalForm.agda`
- `agda/Test/PresentationInvariance/Smoke.agda`

## Preparation

1. Read these existing modules for names and finite-index conventions:
   - `agda/Core/Nat.agda`
   - `agda/Metatheory/Obligations.agda`
   - `agda/Metatheory/Refactoring.agda`
   - `agda/Metatheory/ComputationalReplacement.agda`
2. Confirm whether the repo already exposes a `Fin` type and finite sums.
   Prefer the existing local surface over importing new Cubical library
   machinery.
3. Check how `PrimitiveCost`, `HistoricalSupport`, and arity are represented
   today, then adapt the new normal form to those names.

## Implementation Sketch

1. `CanonicalTelescope.agda`
   - define a finite telescope record with a field count and field lookup;
   - expose cardinality and simple isomorphism/correspondence helpers;
   - keep the module syntactic and `--safe`.
2. `TraceCostNormalForm.agda`
   - define `TraceCostField` with support, arity, and primitive-cost status;
   - define `CanonicalTraceCostNormalForm`;
   - define the primitive and derived subtelescope views;
   - define `mu-of-trace-cost-normal-form` as the primitive trace count.
3. `Test/PresentationInvariance/Smoke.agda`
   - import the two new modules;
   - expose a few top-level aliases so future regressions fail early.

## Acceptance Commands

```bash
cd agda
agda --transliterate Metatheory/CanonicalTelescope.agda
agda --transliterate Metatheory/TraceCostNormalForm.agda
agda --transliterate Test/PresentationInvariance/Smoke.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Plan Updates After Completion

- Remove Phase 1 from `mechanization_plan.md`.
- Add the new modules to `docs/theorem_index.md` under the planned bridge
  section.
- Update this file to Phase 2: `PresentationEquivalence.agda` and
  `MuInvariance.agda`.
