# Next Step Plan: Presentation Equivalence And Mu Invariance

## Goal

Implement Phase 2 of `mechanization_plan.md`: make the minimal opaque trace
cost `mu` invariant under an explicit finite set of presentation-equivalence
generators.

## Files To Create

- `agda/Metatheory/PresentationEquivalence.agda`
- `agda/Metatheory/MuInvariance.agda`
- `agda/Test/PresentationInvariance/RebundleRecord.agda`
- `agda/Test/PresentationInvariance/SplitShell.agda`
- `agda/Test/PresentationInvariance/CurryUncurry.agda`
- `agda/Test/PresentationInvariance/TransparentAlias.agda`
- `agda/Test/PresentationInvariance/DuplicateTrace.agda`

## Preparation

1. Read the Phase 1 modules:
   - `agda/Metatheory/CanonicalTelescope.agda`
   - `agda/Metatheory/TraceCostNormalForm.agda`
2. Re-read these existing modules for reusable proof surfaces:
   - `agda/Metatheory/Refactoring.agda`
   - `agda/Metatheory/ComputationalReplacement.agda`
   - `agda/Metatheory/TracePrinciple.agda`
3. Confirm which proof combinators for equality chains and isomorphism
   composition are already local, and reuse them instead of adding a new
   abstraction layer.

## Implementation Sketch

1. `PresentationEquivalence.agda`
   - define `PresentationStep` over `CanonicalTraceCostNormalForm`;
   - include generators for Sigma reassociation, record splitting/bundling,
     Pi curry/uncurry, transport fields, transparent alias insert/delete, and
     duplicate derived trace deletion;
   - define reflexive, symmetric, transitive `PresentationEquivalent`;
   - expose preservation lemmas for support and primitive cost.
2. `MuInvariance.agda`
   - define `TransparentlyGenerated` and `RequiresPrimitive`;
   - prove `mu-preserved-by-presentation-step`;
   - lift step preservation to `mu-invariant-under-presentation-equivalence`;
   - connect derived deletion to the Phase 1 primitive subtelescope and to
     `ComputationalReplacement` where possible.
3. Smoke tests
   - each test module should import the new theorem-facing modules and expose
     the specific generator/theorem for the named scenario;
   - keep examples small and finite so failures point at the intended surface.

## Acceptance Commands

```bash
cd agda
agda --transliterate Metatheory/PresentationEquivalence.agda
agda --transliterate Metatheory/MuInvariance.agda
agda --transliterate Test/PresentationInvariance/RebundleRecord.agda
agda --transliterate Test/PresentationInvariance/SplitShell.agda
agda --transliterate Test/PresentationInvariance/CurryUncurry.agda
agda --transliterate Test/PresentationInvariance/TransparentAlias.agda
agda --transliterate Test/PresentationInvariance/DuplicateTrace.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Plan Updates After Completion

- Remove Phase 2 from `mechanization_plan.md`.
- Move `PresentationEquivalence.agda`, `MuInvariance.agda`, and their smoke
  tests from gaps into the current baseline.
- Add the new theorem names to `docs/theorem_index.md`.
- Replace this file with the Phase 3 raw structural syntax and typing plan.
