# Next Step Plan: Active-Basis Naturality

## Goal

Implement Phase 6 of `mechanization_plan.md`: show that active-basis coverage
is a consequence of global action totality, and keep that theorem separate
from the depth-two recurrence. The point of this phase is non-circularity:
coverage should entail the active-basis contract and the existing density
package, but coverage alone should not imply a depth-two window or Fibonacci
growth.

## Files To Create Or Update

- Create `agda/Metatheory/FiniteInterfaceBasis.agda`.
- Create `agda/Metatheory/GlobalActionSemantics.agda`.
- Create `agda/Metatheory/ActiveBasisContract.agda`.
- Create `agda/Test/ActiveBasisExamples.agda`.
- After the modules type-check, add the Phase 6 theorem names to
  `docs/theorem_index.md`.
- After completion, remove Phase 6 from `mechanization_plan.md`, move the new
  modules into the current baseline, and leave a dated completion note.

## Preparation

1. Re-read the counted interface and density surface:
   - `agda/Metatheory/InterfaceCalculus.agda`
   - `agda/Metatheory/CanonicityDensity.agda`
   - `agda/Metatheory/CanonicalTelescope.agda`
2. Re-read the bridge counting layer now available from earlier phases:
   - `agda/Metatheory/SurfaceNormalizationBridge.agda`
   - `agda/Metatheory/SurfaceToHornImage.agda`
3. Re-read the recurrence/window modules only to avoid accidentally depending
   on them in the coverage theorem:
   - `agda/Metatheory/ChronologicalWindow.agda`
   - `agda/Metatheory/UniversalRecurrence.agda`
   - `agda/Core/AffineRecurrence.agda`

## Implementation Sketch

1. In `FiniteInterfaceBasis.agda`, define a finite active-interface basis.
   - Keep the API proof-relevant: records for interface fields, transparent
     equivalence, and basis families.
   - Export `basis-families-exist`,
     `basis-family-cardinality-invariant`, and
     `basis-action-equivalence`.

2. In `GlobalActionSemantics.agda`, define the payload for a global action.
   - Model the advertised scope as the whole active interface.
   - Model totality as acting on every basis site.
   - Keep this layer free of chronological-window or recurrence imports.

3. In `ActiveBasisContract.agda`, connect totality to density.
   - Export `global-action-totality-implies-active-basis-contract`.
   - Reuse `Metatheory/CanonicityDensity.agda` where possible instead of
     restating density.
   - Export `active-basis-contract-entails-density`.

4. Add the non-circularity examples.
   - Provide a small coverage model with a UIP/depth-one collapse.
   - Provide a small coverage model with an artificial depth-three window.
   - Export `coverage-alone-does-not-imply-depth-two-window`.
   - Export `coverage-alone-does-not-imply-fibonacci`.

5. Add `agda/Test/ActiveBasisExamples.agda`.
   - Import the three new modules.
   - Alias every Phase 6 expected export so the smoke module catches missing
     public names.

## Acceptance Commands

```bash
cd agda
agda --transliterate Metatheory/FiniteInterfaceBasis.agda
agda --transliterate Metatheory/GlobalActionSemantics.agda
agda --transliterate Metatheory/ActiveBasisContract.agda
agda --transliterate Test/ActiveBasisExamples.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Guardrails

- Do not import the Fibonacci or exact-depth theorem into the proof of
  active-basis coverage.
- If a helper is only a finite counting lemma, keep it in
  `FiniteInterfaceBasis.agda`; if it mentions global actions, keep it in
  `GlobalActionSemantics.agda`.
- Record any new Cubical Agda warning site in `mechanization_plan.md`. The
  existing `UnsupportedIndexedMatch` warnings from `Obligations` and
  `TracePrinciple` remain expected.
