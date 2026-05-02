# Next Step Plan: Sparse Dependency Recurrence And Full-Coupling Envelope

## Goal

Implement Phase 7 of `mechanization_plan.md`: formalize a sparse dependency
hierarchy where ordinary local extensions can have zero or finite footprints,
and the Fibonacci/affine depth-two law appears only as the fully coupled
endpoint.

## Files To Create Or Update

- Create `agda/Metatheory/SparseDependencyRecurrence.agda`.
- Create `agda/Metatheory/FullCouplingEnvelope.agda`.
- Create `agda/Test/SparseRecurrenceSmoke.agda`.
- After the modules type-check, add the Phase 7 theorem names to
  `docs/theorem_index.md`.
- Add the new Phase 7 modules to `scripts/check_coherence_depth_artifact.sh`.
- After completion, remove Phase 7 from `mechanization_plan.md`, move the new
  modules into the current baseline, and leave a dated completion note.

## Preparation

1. Re-read the recurrence/counting surface:
   - `agda/Metatheory/UniversalRecurrence.agda`
   - `agda/Metatheory/TraceCostNormalForm.agda`
   - `agda/Metatheory/InterfaceCalculus.agda`
2. Re-read the chronological-window and arithmetic endpoints:
   - `agda/Metatheory/ChronologicalWindow.agda`
   - `agda/Metatheory/TwoDFoundations.agda`
   - `agda/Core/AffineRecurrence.agda`
3. Re-read the Phase 6 active-basis modules only for terminology consistency:
   - `agda/Metatheory/FiniteInterfaceBasis.agda`
   - `agda/Metatheory/GlobalActionSemantics.agda`
   - `agda/Metatheory/ActiveBasisContract.agda`

## Implementation Sketch

1. In `SparseDependencyRecurrence.agda`, define the finite sparse footprint.
   - Introduce `CouplingFootprint n` with an explicit finite dependency
     surface over previous layers.
   - Introduce `SparseWindowedContext` with footprint, layer cost, and payload.
   - Export `sparse-windowed-recurrence`.

2. Add zero and sparse examples.
   - Export `transparent-growth-zero-footprint`.
   - Export `orthogonal-extension-zero-or-sparse`.
   - Export `orthogonal-extension-below-full-envelope`.
   - Keep ordinary orthogonal extensions modeled as sparse cases, not as
     counterexamples to the recurrence framework.

3. In `FullCouplingEnvelope.agda`, define the maximal full-coupling package.
   - Build on `Metatheory/UniversalRecurrence.agda`.
   - Define `full-coupling-envelope` as the footprint containing the whole
     active previous-window interface.
   - Export `full-coupling-specializes-sparse-recurrence`.

4. Connect the full endpoint to the existing depth-two affine law.
   - Export `full-coupling-depth-two-affine-law`.
   - Make clear in the types that the Fibonacci endpoint consumes a full
     coupling envelope plus the chronological-window reduction, not mere
     active-basis coverage.

5. Add `agda/Test/SparseRecurrenceSmoke.agda`.
   - Import both new modules.
   - Alias every Phase 7 expected export so missing public names fail fast.

## Acceptance Commands

```bash
cd agda
agda --transliterate Metatheory/SparseDependencyRecurrence.agda
agda --transliterate Metatheory/FullCouplingEnvelope.agda
agda --transliterate Test/SparseRecurrenceSmoke.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Guardrails

- Do not present sparse recurrence as Fibonacci growth.
- Do not import the Phase 6 active-basis contract as if it implied a
  chronological window.
- Keep the sparse/full distinction proof-relevant, with explicit footprints
  rather than Boolean tags.
- If a full-coupling proof needs a helper from `UniversalRecurrence.agda`,
  reuse it instead of restating counted-window arithmetic.
