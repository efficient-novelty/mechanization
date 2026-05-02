# Next Step Plan: Top-Level Integration

## Goal

Implement Phase 9 of `mechanization_plan.md`: make the completed bridge,
counting, sparse/full recurrence, and case-study work part of the repository's
theorem-facing surface.

## Files To Create Or Update

- Update `agda/PEN.agda`.
- Update `agda/Test/MetatheorySmoke.agda`.
- Update `agda/README.md`.
- Review and update `docs/theorem_index.md` if any top-level aliases need
  additional stable names.
- Review `scripts/check_coherence_depth_artifact.sh` after `PEN.agda` and the
  smoke module imports are expanded.

## Preparation

1. Re-read the public export style in `agda/PEN.agda`.
   - Keep any broad imports controlled with `using` if names would collide.
   - Pay special attention to `ActiveInterface`, which is defined in both
     `Metatheory.InterfaceCalculus` and `Metatheory.FiniteInterfaceBasis`.
2. Re-read the existing smoke aliases in `agda/Test/MetatheorySmoke.agda`.
   - Add aliases only for stable theorem-facing names.
   - Avoid duplicating the dedicated smoke surfaces unless the names should be
     visible through `PEN.agda`.
3. Re-read the modules that Phase 9 wants public:
   - `agda/Metatheory/SurfaceNormalizationBridge.agda`
   - `agda/Metatheory/SurfaceToHornImage.agda`
   - `agda/Metatheory/FiniteInterfaceBasis.agda`
   - `agda/Metatheory/GlobalActionSemantics.agda`
   - `agda/Metatheory/ActiveBasisContract.agda`
   - `agda/Metatheory/SparseDependencyRecurrence.agda`
   - `agda/Metatheory/FullCouplingEnvelope.agda`
   - `agda/CaseStudies/*.agda`

## Implementation Sketch

1. Extend `agda/PEN.agda` with public imports for the bridge modules.
   - Prefer the explicit list from `mechanization_plan.md`.
   - If importing `Metatheory.FiniteInterfaceBasis` causes an
     `ActiveInterface` collision, use a selective `using` list or rename.
   - Add case-study imports only if they should be part of the public theorem
     surface; otherwise keep them checked through the artifact script and index.

2. Extend `agda/Test/MetatheorySmoke.agda`.
   - Add aliases for canonical telescope and trace-cost normal-form exports.
   - Add aliases for presentation equivalence and `mu` invariance.
   - Add aliases for raw structural syntax, raw typing, normalization bridge,
     horn-image theorem, active-basis contract, sparse recurrence, full
     coupling, and selected case-study summaries if publicly imported.

3. Update `agda/README.md`.
   - Document the fixed raw extension-calculus boundary.
   - List the new bridge/counting modules and the case-study audit command.
   - Keep the limitation explicit: this is not an arbitrary Cubical Agda parser
     or elaboration theorem.

4. Re-run and adjust `docs/theorem_index.md`.
   - Confirm every new top-level smoke alias has a stable theorem-index row.
   - Confirm the case-study commands and audit command remain present.

5. Re-run `scripts/check_coherence_depth_artifact.sh`.
   - If the PEN import expansion creates name collisions, narrow imports rather
     than changing existing theorem names.
   - If new warnings appear in new modules, document them in
     `mechanization_plan.md`; otherwise leave the known warning caveat as-is.

## Acceptance Commands

```bash
cd agda
agda --transliterate PEN.agda
agda --transliterate Test/MetatheorySmoke.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
agda --transliterate Test/ActiveBasisExamples.agda
agda --transliterate Test/SparseRecurrenceSmoke.agda
agda --transliterate Test/ClutchingSmoke.agda
agda --transliterate Test/Fibonacci.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Guardrails

- Do not broaden the mechanized claim beyond the fixed raw extension calculus.
- Do not hide or suppress Cubical Agda warnings.
- Do not introduce `postulate` in theorem-facing modules.
- Preserve existing public names in `PEN.agda`; narrow imports around
  collisions instead of renaming established APIs.
- Keep the artifact script as the authoritative integration check.
