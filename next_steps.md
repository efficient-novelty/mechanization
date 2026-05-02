# Next Step Plan: Surface-To-Horn Image Theorem

## Goal

Implement Phase 5 of `mechanization_plan.md`: prove that admissible raw
structural clauses from the fixed extension calculus normalize into the
horn-generated structural obligation language. Keep the scope explicit: this
is a theorem about `RawStructuralSyntax`/`RawStructuralTyping`, not a parser or
elaborator for arbitrary Cubical Agda source.

## Files To Create Or Update

- Create `agda/Metatheory/SurfaceToHornImage.agda`.
- Update `agda/Test/SurfaceBridgeSmoke.agda` to import and expose the Phase 5
  names.
- After the module type-checks, add the new theorem names to
  `docs/theorem_index.md`.
- After completion, remove Phase 5 from `mechanization_plan.md`, move
  `SurfaceToHornImage.agda` into the current baseline, and leave a dated
  completion note.

## Preparation

1. Re-read the Phase 4 bridge:
   - `agda/Metatheory/SurfaceNormalizationBridge.agda`
2. Re-read the raw classification layer:
   - `agda/Metatheory/RawStructuralSyntax.agda`
   - `agda/Metatheory/RawStructuralTyping.agda`
3. Re-read the horn machinery:
   - `agda/Metatheory/KanSubsumption.agda`
   - especially `HornExtensionFiber`, `structural-horn-language`,
     `structural-integration-horn-reduction`, and
     `remote-layer-obligation-derived`.
4. Re-read the computational replacement and normal-form machinery:
   - `agda/Metatheory/TraceCostNormalForm.agda`
   - `agda/Metatheory/ComputationalReplacement.agda`
   - `agda/Metatheory/MuInvariance.agda`

## Implementation Sketch

1. Define a theorem-facing horn-image record for one typed raw structural role.
   It should expose:
   - the normalized trace-cost field from Phase 4;
   - whether the role is unary action, binary comparison, or packaged horn;
   - the historical support selected by the normalizer;
   - the primitive/derived cost selected by the normalizer.

2. Implement `surface-to-horn-normal-form`.
   - Pattern-match on `TypedStructuralRole`.
   - `unary-action-role`: connect to unary normalized support.
   - `binary-comparison-role`: connect to binary normalized support.
   - `horn-boundary-role`: package the existing typed boundary and mark the
     normalized field as derived.

3. Implement preservation exports:
   - `surface-to-horn-preserves-support`
   - `surface-to-horn-preserves-arity`
   - `surface-to-horn-preserves-primitive-cost`

4. Implement the higher-structural classification exports:
   - `higher-structural-fields-derived`
   - `higher-raw-structural-traces-derived`
   - `raw-syntax-no-naked-higher-structural-projections`

5. Implement completeness exports:
   - `horn-image-complete-for-structural-clauses`
   - `raw-structural-normalizes-to-horn`

6. Keep the non-tautology boundary visible.
   - Reuse `algebraic-field-is-payload-not-structural-trace`.
   - Reuse `naked-higher-face-rejected-or-packaged`.
   - Do not let arbitrary higher user operations become structural traces
     unless `RawStructuralTyping` packages them as a horn boundary.

## Acceptance Commands

```bash
cd agda
agda --transliterate Metatheory/SurfaceToHornImage.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Known Warnings To Track

The existing Cubical Agda `UnsupportedIndexedMatch` warnings from
`Metatheory/Obligations.agda` and `Metatheory/TracePrinciple.agda` may appear
when importing the Phase 4 bridge. Do not hide them. If Phase 5 adds any new
warning site, record it in `mechanization_plan.md`.
