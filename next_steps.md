# Next Step Plan: Surface Normalization Bridge

## Goal

Implement Phase 4 of `mechanization_plan.md`: connect admissible raw
extensions from `RawStructuralSyntax` and `RawStructuralTyping` to canonical
trace presentations, without claiming to parse arbitrary Cubical Agda source.

## Files To Create

- `agda/Metatheory/SurfaceNormalizationBridge.agda`
- `agda/Test/SurfaceBridgeSmoke.agda`

## Preparation

1. Re-read the just-added raw surface:
   - `agda/Metatheory/RawStructuralSyntax.agda`
   - `agda/Metatheory/RawStructuralTyping.agda`
2. Re-read the canonical target machinery:
   - `agda/Metatheory/CanonicalTelescope.agda`
   - `agda/Metatheory/TraceCostNormalForm.agda`
   - `agda/Metatheory/PresentationEquivalence.agda`
   - `agda/Metatheory/MuInvariance.agda`
3. Re-read the sealed/public counting bridge:
   - `agda/Metatheory/InterfaceCalculus.agda`
   - `agda/Metatheory/TracePrinciple.agda`
4. Keep an eye on the existing `UnsupportedIndexedMatch` warning from
   `Metatheory/Obligations.agda`; do not hide it or introduce new warnings.

## Implementation Sketch

1. Define `CanonicalNormalizedSignature`.
   - Fields:
     - `payloadFields : CanonicalTelescope`
     - `traceFields : CanonicalTraceCostNormalForm`
   - Keep the level and historical-depth indices explicit enough that Phase 5
     can state horn-image theorems over the normalized trace fields.

2. Define raw-to-canonical counting helpers.
   - Convert `RawTelescope` field counts into `CanonicalTelescope` counts.
   - Map typed `act` roles to unary trace fields.
   - Map typed `cmp` roles to binary trace fields.
   - Map typed `horn` roles to higher derived trace fields backed by the
     packaged boundary.
   - Preserve algebraic payload fields as payload/canonical payload material,
     not structural traces.

3. Implement `normalizeRawExtension`.
   - Signature:
     ```agda
     normalizeRawExtension :
       (B : LibraryState ℓ) →
       (e : RawExtension ℓ) →
       AdmissibleRawExtension B e →
       CanonicalNormalizedSignature ...
     ```
   - The implementation may stay theorem-facing and syntactic, but it must
     consume the admissibility package rather than ignoring it.

4. Export the Phase 4 names:
   - `raw-extension-elaborates-to-candidate`
   - `raw-extension-normalizes-to-canonical-signature`
   - `raw-trace-normalizes-to-canonical-signature`
   - `normalize-preserves-support`
   - `normalize-preserves-arity`
   - `normalize-preserves-primitive-cost`
   - `normalization-respects-presentation-equivalence`
   - `normalized-signature-matches-counted-interface`

5. Add `agda/Test/SurfaceBridgeSmoke.agda`.
   - Import `SurfaceNormalizationBridge`.
   - Include a tiny smoke object if needed to force the main exports to
     elaborate.

## Design Constraints

- The bridge is for the fixed raw extension calculus only.
- Do not add `postulate`.
- Keep quotient/presentation operations explicit: flattening, rebundling,
  splitting, Sigma reassociation, Pi currying/uncurrying, exported equality
  transport, transparent aliases, and duplicate derived trace deletion should
  be visible through the existing presentation-equivalence machinery.
- Do not integrate into `PEN.agda` until the module and smoke test type-check.

## Acceptance Commands

```bash
cd agda
agda --transliterate Metatheory/SurfaceNormalizationBridge.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Plan Updates After Completion

- Remove Phase 4 from `mechanization_plan.md`.
- Move `SurfaceNormalizationBridge.agda` and `SurfaceBridgeSmoke.agda` from
  gaps into the current baseline.
- Add the exported Phase 4 theorem names to `docs/theorem_index.md`.
- Replace this file with the Phase 5 surface-to-horn image theorem plan.
