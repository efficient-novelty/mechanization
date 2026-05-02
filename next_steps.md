# Next Step Plan: Raw Structural Syntax And Typing

## Goal

Implement Phase 3 of `mechanization_plan.md`: formalize the fixed raw
extension calculus used by the paper, and separate raw written clauses, typed
structural roles, and algebraic payload fields before the later normalization
bridge.

## Files To Create

- `agda/Metatheory/RawStructuralSyntax.agda`
- `agda/Metatheory/RawStructuralTyping.agda`

## Preparation

1. Re-read the counted interfaces and sealed-layer surface:
   - `agda/Metatheory/InterfaceCalculus.agda`
   - `agda/Metatheory/TracePrinciple.agda`
   - `agda/Metatheory/CanonicalTelescope.agda`
   - `agda/Metatheory/TraceCostNormalForm.agda`
2. Re-read the structural obligation targets:
   - `agda/Metatheory/Obligations.agda`
   - `agda/Metatheory/KanSubsumption.agda`
   - `agda/Metatheory/ChronologicalWindow.agda`
3. Confirm local conventions for tiny finite syntax records and proof-relevant
   contracts. Prefer records with explicit fields over Boolean admissibility
   flags.

## Implementation Sketch

1. `RawStructuralSyntax.agda`
   - Define raw references: `LayerRef`, `NewPayloadRef`, `BasisSite`.
   - Define raw field records: `PayloadField`, `AlgebraicPayloadField`,
     `RawBoundary`.
   - Define `RawTelescope` as a small counted telescope over a field type.
   - Define `RawStructuralClause` with constructors:
     `act`, `cmp`, and `horn`.
   - Define `ExportPolicy` and `RawExtension` with payload, structural,
     algebraic, and export-policy components.
   - Export projection helpers:
     `raw-extension-payload-fields`,
     `raw-extension-structural-clauses`,
     `raw-extension-algebraic-fields`.

2. `RawStructuralTyping.agda`
   - Import `RawStructuralSyntax`.
   - Define the theorem-facing `LibraryState` parameter by reusing or wrapping
     the existing one from `InterfaceCalculus`.
   - Define proof-relevant predicates:
     `PayloadWellTyped`, `StructuralClausesWellTyped`,
     `AlgebraicWellTyped`, `SealingDerivation`, `OpacityRespected`,
     `ExportPolicySound`.
   - Define `AdmissibleRawExtension` with those fields.
   - Prove/export classification lemmas:
     `act-clause-has-unary-support`,
     `cmp-clause-has-binary-support`,
     `horn-clause-has-higher-boundary-support`,
     `algebraic-field-is-payload-not-structural-trace`,
     `naked-higher-face-rejected-or-packaged`.

## Design Constraints

- Do not claim this parses arbitrary Cubical Agda programs.
- Higher user operations should classify as algebraic payload unless they are
  explicitly packaged as typed structural integration traces.
- Avoid making the later horn-image theorem tautological: the syntax may admit
  higher-looking raw material, while the typing layer must explain why
  structural higher clauses are boundary/filler packages.
- Keep the modules theorem-facing and postulate-free.

## Acceptance Commands

```bash
cd agda
agda --transliterate Metatheory/RawStructuralSyntax.agda
agda --transliterate Metatheory/RawStructuralTyping.agda
cd ..
./scripts/check_coherence_depth_artifact.sh
```

## Plan Updates After Completion

- Remove Phase 3 from `mechanization_plan.md`.
- Move `RawStructuralSyntax.agda` and `RawStructuralTyping.agda` from gaps
  into the current baseline.
- Add the exported raw syntax and typing theorem names to
  `docs/theorem_index.md`.
- Replace this file with the Phase 4 surface normalization bridge plan.
