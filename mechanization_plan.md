# Mechanization Plan For The Missing Coherence-Depth Agda Bridge

Date: 2026-05-02

Target paper: `1_coherence_depth.tex`

Source plan: `paper_improvement_plan.md`

This plan replaces the older "all complete" backlog with the remaining
Cubical Agda mechanization needed to make the paper improvement plan true.
The existing theorem-facing core already type-checks through `agda/PEN.agda`
and `agda/Test/MetatheorySmoke.agda`; the missing work is the surface bridge
and its supporting counted-interface machinery.

The goal is not to parse arbitrary Cubical Agda programs. The goal is to
mechanize the fixed raw extension calculus defined by the paper and prove that
admissible raw structural declarations normalize to the canonical
horn-generated trace interface used by the existing theorem stack.

## Current Baseline

Already present and usable:

- `agda/Metatheory/Obligations.agda`
- `agda/Metatheory/InterfaceCalculus.agda`
- `agda/Metatheory/Refactoring.agda`
- `agda/Metatheory/CanonicityDensity.agda`
- `agda/Metatheory/TracePrinciple.agda`
- `agda/Metatheory/ComputationalReplacement.agda`
- `agda/Metatheory/Extensional.agda`
- `agda/Metatheory/KanSubsumption.agda`
- `agda/Metatheory/UpperBound.agda`
- `agda/Metatheory/ChronologicalWindow.agda`
- `agda/Metatheory/ExactDepth.agda`
- `agda/Metatheory/TwoDFoundations.agda`
- `agda/Metatheory/TwoLTTFoundations.agda`
- `agda/Metatheory/AdjunctionBarrier.agda`
- `agda/Metatheory/UniversalRecurrence.agda`
- `agda/Metatheory/CanonicalTelescope.agda`
- `agda/Metatheory/TraceCostNormalForm.agda`
- `agda/Geometry/Clutching.agda`
- `agda/Core/AffineRecurrence.agda`
- `agda/Core/DepthOneAffine.agda`
- `agda/Test/MetatheorySmoke.agda`
- `agda/Test/ClutchingSmoke.agda`
- `agda/Test/Fibonacci.agda`
- `agda/Test/PresentationInvariance/Smoke.agda`

These checks currently pass:

```bash
cd agda
agda --transliterate PEN.agda
agda --transliterate Metatheory/CanonicalTelescope.agda
agda --transliterate Metatheory/TraceCostNormalForm.agda
agda --transliterate Test/MetatheorySmoke.agda
agda --transliterate Test/PresentationInvariance/Smoke.agda
agda --transliterate Test/ClutchingSmoke.agda
agda --transliterate Test/Fibonacci.agda
```

Known caveat: these checks emit Cubical Agda `UnsupportedIndexedMatch`
warnings in several theorem modules. Do not hide those warnings; track whether
any planned bridge theorem relies computationally on the warned definitions
under transport.

## Main Gaps

The following files requested by `paper_improvement_plan.md` are currently
absent and are the core backlog:

- `agda/Metatheory/RawStructuralSyntax.agda`
- `agda/Metatheory/RawStructuralTyping.agda`
- `agda/Metatheory/SurfaceNormalizationBridge.agda`
- `agda/Metatheory/SurfaceToHornImage.agda`
- `agda/Metatheory/FiniteInterfaceBasis.agda`
- `agda/Metatheory/GlobalActionSemantics.agda`
- `agda/Metatheory/ActiveBasisContract.agda`
- `agda/Metatheory/PresentationEquivalence.agda`
- `agda/Metatheory/MuInvariance.agda`
- `agda/Metatheory/SparseDependencyRecurrence.agda`
- `agda/Metatheory/FullCouplingEnvelope.agda`
- `agda/CaseStudies/*.agda`
- `agda/Test/SurfaceBridgeSmoke.agda`
- `agda/Test/ActiveBasisExamples.agda`
- `agda/Test/SparseRecurrenceSmoke.agda`
- `agda/Test/PresentationInvariance/RebundleRecord.agda`
- `agda/Test/PresentationInvariance/SplitShell.agda`
- `agda/Test/PresentationInvariance/CurryUncurry.agda`
- `agda/Test/PresentationInvariance/TransparentAlias.agda`
- `agda/Test/PresentationInvariance/DuplicateTrace.agda`

Supporting documentation and audit artifacts are also absent:

- `docs/case_studies/coherence_depth_universe_extension.md`
- `docs/case_studies/coherence_depth_global_modality.md`
- `docs/case_studies/coherence_depth_promoted_interface.md`
- `docs/case_studies/coherence_depth_sparse_datatype.md`
- `docs/reports/coherence_depth_case_study_report.md`
- `scripts/coherence_depth_audit.py`
- `runs/coherence_depth_case_studies/*.yaml`

## Invariants For All New Agda Work

- Use `{-# OPTIONS --cubical --safe --guardedness #-}` unless a module has a
  documented reason not to.
- Do not introduce `postulate` in theorem-facing modules.
- Keep the bridge abstract over full CCHM term syntax; formalize the paper's
  extension-calculus judgments, not all Agda surface syntax.
- Reuse existing modules rather than duplicating their definitions:
  `InterfaceCalculus`, `Obligations`, `ComputationalReplacement`,
  `KanSubsumption`, `ChronologicalWindow`, `Refactoring`,
  `CanonicityDensity`, and `UniversalRecurrence`.
- Prefer proof-relevant records over raw Boolean tags for admissibility,
  primitive/derived status, transparent generation, and presentation
  equivalence.
- Every new theorem-facing module must get a smoke import under `agda/Test/`.

Phase 0 audit cleanup was completed on 2026-05-02. The trust-boundary table is
in `docs/coherence_depth_trust_boundary.md`, the theorem-name index is in
`docs/theorem_index.md`, and `scripts/check_coherence_depth_artifact.sh`
checks the baseline Agda modules and theorem-facing postulate boundary. The
script uses native `agda` when it is on `PATH` and falls back to
`powershell.exe` for Agda invocations in Windows Git Bash, because this
workspace exposes Agda in PowerShell but not necessarily in Bash.

## Phase 2: Presentation Equivalence And `mu` Invariance

Goal: make the phrase "presentation-invariant minimal opaque cost" formally
credible.

Deliverables:

- `agda/Metatheory/PresentationEquivalence.agda`
- `agda/Metatheory/MuInvariance.agda`
- `agda/Test/PresentationInvariance/RebundleRecord.agda`
- `agda/Test/PresentationInvariance/SplitShell.agda`
- `agda/Test/PresentationInvariance/CurryUncurry.agda`
- `agda/Test/PresentationInvariance/TransparentAlias.agda`
- `agda/Test/PresentationInvariance/DuplicateTrace.agda`

Core definitions:

```agda
data PresentationStep : CanonicalTraceCostNormalForm ->
                        CanonicalTraceCostNormalForm -> Set where
  reassocSigma             : ...
  splitRecord              : ...
  bundleRecord             : ...
  curryPi                  : ...
  uncurryPi                : ...
  transportField           : ...
  transparentAliasInsert   : ...
  transparentAliasDelete   : ...
  duplicateDerivedDelete   : ...

data PresentationEquivalent : CanonicalTraceCostNormalForm ->
                              CanonicalTraceCostNormalForm -> Set where
  refl  : ...
  sym   : ...
  trans : ...
  step  : PresentationStep Gamma Delta -> ...
```

Define primitive status semantically:

```agda
TransparentlyGenerated : CanonicalTraceCostNormalForm -> TraceCostField -> Set
RequiresPrimitive Gamma phi = Not (TransparentlyGenerated Gamma phi)
```

Expected exports:

```agda
presentation-step-preserves-trace-support
presentation-step-preserves-primitive-cost
presentation-equivalence-preserves-trace-fields
presentation-equivalence-preserves-primitive-cost
mu-preserved-by-presentation-step
mu-invariant-under-presentation-equivalence
derived-field-deletion-preserves-mu
requires-primitive-field-essential
computational-replacement-preserves-mu
```

Implementation notes:

- Connect `computational-replacement-preserves-mu` to
  `Metatheory/ComputationalReplacement.agda` rather than reproving replacement.
- The essentiality theorem should not be a label-preservation tautology.
  Primitive means "not transparently generated"; deletion of such a field must
  break presentation equivalence.
- The existing `Metatheory/Refactoring.agda` can remain as the coarse older
  package; this phase supplies the generator-by-generator presentation setoid
  requested by `paper_improvement_plan.md`.

Acceptance:

```bash
cd agda
agda --transliterate Metatheory/PresentationEquivalence.agda
agda --transliterate Metatheory/MuInvariance.agda
agda --transliterate Test/PresentationInvariance/RebundleRecord.agda
agda --transliterate Test/PresentationInvariance/SplitShell.agda
agda --transliterate Test/PresentationInvariance/CurryUncurry.agda
agda --transliterate Test/PresentationInvariance/TransparentAlias.agda
agda --transliterate Test/PresentationInvariance/DuplicateTrace.agda
```

## Phase 3: Raw Structural Syntax And Typing

Goal: formalize the fixed extension calculus surface without claiming to cover
arbitrary Cubical Agda syntax.

Deliverables:

- `agda/Metatheory/RawStructuralSyntax.agda`
- `agda/Metatheory/RawStructuralTyping.agda`

Core syntax:

```agda
record LayerRef : Set where ...
record BasisSite : Set where ...
record PayloadField : Set where ...
record AlgebraicPayloadField : Set where ...
record RawBoundary : Set where ...

data RawStructuralClause : Set where
  act  : NewPayloadRef -> BasisSite -> RawStructuralClause
  cmp  : NewPayloadRef -> BasisSite -> BasisSite -> RawStructuralClause
  horn : RawBoundary -> RawStructuralClause

record RawExtension : Set where
  field
    payload      : RawTelescope PayloadField
    structural   : RawTelescope RawStructuralClause
    algebraic    : RawTelescope AlgebraicPayloadField
    exportPolicy : ExportPolicy
```

Typing/admissibility:

```agda
record AdmissibleRawExtension
  (B : LibraryState) (e : RawExtension) : Set where
  field
    payloadWellTyped    : PayloadWellTyped B e
    structuralWellTyped : StructuralClausesWellTyped B e
    algebraicWellTyped  : AlgebraicWellTyped B e
    sealingDerivation   : SealingDerivation B e
    opacityRespected    : OpacityRespected B e
    exportPolicySound   : ExportPolicySound B e
```

Expected exports:

```agda
raw-extension-payload-fields
raw-extension-structural-clauses
raw-extension-algebraic-fields
act-clause-has-unary-support
cmp-clause-has-binary-support
horn-clause-has-higher-boundary-support
algebraic-field-is-payload-not-structural-trace
naked-higher-face-rejected-or-packaged
```

Implementation notes:

- The key reviewer-facing distinction is:
  raw written clause, typed structural role, and normalized trace field.
- Higher user operations must be classified as algebraic payload unless they
  are typed structural integration traces.
- Avoid making `horn` the only possible higher clause in a way that makes the
  bridge theorem tautological. The typing layer should explain why a higher
  structural role has a boundary/filler package.

Acceptance:

```bash
cd agda
agda --transliterate Metatheory/RawStructuralSyntax.agda
agda --transliterate Metatheory/RawStructuralTyping.agda
```

## Phase 4: Surface Normalization Bridge

Goal: connect admissible raw declarations to canonical trace presentations.

Deliverables:

- `agda/Metatheory/SurfaceNormalizationBridge.agda`
- `agda/Test/SurfaceBridgeSmoke.agda`

Core normalizer:

```agda
record CanonicalNormalizedSignature : Set where
  field
    payloadFields : CanonicalTelescope
    traceFields   : CanonicalTraceCostNormalForm

normalizeRawExtension :
  (B : LibraryState) ->
  (e : RawExtension) ->
  AdmissibleRawExtension B e ->
  CanonicalNormalizedSignature
```

Expected exports:

```agda
raw-extension-elaborates-to-candidate
raw-extension-normalizes-to-canonical-signature
raw-trace-normalizes-to-canonical-signature
normalize-preserves-support
normalize-preserves-arity
normalize-preserves-primitive-cost
normalization-respects-presentation-equivalence
normalized-signature-matches-counted-interface
```

Implementation notes:

- This module is the concrete replacement for the current paper-level
  `rem:bridge-target`.
- Use `CanonicalTelescope`, `TraceCostNormalForm`, and `MuInvariance`.
- Connect to `InterfaceCalculus` and `TracePrinciple` for sealed-layer and
  public-counting interpretation.
- Keep quotient operations explicit:
  telescope flattening, record rebundling/splitting, Sigma reassociation,
  Pi currying/uncurrying, transport along exported equalities, transparent
  aliases, duplicate derived trace removal.

Acceptance:

```bash
cd agda
agda --transliterate Metatheory/SurfaceNormalizationBridge.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
```

## Phase 5: Surface-To-Horn Image Theorem

Goal: prove the central missing theorem from `paper_improvement_plan.md`:
admissible raw structural clauses normalize into the horn-generated structural
obligation language.

Deliverables:

- `agda/Metatheory/SurfaceToHornImage.agda`
- updates to `agda/Test/SurfaceBridgeSmoke.agda`

Expected exports:

```agda
surface-to-horn-normal-form
surface-to-horn-preserves-support
surface-to-horn-preserves-arity
surface-to-horn-preserves-primitive-cost
higher-structural-fields-derived
higher-raw-structural-traces-derived
raw-syntax-no-naked-higher-structural-projections
horn-image-complete-for-structural-clauses
raw-structural-normalizes-to-horn
```

Proof shape:

- Induct on typed structural clauses.
- `act`: normalizes to unary trace support.
- `cmp`: normalizes to binary comparison trace support.
- `horn`: packages the typed boundary and uses
  `Metatheory/KanSubsumption.agda`'s `HornExtensionFiber` and
  horn-reduction surface.
- Higher structural clauses are derived in the canonical trace-cost normal
  form after the existing computational-replacement theorem is applied.

Non-tautology requirement:

- The theorem must also expose a classification lemma showing that arbitrary
  higher user operations are payload/algebraic structure, not structural trace.
- Naked higher remote faces must be rejected by admissibility or packaged as a
  horn boundary with filler.

Acceptance:

```bash
cd agda
agda --transliterate Metatheory/SurfaceToHornImage.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
```

Paper payoff:

- The mechanization table can change the raw bridge from "paper-level only" to
  "mechanized for the fixed raw extension calculus".
- The paper must still say this is not a parser theorem for arbitrary Cubical
  Agda programs.

## Phase 6: Active-Basis Naturality

Goal: show that active-basis coverage follows from global action totality and
does not smuggle in the depth-two recurrence.

Deliverables:

- `agda/Metatheory/FiniteInterfaceBasis.agda`
- `agda/Metatheory/GlobalActionSemantics.agda`
- `agda/Metatheory/ActiveBasisContract.agda`
- `agda/Test/ActiveBasisExamples.agda`

Core definitions:

```agda
record ActiveInterface : Set where
  field
    fieldCount  : Nat
    fieldAt     : Fin fieldCount -> InterfaceField
    transparent : TransparentEquivalence fieldAt
    basis       : BasisFamily transparent

record GlobalActionPayload (I : ActiveInterface) : Set where
  field
    advertisedScope : WholeActiveInterface I
    actsOnField     : InterfaceFieldOf I -> Set

record ActionTotality {I : ActiveInterface}
  (X : GlobalActionPayload I) : Set where
  field
    actsOnEveryBasisSite : ...
```

Expected exports:

```agda
basis-families-exist
basis-family-cardinality-invariant
basis-action-equivalence
global-action-totality-implies-active-basis-contract
active-basis-contract-entails-density
coverage-alone-does-not-imply-depth-two-window
coverage-alone-does-not-imply-fibonacci
```

Implementation notes:

- Reuse `Metatheory/CanonicityDensity.agda` for the density theorem-facing
  package where possible.
- The non-circularity theorem can be proved by two explicit small models:
  active-basis coverage with UIP depth-one collapse, and active-basis coverage
  with an artificial depth-three window.

Acceptance:

```bash
cd agda
agda --transliterate Metatheory/FiniteInterfaceBasis.agda
agda --transliterate Metatheory/GlobalActionSemantics.agda
agda --transliterate Metatheory/ActiveBasisContract.agda
agda --transliterate Test/ActiveBasisExamples.agda
```

## Phase 7: Sparse Dependency Recurrence And Full-Coupling Envelope

Goal: formalize the sparse/full hierarchy so the Fibonacci theorem is the
maximal fully coupled specialization, not a claim about all library growth.

Deliverables:

- `agda/Metatheory/SparseDependencyRecurrence.agda`
- `agda/Metatheory/FullCouplingEnvelope.agda`
- `agda/Test/SparseRecurrenceSmoke.agda`

Core definitions:

```agda
record CouplingFootprint (n : Nat) : Set where
  field
    dependsOn : FiniteSubset (PreviousLayers n)

record SparseWindowedContext : Set where
  field
    footprint : (n : Nat) -> CouplingFootprint n
    layerCost : Nat -> Nat
    payload   : Nat -> Nat
```

Expected exports:

```agda
sparse-windowed-recurrence
transparent-growth-zero-footprint
orthogonal-extension-zero-or-sparse
orthogonal-extension-below-full-envelope
full-coupling-envelope
full-coupling-specializes-sparse-recurrence
full-coupling-depth-two-affine-law
```

Implementation notes:

- Build on `Metatheory/UniversalRecurrence.agda`.
- The full-coupling case should recover the existing two-layer affine law after
  the cubical chronological-window theorem reduces the active footprint to the
  previous two layers.
- Sparse recurrence should be stated over an explicit finite footprint, so
  ordinary orthogonal extensions are modeled rather than dismissed.

Acceptance:

```bash
cd agda
agda --transliterate Metatheory/SparseDependencyRecurrence.agda
agda --transliterate Metatheory/FullCouplingEnvelope.agda
agda --transliterate Test/SparseRecurrenceSmoke.agda
```

## Phase 8: Case Studies And Audit Data

Goal: supply toy-but-explicit examples and nonexamples using the same abstract
counting machinery.

Deliverables:

- `agda/CaseStudies/UniverseExtension.agda`
- `agda/CaseStudies/GlobalModality.agda`
- `agda/CaseStudies/PromotedInterface.agda`
- `agda/CaseStudies/SparseDatatype.agda`
- `docs/case_studies/coherence_depth_universe_extension.md`
- `docs/case_studies/coherence_depth_global_modality.md`
- `docs/case_studies/coherence_depth_promoted_interface.md`
- `docs/case_studies/coherence_depth_sparse_datatype.md`
- `docs/reports/coherence_depth_case_study_report.md`
- `runs/coherence_depth_case_studies/*.yaml`
- `scripts/coherence_depth_audit.py`

Each case study must record:

- payload fields;
- active interface footprint;
- unary trace obligations;
- binary trace obligations;
- higher horn obligations and derived status;
- whether active-basis totality holds;
- expected `mu` contribution;
- whether the transparent, sparse, full-coupling, or no-recurrence law applies.

Minimum examples:

- transparent lemma extension: zero latency;
- sparse datatype extension: finite local footprint, no full recurrence;
- promoted interface package: classified by active-basis coverage;
- global modality or universe extension: full-coupling recurrence;
- refactored presentation of the same extension: same `mu`.

Acceptance:

```bash
cd agda
agda --transliterate CaseStudies/UniverseExtension.agda
agda --transliterate CaseStudies/GlobalModality.agda
agda --transliterate CaseStudies/PromotedInterface.agda
agda --transliterate CaseStudies/SparseDatatype.agda
cd ..
python scripts/coherence_depth_audit.py runs/coherence_depth_case_studies
```

## Phase 9: Top-Level Integration

Goal: make the new bridge part of the repository's theorem-facing surface.

Deliverables:

- update `agda/PEN.agda`;
- update `agda/Test/MetatheorySmoke.agda`;
- update `agda/README.md`;
- update `docs/theorem_index.md`;
- update `scripts/check_coherence_depth_artifact.sh`.

Add public imports only after the individual modules type-check cleanly:

```agda
open import Metatheory.CanonicalTelescope public
open import Metatheory.TraceCostNormalForm public
open import Metatheory.PresentationEquivalence public
open import Metatheory.MuInvariance public
open import Metatheory.RawStructuralSyntax public
open import Metatheory.RawStructuralTyping public
open import Metatheory.SurfaceNormalizationBridge public
open import Metatheory.SurfaceToHornImage public
open import Metatheory.FiniteInterfaceBasis public
open import Metatheory.GlobalActionSemantics public
open import Metatheory.ActiveBasisContract public
open import Metatheory.SparseDependencyRecurrence public
open import Metatheory.FullCouplingEnvelope public
```

Acceptance:

```bash
cd agda
agda --transliterate PEN.agda
agda --transliterate Test/MetatheorySmoke.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
agda --transliterate Test/ActiveBasisExamples.agda
agda --transliterate Test/SparseRecurrenceSmoke.agda
agda --transliterate Test/ClutchingSmoke.agda
agda --transliterate Test/Fibonacci.agda
```

## Phase 10: Paper Rewrite After Mechanization

Goal: make `1_coherence_depth.tex` match the completed mechanization.

Paper edits:

1. Replace "paper-level bridge" with "mechanized bridge for the fixed raw
   extension calculus" only after `SurfaceToHornImage.agda` and
   `MuInvariance.agda` type-check.
2. Keep the explicit limitation:
   this is not a parser or elaboration theorem for arbitrary Cubical Agda
   programs.
3. Add or update the mechanization table with:
   - raw syntax bridge;
   - canonical trace and `mu` invariance;
   - active-basis naturality;
   - sparse/full recurrence;
   - case studies and audit data.
4. Replace broad prose like "reasonable presentations" with:
   presentations generated by the explicit `PresentationStep` constructors.
5. Present the Fibonacci law as the fully coupled endpoint of the sparse
   dependency calculus.

Acceptance:

```bash
latexmk -pdf 1_coherence_depth.tex
```

and the final mechanization table names only modules that exist and
type-check.

## Final Done Criteria

The missing mechanization from `paper_improvement_plan.md` is implemented when:

1. All files listed in the "Main Gaps" section either exist or have been
   intentionally removed from the paper/plan with a documented reason.
2. `SurfaceToHornImage.agda` exports the raw-to-horn bridge theorem for the
   fixed extension calculus.
3. `MuInvariance.agda` proves `mu` invariance under the explicit presentation
   equivalence generators.
4. `ActiveBasisContract.agda` proves active-basis coverage from global action
   totality and includes a non-circularity example.
5. `SparseDependencyRecurrence.agda` and `FullCouplingEnvelope.agda` formalize
   sparse recurrence and the fully coupled specialization.
6. The new smoke tests type-check.
7. The theorem-facing modules remain postulate-free.
8. `1_coherence_depth.tex` no longer overstates or understates the formalized
   boundary.
