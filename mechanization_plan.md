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
- `agda/Metatheory/PresentationEquivalence.agda`
- `agda/Metatheory/MuInvariance.agda`
- `agda/Metatheory/RawStructuralSyntax.agda`
- `agda/Metatheory/RawStructuralTyping.agda`
- `agda/Metatheory/SurfaceNormalizationBridge.agda`
- `agda/Metatheory/SurfaceToHornImage.agda`
- `agda/Metatheory/FiniteInterfaceBasis.agda`
- `agda/Metatheory/GlobalActionSemantics.agda`
- `agda/Metatheory/ActiveBasisContract.agda`
- `agda/Metatheory/SparseDependencyRecurrence.agda`
- `agda/Metatheory/FullCouplingEnvelope.agda`
- `agda/Geometry/Clutching.agda`
- `agda/Core/AffineRecurrence.agda`
- `agda/Core/DepthOneAffine.agda`
- `agda/Test/MetatheorySmoke.agda`
- `agda/Test/ClutchingSmoke.agda`
- `agda/Test/Fibonacci.agda`
- `agda/Test/PresentationInvariance/Smoke.agda`
- `agda/Test/PresentationInvariance/RebundleRecord.agda`
- `agda/Test/PresentationInvariance/SplitShell.agda`
- `agda/Test/PresentationInvariance/CurryUncurry.agda`
- `agda/Test/PresentationInvariance/TransparentAlias.agda`
- `agda/Test/PresentationInvariance/DuplicateTrace.agda`
- `agda/Test/SurfaceBridgeSmoke.agda`
- `agda/Test/ActiveBasisExamples.agda`
- `agda/Test/SparseRecurrenceSmoke.agda`

These checks currently pass:

```bash
cd agda
agda --transliterate PEN.agda
agda --transliterate Metatheory/CanonicalTelescope.agda
agda --transliterate Metatheory/TraceCostNormalForm.agda
agda --transliterate Metatheory/PresentationEquivalence.agda
agda --transliterate Metatheory/MuInvariance.agda
agda --transliterate Metatheory/RawStructuralSyntax.agda
agda --transliterate Metatheory/RawStructuralTyping.agda
agda --transliterate Metatheory/SurfaceNormalizationBridge.agda
agda --transliterate Metatheory/SurfaceToHornImage.agda
agda --transliterate Metatheory/FiniteInterfaceBasis.agda
agda --transliterate Metatheory/GlobalActionSemantics.agda
agda --transliterate Metatheory/ActiveBasisContract.agda
agda --transliterate Metatheory/SparseDependencyRecurrence.agda
agda --transliterate Metatheory/FullCouplingEnvelope.agda
agda --transliterate Test/MetatheorySmoke.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
agda --transliterate Test/ActiveBasisExamples.agda
agda --transliterate Test/SparseRecurrenceSmoke.agda
agda --transliterate Test/PresentationInvariance/Smoke.agda
agda --transliterate Test/PresentationInvariance/RebundleRecord.agda
agda --transliterate Test/PresentationInvariance/SplitShell.agda
agda --transliterate Test/PresentationInvariance/CurryUncurry.agda
agda --transliterate Test/PresentationInvariance/TransparentAlias.agda
agda --transliterate Test/PresentationInvariance/DuplicateTrace.agda
agda --transliterate Test/ClutchingSmoke.agda
agda --transliterate Test/Fibonacci.agda
```

Known caveat: these checks emit Cubical Agda `UnsupportedIndexedMatch`
warnings in several theorem modules. Do not hide those warnings; track whether
any planned bridge theorem relies computationally on the warned definitions
under transport.

## Main Gaps

Phase 8 case-study and audit artifacts now exist and type-check. The remaining
core backlog is top-level integration and the paper rewrite:

- Phase 9: update the theorem-facing repository surface so the bridge and
  case-study modules are visible through the expected smoke checks and artifact
  script.
- Phase 10: revise `1_coherence_depth.tex` so its mechanization claims match
  the completed fixed-extension-calculus boundary.

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

Phase 2 presentation equivalence and `mu` invariance was completed on
2026-05-02. The generator-by-generator presentation layer is implemented in
`agda/Metatheory/PresentationEquivalence.agda`, `mu` invariance and the
computational-replacement bridge are implemented in
`agda/Metatheory/MuInvariance.agda`, and the five named presentation smoke
modules type-check. The checks retain the existing Cubical Agda
`UnsupportedIndexedMatch` warning from `Metatheory/Obligations.agda`.

Phase 3 raw structural syntax and typing was completed on 2026-05-02. The
fixed raw extension calculus is implemented in
`agda/Metatheory/RawStructuralSyntax.agda`; admissibility and the raw
clause-classification layer are implemented in
`agda/Metatheory/RawStructuralTyping.agda`. The module distinguishes raw
written clauses, typed structural roles, and algebraic payload fields; higher
user operations classify as algebraic payload unless a structural role packages
them as a horn boundary. Both modules type-check and are now included in
`scripts/check_coherence_depth_artifact.sh`.

Phase 4 surface normalization bridge was completed on 2026-05-02. The concrete
bridge is implemented in
`agda/Metatheory/SurfaceNormalizationBridge.agda`, with a smoke surface in
`agda/Test/SurfaceBridgeSmoke.agda`. The normalizer consumes
`AdmissibleRawExtension`, flattens payload plus algebraic fields into a
canonical payload telescope, maps `act`/`cmp`/packaged `horn` structural roles
to a two-layer canonical trace-cost normal form, and exposes the requested
support, arity, primitive-cost, presentation-equivalence, and counted-interface
exports. Both files type-check. The checks retain the known
`UnsupportedIndexedMatch` warnings from existing finite-index helpers in
`Metatheory/Obligations.agda` and `Metatheory/TracePrinciple.agda`.

Phase 5 surface-to-horn image theorem was completed on 2026-05-02. The
theorem-facing horn image record and raw structural normalization package are
implemented in `agda/Metatheory/SurfaceToHornImage.agda`, and
`agda/Test/SurfaceBridgeSmoke.agda` now imports and exposes the Phase 5 names.
The module proves that typed `act` clauses image to unary support, typed `cmp`
clauses image to binary support, and packaged `horn` clauses image to derived
canonical trace fields. It also keeps the non-tautology boundary visible:
higher user operations remain algebraic payload fields, while naked higher
structural faces are rejected or packaged as horn boundaries by admissibility.
The focused checks
`agda --transliterate Metatheory/SurfaceToHornImage.agda` and
`agda --transliterate Test/SurfaceBridgeSmoke.agda` pass. No new warning sites
were introduced; the checks retain only the known `UnsupportedIndexedMatch`
warnings from `Metatheory/Obligations.agda` and
`Metatheory/TracePrinciple.agda`.

Phase 6 active-basis naturality was completed on 2026-05-02. The finite
active-interface basis is implemented in
`agda/Metatheory/FiniteInterfaceBasis.agda`, the global advertised-scope and
deterministic action-totality semantics are implemented in
`agda/Metatheory/GlobalActionSemantics.agda`, and the active-basis contract,
density package, and non-circularity examples are implemented in
`agda/Metatheory/ActiveBasisContract.agda`. The smoke surface
`agda/Test/ActiveBasisExamples.agda` aliases every Phase 6 expected export.
The focused checks
`agda --transliterate Metatheory/FiniteInterfaceBasis.agda`,
`agda --transliterate Metatheory/GlobalActionSemantics.agda`,
`agda --transliterate Metatheory/ActiveBasisContract.agda`, and
`agda --transliterate Test/ActiveBasisExamples.agda` pass. No new warning
sites were introduced; the checks retain the known `UnsupportedIndexedMatch`
warning from `Metatheory/Obligations.agda`.

Phase 7 sparse dependency recurrence and full-coupling envelope was completed
on 2026-05-02. The finite dependency footprint, sparse context, sparse
recurrence package, transparent zero-footprint package, and sparse/full
bounding lemmas are implemented in
`agda/Metatheory/SparseDependencyRecurrence.agda`; the maximal full-coupling
envelope, sparse specialization, and depth-two affine endpoint are implemented
in `agda/Metatheory/FullCouplingEnvelope.agda`. The smoke surface
`agda/Test/SparseRecurrenceSmoke.agda` aliases every expected Phase 7 export.
The focused checks
`agda --transliterate Metatheory/SparseDependencyRecurrence.agda`,
`agda --transliterate Metatheory/FullCouplingEnvelope.agda`, and
`agda --transliterate Test/SparseRecurrenceSmoke.agda` pass. No new warning
sites were introduced; the checks retain the known `UnsupportedIndexedMatch`
warnings from existing theorem modules.

Phase 8 case studies and audit data was completed on 2026-05-02. The shared
case-study summary vocabulary is implemented in `agda/CaseStudies/Common.agda`;
the four theorem-facing examples are implemented in
`agda/CaseStudies/UniverseExtension.agda`,
`agda/CaseStudies/GlobalModality.agda`,
`agda/CaseStudies/PromotedInterface.agda`, and
`agda/CaseStudies/SparseDatatype.agda`. The markdown case-study pages, report,
six YAML fixtures, and `scripts/coherence_depth_audit.py` are present. The
audit records the transparent zero-latency lemma extension, sparse datatype,
promoted active-basis interface, full-coupling universe/global examples, and a
refactored universe presentation with the same `mu`. The focused Agda checks,
audit script, and `scripts/check_coherence_depth_artifact.sh` pass. The checks
retain only the known Cubical Agda `UnsupportedIndexedMatch` warnings from
existing theorem modules.

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
