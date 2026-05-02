# Next Step Plan: Case Studies And Audit Data

## Goal

Implement Phase 8 of `mechanization_plan.md`: add toy-but-explicit case
studies and an audit dataset that exercise the same counting vocabulary used by
the bridge modules.

## Files To Create Or Update

- Create `agda/CaseStudies/UniverseExtension.agda`.
- Create `agda/CaseStudies/GlobalModality.agda`.
- Create `agda/CaseStudies/PromotedInterface.agda`.
- Create `agda/CaseStudies/SparseDatatype.agda`.
- Create `docs/case_studies/coherence_depth_universe_extension.md`.
- Create `docs/case_studies/coherence_depth_global_modality.md`.
- Create `docs/case_studies/coherence_depth_promoted_interface.md`.
- Create `docs/case_studies/coherence_depth_sparse_datatype.md`.
- Create `docs/reports/coherence_depth_case_study_report.md`.
- Create `runs/coherence_depth_case_studies/*.yaml`.
- Create `scripts/coherence_depth_audit.py`.
- After the Agda and audit checks pass, add case-study commands to
  `docs/theorem_index.md` and `scripts/check_coherence_depth_artifact.sh`.

## Preparation

1. Re-read the bridge/counting modules:
   - `agda/Metatheory/SurfaceNormalizationBridge.agda`
   - `agda/Metatheory/SurfaceToHornImage.agda`
   - `agda/Metatheory/FiniteInterfaceBasis.agda`
   - `agda/Metatheory/ActiveBasisContract.agda`
   - `agda/Metatheory/SparseDependencyRecurrence.agda`
   - `agda/Metatheory/FullCouplingEnvelope.agda`
2. Re-read the case-study expectations in `paper_improvement_plan.md` and
   `1_coherence_depth.tex` so the examples align with the paper vocabulary.
3. Inspect existing docs/report style under `docs/` before adding the new
   markdown files.

## Implementation Sketch

1. Create a shared case-study shape inside the Agda case-study modules or a
   small helper if duplication becomes meaningful.
   - Record payload fields.
   - Record active interface footprint.
   - Record unary, binary, and higher horn obligations.
   - Record whether higher horn obligations are derived.
   - Record expected `mu` contribution.
   - Record which recurrence law applies.

2. Implement the four Agda examples.
   - `UniverseExtension`: full-coupling recurrence endpoint.
   - `GlobalModality`: full-coupling or global active-basis example.
   - `PromotedInterface`: active-basis coverage without claiming it alone
     proves a chronological window.
   - `SparseDatatype`: explicit finite local footprint with no full recurrence
     claim.

3. Add markdown case-study pages.
   - Keep each page aligned to the same headings so the audit script can cross
     check YAML fields against prose.
   - Explicitly state payload fields, footprint, trace obligations, derived
     status, totality, `mu`, and recurrence classification.

4. Add YAML audit fixtures under `runs/coherence_depth_case_studies/`.
   - One YAML file per case study.
   - Use stable keys for all required fields.
   - Include a refactored-presentation entry that has the same `mu` as its
     source example.

5. Implement `scripts/coherence_depth_audit.py`.
   - Validate required keys.
   - Check that transparent examples have zero footprint and zero `mu`.
   - Check sparse examples do not claim full-coupling recurrence.
   - Check full-coupling examples declare the full envelope.
   - Check refactored presentations preserve `mu`.

## Acceptance Commands

```bash
cd agda
agda --transliterate CaseStudies/UniverseExtension.agda
agda --transliterate CaseStudies/GlobalModality.agda
agda --transliterate CaseStudies/PromotedInterface.agda
agda --transliterate CaseStudies/SparseDatatype.agda
cd ..
python scripts/coherence_depth_audit.py runs/coherence_depth_case_studies
./scripts/check_coherence_depth_artifact.sh
```

## Guardrails

- Do not turn case studies into a parser or elaborator for arbitrary Cubical
  Agda.
- Do not claim active-basis totality alone implies the Fibonacci law.
- Keep sparse datatype examples explicitly finite-footprint, not
  full-envelope.
- Keep YAML, markdown, and Agda names synchronized so the audit script can be
  mechanical instead of interpretive.
