# Coherence-Depth Theorem Index

Date: 2026-05-02

This index gives the stable names to search for when checking the Agda side of
`1_coherence_depth.tex`.

## Top-Level Checks

```bash
cd agda
agda --transliterate PEN.agda
agda --transliterate Test/MetatheorySmoke.agda
agda --transliterate Test/PresentationInvariance/Smoke.agda
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
agda --transliterate CaseStudies/UniverseExtension.agda
agda --transliterate CaseStudies/GlobalModality.agda
agda --transliterate CaseStudies/PromotedInterface.agda
agda --transliterate CaseStudies/SparseDatatype.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
agda --transliterate Test/ActiveBasisExamples.agda
agda --transliterate Test/SparseRecurrenceSmoke.agda
agda --transliterate Test/PresentationInvariance/RebundleRecord.agda
agda --transliterate Test/PresentationInvariance/SplitShell.agda
agda --transliterate Test/PresentationInvariance/CurryUncurry.agda
agda --transliterate Test/PresentationInvariance/TransparentAlias.agda
agda --transliterate Test/PresentationInvariance/DuplicateTrace.agda
agda --transliterate Geometry/Clutching.agda
agda --transliterate Test/ClutchingSmoke.agda
agda --transliterate Test/Fibonacci.agda
cd ..
python scripts/coherence_depth_audit.py runs/coherence_depth_case_studies
```

## Theorem Names

| Name | Module | Role |
|---|---|---|
| `transparent-growth-keeps-library-state` | `Metatheory.InterfaceCalculus` | transparent growth stays in one library state |
| `transparent-definitions-preserve-active-interface` | `Metatheory.InterfaceCalculus` | transparent definitions do not enlarge the active interface |
| `transparent-definitions-have-zero-integration-latency` | `Metatheory.InterfaceCalculus` | zero latency for transparent growth |
| `transparent-user-level-code-lies-outside-the-recurrence` | `Metatheory.InterfaceCalculus` | transparent code is outside the recurrence law |
| `historical-arity-forces-cell-dimension` | `Metatheory.Obligations` | arity-to-cell-dimension dictionary |
| `irreducible-obligation-requires-cell` | `Metatheory.Obligations` | irreducible obligations require a coherence cell |
| `history-truncates-to-one` | `Metatheory.Extensional` | UIP/extensional depth-one collapse |
| `structural-integration-horn-reduction` | `Metatheory.KanSubsumption` | structural obligations reduce to horn-extension form |
| `remote-layer-obligation-derived` | `Metatheory.KanSubsumption` | remote higher obligation is derived |
| `telescopic-subsumption` | `Metatheory.KanSubsumption` | telescopic subsumption chain |
| `telescopic-remote-comparison-derived` | `Metatheory.KanSubsumption` | remote comparison derivation through exported trace |
| `structural-obligation-set-equivalence` | `Metatheory.UpperBound` | equivalence of higher and depth-two obligation sets |
| `structural-stabilizes-at-two` | `Metatheory.UpperBound` | exact upper-bound stabilization |
| `structural-obligation-contractible-factorization` | `Metatheory.UpperBound` | contractible factor decomposition |
| `contractible-remote-factor-contractible` | `Metatheory.UpperBound` | remote factor is contractible |
| `primitive-obligations-factor-through-last-two` | `Metatheory.ChronologicalWindow` | primitive obligations factor through last two layers |
| `two-layer-chronological-window` | `Metatheory.ChronologicalWindow` | chronological window size two |
| `chronological-markov-blanket` | `Metatheory.ChronologicalWindow` | last two layers as chronological Markov blanket |
| `explicit-binary-sealing-obstruction` | `Metatheory.AdjunctionBarrier` | binary lower-bound obstruction |
| `triangle-identity-corollary` | `Metatheory.AdjunctionBarrier` | adjunction-style binary coherence corollary |
| `adjunction-barrier` | `Metatheory.AdjunctionBarrier` | packaged lower-bound barrier |
| `clutching-family-theorem` | `Geometry.Clutching` | clutching-family lower-bound witness |
| `hopf-binary-clutching-nontrivial` | `Geometry.Clutching` | nontrivial Hopf binary clutching datum |
| `clutching-horn-extension-fiber-contractible` | `Geometry.Clutching` | contractible horn fiber for clutching witness |
| `structural-coherence-depth-exactly-two` | `Metatheory.ExactDepth` | exact structural depth two |
| `cubical-coherence-depth-exactly-two` | `Metatheory.ExactDepth` | cubical depth-two corollary |
| `cubical-chronological-window-size-exactly-two` | `Metatheory.ExactDepth` | exact cubical chronological window |
| `primitive-depth-two-law-for-2d-foundations` | `Metatheory.TwoDFoundations` | primitive/window law for 2D foundations |
| `depth-two-law-for-2d-foundations` | `Metatheory.TwoDFoundations` | abstract 2D-foundations depth theorem |
| `constant-payload-depth-two-law` | `Metatheory.TwoDFoundations` | constant-payload affine consequence |
| `cubical-depth-two-law-for-2d-foundations` | `Metatheory.TwoDFoundations` | cubical 2D-foundations instance |
| `two-level-depth-two-law-for-2d-foundations` | `Metatheory.TwoLTTFoundations` | strict/fibrant 2LTT-style instance |
| `refactoring-invariance` | `Metatheory.Refactoring` | coarse refactoring invariance |
| `global-admissibility-forces-maximal-interface-density` | `Metatheory.CanonicityDensity` | active basis density for global admissibility |
| `basis-families-exist` | `Metatheory.FiniteInterfaceBasis` | canonical finite basis family for a counted active interface |
| `basis-family-cardinality-invariant` | `Metatheory.FiniteInterfaceBasis` | basis cardinality agrees with the active interface count |
| `basis-action-equivalence` | `Metatheory.FiniteInterfaceBasis` | basis sites are equivalent to active interface fields |
| `global-action-totality-implies-active-basis-contract` | `Metatheory.ActiveBasisContract` | global totality covers every active-basis site |
| `active-basis-contract-entails-density` | `Metatheory.ActiveBasisContract` | deterministic active-basis coverage yields a contractible action datum at each field |
| `coverage-alone-does-not-imply-depth-two-window` | `Metatheory.ActiveBasisContract` | explicit coverage model with depth-one collapse refutes circular depth-two inference |
| `coverage-alone-does-not-imply-fibonacci` | `Metatheory.ActiveBasisContract` | explicit coverage model without Fibonacci growth refutes circular recurrence inference |
| `sparse-windowed-recurrence` | `Metatheory.SparseDependencyRecurrence` | recurrence over an explicit finite dependency footprint |
| `transparent-growth-zero-footprint` | `Metatheory.SparseDependencyRecurrence` | transparent elaboration has an empty sparse footprint and zero latency |
| `orthogonal-extension-zero-or-sparse` | `Metatheory.SparseDependencyRecurrence` | ordinary orthogonal extensions classify as zero-footprint or sparse-footprint cases |
| `orthogonal-extension-below-full-envelope` | `Metatheory.SparseDependencyRecurrence` | sparse dependency counts are bounded by the full previous-window envelope |
| `full-coupling-envelope` | `Metatheory.FullCouplingEnvelope` | maximal footprint containing every previous-window dependency site |
| `full-coupling-specializes-sparse-recurrence` | `Metatheory.FullCouplingEnvelope` | the full envelope is a specialization of sparse recurrence |
| `full-coupling-depth-two-affine-law` | `Metatheory.FullCouplingEnvelope` | full depth-two coupling exposes the existing constant-payload affine law |
| `universe-extension-summary` | `CaseStudies.UniverseExtension` | counted full-coupling universe-extension case-study summary |
| `universe-refactored-presentation-same-mu` | `CaseStudies.UniverseExtension` | refactored universe-extension presentation records the same `mu` |
| `global-modality-summary` | `CaseStudies.GlobalModality` | counted full-coupling global-modality case-study summary |
| `promoted-interface-summary` | `CaseStudies.PromotedInterface` | counted active-basis promoted-interface case-study summary |
| `transparent-lemma-zero-footprint` | `CaseStudies.PromotedInterface` | transparent lemma extension has zero sparse footprint and zero latency |
| `coverage-does-not-prove-window` | `CaseStudies.PromotedInterface` | case-study alias for active-basis non-circularity around depth-two windows |
| `coverage-does-not-prove-fibonacci` | `CaseStudies.PromotedInterface` | case-study alias for active-basis non-circularity around Fibonacci growth |
| `sparse-datatype-summary` | `CaseStudies.SparseDatatype` | counted sparse datatype case-study summary |
| `sparse-datatype-recurrence` | `CaseStudies.SparseDatatype` | sparse datatype recurrence over one local dependency in a three-site window |
| `sparse-datatype-below-full-envelope` | `CaseStudies.SparseDatatype` | sparse datatype footprint is bounded by the full previous-window envelope |
| `integration-trace-principle` | `Metatheory.TracePrinciple` | payload plus trace decomposition |
| `canonical-telescope-cardinality` | `Metatheory.CanonicalTelescope` | finite cardinality of a canonical telescope |
| `trace-cost-normal-form-cardinality` | `Metatheory.TraceCostNormalForm` | finite cardinality of a trace-cost normal form |
| `primitive-trace-subtelescope` | `Metatheory.TraceCostNormalForm` | primitive trace-field subtelescope |
| `derived-trace-subtelescope` | `Metatheory.TraceCostNormalForm` | derived trace-field subtelescope |
| `mu-of-trace-cost-normal-form` | `Metatheory.TraceCostNormalForm` | minimal opaque trace count for a normal form |
| `presentation-step-preserves-trace-support` | `Metatheory.PresentationEquivalence` | each explicit presentation generator preserves trace support count |
| `presentation-step-preserves-primitive-cost` | `Metatheory.PresentationEquivalence` | each explicit presentation generator preserves primitive trace count |
| `presentation-equivalence-preserves-trace-fields` | `Metatheory.PresentationEquivalence` | reflexive/symmetric/transitive closure preserves trace fields |
| `presentation-equivalence-preserves-primitive-cost` | `Metatheory.PresentationEquivalence` | presentation equivalence preserves primitive trace count |
| `TransparentlyGenerated` | `Metatheory.MuInvariance` | semantically derived trace fields |
| `RequiresPrimitive` | `Metatheory.MuInvariance` | fields not transparently generated |
| `mu-preserved-by-presentation-step` | `Metatheory.MuInvariance` | `mu` is invariant under one presentation generator |
| `mu-invariant-under-presentation-equivalence` | `Metatheory.MuInvariance` | `mu` is invariant under presentation equivalence |
| `derived-field-deletion-preserves-mu` | `Metatheory.MuInvariance` | duplicate derived deletion preserves minimal opaque cost |
| `requires-primitive-field-essential` | `Metatheory.MuInvariance` | primitive fields cannot be transparently generated |
| `computational-replacement-preserves-mu` | `Metatheory.MuInvariance` | connects `mu` preservation to computational replacement |
| `raw-extension-payload-fields` | `Metatheory.RawStructuralSyntax` | payload telescope projection for the fixed raw extension calculus |
| `raw-extension-structural-clauses` | `Metatheory.RawStructuralSyntax` | structural-clause telescope projection for the fixed raw extension calculus |
| `raw-extension-algebraic-fields` | `Metatheory.RawStructuralSyntax` | algebraic payload telescope projection for the fixed raw extension calculus |
| `act-clause-has-unary-support` | `Metatheory.RawStructuralTyping` | action clauses classify as unary structural trace support |
| `cmp-clause-has-binary-support` | `Metatheory.RawStructuralTyping` | comparison clauses classify as binary structural trace support |
| `horn-clause-has-higher-boundary-support` | `Metatheory.RawStructuralTyping` | horn clauses classify as packaged higher boundary support |
| `algebraic-field-is-payload-not-structural-trace` | `Metatheory.RawStructuralTyping` | higher user operations are algebraic payload, not structural trace projections |
| `naked-higher-face-rejected-or-packaged` | `Metatheory.RawStructuralTyping` | admissible higher structural material is either rejected or packaged as a horn boundary |
| `CanonicalNormalizedSignature` | `Metatheory.SurfaceNormalizationBridge` | canonical payload plus trace normal form for an admissible raw extension |
| `normalizeRawExtension` | `Metatheory.SurfaceNormalizationBridge` | normalize the fixed raw extension calculus to a canonical counted signature |
| `raw-extension-elaborates-to-candidate` | `Metatheory.SurfaceNormalizationBridge` | package an admissible raw extension as a theorem-facing candidate |
| `raw-extension-normalizes-to-canonical-signature` | `Metatheory.SurfaceNormalizationBridge` | named bridge from admissible raw extension to canonical signature |
| `raw-trace-normalizes-to-canonical-signature` | `Metatheory.SurfaceNormalizationBridge` | normalize one typed raw structural role to a trace-cost field |
| `normalize-preserves-support` | `Metatheory.SurfaceNormalizationBridge` | raw normalization preserves the selected trace support |
| `normalize-preserves-arity` | `Metatheory.SurfaceNormalizationBridge` | normalized arity matches normalized historical support |
| `normalize-preserves-primitive-cost` | `Metatheory.SurfaceNormalizationBridge` | raw normalization preserves primitive/derived trace cost |
| `normalization-respects-presentation-equivalence` | `Metatheory.SurfaceNormalizationBridge` | presentation-equivalent normal forms have equal `mu` |
| `normalized-signature-matches-counted-interface` | `Metatheory.SurfaceNormalizationBridge` | normalized payload and trace counts match the raw counted interface |
| `SurfaceHornImage` | `Metatheory.SurfaceToHornImage` | theorem-facing image record for one typed raw structural role |
| `surface-to-horn-normal-form` | `Metatheory.SurfaceToHornImage` | typed raw structural roles normalize into the horn-image classification |
| `surface-to-horn-preserves-support` | `Metatheory.SurfaceToHornImage` | horn-image normalization preserves the selected trace support |
| `surface-to-horn-preserves-arity` | `Metatheory.SurfaceToHornImage` | horn-image normalization preserves support arity |
| `surface-to-horn-preserves-primitive-cost` | `Metatheory.SurfaceToHornImage` | horn-image normalization preserves primitive/derived trace cost |
| `higher-structural-fields-derived` | `Metatheory.SurfaceToHornImage` | packaged higher horn fields are derived in the canonical trace normal form |
| `higher-raw-structural-traces-derived` | `Metatheory.SurfaceToHornImage` | admissible higher raw structural material is rejected or derived through a horn package |
| `raw-syntax-no-naked-higher-structural-projections` | `Metatheory.SurfaceToHornImage` | admissibility boundary for naked higher structural projections |
| `horn-image-complete-for-structural-clauses` | `Metatheory.SurfaceToHornImage` | every well-typed raw structural clause has a horn image |
| `raw-structural-normalizes-to-horn` | `Metatheory.SurfaceToHornImage` | admissible raw structural telescopes normalize to the horn-image package |
| `computational-replacement-preserves-canonical-presentation` | `Metatheory.ComputationalReplacement` | replacement preserves canonical presentation |
| `higher-arity-fields-disappear-from-minimal-signature` | `Metatheory.ComputationalReplacement` | higher arity disappears from minimal signature |
| `higher-arity-computational-replacement` | `Metatheory.ComputationalReplacement` | packaged higher-arity replacement |
| `universal-affine-recurrence` | `Metatheory.UniversalRecurrence` | counted historical-window recurrence |
| `universal-affine-recurrence-from-coherence` | `Metatheory.UniversalRecurrence` | recurrence from coherence/window package |
| `depth1-affine-growth` | `Core.DepthOneAffine` | depth-one affine growth |
| `Delta-depth1-closed` | `Core.DepthOneAffine` | depth-one closed delta form |
| `tau-depth1-closed` | `Core.DepthOneAffine` | depth-one closed cumulative form |
| `Delta-bootstrap` | `Core.AffineRecurrence` | payload-aware depth-two recurrence bootstrap |
| `U-bootstrap-closed` | `Core.AffineRecurrence` | shifted Fibonacci sequence |
| `tau-bootstrap-closed` | `Core.AffineRecurrence` | cumulative shifted Fibonacci form |

## Planned Names Not Yet Present

Phase 8 case-study and audit artifacts are now present. The remaining planned
names are the Phase 9 top-level integration aliases and the Phase 10 paper
rewrite references.
