# Coherence-Depth Case Study Report

Date: 2026-05-02

Phase 8 adds explicit examples and audit data for the fixed extension-calculus
bridge. The examples are intentionally small: they exercise the counting
surface and recurrence classifications without pretending to parse arbitrary
Cubical Agda programs.

| Case | Agda module | Recurrence | `mu` | Audit fixture |
|---|---|---|---:|---|
| Universe extension | `CaseStudies.UniverseExtension` | full-coupling | 2 | `universe_extension.yaml` |
| Universe extension, refactored | `CaseStudies.UniverseExtension` | full-coupling | 2 | `universe_extension_refactored.yaml` |
| Global modality | `CaseStudies.GlobalModality` | full-coupling | 3 | `global_modality.yaml` |
| Promoted interface | `CaseStudies.PromotedInterface` | promoted-active-basis | 2 | `promoted_interface.yaml` |
| Transparent lemma extension | `CaseStudies.PromotedInterface` | transparent-zero | 0 | `transparent_lemma_extension.yaml` |
| Sparse datatype | `CaseStudies.SparseDatatype` | sparse-local | 1 | `sparse_datatype.yaml` |

The audit script checks the stable mechanical facts: transparent extensions
must have zero footprint and zero `mu`; sparse extensions must not claim the
full envelope; full-coupling extensions must declare the full envelope; promoted
interface examples must include active-basis totality; and refactored
presentations preserve `mu` within their presentation group.
