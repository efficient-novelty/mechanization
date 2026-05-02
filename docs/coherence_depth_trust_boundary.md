# Coherence-Depth Trust Boundary

Date: 2026-05-02

This document records what the current Cubical Agda artifact checks directly,
what is read through the paper-level bridge in `1_coherence_depth.tex`, and
which files are outside the theorem-facing trust boundary.

The current checked core is the existing coherence-depth theorem package. The
raw surface bridge requested in `paper_improvement_plan.md` is still future
work and starts with `Metatheory/CanonicalTelescope.agda`.

## Trusted Base

- Agda 2.8.0 with Cubical support.
- The Cubical library declared by `agda/pen.agda-lib`.
- Repository-local arithmetic in `agda/Core/Nat.agda`.
- The checked theorem-facing modules listed below.

Known warning class: several current theorem modules type-check with Cubical
Agda `UnsupportedIndexedMatch` warnings. These warnings should remain visible
in artifact checks until the affected proofs are refactored or the warning is
shown irrelevant for the bridge layer.

## Theorem-Facing Map

| Paper result | Agda module | Main theorem/package names | Postulate-free? | Bridge dependency |
|---|---|---|---:|---|
| `prop:transparent` | `Metatheory/InterfaceCalculus.agda` | `transparent-growth-keeps-library-state`, `transparent-definitions-preserve-active-interface`, `transparent-definitions-have-zero-integration-latency`, `transparent-user-level-code-lies-outside-the-recurrence` | yes | no |
| `lem:arity-dimension` | `Metatheory/Obligations.agda` | `historical-arity-forces-cell-dimension`, `irreducible-obligation-requires-cell` | yes | no |
| `thm:extensional` | `Metatheory/Extensional.agda` | `UIP-forces-depth-1`, `history-truncates-to-one` | yes | no |
| `lem:horn-reduction` | `Metatheory/KanSubsumption.agda` | `HornExtensionFiber`, `structural-integration-horn-reduction`, `remote-layer-obligation-derived` | yes | no |
| `lem:telescopic` | `Metatheory/KanSubsumption.agda` | `TelescopicSubsumptionView`, `telescopic-subsumption`, `telescopic-remote-comparison-derived` | yes | yes, for surface-signature reading |
| `thm:upper` | `Metatheory/UpperBound.agda` | `structural-obligation-set-equivalence`, `structural-stabilizes-at-two` | yes | no |
| `cor:contractible-factor` | `Metatheory/UpperBound.agda` | `ContractibleRemoteFactor`, `structural-obligation-contractible-factorization`, `contractible-remote-factor-contractible` | yes | no |
| `cor:chrono-window` | `Metatheory/ChronologicalWindow.agda` | `primitive-obligations-factor-through-last-two`, `two-layer-chronological-window`, `chronological-markov-blanket` | yes | no |
| `thm:adjunction` | `Metatheory/AdjunctionBarrier.agda` | `explicit-binary-sealing-obstruction`, `triangle-identity-corollary`, `adjunction-barrier` | yes | no |
| `thm:clutching` | `Geometry/Clutching.agda` | `clutching-family-theorem`, `hopf-binary-clutching-nontrivial`, `clutching-horn-extension-fiber-contractible` | yes | no |
| `cor:d2` | `Metatheory/ExactDepth.agda` | `structural-coherence-depth-exactly-two`, `cubical-coherence-depth-exactly-two`, `cubical-chronological-window-size-exactly-two` | yes | no |
| `thm:2d-foundations`, `cor:2ltt-instantiation` | `Metatheory/TwoDFoundations.agda`, `Metatheory/TwoLTTFoundations.agda` | `depth-two-law-for-2d-foundations`, `cubical-depth-two-law-for-2d-foundations`, `two-level-depth-two-law-for-2d-foundations` | yes | no |
| `cor:refactoring` | `Metatheory/Refactoring.agda` | `historical-support-correspondence`, `refactoring-invariance` | yes | no |
| `thm:canonicity` | `Metatheory/CanonicityDensity.agda` | `CanonicityDensityTheorem`, `global-admissibility-forces-maximal-interface-density` | yes | yes, for raw-surface interpretation |
| `thm:trace` | `Metatheory/TracePrinciple.agda`, `Metatheory/InterfaceCalculus.agda` | `IntegrationTracePrinciple`, `public-counting-normal-form`, `integration-trace-principle` | yes | yes, for raw-surface interpretation |
| `thm:computational-replacement` | `Metatheory/ComputationalReplacement.agda` | `CanonicalTraceSignature`, `TracePresentation`, `computational-replacement-preserves-canonical-presentation`, `higher-arity-computational-replacement` | yes | yes, to connect raw declarations to trace presentations |
| `thm:factorization-complete`, `thm:higher-elim` | `Metatheory/KanSubsumption.agda`, `Metatheory/UpperBound.agda`, `Metatheory/ChronologicalWindow.agda`, `Metatheory/ComputationalReplacement.agda` | `structural-primitive-eliminates-above-two`, `primitive-obligations-factor-through-last-two`, `higher-arity-fields-disappear-from-minimal-signature` | yes | yes |
| `thm:recurrence` | `Metatheory/UniversalRecurrence.agda` | `UniversalAffineRecurrence`, `universal-affine-recurrence`, `universal-affine-recurrence-from-coherence` | yes | yes, for `mu` reading |
| `cor:d1` | `Core/DepthOneAffine.agda` | `depth1-affine-growth`, `Delta-depth1-closed`, `tau-depth1-closed` | yes | yes, for `mu` reading |
| `cor:fibonacci` | `Core/AffineRecurrence.agda` | `Delta-bootstrap`, `U-bootstrap-closed`, `tau-bootstrap-closed` | yes | yes, for `mu` reading |

## Outside The Current Theorem-Facing Boundary

- The three-part raw bridge isolated by `rem:bridge-target` in the paper:
  raw declarations to canonical trace presentations, presentation invariance
  for raw-normalized signatures, and matching canonical signatures with the
  counted interface.
- The planned modules `SurfaceToHornImage.agda`, `MuInvariance.agda`,
  `ActiveBasisContract.agda`, `SparseDependencyRecurrence.agda`, and
  `FullCouplingEnvelope.agda`.
- Auxiliary bridge payload harnesses such as `agda/Test/BridgePayloadContract.agda`;
  that file intentionally contains postulated contract predicates and is not
  imported by `PEN.agda` or the theorem smoke tests.

