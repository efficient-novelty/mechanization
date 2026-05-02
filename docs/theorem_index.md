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
agda --transliterate Geometry/Clutching.agda
agda --transliterate Test/ClutchingSmoke.agda
agda --transliterate Test/Fibonacci.agda
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
| `integration-trace-principle` | `Metatheory.TracePrinciple` | payload plus trace decomposition |
| `canonical-telescope-cardinality` | `Metatheory.CanonicalTelescope` | finite cardinality of a canonical telescope |
| `trace-cost-normal-form-cardinality` | `Metatheory.TraceCostNormalForm` | finite cardinality of a trace-cost normal form |
| `primitive-trace-subtelescope` | `Metatheory.TraceCostNormalForm` | primitive trace-field subtelescope |
| `derived-trace-subtelescope` | `Metatheory.TraceCostNormalForm` | derived trace-field subtelescope |
| `mu-of-trace-cost-normal-form` | `Metatheory.TraceCostNormalForm` | minimal opaque trace count for a normal form |
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

These names are intentionally absent until the remaining bridge phases are
implemented:

- `raw-extension-elaborates-to-candidate`
- `raw-trace-normalizes-to-canonical-signature`
- `surface-to-horn-normal-form`
- `raw-structural-normalizes-to-horn`
- `mu-invariant-under-presentation-equivalence`
- `requires-primitive-field-essential`
- `global-action-totality-implies-active-basis-contract`
- `coverage-alone-does-not-imply-fibonacci`
- `sparse-windowed-recurrence`
- `full-coupling-specializes-sparse-recurrence`
