# PEN: Principle of Efficient Novelty in Cubical Agda

Mechanization of the PEN framework in Agda. The repository now has two
connected but distinct centers of gravity:

- the original counting/oracle artifact for recurrence, novelty, and schema
  enumeration
- the coherence-depth theorem package used by `1_coherence_depth.tex`

## Setup

### Prerequisites

1. Agda 2.8.0 or newer with Cubical support
2. The Cubical library from <https://github.com/agda/cubical>

### Installation

```bash
# Clone the cubical library if needed
git clone https://github.com/agda/cubical.git ~/.agda/cubical

# Register it with Agda
echo "$HOME/.agda/cubical/cubical.agda-lib" >> ~/.agda/libraries

# Move into the Agda workspace
cd /mnt/c/dev/pen/agda

# Type-check the top-level artifact
agda --transliterate PEN.agda
```

`--transliterate` is useful in this repository because many theorem names and
comments use Unicode identifiers.

## Project Structure

The top-level Agda tree currently looks like this:

```text
agda/
|- pen.agda-lib
|- PEN.agda
|- README.md
|- progress_tracking.md
|- Core/
|- Metatheory/
|- ObligationGraph/
|- Saturation/
|- Adjunction/
|- Oracle/
|- OpSchema/
|- Geometry/
|- Logic/
|- bridge/
`- Test/
```

The most important directories are:

- `Core/`: arithmetic and paper-facing recurrence packages, including
  `Nat.agda`, `AffineRecurrence.agda`, and `DepthOneAffine.agda`
- `Metatheory/`: theorem-facing coherence-depth surface used by the paper
- `ObligationGraph/` and `Saturation/`: the older counting/barrier model that
  still supports the recurrence side of the artifact
- `Adjunction/`: supporting lower-bound scaffolding
- `Oracle/` and `OpSchema/`: novelty and capability measurement work
- `Test/`: lightweight regression modules for the main surfaces

## Current Status

### Coherence-Depth Track: Integrated

The coherence-depth theorem package is now exposed from `PEN.agda` and has a
top-level smoke import in `Test/MetatheorySmoke.agda`.

Current theorem-facing modules:

- `Metatheory/Obligations.agda` defines the paper's obligation-language
  surface:
  `HistoricalSupport`, `PrimitiveCost`, `ObligationLanguage`,
  `StabilizesAt`, `HasCoherenceDepth`, `FactorsThroughWindow`, and
  `HasChronologicalWindowSize`.
- `Metatheory/Obligations.agda` also now includes the arity-to-dimension
  surface for `lem:arity-dimension`:
  `Positive`, `CoherenceCellShape`,
  `historical-arity-forces-cell-dimension`, and
  `irreducible-obligation-requires-cell`.
- `Metatheory/Extensional.agda` proves the UIP/extensional collapse via
  `UIP-forces-depth-1` and `history-truncates-to-one`.
- `Metatheory/KanSubsumption.agda` now packages both the raw arity-3 open-box
  derivation surface, the exact theorem-facing horn-reduction wrapper, and the
  exact telescopic wrapper for remote binary comparisons via
  `HornExtensionFiber`, `horn-extension-fiber-contractible`,
  `structural-horn-language`,
  `structural-integration-horn-reduction`,
  `remote-layer-obligation-derived`,
  `TelescopicTraceChain`,
  `TelescopicSubsumptionView`,
  `telescopic-subsumption`,
  `telescopic-remote-comparison-derived`,
  `arity3-obligation-syntactically-derivable`,
  `history-beyond-two-algorithmically-subsumed`, and
  `arity3-open-box-hfilled`.
- `Metatheory/UpperBound.agda` turns that horn-extension package into the
  exact paper-facing upper bound for `thm:upper` via
  `structural-obligation-set-equivalence` and
  `structural-stabilizes-at-two`.
- `Metatheory/ChronologicalWindow.agda` upgrades that upper bound to the
  exact paper-facing chronological-window corollary `cor:chrono-window` via
  `primitive-obligations-factor-through-last-two`,
  `one-layer-window-insufficient`,
  `two-layer-chronological-window`, and
  `chronological-markov-blanket`.
- `Metatheory/ExactDepth.agda` packages the exact paper-facing depth-two
  corollary `cor:d2` via
  `stabilization-at-zero-impossible`,
  `stabilization-at-one-impossible`,
  `structural-coherence-depth-exactly-two`,
  `structural-chronological-window-size-exactly-two`,
  `cubical-coherence-depth-exactly-two`, and
  `cubical-chronological-window-size-exactly-two`, while threading through
  the lower-bound witnesses
  `cubical-binary-sealing-obstruction` and
  `cubical-triangle-identity-corollary`.
- `Metatheory/AdjunctionBarrier.agda` packages the lower bound against global
  depth-1 collapse via `explicit-binary-sealing-obstruction`,
  `triangle-identity-corollary`, `depth1-insufficient`, and
  `adjunction-barrier`.
- `Metatheory/TwoDFoundations.agda` packages the exact paper-facing abstract
  depth-two law `thm:2d-foundations` via
  `FullyCoupled2DFoundation`,
  `depth-two-law-for-2d-foundations`,
  `chronological-window-size-two-for-2d-foundations`,
  `constant-payload-depth-two-law`,
  `cubical-2d-foundation`,
  `cubical-depth-two-law-for-2d-foundations`, and
  `cubical-chronological-window-size-two-for-2d-foundations`.
- `Core/AffineRecurrence.agda` and `Core/DepthOneAffine.agda` package the
  payload-aware affine recurrence and the paper-facing depth-1 corollary.

What this means in practice:

- the extensional depth-1 collapse is mechanized
- the exact cubical upper bound `O^(k)(X) ~= O^(2)(X)` is mechanized
- the lower bound against cubical depth-1 collapse is mechanized
- the exact horn-reduction surface and its arity-3 computational witness are
  mechanized
- the exact telescopic subsumption wrapper for remote binary comparisons is
  mechanized
- the exact chronological-window corollary is mechanized
- the exact `d = 2` coherence-depth corollary is mechanized
- the abstract `2`D-foundations wrapper is mechanized
- the paper's arity-to-dimension dictionary is now mechanized
- the recurrence side has both the payload-aware affine statement and the
  depth-1 closed forms

What is still open on the paper-facing coherence-depth plan:

- the remaining backlog in `mechanization_plan.md` now starts at
  `prop:transparent`
- exact paper-level wrappers such as the later
  interface/trace/canonicity/clutching results are still pending

### Counting / Oracle Track

The original PEN artifact remains active and is no longer just a stub:

- `ObligationGraph/Recurrence.agda` still provides the stable payload-free
  recurrence surface
- `Oracle/Kappa.agda`, `Oracle/Nu.agda`, and `Oracle/Efficiency.agda` contain
  the basic oracle and selection-measure work
- `OpSchema/` contains the more advanced operation-schema-based novelty track
  used for the R1-R16 validation work

### Remaining Large Gap

The end-to-end Genesis selection loop is still not implemented as a completed
Agda artifact. The repository contains bridge and engine work around that
direction, but the main unfinished Agda milestone is still the selection-loop
story rather than the already-integrated theorem package above.

## Useful Entry Points

If you are trying to orient yourself quickly, start here:

- `PEN.agda`: top-level export surface
- `Metatheory/Obligations.agda`: obligation-language and arity/dimension API
- `Metatheory/Extensional.agda`: depth-1 theorem
- `Metatheory/KanSubsumption.agda`: horn-reduction, telescopic subsumption,
  and arity-3 open-box package
- `Metatheory/UpperBound.agda`: exact depth-two upper-bound/stabilization
  wrapper
- `Metatheory/ChronologicalWindow.agda`: exact chronological-window wrapper
- `Metatheory/ExactDepth.agda`: exact depth-two corollary wrapper
- `Metatheory/AdjunctionBarrier.agda`: lower-bound obstruction package
- `Metatheory/TwoDFoundations.agda`: abstract 2D-foundations wrapper and
  constant-payload affine/Fibonacci consequence
- `Core/AffineRecurrence.agda`: payload-aware recurrence
- `Test/MetatheorySmoke.agda`: lightweight regression import for the theorem
  package
- `progress_tracking.md`: longer historical log for the Agda work

## Verification Commands

For the integrated coherence-depth surface:

```bash
cd /mnt/c/dev/pen/agda
agda --transliterate PEN.agda
agda --transliterate Test/MetatheorySmoke.agda
agda --transliterate Test/Fibonacci.agda
```

Useful additional checks:

```bash
agda --transliterate Metatheory/Obligations.agda
agda --transliterate Metatheory/Extensional.agda
agda --transliterate Metatheory/KanSubsumption.agda
agda --transliterate Metatheory/UpperBound.agda
agda --transliterate Metatheory/ChronologicalWindow.agda
agda --transliterate Metatheory/ExactDepth.agda
agda --transliterate Metatheory/AdjunctionBarrier.agda
agda --transliterate Metatheory/TwoDFoundations.agda
agda --transliterate Test/OpSchemaTest.agda
agda --transliterate Test/BlindTest.agda
```

## Known Issues

1. The repository intentionally defines its own `Nat` surface in
   `Core/Nat.agda` because some Cubical library data imports trigger Agda
   2.8.0 compatibility problems in this setup.
2. Some reflection-heavy oracle work has different ergonomics from the Cubical
   theorem package; `progress_tracking.md` is the best place to check the
   current caveats for those modules.

## References

1. Univalent Foundations Program, *Homotopy Type Theory*, 2013
2. Cohen et al., "Cubical Type Theory", TYPES 2015
3. Vezzosi et al., "Cubical Agda", ICFP 2019
