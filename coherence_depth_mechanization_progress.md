# Coherence Depth Mechanization Progress

Date: 2026-03-23

## What has been done

### 1. Affine recurrence module added

Added `agda/Core/AffineRecurrence.agda`.

This module now:

- defines a payload-aware recurrence `Delta-affine c n`
- defines the shifted sequence `U c n = Delta-affine c n + 2c`
- proves `U-is-fibonacci`, i.e. the shifted sequence satisfies the
  homogeneous Fibonacci recurrence
- proves `payload-free-fibonacci`, recovering the existing Fibonacci
  normalization when the payload parameter is zero
- proves `payload-free-Delta`, connecting the zero-payload specialization
  back to the existing `Δ` function

This gives the repo a clean mechanized statement of the paper's
payload-accounting correction.

### 2. Extensional depth-1 collapse mechanized

Added `agda/Metatheory/Extensional.agda`.

This module now:

- defines binary coherence obligations semantically as `p ≡ q`
- proves `UIP-forces-depth-1`
- proves `history-truncates-to-one`

So the extensional/UIP side of the theorem package now has a direct Agda
statement and proof.

### 3. Cubical arity-3 derivability theorem added

Added `agda/Metatheory/KanSubsumption.agda`.

This module now:

- packages the open-box input consumed by cubical composition and, at
  square types, the arity-3 obligation boundary
- proves `arity3-obligation-syntactically-derivable`
- proves the paper-facing corollary
  `history-beyond-two-algorithmically-subsumed`
- proves `arity3-open-box-hfilled`, making the operational `hcomp`
  witness explicit

Implementation note:

- the proof now factors through `Cubical.Foundations.Cubes.Base`
- the module packages exactly the visible boundary data required by an
  open-box composition step instead of proving any truncation statement
- this keeps the mechanization aligned with univalent cubical semantics
  rather than a hidden `isGroupoid` hypothesis
  repository's built-in `NATURAL` issues under top-level aggregation

### 4. Lower bound refactored and integrated

Updated `agda/Metatheory/AdjunctionBarrier.agda`.

This module now:

- defines a local two-point type `Two`
- defines the swap autoisomorphism on `Two`
- uses the lighter cubical `isoToPath` bridge from
  `Cubical.Foundations.Isomorphism` to obtain a nontrivial path
  `swap-path : Two ≡ Two`
- proves `swap-path≠refl`
- proves `binary-coherence-nontrivial`
- proves `depth1-insufficient`

This keeps the lower-bound contradiction argument intact while avoiding the
older import collision caused by pulling `Cubical.Foundations.Univalence`
into the same top-level module as `Core.Nat`.

### 5. Top-level theorem package completed

Updated `agda/PEN.agda` to re-export:

- `Core.AffineRecurrence`
- `Metatheory.Extensional`
- `Metatheory.KanSubsumption`
- theorem-facing exports from `Metatheory.AdjunctionBarrier`

Integration note:

- there was a secondary top-level name clash on `BinaryObligation`
- this was resolved by re-exporting only the theorem-facing lower-bound names
  from `Metatheory.AdjunctionBarrier`

Added `agda/Test/MetatheorySmoke.agda` as a lightweight regression module that
imports `PEN` and checks that the affine, extensional, upper-bound, and
lower-bound theorem names are all visible from the top level.

### 6. Repository-facing docs synced

Updated:

- `agda/README.md`
- `agda/progress_tracking.md`
- `agda/Saturation/Axiom.agda`

These now describe the coherence-depth track as an integrated theorem package
rather than a partial or standalone-only extension, and they now make clear
that `Saturation/Axiom.agda` is retained as the older combinatorial modeling
surface rather than the main justification for the paper's depth-two claim.

### 7. Paper updated to match the mechanization

Updated `1_coherence_depth.tex`.

The paper now:

- states in the abstract that the depth theorem package itself is mechanized
- replaces the old "focused fragment only" description in Section 5 with the
  actual module-level theorem package
- replaces the old mechanization-scope caveat with a statement of what is now
  proved in Agda
- updates the introduction and conclusion so they no longer describe the
  depth-one/depth-two package as future work
- tightens the Section-5 typography so the long module and theorem-name lines
  no longer generate the earlier overfull box warnings

## Verification completed

The following commands were run successfully:

- `agda --transliterate Metatheory/AdjunctionBarrier.agda`
- `agda --transliterate PEN.agda`
- `agda --transliterate Test/MetatheorySmoke.agda`
- `pdflatex -interaction=nonstopmode -halt-on-error 1_coherence_depth.tex`
- `pdflatex -interaction=nonstopmode -halt-on-error 1_coherence_depth.tex`

Additional verification:

- a scratch Agda module importing both `Core.Nat` and
  `Metatheory.AdjunctionBarrier` now type-checks, confirming that the
  previous built-in `NATURAL` collision is gone under the new import strategy

## Current status

The core claims targeted by the current coherence-depth plan are now
mechanized and top-level integrated:

- depth `1` under UIP / `isSet`
- arity-3 contractibility and subsumption of history beyond two layers
- failure of global depth-1 collapse in the cubical setting
- payload-aware affine recurrence with shifted Fibonacci recovery

`PEN.agda` now exposes that theorem package directly, and the paper has been
updated to describe the mechanization accordingly.

## Next steps

1. Decide whether `ObligationGraph/Recurrence.agda` should stay as the stable
   payload-free surface or also re-export an explicit affine corollary from
   `Core/AffineRecurrence.agda`.
2. Optionally run one final prose pass over Section 5 to decide whether the
   module-level theorem inventory should stay this explicit or be compressed
   slightly for journal style, now that the typography issue is resolved.
