# Paper Improvement Plan for `1_coherence_depth.tex`

## Locked End-State

1. The fixed fully coupled cubical extension calculus over a chosen CCHM-style core is the sole theorem object.
2. Exact obligation stabilization, minimal-signature elimination, and recent-history factorization remain explicitly separated.
3. The abstract wrapper is stated only by explicit hypotheses and is claimed only for the named cubical and strict/fibrant $2$LTT-style instances.
4. The mechanization boundary is described by the exact three-part bridge package of `rem:bridge-target`, not by broader paper-level rhetoric.

## Remaining Execution Order

1. Final build/PDF spot-check when a TeX toolchain is available.
2. Freeze the text if that build is clean, or fix only mechanical fallout if it is not.

## Remaining Work

### A. Build verification

Target:

- rebuild `1_coherence_depth.tex` and inspect any warning or layout fallout around the bridge-boundary edits;
- focus the PDF spot-check on the abstract, introduction tables, `rem:bridge-target`, `sec:mechanization`, and the conclusion.

Fallback:

- if no TeX binary is available, keep the missing-toolchain note explicit and do not reopen wording without a concrete build-driven reason.

Why this matters:

- after the wording and claim-scope cleanup, the remaining risk is unverified typesetting rather than mathematical overclaiming.
