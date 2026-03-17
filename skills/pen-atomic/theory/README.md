# Theory Tree

Use this folder as the math-facing companion to the engineering references in `../references/`.
It distills the TeX corpus into the theory we actually need for the `pen-atomic` rewrite.

## Reading Order

1. [pen-model.md](pen-model.md) for the PEN objective, cost model, and selection rule.
2. [coherence-and-scaling.md](coherence-and-scaling.md) for the `d = 2` theorem and Fibonacci debt/bar growth.
3. [novelty-selection-and-rejection.md](novelty-selection-and-rejection.md) for `nu`, minimal overshoot, and why discrete branches lose.
4. [genesis.md](genesis.md) for the exact 15-step strict canon used by the rewrite.
5. [late-framework-abstraction.md](late-framework-abstraction.md) for the shell-first reading of steps 10 to 14.
6. [terminal-dct.md](terminal-dct.md) for the step-15 temporal shell, the DCT completion story, and the halting claim.
7. [downstream-interpretations.md](downstream-interpretations.md) for the physics and RH papers that depend on the late PEN library but must not steer search.
8. [source-map.md](source-map.md) for the full manuscript-by-manuscript map of what was kept, merged, or demoted.

## What Is Canonical For The Rewrite

- The strict quantitative canon is the 15-step table in [genesis.md](genesis.md), copied from `pen_paper.tex`.
- The core metatheory is the combination of:
  - the PEN cost model in [pen-model.md](pen-model.md),
  - the coherence-depth and Fibonacci recurrence package in [coherence-and-scaling.md](coherence-and-scaling.md),
  - the novelty and rejection theorems in [novelty-selection-and-rejection.md](novelty-selection-and-rejection.md).
- Steps 10 to 15 should be read shell-first, not as target labels. That boundary is captured in [late-framework-abstraction.md](late-framework-abstraction.md) and [terminal-dct.md](terminal-dct.md).

## Proof-Status Boundary

- Exact or near-exact canon:
  - the strict 15-step ordering,
  - the charged `kappa`,
  - the strict bar values,
  - the early and middle `nu` counts,
  - the `d = 2` coherence-window theorem package.
- Expository but still important:
  - the semantic readings of steps 10 to 14,
  - the API-style clause bundles used to explain late shells.
- Explicitly non-canonical for the rewrite unless the user asks for them:
  - older paper-level late totals such as `26, 34, 46, 62, 103`,
  - cosmology, physics, or RH interpretations built on top of steps 14 and 15.

## How This Tree Relates To `../references/`

- `../references/` is the engineering and donor-code layer.
- `theory/` is the math and proof-contract layer.
- When a reference file talks about theory, it should point here first.
- The raw TeX files remain provenance, not the primary reading surface.

