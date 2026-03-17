# Search and Selection

## Core Strict Loop

The current strict lane in `engine/src/RunAbInitio.hs` is roughly:

```text
for each step:
  compute bar from prior discovered (nu, kappa)
  compute strict admissibility bands from interface debt
  search exact kappa bands in priority order
  dedupe by canonical key
  evaluate candidates
  keep only bar-clearing candidates
  apply semantic minimality
  if viable set exists:
    restrict to minimum kappa
    pick minimal positive overshoot with deterministic tie-breaks
    insert selected structure into library
  else:
    stop
```

This is the semantic skeleton worth keeping.

## Admissibility

The current donor is `engine/src/AbInitioPolicy.hs`.

Key ideas:

- bootstrap steps 1 to 3 have explicit fixed caps and bands.
- later steps derive a `cap` and exact bands from interface debt over a depth-2 active window.
- export weight is structural, not name-based.

The main donor concepts are:

- `countActiveExports`
- `estimateObligationSurface`
- `strictInterfaceDebt`
- `strictAdmissibility`

The rewrite should preserve the principle:

- open cost bands because structural debt demands them,
- not because a semantic target family is expected next.

## Atomic Enumeration

`engine/src/MBTTEnum.hs` is the main donor for atomic generation.

Important features already present:

- typed AST generation over the MBTT grammar
- budget splitting for compound forms
- expression caching by context depth and library fingerprint
- exact clause-band filter
- connectedness and library-window filters
- canonical dedupe
- frontier modes, including obligation-guided retention

Important weakness:

- this module is still where the atomic frontier hit the semantic noise wall in practice.

For the rewrite, port the semantics and data shapes, not the assumption that the current search settings are enough.

## Molecular Shortcut in the Current Repo

The current strict lane does this before atomic enumeration:

```text
enumerateStrictBandCandidates(lib, band):
  if molecular candidates exist in this exact band:
    use only molecular candidates
  else:
    fall back to atomic MBTT enumeration
```

That means the current 15-step success is not evidence that open-ended atomic discovery is solved.

## Evaluation

There are three relevant evaluator layers:

### Structural or native `nu`

- `engine/src/MBTTNu.hs`
- `engine/src/StructuralNu.hs`

This is the anonymous AST-based novelty computation.

### Selection math

The strict lane compares each candidate's `rho = nu / kappa` to the current bar.

### Current molecular hybrid

When a seed comes from the molecular lane, the current strict path computes:

```text
hybrid strict nu = nu_G + nu_C + selected topological nu_H
```

This is exactly why late strict `nu` values diverge from the larger structural paper values.

## Selection Key

The current strict tie-break logic is not just `rho`.

`honestSelectionKey` in `RunAbInitio.hs` orders by:

1. minimal positive overshoot above the bar
2. lower charged `kappa`
3. stronger eliminator score
4. stronger former lifecycle score
5. stronger dependent motive density
6. stronger internal adjoint score
7. higher library-reference density
8. higher generic binder count
9. higher closure score
10. lower bit cost
11. stable structural hash

Even if the Rust rewrite simplifies this, it should do so consciously.

## Minimality

`engine/src/StrictMinimality.hs` moved the current repo from naive deletion checks to terminal SCC amputation.

The idea:

- build a clause dependency graph over the telescope,
- compute SCCs,
- try removing terminal SCC subbundles,
- reject a candidate if a smaller admissible subbundle still clears the bar.

This is a strong donor idea for the rewrite, especially because atomic search otherwise tends to keep packed junk bundles.

## Resume and Determinism

The user's frozen architecture moves checkpointing into a first-class contract. That is the right direction. The old Haskell engine has caches and band-local state, but the new repo should make step and frontier resume explicit from the start.

## What to Keep for the Atomic Rewrite

- exact-band search
- structural admissibility
- canonical dedupe
- deterministic ordering
- SCC minimality
- name-free evaluation
- post-hoc-only decoding

## What to Change for the Atomic Rewrite

- remove molecular candidates from the primary search path
- replace current in-memory frontier behavior with explicit resumable state
- use exact rational or integer comparison in the hot path
- harden anti-junk retention so true eliminator structures are not deleted early

## Primary Source Files

- `engine/src/MBTTEnum.hs`
- `engine/src/AbInitioPolicy.hs`
- `engine/src/RunAbInitio.hs`
- `engine/src/StrictMinimality.hs`
- `engine/src/MBTTNu.hs`
- `engine/src/StructuralNu.hs`
