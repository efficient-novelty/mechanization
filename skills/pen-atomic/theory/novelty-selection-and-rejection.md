# Novelty, Selection, And Rejection

This file collects the theorems that explain why the trajectory is geometric, why minimal overshoot matters, and why discrete competitors keep losing.

## Exact Novelty Split

The manuscripts use:

```text
nu = nu_G + nu_C + nu_H
```

with the following interpretation:

- `nu_G`:
  new formation and introduction schemas,
- `nu_C`:
  eliminators, projections, and cross-interface interaction schemas,
- `nu_H`:
  path, Kan, and computation payload.

The rewrite should preserve this as a structural decomposition, even if implementation details change.

## OIT Versus HIT

Two baseline facts shape the theory.

- Ordinary inductive types have exponential predicate space at linear cost.
- Higher inductive types lose some naive set-valued maps because path constructors constrain eliminators.

The papers then restore superlinear growth through three mechanisms:

1. dependent elimination,
2. library cross-interaction,
3. composite constructions such as products and function types.

The practical consequence is that geometric structure is still favored, even though HITs are more constrained than plain OITs.

## Combinatorial Schema Synthesis

The central late-stage theorem says:

- if a candidate introduces `k` new unary formers into a library of size `|B|`,
- then type-inhabitation schemas at depth `d` grow like `Omega(|B|^d * k)`.

This is the theoretical reason late modal and temporal shells can clear the bar despite small `kappa`.

## Bounded Effort Growth

The late papers also argue that:

- `kappa` grows with the intrinsic arity of a candidate and its compatibility clauses,
- not with total library size.

So late stages can combine a bounded denominator with rapidly increasing interaction payload.

## Divergence Of Efficiency

Under an endless supply of structurally distinct geometric primitives:

- `rho_n` can diverge,
- while the bar grows only as a cumulative average under Fibonacci pressure.

The important boundary is the companion remark:

- divergence is a theorem about what would happen if profitable geometric continuations remained available,
- halting is a theorem about what actually happens in PEN's current strict lane once the temporal-cohesive shell internalizes the remaining continuous continuations.

## Why Discrete Branches Lose

The asymptotic rejection proposition is:

- if `nu_H = 0` and `nu_G = O(kappa)`,
- then the candidate family eventually fails the `d = 2` bar permanently.

Representative first-admissibility examples from the papers:

- `N`: `kappa = 3`, `nu <= 4`, `rho <= 1.33`, already below the step-5 bar `2.14`,
- classical logic and power-set style axioms:
  `rho <= 1.00`,
- scheme, measure, topos, and Galois packages:
  structurally real, but still too discrete under this objective.

For the rewrite, this matters because atomic search should not be surprised if arithmetic-style frontiers remain subcritical.

## Minimal Overshoot, Not Greedy `max-rho`

PEN does not maximize `rho`.
It selects the least supercritical candidate:

```text
choose X with rho(X) >= Bar_n
minimizing rho(X) - Bar_n
```

with lower `kappa` and deterministic structural keys as tie-breaks.

The standard example is:

- at step 5, `S^1` is chosen before `S^2`,
- because `S^2` is too supercritical too early,
- then `S^2` is selected later when the bar actually demands it.

This is not a detail.
The review-hardening ablations show that greedy `max-rho` diverges from the canonical sequence.

## Structural Unity And Minimality

The papers fight axiom packing by demanding:

- one structurally unified bundle,
- no proper sub-bundle that is already well-typed, complete, and bar-clearing.

This becomes the engine-level SCC minimality story:

- packed bundles are only allowed if their proper terminal SCC subbundles fail the bar or become ill-typed.

## Why This Matters For `pen-atomic`

- Preserve minimal positive overshoot.
- Keep novelty structural and name-free.
- Expect discrete packages to underperform unless the objective changes.
- Preserve a strong packed-bundle minimality test, because otherwise junk bundling dominates.

## Cross-Links

- Read [genesis.md](genesis.md) for the exact sequence produced by these rules.
- Read [late-framework-abstraction.md](late-framework-abstraction.md) for the late bundles that survive the bar.
- Read [terminal-dct.md](terminal-dct.md) for the step-15 fixed-point and halting story.

