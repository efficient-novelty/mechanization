# Coherence And Scaling

This file condenses the theorem package behind the `d = 2` lane.

## Coherence Window

The foundational definitions are:

- `O^(k)(X)`:
  the normalized atomic obligations induced when candidate `X` is sealed against `k` layers of history.
- coherence window `d`:
  the least depth after which obligations stabilize.

In the papers:

```text
O^(1)(X) subseteq O^(2)(X) subseteq O^(3)(X) subseteq ...
```

and the foundation has coherence window `d` if `O^(k)(X)` is equivalent to `O^(d)(X)` for all `k >= d`.

## The Extensional Result

Theorem A:

- in extensional MLTT plus UIP, the coherence window collapses to `d = 1`.

Reason:

- higher identity data is propositionally trivial,
- so after unary transport there is no independent higher sealing debt.

For `pen-atomic`, this mainly matters as a contrast class.
The rewrite is not targeting that regime.

## The Intensional Result

Theorem B:

- in the fully coupled CCHM or HoTT lane used by PEN, the coherence window is exactly `d = 2`.

The papers justify this with four overlapping arguments.

### 1. Dimensional Upper Bound

- direct deep library lookups are still arity-1,
- genuine new sealing pressure is binary,
- higher fillers are computed from 2-boundary data in the cubical setting.

So no independent obligation of arity `>= 3` survives as new sealing data.

### 2. Topological Lower Bound

- the Hopf or clutching family forces genuinely binary historical interaction,
- so the system cannot collapse to `d = 1`.

### 3. Adjunction Barrier

- adjunction data needs 0-cells, 1-cells, and 2-cells,
- triangle identities are depth-2 coherence,
- therefore a system with only `d = 1` cannot natively support self-consistent adjoint operators.

This is one reason the late modal and API shells are expected only in the `d = 2` lane.

### 4. Spectral Degeneration

- the obligation filtration degenerates at `E_2`,
- so `F_2` already captures the full independent homology of the obligation complex.

This is the topological expression of the same `d <= 2` result.

## Integration Trace Principle

The coherence manuscripts treat exported interface schemas as the resolved obligations of prior sealing steps.

Operationally, that means:

- a new candidate interacts with opaque exported interfaces,
- not with the full internal derivations of old layers.

This encapsulation is what makes a two-term recurrence possible.

## Complexity Scaling Theorem

For a foundation with coherence window `d`:

```text
Delta_(n+1) = sum_{j = 0}^{d-1} Delta_(n-j)
```

For the PEN lane:

```text
d = 2
Delta_1 = Delta_2 = 1
Delta_n = F_n
tau_n = F_(n+2) - 1
```

So the inflation term converges to the golden ratio:

```text
Phi_n = Delta_n / Delta_(n-1) -> varphi
```

## Why The Bar Rises The Way It Does

The bar uses marginal debt growth, not cumulative elapsed time.

That detail matters:

- using cumulative `tau_n / tau_(n-1)` would block the step-4 infrastructure phase,
- using marginal `Phi_n = Delta_n / Delta_(n-1)` keeps the early trajectory admissible.

## What This Means For The Rewrite

- The new search engine should treat depth-2 active history as a semantic contract, not just a performance heuristic.
- The bar math should be exact and reproducible from the realized `(nu, kappa)` history.
- Resume, checkpoint, and replay logic should preserve the same `d = 2` lane semantics.

## Cross-Links

- Read [pen-model.md](pen-model.md) for the objective that consumes these recurrences.
- Read [genesis.md](genesis.md) for the exact strict bars actually realized in the current canon.

