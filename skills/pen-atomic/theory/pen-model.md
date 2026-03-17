# PEN Model

This file captures the mathematical objective that the Rust rewrite is trying to preserve.

## Core Objects

- A library state `B_n` is a monotone sealed context.
- A candidate `X` is a typed extension over the current library.
- A step inserts one sealed structure, not an arbitrary bag of unrelated axioms.

`pen_paper.tex` phrases the search problem as typed structural optimization over anonymous MBTT ASTs, with semantic labels added only later.

## Dual Costs

PEN uses two different cost notions.

- `Delta(X | B)` is integration latency:
  the size of the obligation graph needed to seal `X` against the active interface.
- `kappa(X)` is construction effort:
  the number of irreducible specification clauses in the chosen shell.

For the rewrite, `kappa` is the denominator that matters in hot-path ranking.
Bit-level MBTT length is an audit and tiebreak layer, not the main objective.

## Novelty And Efficiency

The basic score is:

```text
rho(X) = nu(X) / kappa(X)
```

The novelty split used throughout the manuscripts is:

```text
nu(X) = nu_G(X) + nu_C(X) + nu_H(X)
```

- `nu_G` counts new formation or introduction schemas.
- `nu_C` counts eliminators, projections, and cross-interface schemas.
- `nu_H` counts path, Kan, and computation payload.

For `pen-atomic`, this split matters because the old engine's late drift comes largely from how much of `nu_H` survived the molecular bridge.

## Selection Bar

The history-sensitive threshold is:

```text
Bar_n = Phi_n * Omega_(n-1)
Phi_n = Delta_n / Delta_(n-1)
Omega_(n-1) = (sum_{i < n} nu_i) / (sum_{i < n} kappa_i)
```

Candidates are not selected by maximizing `rho`.
They are selected by clearing the active bar with minimal positive overshoot.

That distinction is central.
It is why the trajectory takes `S^1` before `S^2`, and why the greedy `max-rho` ablation diverges.

## Derivability Versus Irreducibility

The papers repeatedly insist on the line between:

- definitionally derivable structure,
- constructively irreducible structure.

If an operation already reduces from the prior library, it contributes:

```text
kappa_B(o) = 0
```

If it requires a new neutral head, stuck eliminator, or new operational clause, it must be charged.

This is why:

- arithmetic definitions such as addition on `N` do not buy much new `nu`,
- but late API shells such as connections, curvature, or Hilbert structure are charged as new sealed bundles.

## Structural Unity And Minimal API

The admissible object is a single structural bundle, not a curator's pile of unrelated rewards.

The theory papers use three constraints over and over:

- structural unity,
- completeness of the shell,
- minimality against proper sub-bundles.

This is the theoretical precursor to the terminal-SCC minimality filter in the engine.

## Clause Cost Versus Compiled Payload

The proof spine and strict engine distinguish:

- charged conceptual `kappa`,
- larger compiled payload.

For HITs and late shells, compiled eliminators and computation clauses can increase evaluated novelty without increasing charged `kappa`.

That distinction is mathematically important even if the Rust rewrite later narrows it.
The new repo should decide deliberately whether purely atomic discovery still needs this split.

## Shell-First Reading Of Late Steps

From step 10 onward, the papers make a strong firewall:

- the selector sees anonymous typed shells,
- the PEN theorems talk about bar, admissibility, and ordering,
- names such as `cohesion`, `metric`, `Hilbert`, and `DCT` are post-hoc semantic readings.

That firewall should survive the rewrite.
The hot path should rank structure, not labels.

## What `pen-atomic` Should Preserve

- `rho = nu / kappa` as the central objective.
- Exact rational comparison in the hot path.
- Minimal positive overshoot, not greedy `max-rho`.
- The definitional-derivability boundary for charging `kappa`.
- Shell-first late semantics.
- Clause-count primary, bit-audit secondary.

