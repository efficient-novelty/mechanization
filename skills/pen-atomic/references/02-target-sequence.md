# Target Sequence

## The Sequence the New Repo Should Care About

Prefer `../theory/genesis.md` for the theorem-facing canonical table.
This file is the engineering digest of that canon.

The rewrite should target the current 15-step genesis sequence, not the older 16-step variant still visible in some Agda files.

There are two important value regimes:

1. The paper or aspirational target sequence.
2. The current executable strict sequence.

The structures and `kappa` values now line up well. The late `nu` values do not.

## Bar Formula

The strict selection bar is:

```text
Bar_n = Phi_n * Omega_(n-1)
Phi_n = F_n / F_(n-1)        for the d=2 lane
Omega_(n-1) = sum_{i < n} nu_i / sum_{i < n} kappa_i
```

This is implemented in `engine/src/RunAbInitio.hs` via `computeBarD` and backed by `engine/src/CoherenceWindow.hs`.

## Canonical 15-Step Table

| Step | Name | Delta | Kappa | Paper target nu | Current strict nu | Current strict rho |
| --- | --- | ---: | ---: | ---: | ---: | ---: |
| 1 | Universe | 1 | 2 | 1 | 1 | 0.50 |
| 2 | Unit | 1 | 1 | 1 | 1 | 1.00 |
| 3 | Witness | 2 | 1 | 2 | 2 | 2.00 |
| 4 | Pi/Sigma | 3 | 3 | 5 | 5 | 1.67 |
| 5 | S1 | 5 | 3 | 7 | 7 | 2.33 |
| 6 | Trunc | 8 | 3 | 8 | 8 | 2.67 |
| 7 | S2 | 13 | 3 | 10 | 10 | 3.33 |
| 8 | S3 | 21 | 5 | 18 | 18 | 3.60 |
| 9 | Hopf | 34 | 4 | 17 | 17 | 4.25 |
| 10 | Cohesion | 55 | 4 | 19 | 19 | 4.75 |
| 11 | Connections | 89 | 5 | 26 | 27 | 5.40 |
| 12 | Curvature | 144 | 6 | 34 | 35 | 5.83 |
| 13 | Metric | 233 | 7 | 46 | 47 | 6.71 |
| 14 | Hilbert | 377 | 9 | 62 | 63 | 7.00 |
| 15 | DCT | 610 | 8 | 103 | 88 | 11.00 |

## Current Strict Bars

These are the bars recorded in the current strict trace summarized in `../theory/genesis.md`:

| Step | Bar |
| --- | ---: |
| 1 | 0.50 |
| 2 | 0.50 |
| 3 | 1.33 |
| 4 | 1.50 |
| 5 | 2.14 |
| 6 | 2.56 |
| 7 | 3.00 |
| 8 | 3.43 |
| 9 | 4.01 |
| 10 | 4.46 |
| 11 | 4.91 |
| 12 | 5.47 |
| 13 | 6.07 |
| 14 | 6.78 |
| 15 | 7.51 |

## Why the Mismatch Matters

The rewrite must not silently optimize for the wrong finish line.

### Structures and kappa

These are close to stable:

- structure order is now stable in the strict lane,
- charged `kappa` is stable,
- steps 1 through 10 line up well enough to treat as solid donor behavior.

### Late nu

These remain unsettled:

- steps 11 through 14 are off by +1 in the current strict lane,
- step 15 is far below the paper target because the current strict molecular scorer undercounts the full structural payload.

The key distinction is:

- `computeNativeNu(exact DCT AST)` reaches the larger structural value,
- but the current strict molecular path only charges `nu_G + nu_C + selected topological nu_H`, which leaves DCT at `88`.

## Step-by-Step Notes

### Steps 1 to 3

These are still genuinely atomic in the current repo and provide the best clean donor behavior for a new atomic search engine.

### Steps 4 to 10

The current repo recovers the right structures, but mostly via molecular compilation rather than pure atomic invention. Use the semantic results, not the generator design, as the donor.

### Steps 11 to 15

These are the most unstable region:

- structure identity is stable enough,
- `kappa` is stable enough,
- `nu` accounting is still under active tension between paper-level structural audits and current executable strict scoring.

## Success Criteria for the Rewrite

A new repo will likely need staged goals.

### Minimal success

- recover the 15 structures in order
- recover the charged `kappa`
- preserve the bar-clearing order

### Stronger success

- recover the current strict `nu` values with a cleaner atomic engine
- preserve deterministic replay and checkpointing

### Strongest success

- recover the paper-level late `nu` values without cheating or hiding molecular templates

## Primary Source Files

- `../theory/genesis.md`
- `../theory/terminal-dct.md`
- `../theory/late-framework-abstraction.md`
- `strict_intelligence_plan.md`
- `pseudo_code.md`
- `engine/src/RunAbInitio.hs`
- `engine/src/CoherenceWindow.hs`
