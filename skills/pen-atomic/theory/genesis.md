# Genesis

This is the exact 15-step strict canon that `pen-atomic` should treat as the default target.
The quantitative table below is copied from `pen_paper.tex`, not from the older paper-level structural audits.

## How To Read This File

- The `Selected AST skeleton` column is the exact structure description used in the strict canonical table.
- The late rows are shell-first:
  the selector found anonymous typed shells, and the human names are semantic readings.
- The `nu` values here are the strict machine values from `pen_paper.tex`.
  Some other TeX files still use older late totals.

## Exact Strict Table From `pen_paper.tex`

| Step | tau | Selected AST skeleton | Delta | nu | kappa | rho | Bar |
| --- | ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 1 | 1 | Universe `U_0` | 1 | 1 | 2 | 0.50 | --- |
| 2 | 2 | Unit type `1` | 1 | 1 | 1 | 1.00 | 0.50 |
| 3 | 4 | Witness `star : 1` | 2 | 2 | 1 | 2.00 | 1.33 |
| 4 | 7 | `Pi`/`Sigma` types | 3 | 5 | 3 | 1.67 | 1.50 |
| 5 | 12 | Circle `S^1` | 5 | 7 | 3 | 2.33 | 2.14 |
| 6 | 20 | Propositional truncation | 8 | 8 | 3 | 2.67 | 2.56 |
| 7 | 33 | Sphere `S^2` | 13 | 10 | 3 | 3.33 | 3.00 |
| 8 | 54 | H-space `S^3` | 21 | 18 | 5 | 3.60 | 3.43 |
| 9 | 88 | Hopf fibration | 34 | 17 | 4 | 4.25 | 4.01 |
| 10 | 143 | Modal shell (cohesion reading) | 55 | 19 | 4 | 4.75 | 4.46 |
| 11 | 232 | Connection shell | 89 | 27 | 5 | 5.40 | 4.91 |
| 12 | 376 | Curvature shell | 144 | 35 | 6 | 5.83 | 5.47 |
| 13 | 609 | Endomorphic operator bundle (metric reading) | 233 | 47 | 7 | 6.71 | 6.07 |
| 14 | 986 | Hilbert-functional shell | 377 | 63 | 9 | 7.00 | 6.78 |
| 15 | 1596 | Temporal-cohesive shell (semantic DCT completion) | 610 | 88 | 8 | 11.00 | 7.51 |

## Phase Structure

The papers divide the sequence into four phases.

### Steps 1 To 4: Bootstrap

- Universe
- Unit
- Witness
- `Pi`/`Sigma`

This is the minimal dependent-typed substrate.

### Steps 5 To 9: Geometric Ascent

- `S^1`
- truncation
- `S^2`
- `S^3`
- Hopf

This is the part of the sequence most directly tied to geometric and homotopical pressure.

### Steps 10 To 14: Framework Abstraction

- cohesion shell
- connections shell
- curvature shell
- metric-reading operator bundle
- Hilbert shell

These are best read as promoted API shells over the geometric core, not as fully elaborated human theories.

### Step 15: Synthesis

- temporal-cohesive shell
- semantic completion read as DCT

This is the terminal shell in the current strict lane.

## Proof-Status Boundary

The strict proof spine sharpens the status of the late rows.

- steps 1 to 10:
  exact or exact-reconstruction language is reasonable,
- steps 11 to 14:
  expository reconstructions are allowed, but they must match the strict totals,
- step 15:
  only the strict total `nu = 88` is canonical.

That boundary matters for the rewrite because it tells us what is settled versus what is still explanatory overlay.

## The Bar Formula

The strict bar is:

```text
Bar_n = Phi_n * Omega_(n-1)
Phi_n = F_n / F_(n-1)
Omega_(n-1) = (sum_{i < n} nu_i) / (sum_{i < n} kappa_i)
```

The detailed proof spine and `genesis_sequence.tex` also normalize the bootstrap with an initial `0.50` threshold.
That is why step 1 is sometimes discussed against `0.50` even though the main table above prints `---`.

## Quantitative Drift To Watch

Other manuscripts still use an older paper-level late regime:

| Step | Older late `nu` values seen elsewhere |
| --- | ---: |
| 11 | 26 |
| 12 | 34 |
| 13 | 46 |
| 14 | 62 |
| 15 | 103 |

For `pen-atomic`, the default quantitative target is still the strict canon above.
If the rewrite later aims to recover the larger structural totals, treat that as a second milestone, not as something the current repo already settled.

## Why This File Is The Default Target

This table is the cleanest single statement of:

- the exact structure order,
- the charged `kappa`,
- the strict `nu`,
- the strict bar clearances,
- and the shell-first semantic boundary.

Everything else in the theory tree either explains why this sequence is mathematically expected or clarifies where the late semantic readings came from.

