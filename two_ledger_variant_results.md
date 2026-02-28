# Two-ledger Variant Run (V1/V2/V3)

## Inputs
- Baseline Step 13 values: nu=46, kappa_local=7, rho=6.5714.
- Bars tested: 5.99 and 6.58 (both appear in current manuscript text).
- kappa_scalar sweep: 1..12.

## Variant definitions
- **V1 (strict first-use full-charge):** rho = nu / (kappa_local + kappa_scalar).
- **V2 (two-ledger amortized):** row rho = nu / kappa_local, scalar charged in infrastructure ledger.
- **V3 (interaction-credit extension):** add integer nu bonus needed to cross each bar under V1/V2.

## Aggregate findings
- V1 fails both bars for every tested kappa_scalar in [1,12]: **True**.
- V2 row rho is constant: 6.5714 (passes 5.99, fails 6.58 by 0.0086).
- V3 minimum bonus under V2 to pass bar=6.58 is constant at +1.
- V3 minimum bonus under V1 grows with kappa_scalar (table below).

## Table (selected points)

| kappa_scalar | V1 rho | V2 rho | V3 bonus req under V1 (bar 5.99 / 6.58) | V3 bonus req under V2 (bar 5.99 / 6.58) |
|---:|---:|---:|---:|---:|
| 1 | 5.750 | 6.571 | 2 / 7 | 0 / 1 |
| 2 | 5.111 | 6.571 | 8 / 14 | 0 / 1 |
| 3 | 4.600 | 6.571 | 14 / 20 | 0 / 1 |
| 5 | 3.833 | 6.571 | 26 / 33 | 0 / 1 |
| 8 | 3.067 | 6.571 | 44 / 53 | 0 / 1 |
| 12 | 2.421 | 6.571 | 68 / 80 | 0 / 1 |

## Interpretation
- If strict first-use charging is mandatory, Step 13 must be reordered/delayed unless very large novelty bonuses are justified.
- Under two-ledger accounting, Step 13 remains viable against bar 5.99 and nearly tied against 6.58; harmonizing bar definition is critical.
- A robust publication strategy is to report V2 as primary and V1 as stress sensitivity, plus an explicit V3 bonus rationale if used.
