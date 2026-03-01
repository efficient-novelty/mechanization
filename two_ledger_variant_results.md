# Variant Analysis (V1 strict-first policy)

## Policy and constants
- Enforced bar: 5.99 (Step 13).
- Baseline metric entry: nu=46, kappa=7, rho=6.5714.
- kappa_scalar sweep: 1..12.

## Variant definitions
- **V1 (accepted policy):** strict first-use full-charge, rho = nu/(kappa + kappa_scalar).
- **V2 (rejected policy):** amortized ledger; shown only as comparison baseline.
- **V3 (accepted extension):** strict V1 with emergent delta-nu from new scalar interactions.

## Findings
- V1 fails for all tested kappa_scalar in [1,12]: **True**.
- V2 comparison rho remains 6.5714, but V2 is rejected as accounting policy.
- V3 minimum required delta-nu to pass grows linearly with kappa_scalar.
- At delta-nu=20, V3 passes for kappa_scalar<=4 and fails for kappa_scalar>=5.

## Table

| kappa_scalar | kappa_full | V1 rho | V3 min delta-nu to pass | V3 rho with +20 | V3 +20 passes? |
|---:|---:|---:|---:|---:|:---:|
| 1 | 8 | 5.750 | 2 | 8.250 | yes |
| 2 | 9 | 5.111 | 8 | 7.333 | yes |
| 3 | 10 | 4.600 | 14 | 6.600 | yes |
| 4 | 11 | 4.182 | 20 | 6.000 | yes |
| 5 | 12 | 3.833 | 26 | 5.500 | no |
| 6 | 13 | 3.538 | 32 | 5.077 | no |
| 7 | 14 | 3.286 | 38 | 4.714 | no |
| 8 | 15 | 3.067 | 44 | 4.400 | no |
| 9 | 16 | 2.875 | 50 | 4.125 | no |
| 10 | 17 | 2.706 | 56 | 3.882 | no |
| 11 | 18 | 2.556 | 62 | 3.667 | no |
| 12 | 19 | 2.421 | 68 | 3.474 | no |

## Recommendation from this run
1. Overturn A3 and reject amortized ledger (V2) as normative accounting.
2. Keep strict V1 as the only canonical scoring policy.
3. Continue with V3 only if delta-nu is obtained mechanically (uniform-nu or equivalent), not hand-assigned.
4. Prioritize low-kappa scalar route design (e.g., topological arithmetic or synthetic continuum insertion) because V3+20 only rescues kappa_scalar<=4.
