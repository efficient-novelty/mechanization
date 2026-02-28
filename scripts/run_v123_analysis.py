#!/usr/bin/env python3
from pathlib import Path
import csv

NU0 = 46
K0 = 7
BARS = [5.99, 6.58]
KAPPA_SCALAR_VALUES = list(range(1, 13))

out_csv = Path('runs/v123_analysis.csv')
out_md = Path('two_ledger_variant_results.md')

rows = []
for ks in KAPPA_SCALAR_VALUES:
    v1_rho = NU0 / (K0 + ks)
    v2_rho = NU0 / K0
    # minimum integer bonus needed to pass each bar
    req_bonus_v1 = {bar: max(0, int(bar * (K0 + ks) - NU0 + 0.999999999)) for bar in BARS}
    req_bonus_v2 = {bar: max(0, int(bar * K0 - NU0 + 0.999999999)) for bar in BARS}
    rows.append({
        'kappa_scalar': ks,
        'v1_rho': v1_rho,
        'v2_rho': v2_rho,
        'v1_pass_5_99': v1_rho > 5.99,
        'v1_pass_6_58': v1_rho > 6.58,
        'v2_pass_5_99': v2_rho > 5.99,
        'v2_pass_6_58': v2_rho > 6.58,
        'v3_bonus_req_v1_bar5_99': req_bonus_v1[5.99],
        'v3_bonus_req_v1_bar6_58': req_bonus_v1[6.58],
        'v3_bonus_req_v2_bar5_99': req_bonus_v2[5.99],
        'v3_bonus_req_v2_bar6_58': req_bonus_v2[6.58],
    })

with out_csv.open('w', newline='') as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
    w.writeheader()
    w.writerows(rows)

# Build markdown summary
v1_fail_all = all(not r['v1_pass_5_99'] and not r['v1_pass_6_58'] for r in rows)

lines = []
lines.append('# Two-ledger Variant Run (V1/V2/V3)')
lines.append('')
lines.append('## Inputs')
lines.append(f'- Baseline Step 13 values: nu={NU0}, kappa_local={K0}, rho={NU0/K0:.4f}.')
lines.append('- Bars tested: 5.99 and 6.58 (both appear in current manuscript text).')
lines.append('- kappa_scalar sweep: 1..12.')
lines.append('')
lines.append('## Variant definitions')
lines.append('- **V1 (strict first-use full-charge):** rho = nu / (kappa_local + kappa_scalar).')
lines.append('- **V2 (two-ledger amortized):** row rho = nu / kappa_local, scalar charged in infrastructure ledger.')
lines.append('- **V3 (interaction-credit extension):** add integer nu bonus needed to cross each bar under V1/V2.')
lines.append('')
lines.append('## Aggregate findings')
lines.append(f'- V1 fails both bars for every tested kappa_scalar in [1,12]: **{v1_fail_all}**.')
lines.append(f'- V2 row rho is constant: {NU0/K0:.4f} (passes 5.99, fails 6.58 by 0.0086).')
lines.append('- V3 minimum bonus under V2 to pass bar=6.58 is constant at +1.')
lines.append('- V3 minimum bonus under V1 grows with kappa_scalar (table below).')
lines.append('')
lines.append('## Table (selected points)')
lines.append('')
lines.append('| kappa_scalar | V1 rho | V2 rho | V3 bonus req under V1 (bar 5.99 / 6.58) | V3 bonus req under V2 (bar 5.99 / 6.58) |')
lines.append('|---:|---:|---:|---:|---:|')
for ks in [1,2,3,5,8,12]:
    r = next(x for x in rows if x['kappa_scalar']==ks)
    lines.append(f"| {ks} | {r['v1_rho']:.3f} | {r['v2_rho']:.3f} | {r['v3_bonus_req_v1_bar5_99']} / {r['v3_bonus_req_v1_bar6_58']} | {r['v3_bonus_req_v2_bar5_99']} / {r['v3_bonus_req_v2_bar6_58']} |")
lines.append('')
lines.append('## Interpretation')
lines.append('- If strict first-use charging is mandatory, Step 13 must be reordered/delayed unless very large novelty bonuses are justified.')
lines.append('- Under two-ledger accounting, Step 13 remains viable against bar 5.99 and nearly tied against 6.58; harmonizing bar definition is critical.')
lines.append('- A robust publication strategy is to report V2 as primary and V1 as stress sensitivity, plus an explicit V3 bonus rationale if used.')

out_md.write_text('\n'.join(lines) + '\n')
print(f'Wrote {out_csv} and {out_md}')
