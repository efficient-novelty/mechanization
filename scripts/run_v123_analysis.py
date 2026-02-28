#!/usr/bin/env python3
"""Strict V1-first variant analysis for PEN Step 13.

V1: Strict first-use full-charge (required policy)
V2: Amortized ledger (computed for comparison only; flagged as rejected)
V3: Strict V1 + mechanically-required delta-nu to clear the bar
"""
from pathlib import Path
import csv
import math

NU0 = 46
K0 = 7
BAR = 5.99
KAPPA_SCALAR_VALUES = list(range(1, 13))
DELTA_NU_PROBE = 20  # matches the reviewer-proposed "~+20 schemas" checkpoint

rows = []
for ks in KAPPA_SCALAR_VALUES:
    k_full = K0 + ks
    v1_rho = NU0 / k_full
    delta_min = max(0, math.ceil(BAR * k_full - NU0))

    # V2 is retained only for comparison; it is not accepted policy.
    v2_rho = NU0 / K0

    # V3 strict: add emergent interaction schemas to nu under full-charge kappa
    v3_rho_probe = (NU0 + DELTA_NU_PROBE) / k_full
    v3_pass_probe = v3_rho_probe > BAR

    rows.append(
        {
            "kappa_scalar": ks,
            "kappa_full": k_full,
            "v1_rho": v1_rho,
            "v1_pass": v1_rho > BAR,
            "v2_rho_comparison_only": v2_rho,
            "v3_delta_nu_min_for_pass": delta_min,
            "v3_rho_with_delta20": v3_rho_probe,
            "v3_pass_with_delta20": v3_pass_probe,
        }
    )

csv_path = Path("runs/v123_analysis.csv")
csv_path.parent.mkdir(parents=True, exist_ok=True)
with csv_path.open("w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
    w.writeheader()
    w.writerows(rows)

# Build markdown report
md = []
md.append("# Variant Analysis (V1 strict-first policy)")
md.append("")
md.append("## Policy and constants")
md.append(f"- Enforced bar: {BAR:.2f} (Step 13).")
md.append(f"- Baseline metric entry: nu={NU0}, kappa={K0}, rho={NU0/K0:.4f}.")
md.append("- kappa_scalar sweep: 1..12.")
md.append("")
md.append("## Variant definitions")
md.append("- **V1 (accepted policy):** strict first-use full-charge, rho = nu/(kappa + kappa_scalar).")
md.append("- **V2 (rejected policy):** amortized ledger; shown only as comparison baseline.")
md.append("- **V3 (accepted extension):** strict V1 with emergent delta-nu from new scalar interactions.")
md.append("")
all_v1_fail = all(not r["v1_pass"] for r in rows)
md.append("## Findings")
md.append(f"- V1 fails for all tested kappa_scalar in [1,12]: **{all_v1_fail}**.")
md.append("- V2 comparison rho remains 6.5714, but V2 is rejected as accounting policy.")
md.append("- V3 minimum required delta-nu to pass grows linearly with kappa_scalar.")
md.append(f"- At delta-nu={DELTA_NU_PROBE}, V3 passes for kappa_scalar<=4 and fails for kappa_scalar>=5.")
md.append("")
md.append("## Table")
md.append("")
md.append("| kappa_scalar | kappa_full | V1 rho | V3 min delta-nu to pass | V3 rho with +20 | V3 +20 passes? |")
md.append("|---:|---:|---:|---:|---:|:---:|")
for r in rows:
    md.append(
        f"| {r['kappa_scalar']} | {r['kappa_full']} | {r['v1_rho']:.3f} | {r['v3_delta_nu_min_for_pass']} | {r['v3_rho_with_delta20']:.3f} | {'yes' if r['v3_pass_with_delta20'] else 'no'} |"
    )

md.append("")
md.append("## Recommendation from this run")
md.append("1. Overturn A3 and reject amortized ledger (V2) as normative accounting.")
md.append("2. Keep strict V1 as the only canonical scoring policy.")
md.append("3. Continue with V3 only if delta-nu is obtained mechanically (uniform-nu or equivalent), not hand-assigned.")
md.append("4. Prioritize low-kappa scalar route design (e.g., topological arithmetic or synthetic continuum insertion) because V3+20 only rescues kappa_scalar<=4.")

Path("two_ledger_variant_results.md").write_text("\n".join(md) + "\n")
print(f"Wrote {csv_path} and two_ledger_variant_results.md")
