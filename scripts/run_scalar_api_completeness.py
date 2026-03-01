#!/usr/bin/env python3
"""Scalar API completeness check for PEN Step 13.

Experiment requested by reviewer:
- Add explicit scalar witnesses/operations (0,1,+,*,−)
- Add commutative-ring laws (postulated or definitional)
- Re-score Step 13 under strict first-use charging
- Count "other side of the ledger": ν_C uplift from algebraic cross-interactions

This script is intentionally toolchain-independent (pure Python), so it runs in
CI/dev environments without GHC/runghc.
"""
from __future__ import annotations

import csv
import json
import math
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path

# Baseline constants from pen_unified.tex Step 13
BAR = 5.99
NU_BASE = 46
KAPPA_BASE = 7

# Metric-side clause fanout used for conservative cross-interaction accounting.
# Step 13 has 7 metric API clauses.
METRIC_CLAUSES = 7


@dataclass(frozen=True)
class Scenario:
    name: str
    kappa_scalar: int
    scalar_ops: int
    scalar_laws: int
    description: str


SCENARIOS = [
    Scenario(
        name="ops_only_definitional_laws",
        kappa_scalar=5,
        scalar_ops=5,
        scalar_laws=0,
        description="Charge 0,1,+,*,− only; ring laws assumed definitional/derived.",
    ),
    Scenario(
        name="full_postulated_commutative_ring",
        kappa_scalar=15,
        scalar_ops=5,
        scalar_laws=10,
        description="Charge 0,1,+,*,− plus 10 postulated commutative-ring law clauses.",
    ),
]


def delta_nu_min(kappa_full: int) -> int:
    """Minimum additional ν required to clear the Step-13 bar under strict V1."""
    return max(0, math.ceil(BAR * kappa_full - NU_BASE))


def conservative_delta_nu_c(s: Scenario) -> int:
    """Lower bound: each scalar clause induces >=1 elimination/cross schema with metric API.

    This is intentionally conservative (linear), not bilinear.
    """
    return s.kappa_scalar


def dense_delta_nu_c(s: Scenario) -> int:
    """Upper-leaning but still structural estimate: full clause cross-product fanout.

    Each scalar clause can interact with each metric clause.
    """
    return s.kappa_scalar * METRIC_CLAUSES


def ops_weighted_delta_nu_c(s: Scenario) -> int:
    """Midpoint estimate with higher weight on algebraic operation clauses.

    Rationale: operations (+,*,−) participate in more rewrite/transport schemas
    than identity laws. We model this with weights 3 (ops) and 1 (laws).
    """
    return 3 * s.scalar_ops + 1 * s.scalar_laws


def evaluate(s: Scenario) -> dict:
    k_full = KAPPA_BASE + s.kappa_scalar
    v1_rho = NU_BASE / k_full
    dmin = delta_nu_min(k_full)

    # three interaction-accounting regimes for ν_C uplift
    d_c_lo = conservative_delta_nu_c(s)
    d_c_mid = ops_weighted_delta_nu_c(s)
    d_c_hi = dense_delta_nu_c(s)

    # We treat the interaction uplift as elimination-side novelty (ν_C).
    # Total ν uplift here equals ν_C uplift (conservative choice; no extra ν_G/ν_H bonus).
    rho_lo = (NU_BASE + d_c_lo) / k_full
    rho_mid = (NU_BASE + d_c_mid) / k_full
    rho_hi = (NU_BASE + d_c_hi) / k_full

    return {
        "scenario": s.name,
        "description": s.description,
        "kappa_scalar": s.kappa_scalar,
        "kappa_full": k_full,
        "delta_nu_min_to_clear_bar": dmin,
        "v1_rho_no_interactions": round(v1_rho, 6),
        "delta_nu_c_low": d_c_lo,
        "rho_with_low_nu_c": round(rho_lo, 6),
        "pass_low": rho_lo > BAR,
        "delta_nu_c_mid": d_c_mid,
        "rho_with_mid_nu_c": round(rho_mid, 6),
        "pass_mid": rho_mid > BAR,
        "delta_nu_c_high": d_c_hi,
        "rho_with_high_nu_c": round(rho_hi, 6),
        "pass_high": rho_hi > BAR,
        "margin_mid": round(rho_mid - BAR, 6),
    }


def main() -> None:
    run_id = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    out_root = Path("runs") / "scalar_api_completeness" / run_id
    out_root.mkdir(parents=True, exist_ok=True)

    rows = [evaluate(s) for s in SCENARIOS]

    csv_path = out_root / "results.csv"
    with csv_path.open("w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

    manifest = {
        "run_id": run_id,
        "bar": BAR,
        "nu_base": NU_BASE,
        "kappa_base": KAPPA_BASE,
        "metric_clauses": METRIC_CLAUSES,
        "scenarios": [s.__dict__ for s in SCENARIOS],
        "nu_c_models": {
            "low": "delta_nu_c = kappa_scalar",
            "mid": "delta_nu_c = 3*scalar_ops + scalar_laws",
            "high": "delta_nu_c = kappa_scalar * metric_clauses",
        },
        "generator": "scripts/run_scalar_api_completeness.py",
    }
    (out_root / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")

    lines = [
        "# Scalar API Completeness Experiment (Step 13)",
        "",
        f"Run id: `{run_id}`",
        f"Strict bar: `{BAR}`",
        "",
        "## Outcome summary",
    ]
    for r in rows:
        lines.append(
            "- "
            f"`{r['scenario']}`: k_full={r['kappa_full']}, "
            f"delta_nu_min={r['delta_nu_min_to_clear_bar']}, "
            f"V1(no interactions)={r['v1_rho_no_interactions']:.3f}, "
            f"mid-model delta_nu_c={r['delta_nu_c_mid']}, "
            f"rho_mid={r['rho_with_mid_nu_c']:.3f}, pass_mid={str(r['pass_mid']).lower()}"
        )

    lines += [
        "",
        "## Interpretation",
        "- Adding scalar axioms increases kappa immediately under strict first-use charging.",
        "- Counting algebraic cross-interactions on ν_C partially compensates that cost.",
        "- For the fully postulated commutative-ring scenario, passing Step 13 requires a large ν_C uplift (or additional ν_G/ν_H gains).",
    ]

    (out_root / "summary.md").write_text("\n".join(lines) + "\n")

    latest = Path("runs") / "scalar_api_completeness" / "latest"
    latest.parent.mkdir(parents=True, exist_ok=True)
    if latest.exists() or latest.is_symlink():
        latest.unlink()
    latest.symlink_to(run_id)

    # convenience report copy for docs
    docs_report = Path("docs") / "reports" / "scalar_api_completeness_report.md"
    docs_report.write_text("\n".join(lines) + "\n")

    print(out_root)


if __name__ == "__main__":
    main()
