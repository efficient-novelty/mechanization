#!/usr/bin/env python3
"""
PEN Formula Sensitivity Analysis — Phase 1

Tests alternative ν_H formulas to determine whether d² is uniquely
determined or one of many choices that reproduce the Genesis Sequence.

Also performs a feasible region scan: for all integer values of
(ν_H(S¹), ν_H(S²), ν_H(S³)), checks which triplets reproduce the sequence.
"""

import math
import sys

# ===== Fibonacci =====

def fib(n):
    """Fibonacci number F_n (1-indexed: F_1=1, F_2=1, F_3=2, ...)"""
    if n <= 0:
        return 0
    a, b = 1, 1
    for _ in range(n - 1):
        a, b = b, a + b
    return a

# ===== Genesis Sequence Data =====
# Using the paper's spectral decomposition (Table 2) for HIT steps,
# and total ν for all other steps.
#
# Format: (index, name, kappa, nu_total, is_hit, m, max_d, nu_G, nu_C)
# For non-HIT steps: m=0, max_d=0, and nu_total is fixed.
# For HIT steps: nu_total = nu_G + nu_H_formula(m, max_d) + nu_C.

GENESIS_BASE = [
    # (idx, name, kappa, nu_fixed, is_hit, m, max_d, nu_G, nu_C)
    ( 1, "Universe",     2,   1,  False, 0, 0, 0, 1),
    ( 2, "Unit",         1,   1,  False, 0, 0, 1, 0),
    ( 3, "Witness",      1,   2,  False, 0, 0, 1, 1),
    ( 4, "Pi/Sigma",     3,   5,  False, 0, 0, 2, 3),
    ( 5, "S1",           3,  -1,  True,  1, 1, 5, 0),   # nu = 5 + f(1,1) + 0
    ( 6, "PropTrunc",    3,   8,  False, 0, 0, 0, 8),
    ( 7, "S2",           3,  -1,  True,  1, 2, 5, 0),   # nu = 5 + f(1,2) + 0
    ( 8, "S3",           5,  -1,  True,  1, 3, 5, 3),   # nu = 5 + f(1,3) + 3
    ( 9, "Hopf",         4,  17,  False, 0, 0, 0, 17),
    (10, "Lie",          6,   9,  False, 0, 0, 0, 9),
    (11, "Cohesion",     4,  19,  False, 0, 0, 0, 19),
    (12, "Connections",  5,  26,  False, 0, 0, 0, 26),
    (13, "Curvature",    6,  34,  False, 0, 0, 0, 34),
    (14, "Metric",       7,  43,  False, 0, 0, 0, 43),
    (15, "Hilbert",      9,  60,  False, 0, 0, 0, 60),
    (16, "DCT",          8, 105,  False, 0, 0, 105, 0),
]

# Expected paper ordering (Lie never clears — 15/15 result)
PAPER_ORDER = [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16]

# ===== Build candidates given a ν_H formula =====

def build_candidates(nu_h_func):
    """
    Build candidate dict from GENESIS_BASE, computing nu for HIT steps
    using the provided nu_h_func(m, max_d).
    """
    candidates = {}
    for idx, name, kappa, nu_fixed, is_hit, m, max_d, nu_G, nu_C in GENESIS_BASE:
        if is_hit:
            nu_h = nu_h_func(m, max_d)
            nu = nu_G + nu_h + nu_C
        else:
            nu = nu_fixed
        rho = nu / kappa if kappa > 0 else 0.0
        candidates[idx] = {
            'name': name, 'kappa': kappa, 'nu': nu, 'rho': rho,
            'is_hit': is_hit,
        }
    return candidates


# ===== PEN Simulation =====

def simulate(candidates):
    """
    Run PEN simulation with given candidates.
    Returns (realized_order, details) where details is a list of dicts.
    """
    realized = []
    details = []
    cum_nu = 0.0
    cum_kappa = 0
    horizon = 2
    remaining = set(candidates.keys())
    idle = 0

    while remaining and idle < 50:
        n = len(realized) + 1

        if n <= 1 or cum_kappa == 0:
            bar = 0.0
        else:
            phi = fib(n) / fib(n - 1)
            omega = cum_nu / cum_kappa
            bar = phi * omega

        clearing = []
        for idx in remaining:
            c = candidates[idx]
            if c['kappa'] <= horizon and c['rho'] >= bar:
                overshoot = c['rho'] - bar
                clearing.append((overshoot, c['kappa'], idx))

        if not clearing:
            horizon = min(horizon + 1, 20)
            idle += 1
            continue

        clearing.sort()
        _, _, winner = clearing[0]
        c = candidates[winner]
        realized.append(winner)
        details.append({
            'n': n, 'idx': winner, 'name': c['name'],
            'nu': c['nu'], 'kappa': c['kappa'],
            'rho': c['rho'], 'bar': bar,
            'margin': c['rho'] - bar,
        })
        cum_nu += c['nu']
        cum_kappa += c['kappa']
        remaining.remove(winner)

        delta = fib(n)
        horizon = min(delta + 1, 20)
        idle = 0

    return realized, details


def count_correct(realized, expected=PAPER_ORDER):
    """Count leading steps that match expected ordering."""
    n = 0
    for i in range(min(len(realized), len(expected))):
        if realized[i] == expected[i]:
            n += 1
        else:
            break
    return n


# ===== Alternative ν_H Formulas =====

FORMULAS = [
    ("A: m + d²  (current)",    lambda m, d: m + d**2),
    ("B: m + d  (linear)",      lambda m, d: m + d),
    ("C: m + d(d+1)/2  (tri.)", lambda m, d: m + d*(d+1)//2),
    ("D: m + 2^d  (exp.)",      lambda m, d: m + 2**d),
    ("E: m·(d+1)  (prod.)",     lambda m, d: m * (d + 1)),
    ("F: d²  (no m)",           lambda m, d: d**2),
    ("G: m·d²  (scaled)",       lambda m, d: m * d**2),
    ("H: m + d(d-1)+1",         lambda m, d: m + d*(d-1) + 1),
    ("I: m + d²+d  (oblong)",   lambda m, d: m + d**2 + d),
    ("J: m + ⌈d²/2⌉",          lambda m, d: m + math.ceil(d**2 / 2)),
    ("K: 2d",                   lambda m, d: 2 * d),
    ("L: d(d+1)",               lambda m, d: d * (d + 1)),
    ("M: m + d³  (cubic)",      lambda m, d: m + d**3),
    ("N: m + 2d²  (doubled)",   lambda m, d: m + 2*d**2),
    ("O: m + d² - 1",           lambda m, d: m + d**2 - 1),
    ("P: m + d² + 1",           lambda m, d: m + d**2 + 1),
]


# ===== Phase 1: Test All Formulas =====

def test_all_formulas():
    print("=" * 80)
    print("PHASE 1: Alternative ν_H Formula Sensitivity Analysis")
    print("=" * 80)
    print()
    print("For each formula f(m, d), the HIT steps get:")
    print("  S¹: ν = 5 + f(1,1) + 0")
    print("  S²: ν = 5 + f(1,2) + 0")
    print("  S³: ν = 5 + f(1,3) + 3")
    print()

    # Header
    print(f"{'Formula':<28s} | {'ν_H(S¹)':>7s} {'ν_H(S²)':>7s} {'ν_H(S³)':>7s} | "
          f"{'ν(S¹)':>5s} {'ν(S²)':>5s} {'ν(S³)':>5s} | "
          f"{'ρ(S¹)':>6s} {'ρ(S²)':>6s} {'ρ(S³)':>6s} | "
          f"{'Steps':>5s} | {'First miss':<12s} | Margins (S¹,S²,S³)")
    print("-" * 145)

    results = []
    for label, func in FORMULAS:
        nh1 = func(1, 1)  # S¹
        nh2 = func(1, 2)  # S²
        nh3 = func(1, 3)  # S³

        nu1 = 5 + nh1 + 0  # S¹ total
        nu2 = 5 + nh2 + 0  # S² total
        nu3 = 5 + nh3 + 3  # S³ total

        rho1 = nu1 / 3     # S¹: κ=3
        rho2 = nu2 / 3     # S²: κ=3
        rho3 = nu3 / 5     # S³: κ=5

        cands = build_candidates(func)
        realized, details = simulate(cands)
        correct = count_correct(realized)

        # Find first miss
        if correct >= len(PAPER_ORDER):
            first_miss = "—"
        else:
            if correct < len(realized):
                got = candidates_name(realized[correct], cands)
                expected = candidates_name(PAPER_ORDER[correct], cands)
                first_miss = f"got {got}"
            else:
                first_miss = "stalled"

        # Extract margins for HIT steps
        margins = []
        for d in details:
            if d['idx'] in (5, 7, 8):
                margins.append(f"{d['margin']:+.3f}")

        margin_str = ", ".join(margins) if margins else "N/A"

        print(f"{label:<28s} | {nh1:>7d} {nh2:>7d} {nh3:>7d} | "
              f"{nu1:>5d} {nu2:>5d} {nu3:>5d} | "
              f"{rho1:>6.2f} {rho2:>6.2f} {rho3:>6.2f} | "
              f"{correct:>5d} | {first_miss:<12s} | {margin_str}")

        results.append({
            'label': label, 'nh': (nh1, nh2, nh3),
            'nu': (nu1, nu2, nu3), 'correct': correct,
            'realized': realized, 'details': details,
        })

    print()
    return results


def candidates_name(idx, cands):
    if idx in cands:
        return cands[idx]['name']
    return f"#{idx}"


# ===== Phase 1b: Detailed comparison for formulas that work =====

def detailed_comparison(results):
    print("=" * 80)
    print("DETAILED COMPARISON: Formulas that reproduce ≥9 steps")
    print("=" * 80)
    print()

    working = [r for r in results if r['correct'] >= 9]
    if not working:
        print("  No formulas reproduce ≥9 steps!")
        return

    for r in working:
        print(f"--- {r['label']} ({r['correct']} steps correct) ---")
        print(f"  ν_H values: S¹={r['nh'][0]}, S²={r['nh'][1]}, S³={r['nh'][2]}")
        print(f"  Total  ν:   S¹={r['nu'][0]}, S²={r['nu'][1]}, S³={r['nu'][2]}")
        print()
        print(f"  {'n':>3s} | {'Structure':<14s} | {'ν':>4s} | {'κ':>4s} | "
              f"{'ρ':>7s} | {'Bar':>7s} | {'Margin':>7s}")
        print(f"  {'---':>3s}-+-{'':->14s}-+-{'':->4s}-+-{'':->4s}-+-"
              f"{'':->7s}-+-{'':->7s}-+-{'':->7s}")
        for d in r['details']:
            marker = " *" if d['idx'] in (5, 7, 8) else ""
            print(f"  {d['n']:>3d} | {d['name']:<14s} | {d['nu']:>4.0f} | {d['kappa']:>4d} | "
                  f"{d['rho']:>7.3f} | {d['bar']:>7.3f} | {d['margin']:>+7.3f}{marker}")
        print()

        # Check if ordering differs
        if r['realized'] != PAPER_ORDER[:r['correct']]:
            print(f"  WARNING: Sequence differs from paper!")
            print(f"  Got:      {r['realized']}")
            print(f"  Expected: {PAPER_ORDER}")
        print()


# ===== Phase 4 (from plan): Feasible Region Scan =====

def feasible_region_scan():
    print("=" * 80)
    print("FEASIBLE REGION: Integer ν_H triplets that reproduce the sequence")
    print("=" * 80)
    print()
    print("Scanning all (ν_H(S¹), ν_H(S²), ν_H(S³)) ∈ [0..20]³")
    print("with ν(S¹)=5+h1, ν(S²)=5+h2, ν(S³)=8+h3")
    print()

    # For efficiency, build a custom simulation that takes direct ν_H values
    full_match = []
    nine_plus = []

    for h1 in range(0, 21):
        for h2 in range(0, 21):
            for h3 in range(0, 21):
                nu_s1 = 5 + h1       # ν_G=5, ν_C=0
                nu_s2 = 5 + h2       # ν_G=5, ν_C=0
                nu_s3 = 5 + h3 + 3   # ν_G=5, ν_C=3

                # Build candidates with these specific ν values
                cands = {}
                for idx, name, kappa, nu_fixed, is_hit, m, max_d, nu_G, nu_C in GENESIS_BASE:
                    if idx == 5:
                        nu = nu_s1
                    elif idx == 7:
                        nu = nu_s2
                    elif idx == 8:
                        nu = nu_s3
                    else:
                        nu = nu_fixed
                    rho = nu / kappa if kappa > 0 else 0.0
                    cands[idx] = {'name': name, 'kappa': kappa, 'nu': nu, 'rho': rho}

                realized, _ = simulate(cands)
                correct = count_correct(realized)

                if correct >= 15:
                    full_match.append((h1, h2, h3))
                if correct >= 9:
                    nine_plus.append((h1, h2, h3))

    print(f"Total triplets scanned: {21**3}")
    print(f"Triplets with ≥15 correct: {len(full_match)}")
    print(f"Triplets with ≥9  correct: {len(nine_plus)}")
    print()

    if full_match:
        print("All (ν_H(S¹), ν_H(S²), ν_H(S³)) with 15/15 correct:")
        print(f"  {'h1':>3s}  {'h2':>3s}  {'h3':>3s}  |  {'ν(S¹)':>6s}  {'ν(S²)':>6s}  {'ν(S³)':>6s}  |  "
              f"{'ρ(S¹)':>6s}  {'ρ(S²)':>6s}  {'ρ(S³)':>6s}")
        print("  " + "-" * 75)
        for h1, h2, h3 in sorted(full_match):
            nu1 = 5 + h1
            nu2 = 5 + h2
            nu3 = 8 + h3
            print(f"  {h1:>3d}  {h2:>3d}  {h3:>3d}  |  {nu1:>6d}  {nu2:>6d}  {nu3:>6d}  |  "
                  f"{nu1/3:>6.2f}  {nu2/3:>6.2f}  {nu3/5:>6.2f}")
        print()

        # Ranges
        h1_vals = [t[0] for t in full_match]
        h2_vals = [t[1] for t in full_match]
        h3_vals = [t[2] for t in full_match]
        print(f"  ν_H(S¹) range: [{min(h1_vals)}, {max(h1_vals)}]")
        print(f"  ν_H(S²) range: [{min(h2_vals)}, {max(h2_vals)}]")
        print(f"  ν_H(S³) range: [{min(h3_vals)}, {max(h3_vals)}]")
        print()

        # Check which formulas land in the feasible region
        print("  Formulas that produce feasible triplets:")
        for label, func in FORMULAS:
            t = (func(1,1), func(1,2), func(1,3))
            if t in set(full_match):
                print(f"    ✓ {label}: ({t[0]}, {t[1]}, {t[2]})")
            else:
                status = "≥9" if t in set(nine_plus) else "FAIL"
                print(f"    ✗ {label}: ({t[0]}, {t[1]}, {t[2]}) [{status}]")
        print()

    # 2D projection: for each h1, show which (h2, h3) work
    if full_match:
        print("Feasible region by ν_H(S¹):")
        for h1_val in sorted(set(h1_vals)):
            pairs = [(h2, h3) for h1, h2, h3 in full_match if h1 == h1_val]
            h2_range = (min(p[0] for p in pairs), max(p[0] for p in pairs))
            h3_range = (min(p[1] for p in pairs), max(p[1] for p in pairs))
            print(f"  h1={h1_val}: {len(pairs)} valid, "
                  f"h2∈[{h2_range[0]},{h2_range[1]}], h3∈[{h3_range[0]},{h3_range[1]}]")
        print()

    return full_match, nine_plus


# ===== Bar Analysis: Show what bars the HITs face =====

def bar_analysis():
    """Show the bar values that HITs must clear, independent of formula."""
    print("=" * 80)
    print("REFERENCE: Selection bars at HIT steps (independent of ν_H formula)")
    print("=" * 80)
    print()
    print("Steps 1-4 are fixed (no ν_H dependence). After step 4:")
    print("  cum_ν = 1+1+2+5 = 9,  cum_κ = 2+1+1+3 = 7")
    print("  Ω₄ = 9/7 ≈ 1.286")
    print()

    # Simulate steps 1-4 to get the state
    cum_nu = 0
    cum_kappa = 0
    steps_14 = [
        (1, "Universe", 2, 1),
        (2, "Unit",     1, 1),
        (3, "Witness",  1, 2),
        (4, "Pi/Sigma", 3, 5),
    ]
    for n, name, kappa, nu in steps_14:
        if n <= 1 or cum_kappa == 0:
            bar = 0.0
        else:
            phi = fib(n) / fib(n-1)
            omega = cum_nu / cum_kappa
            bar = phi * omega
        rho = nu / kappa
        print(f"  Step {n} ({name}): ν={nu}, κ={kappa}, ρ={rho:.3f}, Bar={bar:.3f}, "
              f"margin={rho-bar:+.3f}")
        cum_nu += nu
        cum_kappa += kappa

    print()
    print(f"  After step 4: cum_ν={cum_nu}, cum_κ={cum_kappa}, "
          f"Ω={cum_nu/cum_kappa:.4f}")
    print()

    # Step 5: S¹, n=5
    n = 5
    phi5 = fib(5) / fib(4)
    omega4 = cum_nu / cum_kappa
    bar5 = phi5 * omega4
    print(f"  Step 5 (S¹): Bar = Φ₅·Ω₄ = {phi5:.4f} × {omega4:.4f} = {bar5:.4f}")
    print(f"    S¹ needs ρ ≥ {bar5:.4f}, i.e., ν/3 ≥ {bar5:.4f}, i.e., ν ≥ {bar5*3:.2f}")
    print(f"    Minimum integer ν for S¹: {math.ceil(bar5 * 3)}")
    print()

    # After S¹ with formula A (ν=7): compute bars for S² and S³
    # But these depend on what ν(S¹) actually is! Let's show parametrically.
    print("  The bars for S² and S³ depend on ν(S¹), so they shift with the formula.")
    print("  Showing bars for ν(S¹) ∈ {5, 6, 7, 8, 9, 10}:")
    print()
    for nu_s1 in range(5, 11):
        cn = cum_nu + nu_s1
        ck = cum_kappa + 3  # κ(S¹) = 3

        # Step 6: PropTrunc (n=6, ν=8, κ=3)
        bar6 = (fib(6)/fib(5)) * (cn/ck)
        rho6 = 8/3
        cn6 = cn + 8
        ck6 = ck + 3

        # Step 7: S² (n=7, κ=3)
        bar7 = (fib(7)/fib(6)) * (cn6/ck6)
        min_nu_s2 = math.ceil(bar7 * 3)

        # After S² with some ν: Step 8 S³
        # This depends on ν(S²), so show for a range
        print(f"    ν(S¹)={nu_s1}: Bar₅={bar5:.3f}, "
              f"Bar₆={bar6:.3f} (PropTrunc ρ={rho6:.3f}, margin={rho6-bar6:+.3f}), "
              f"Bar₇={bar7:.3f} (S² needs ν≥{min_nu_s2})")

    print()


# ===== Main =====

def main():
    print()

    # Reference: show bars
    bar_analysis()

    # Phase 1: Test all formulas
    results = test_all_formulas()

    # Detailed comparison of working formulas
    detailed_comparison(results)

    # Phase 4: Feasible region scan
    full_match, nine_plus = feasible_region_scan()

    # Summary
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print()
    working_15 = [r for r in results if r['correct'] >= 15]
    working_9 = [r for r in results if r['correct'] >= 9]
    print(f"  Formulas tested:       {len(FORMULAS)}")
    print(f"  Reproduce all 15:      {len(working_15)}")
    print(f"  Reproduce ≥9:          {len(working_9)}")
    print()
    if working_15:
        print(f"  Formulas that reproduce all 15 steps:")
        for r in working_15:
            print(f"    {r['label']}: ν_H = ({r['nh'][0]}, {r['nh'][1]}, {r['nh'][2]})")
    print()
    if full_match:
        h1_vals = sorted(set(t[0] for t in full_match))
        h2_vals = sorted(set(t[1] for t in full_match))
        h3_vals = sorted(set(t[2] for t in full_match))
        print(f"  Feasible region (integer ν_H values, 15/15):")
        print(f"    ν_H(S¹) ∈ [{min(h1_vals)}, {max(h1_vals)}] ({len(h1_vals)} values)")
        print(f"    ν_H(S²) ∈ [{min(h2_vals)}, {max(h2_vals)}] ({len(h2_vals)} values)")
        print(f"    ν_H(S³) ∈ [{min(h3_vals)}, {max(h3_vals)}] ({len(h3_vals)} values)")
        print(f"    Total feasible triplets: {len(full_match)} / {21**3} = "
              f"{100*len(full_match)/21**3:.2f}%")
    print()

    # Key finding
    print("KEY FINDING:")
    if len(working_15) == 1 and working_15[0]['label'].startswith("A"):
        print("  d² is the UNIQUE formula (from this set) that reproduces the sequence.")
        print("  This is either evidence of deep structure or of fine-tuning.")
    elif len(working_15) > 1:
        print(f"  {len(working_15)} formulas reproduce the full sequence.")
        print("  d² is NOT uniquely determined — it's one of several choices.")
        labels = [r['label'] for r in working_15]
        print(f"  Working formulas: {', '.join(labels)}")
    else:
        print("  NO formula (including d²) reproduces the sequence with the paper's")
        print("  decomposition values. Check the data.")
    print()


if __name__ == '__main__':
    main()
