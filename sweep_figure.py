#!/usr/bin/env python3
"""
PEN Novelty Decomposition: 2D Combination Rule Sweep

Generates the sensitivity analysis figure for the paper.
Sweeps (alpha, beta) in ν = α·ν_G + β·ν_H + γ·ν_C (with γ=1.0)
and records how many leading Genesis steps are correctly produced.

Usage:
  python3 sweep_figure.py              # text output + figure if matplotlib available
  python3 sweep_figure.py --text-only  # text output only
"""

import sys
import os

# ===== Genesis Sequence Data =====
# (index, name, kappa, nu_G, nu_H, nu_C)
GENESIS = [
    ( 1, "Universe",     2,  0,  0,   1),
    ( 2, "Unit",         1,  0,  0,   1),
    ( 3, "Witness",      1,  2,  0,   0),
    ( 4, "Pi/Sigma",     3,  0,  0,   5),
    ( 5, "S1",           3,  5,  2,   0),
    ( 6, "PropTrunc",    3,  0,  0,   8),
    ( 7, "S2",           3,  5,  5,   0),
    ( 8, "S3",           5,  5, 10,   3),
    ( 9, "Hopf",         4,  0,  0,  17),
    (10, "Lie",          6,  0,  0,   9),
    (11, "Cohesion",     4,  0,  0,  19),
    (12, "Connections",  5,  0,  0,  26),
    (13, "Curvature",    6,  0,  0,  34),
    (14, "Metric",       7,  0,  0,  43),
    (15, "Hilbert",      9,  0,  0,  60),
    (16, "DCT",          8,  0,  0, 150),
]

# Expected paper ordering (Lie never clears — 15/15 result)
PAPER_ORDER = [1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16]

# ===== Fibonacci Sequence =====

def fib(n):
    """Fibonacci number F_n (1-indexed: F_1=1, F_2=1, F_3=2, ...)"""
    if n <= 0:
        return 0
    a, b = 1, 1
    for _ in range(n - 1):
        a, b = b, a + b
    return a

# ===== PEN Simulation =====

def simulate(alpha, beta, gamma):
    """
    Run PEN simulation with weighted nu = alpha*nu_G + beta*nu_H + gamma*nu_C.
    Returns list of selected genesis indices in realization order.

    Implements:
      Axiom 1: Bar(n) = Phi_n * Omega_{n-1}
      Axiom 2: H resets to delta+1 after realization, +1 per idle tick
      Axiom 3: Admissibility kappa <= H
      Axiom 4: Minimal overshoot selection (ties: min kappa, min index)
      Axiom 5: Fibonacci integration gaps
    """
    # Build candidates
    candidates = {}
    for idx, name, kappa, nu_g, nu_h, nu_c in GENESIS:
        wnu = alpha * nu_g + beta * nu_h + gamma * nu_c
        rho = wnu / kappa if kappa > 0 else 0.0
        candidates[idx] = {'kappa': kappa, 'nu': wnu, 'rho': rho}

    realized = []
    cum_nu = 0.0
    cum_kappa = 0
    horizon = 2
    remaining = set(candidates.keys())
    idle = 0

    while remaining and idle < 50:
        n = len(realized) + 1  # next realization number

        # Bar(n) = Phi_n * Omega_{n-1}
        if n <= 1 or cum_kappa == 0:
            bar = 0.0
        else:
            phi = fib(n) / fib(n - 1)
            omega = cum_nu / cum_kappa
            bar = phi * omega

        # Find admissible candidates that clear the bar
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

        # Select winner: min overshoot, then min kappa, then min index
        clearing.sort()
        _, _, winner = clearing[0]

        c = candidates[winner]
        realized.append(winner)
        cum_nu += c['nu']
        cum_kappa += c['kappa']
        remaining.remove(winner)

        # H resets to delta+1 after realization
        delta = fib(n)
        horizon = min(delta + 1, 20)
        idle = 0

    return realized


def count_correct(realized, expected=PAPER_ORDER):
    """Count leading steps that match expected ordering."""
    n = 0
    for i in range(min(len(realized), len(expected))):
        if realized[i] == expected[i]:
            n += 1
        else:
            break
    return n

# ===== Sweep Functions =====

def sweep_1d():
    """Sweep each weight individually (others = 1.0). Returns window bounds."""
    results = {}
    for name, pos in [('alpha (grammar)', 0), ('beta (homotopy)', 1), ('gamma (capability)', 2)]:
        lo = hi = None
        for v100 in range(50, 151):
            v = v100 / 100.0
            w = [1.0, 1.0, 1.0]
            w[pos] = v
            seq = simulate(*w)
            c = count_correct(seq)
            if c >= 9:
                if lo is None:
                    lo = v
                hi = v
        results[name] = (lo, hi)
    return results


def sweep_2d(gamma=1.0, lo=0.50, hi=1.50, step=0.02):
    """2D sweep: alpha x beta with fixed gamma. Returns (alphas, betas, grid)."""
    alphas = []
    a = lo
    while a <= hi + 1e-9:
        alphas.append(round(a, 4))
        a += step

    betas = list(alphas)  # same range

    grid = []
    for alpha in alphas:
        row = []
        for beta in betas:
            seq = simulate(alpha, beta, gamma)
            row.append(count_correct(seq))
        grid.append(row)

    return alphas, betas, grid


def sweep_2d_fine():
    """Fine sweep around the island of correctness."""
    return sweep_2d(gamma=1.0, lo=0.70, hi=1.30, step=0.01)

# ===== Output Functions =====

def print_1d_windows(windows):
    print("--- 1D Weight Windows (9+ correct steps) ---")
    print()
    for name, (lo, hi) in windows.items():
        if lo is not None and hi is not None:
            width = hi - lo
            center = (lo + hi) / 2
            print(f"  {name:22s}: [{lo:.2f}, {hi:.2f}]  width={width:.2f}  center={center:.2f}")
        else:
            print(f"  {name:22s}: NO VALID RANGE")
    print()


def print_coarse_grid(alphas, betas, grid):
    """Print the grid at 0.1 resolution for the paper's text table."""
    print("--- 2D Sweep: alpha vs beta (gamma=1.0) ---")
    print("    ** = first 9+ steps correct")
    print()

    # Pick every 5th value (step=0.10 from step=0.02)
    a_indices = [i for i, a in enumerate(alphas) if abs(a * 10 - round(a * 10)) < 0.005]
    b_indices = [j for j, b in enumerate(betas) if abs(b * 10 - round(b * 10)) < 0.005]

    header = "     beta="
    for j in b_indices:
        header += f" {betas[j]:4.1f}"
    print(header)
    print("     " + "-" * (len(b_indices) * 5))

    for i in a_indices:
        row = f"a={alphas[i]:.1f} "
        for j in b_indices:
            val = grid[i][j]
            if val >= 9:
                row += "   **"
            else:
                row += f"   {val:2d}"
        print(row)
    print()


def print_fine_grid(alphas, betas, grid):
    """Print the fine grid at 0.05 resolution."""
    print("--- Fine 2D Sweep (0.70..1.30, step=0.01) ---")
    print("    ** = first 9+ steps correct")
    print()

    # Pick every 5th (step=0.05)
    a_indices = [i for i, a in enumerate(alphas) if abs(a * 20 - round(a * 20)) < 0.005]
    b_indices = [j for j, b in enumerate(betas) if abs(b * 20 - round(b * 20)) < 0.005]

    header = "      beta="
    for j in b_indices:
        header += f" {betas[j]:5.2f}"
    print(header)
    print("      " + "-" * (len(b_indices) * 6))

    for i in a_indices:
        row = f"a={alphas[i]:.2f} "
        for j in b_indices:
            val = grid[i][j]
            if val >= 9:
                row += "    **"
            else:
                row += f"    {val:2d}"
        print(row)
    print()


def print_statistics(alphas, betas, grid):
    """Print summary statistics."""
    total = len(alphas) * len(betas)
    correct_9 = sum(1 for row in grid for v in row if v >= 9)
    correct_15 = sum(1 for row in grid for v in row if v >= 15)

    # Find boundary of 9+ region
    a_with_9 = [alphas[i] for i in range(len(alphas))
                if any(grid[i][j] >= 9 for j in range(len(betas)))]
    b_with_9 = [betas[j] for j in range(len(betas))
                if any(grid[i][j] >= 9 for i in range(len(alphas)))]

    print("--- Statistics ---")
    print(f"  Grid size:   {len(alphas)} x {len(betas)} = {total}")
    print(f"  9+ correct:  {correct_9} cells ({100*correct_9/total:.1f}%)")
    print(f"  15 correct:  {correct_15} cells ({100*correct_15/total:.1f}%)")
    if a_with_9:
        print(f"  alpha range: [{min(a_with_9):.2f}, {max(a_with_9):.2f}]")
    if b_with_9:
        print(f"  beta range:  [{min(b_with_9):.2f}, {max(b_with_9):.2f}]")
    print()


def generate_figure(alphas, betas, grid, path):
    """Generate publication-quality heatmap (requires matplotlib)."""
    try:
        import matplotlib
        matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        import matplotlib.colors as mcolors
        import numpy as np
    except ImportError:
        print("  [matplotlib not available — skipping figure generation]")
        print(f"  Install matplotlib and re-run to generate {path}")
        return False

    data = np.array(grid, dtype=float)

    # Colormap: red (few correct) → yellow → green (9+)
    cmap = plt.cm.RdYlGn
    bounds = list(range(17))
    norm = mcolors.BoundaryNorm(bounds, cmap.N)

    fig, ax = plt.subplots(figsize=(8, 6.5))

    B, A = np.meshgrid(betas, alphas)
    im = ax.pcolormesh(B, A, data, cmap=cmap, norm=norm, shading='nearest')

    # Mark (1.0, 1.0)
    ax.plot(1.0, 1.0, 'k*', markersize=14, markeredgecolor='white', markeredgewidth=0.6)

    # Contour at 8.5 (boundary of 9+ region)
    try:
        ax.contour(B, A, data, levels=[8.5], colors='black', linewidths=1.5, linestyles='--')
    except Exception:
        pass

    ax.set_xlabel(r'$\beta$ (homotopy weight)', fontsize=13)
    ax.set_ylabel(r'$\alpha$ (grammar weight)', fontsize=13)
    ax.set_title(
        r'Genesis Sequence correctness under $\nu = \alpha\nu_G + \beta\nu_H + \gamma\nu_C$'
        + '\n' + r'$\gamma = 1.0$; color = number of correct leading steps',
        fontsize=11
    )

    cbar = fig.colorbar(im, ax=ax, label='Correct leading steps',
                        ticks=[0, 3, 5, 7, 9, 12, 15])

    ax.set_xlim(min(betas), max(betas))
    ax.set_ylim(min(alphas), max(alphas))
    ax.set_aspect('equal')

    plt.tight_layout()
    plt.savefig(path, dpi=300, bbox_inches='tight')

    pdf_path = path.replace('.png', '.pdf')
    plt.savefig(pdf_path, bbox_inches='tight')
    plt.close()

    print(f"  Figure saved: {path}")
    print(f"  Figure saved: {pdf_path}")
    return True


def generate_fine_figure(alphas, betas, grid, path):
    """Generate zoomed-in figure of the island of correctness."""
    try:
        import matplotlib
        matplotlib.use('Agg')
        import matplotlib.pyplot as plt
        import matplotlib.colors as mcolors
        import numpy as np
    except ImportError:
        return False

    data = np.array(grid, dtype=float)

    # Binary colormap: <9 = light red, 9+ = green shades
    colors = ['#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c',
              '#cb181d', '#a50f15', '#67000d', '#004529',
              '#006d2c', '#238b45', '#41ab5d', '#74c476',
              '#a1d99b', '#c7e9c0', '#e5f5e0']
    cmap = mcolors.ListedColormap(colors)
    bounds = list(range(17))
    norm = mcolors.BoundaryNorm(bounds, cmap.N)

    fig, ax = plt.subplots(figsize=(7, 6))

    B, A = np.meshgrid(betas, alphas)
    im = ax.pcolormesh(B, A, data, cmap=cmap, norm=norm, shading='nearest')

    ax.plot(1.0, 1.0, 'w*', markersize=16, markeredgecolor='black', markeredgewidth=0.8)

    try:
        ax.contour(B, A, data, levels=[8.5], colors='white', linewidths=2)
    except Exception:
        pass

    ax.set_xlabel(r'$\beta$ (homotopy weight)', fontsize=13)
    ax.set_ylabel(r'$\alpha$ (grammar weight)', fontsize=13)
    ax.set_title(
        r'Island of correctness: $\nu = \alpha\nu_G + \beta\nu_H + \gamma\nu_C$, $\gamma=1.0$',
        fontsize=11
    )

    cbar = fig.colorbar(im, ax=ax, label='Correct leading steps',
                        ticks=[0, 3, 5, 7, 9, 12, 15])
    ax.set_aspect('equal')

    plt.tight_layout()
    plt.savefig(path, dpi=300, bbox_inches='tight')
    plt.savefig(path.replace('.png', '.pdf'), bbox_inches='tight')
    plt.close()

    print(f"  Fine figure saved: {path}")
    return True

# ===== Critical Margins Analysis =====

def compute_margins():
    """Compute Bar and margin (rho - Bar) for each step at (1,1,1)."""
    print("--- Critical Margins at (alpha, beta, gamma) = (1, 1, 1) ---")
    print()
    print(" n  | Structure      | nu  | kappa | rho    | Phi    | Omega  | Bar    | Margin")
    print("----|----------------|-----|-------|--------|--------|--------|--------|-------")

    cum_nu = 0
    cum_kappa = 0
    for step, (idx, name, kappa, nu_g, nu_h, nu_c) in enumerate(
        [(idx, n, k, g, h, c) for idx, n, k, g, h, c in GENESIS
         if idx in PAPER_ORDER], start=1
    ):
        nu = nu_g + nu_h + nu_c
        rho = nu / kappa if kappa > 0 else 0

        if step <= 1 or cum_kappa == 0:
            bar = 0.0
            phi = 0.0
            omega = 0.0
        else:
            phi = fib(step) / fib(step - 1)
            omega = cum_nu / cum_kappa
            bar = phi * omega

        margin = rho - bar
        print(f"{step:3d} | {name:14s} | {nu:3d} | {kappa:5d} | {rho:6.3f} | "
              f"{phi:6.3f} | {omega:6.3f} | {bar:6.3f} | {margin:+.3f}")

        cum_nu += nu
        cum_kappa += kappa
    print()


# ===== Main =====

def main():
    text_only = '--text-only' in sys.argv

    print("=" * 60)
    print("PEN Novelty Decomposition: Combination Rule Sweep")
    print("=" * 60)
    print()

    # 0. Verify baseline
    seq = simulate(1.0, 1.0, 1.0)
    c = count_correct(seq)
    print(f"Baseline (1.0, 1.0, 1.0): {c} correct steps, {len(seq)} realized")
    print(f"  Realized: {seq}")
    print(f"  Expected: {PAPER_ORDER}")
    match = "MATCH" if seq == PAPER_ORDER else f"DIVERGE at step {c+1}"
    print(f"  Result:   {match}")
    print()

    # 1. Critical margins
    compute_margins()

    # 2. 1D windows
    print("Computing 1D windows...")
    windows = sweep_1d()
    print_1d_windows(windows)

    # 3. Coarse 2D sweep
    print("Computing coarse 2D sweep (0.50..1.50, step=0.02)...")
    alphas, betas, grid = sweep_2d(gamma=1.0, lo=0.50, hi=1.50, step=0.02)
    print_statistics(alphas, betas, grid)
    print_coarse_grid(alphas, betas, grid)

    # 4. Fine 2D sweep
    print("Computing fine 2D sweep (0.70..1.30, step=0.01)...")
    fa, fb, fg = sweep_2d_fine()
    print_fine_grid(fa, fb, fg)

    # 5. Figures
    if not text_only:
        print("Generating figures...")
        base = os.path.dirname(os.path.abspath(__file__))
        generate_figure(alphas, betas, grid,
                        os.path.join(base, 'sweep_figure.png'))
        generate_fine_figure(fa, fb, fg,
                             os.path.join(base, 'sweep_fine_figure.png'))

    # 6. Non-additive rules
    print("--- Non-Additive Combination Rules ---")
    print()

    rules = [
        ("sum",                lambda g,h,c: g+h+c),
        ("max",                lambda g,h,c: max(g,h,c)),
        ("product",            lambda g,h,c: g*h*c),
        ("shifted product",    lambda g,h,c: (1+g)*(1+h)*(1+c)-1),
        ("L2 norm",            lambda g,h,c: (g**2+h**2+c**2)**0.5),
        ("G + H^2 + C",       lambda g,h,c: g + h**2 + c),
        ("2G + H + C",        lambda g,h,c: 2*g + h + c),
        ("G + 2H + C",        lambda g,h,c: g + 2*h + c),
        ("G + H + 2C",        lambda g,h,c: g + h + 2*c),
    ]

    for label, f in rules:
        # Build candidates with this rule
        cands = {}
        for idx, name, kappa, nu_g, nu_h, nu_c in GENESIS:
            wnu = f(nu_g, nu_h, nu_c)
            rho = wnu / kappa if kappa > 0 else 0.0
            cands[idx] = {'kappa': kappa, 'nu': wnu, 'rho': rho}

        # Run simulation with these candidates directly
        realized = []
        cum_nu = 0.0
        cum_kappa = 0
        horizon = 2
        remaining = set(cands.keys())
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
                cc = cands[idx]
                if cc['kappa'] <= horizon and cc['rho'] >= bar:
                    clearing.append((cc['rho'] - bar, cc['kappa'], idx))

            if not clearing:
                horizon = min(horizon + 1, 20)
                idle += 1
                continue

            clearing.sort()
            _, _, winner = clearing[0]
            cc = cands[winner]
            realized.append(winner)
            cum_nu += cc['nu']
            cum_kappa += cc['kappa']
            remaining.remove(winner)
            delta = fib(n)
            horizon = min(delta + 1, 20)
            idle = 0

        correct = count_correct(realized)
        print(f"  {label:22s}: {correct:2d} correct steps  seq={realized[:10]}...")

    print()
    print("Done.")


if __name__ == '__main__':
    main()
