#!/usr/bin/env bash
# repro_ab_initio.sh — Deterministic replication harness for ab-initio runs
#
# Runs all three evaluation modes with fixed seeds and captures:
#   - Console output (log)
#   - Machine-readable CSV (step, name, nu, kappa, rho, bar, delta, source, candidates)
#
# Usage:
#   ./scripts/repro_ab_initio.sh                    # output to runs/<timestamp>/
#   ./scripts/repro_ab_initio.sh my_experiment      # output to runs/my_experiment/
#
# Modes run:
#   1. --structural (primary, publication-grade)
#   2. --structural --window 1 (d=1 extensional stress test)
#   3. --structural --window 3 (d=3 tribonacci stress test)

set -euo pipefail

cd "$(dirname "$0")/.."

# Output directory
TAG="${1:-$(date +%Y%m%d_%H%M%S)}"
OUTDIR="runs/${TAG}"
mkdir -p "$OUTDIR"

echo "=== PEN Replication Harness ==="
echo "Output: $OUTDIR"
echo ""

# Build first
echo "Building..."
(cd engine && cabal build ab-initio 2>&1) | tail -1
echo ""

PENROOT="$(pwd)"

run_mode() {
    local label="$1"
    shift
    local logfile="$PENROOT/$OUTDIR/${label}.log"
    local csvfile="$PENROOT/$OUTDIR/${label}.csv"

    echo "--- Running: $label ($*) ---"
    (cd engine && cabal run ab-initio -- "$@" --csv "$csvfile") > "$logfile" 2>&1
    echo "  Log: $logfile"
    echo "  CSV: $csvfile"
    echo "  Steps: $(tail -1 "$csvfile" | cut -d, -f1)"
    echo ""
}

# Primary mode: structural d=2
run_mode "structural_d2" --structural

# Stress test: d=1 (extensional — should show degraded ordering)
run_mode "structural_d1" --structural --window 1

# Stress test: d=3 (tribonacci — should show over-constrained bar)
run_mode "structural_d3" --structural --window 3

# Paper-calibrated mode (reference comparison)
run_mode "paper_calibrated"

echo "=== Replication complete ==="
echo "All artifacts in: $OUTDIR/"
ls -la "$OUTDIR/"
