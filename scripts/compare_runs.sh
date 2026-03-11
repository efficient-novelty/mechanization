#!/usr/bin/env bash
# compare_runs.sh — Diff two replication runs
#
# Usage:
#   ./scripts/compare_runs.sh runs/baseline runs/candidate
#   ./scripts/compare_runs.sh runs/baseline runs/candidate strict_d2
#
# Compares CSV files between two run directories. If a specific mode is given,
# only compares that mode. Otherwise compares all common CSV files.

set -euo pipefail

if [ $# -lt 2 ]; then
    echo "Usage: $0 <baseline_dir> <candidate_dir> [mode]"
    echo "  common mode examples: strict_d2, strict_d1, strict_d3"
    exit 1
fi

BASE="$1"
CAND="$2"
MODE="${3:-}"

compare_csv() {
    local name="$1"
    local base_csv="$BASE/${name}.csv"
    local cand_csv="$CAND/${name}.csv"

    if [ ! -f "$base_csv" ]; then
        echo "  SKIP: $base_csv not found"
        return
    fi
    if [ ! -f "$cand_csv" ]; then
        echo "  SKIP: $cand_csv not found"
        return
    fi

    echo "--- Comparing: $name ---"
    if diff -q "$base_csv" "$cand_csv" > /dev/null 2>&1; then
        echo "  IDENTICAL"
    else
        echo "  DIFFERENCES FOUND:"
        # Show side-by-side comparison of key columns
        paste -d'|' <(cut -d, -f1,2,3,4,8 "$base_csv") <(cut -d, -f1,2,3,4,8 "$cand_csv") | \
            awk -F'|' 'NR==1 {printf "%-35s | %-35s\n", "BASELINE", "CANDIDATE"; next}
                       $1 != $2 {printf "%-35s | %-35s  <-- DIFF\n", $1, $2; next}
                       {printf "%-35s | %-35s\n", $1, $2}'
    fi
    echo ""
}

if [ -n "$MODE" ]; then
    compare_csv "$MODE"
else
    for csv in "$BASE"/*.csv; do
        name=$(basename "$csv" .csv)
        compare_csv "$name"
    done
fi
