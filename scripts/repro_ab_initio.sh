#!/usr/bin/env bash
# repro_ab_initio.sh - Deterministic replication harness for strict ab-initio runs
#
# Captures console output (log) and machine-readable CSV for the current strict
# discovery lane at d=2, d=1, and d=3.

set -euo pipefail

cd "$(dirname "$0")/.."

if command -v cabal >/dev/null 2>&1; then
    CABAL_BIN=cabal
elif command -v cabal.exe >/dev/null 2>&1; then
    CABAL_BIN=cabal.exe
else
    echo "cabal or cabal.exe is required" >&2
    exit 1
fi

TAG="${1:-$(date +%Y%m%d_%H%M%S)}"
OUTDIR="runs/${TAG}"
mkdir -p "$OUTDIR"

echo "=== PEN Replication Harness ==="
echo "Output: $OUTDIR"
echo ""

echo "Building..."
(cd engine && "$CABAL_BIN" build ab-initio 2>&1) | tail -1
echo ""

PENROOT="$(pwd)"

run_mode() {
    local label="$1"
    shift
    local logfile="$PENROOT/$OUTDIR/${label}.log"
    local csvfile="$PENROOT/$OUTDIR/${label}.csv"
    local csvarg="../$OUTDIR/${label}.csv"

    echo "--- Running: $label ($*) ---"
    (cd engine && "$CABAL_BIN" run ab-initio -- --strict "$@" --csv "$csvarg") > "$logfile" 2>&1
    echo "  Log: $logfile"
    echo "  CSV: $csvfile"
    echo "  Steps: $(tail -1 "$csvfile" | cut -d, -f1)"
    echo ""
}

run_mode "strict_d2"
run_mode "strict_d1" --window 1
run_mode "strict_d3" --window 3

echo "=== Replication complete ==="
echo "All artifacts in: $OUTDIR/"
ls -la "$OUTDIR/"
