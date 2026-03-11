#!/usr/bin/env bash
# benchmark.sh - One-command verification for external reviewers
#
# Runs three strict-mode benchmark profiles and produces a pass/fail report:
#   Profile 1: Strict discovery (15 steps, nu=348, kappa=64, canonical name sequence)
#   Profile 2: Strict d-window sweep (d=2 must outperform d=1 and d=3)
#   Profile 3: Acceptance test suite
#
# Usage:
#   ./scripts/benchmark.sh              # output to runs/benchmark_<timestamp>/
#   ./scripts/benchmark.sh my_review    # output to runs/my_review/

set -euo pipefail

cd "$(dirname "$0")/.."
PENROOT="$(pwd)"

if command -v cabal >/dev/null 2>&1; then
  CABAL_BIN=cabal
elif command -v cabal.exe >/dev/null 2>&1; then
  CABAL_BIN=cabal.exe
else
  echo "cabal or cabal.exe is required" >&2
  exit 1
fi

TAG="${1:-benchmark_$(date +%Y%m%d_%H%M%S)}"
OUTDIR="runs/${TAG}"
mkdir -p "$OUTDIR"

REPORT="$OUTDIR/REPORT.txt"
FAILS=0
EXPECTED_NAMES="Universe,Unit,Witness,Pi,S1,Trunc,S2,S3,Hopf,Cohesion,Connections,Curvature,Metric,Hilbert,DCT"

run_logged() {
  local logfile="$1"
  shift
  set +e
  "$@" > "$logfile" 2>&1
  local ec=$?
  set -e
  return "$ec"
}

{
  echo "============================================"
  echo "PEN Benchmark Report"
  echo "============================================"
  echo "Date:    $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "Git:     $(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')"
  echo "Branch:  $(git branch --show-current 2>/dev/null || echo 'unknown')"
  echo "Output:  $OUTDIR/"
  echo ""
} | tee "$REPORT"

echo "Building engine..." | tee -a "$REPORT"
(cd engine && "$CABAL_BIN" build all 2>&1) | tail -1 | tee -a "$REPORT"
echo "" | tee -a "$REPORT"

echo "--- Profile 1: Strict Discovery ---" | tee -a "$REPORT"
CSV1="$PENROOT/$OUTDIR/strict_d2.csv"
CSV1_ARG="../$OUTDIR/strict_d2.csv"
LOG1="$PENROOT/$OUTDIR/strict_d2.log"
if run_logged "$LOG1" bash -lc "cd \"$PENROOT/engine\" && \"$CABAL_BIN\" run ab-initio -- --strict --csv \"$CSV1_ARG\""; then
  STEPS1=$(tail -n +2 "$CSV1" | wc -l | tr -d ' ')
  TOTAL_NU1=$(tail -n +2 "$CSV1" | awk -F, '{s+=$3} END{print s}')
  TOTAL_K1=$(tail -n +2 "$CSV1" | awk -F, '{s+=$4} END{print s}')
  NAMES1=$(tail -n +2 "$CSV1" | awk -F, '{print $2}' | tr '\n' ',' | sed 's/,$//')
  P1_PASS=true
  if [ "$STEPS1" -ne 15 ]; then P1_PASS=false; fi
  if [ "$TOTAL_NU1" -ne 348 ]; then P1_PASS=false; fi
  if [ "$TOTAL_K1" -ne 64 ]; then P1_PASS=false; fi
  if [ "$NAMES1" != "$EXPECTED_NAMES" ]; then P1_PASS=false; fi
  if $P1_PASS; then
    echo "  PASS: 15 steps, nu=$TOTAL_NU1/348, kappa=$TOTAL_K1/64" | tee -a "$REPORT"
    echo "  Names: $NAMES1" | tee -a "$REPORT"
  else
    echo "  FAIL: steps=$STEPS1, nu=$TOTAL_NU1/348, kappa=$TOTAL_K1/64" | tee -a "$REPORT"
    echo "  Names: $NAMES1" | tee -a "$REPORT"
    FAILS=$((FAILS + 1))
  fi
else
  echo "  FAIL: strict discovery command failed (see strict_d2.log)" | tee -a "$REPORT"
  FAILS=$((FAILS + 1))
fi
echo "" | tee -a "$REPORT"

echo "--- Profile 2: Strict Coherence Window Sweep ---" | tee -a "$REPORT"
for D in 1 2 3; do
  CSV="$PENROOT/$OUTDIR/strict_d${D}.csv"
  CSV_ARG="../$OUTDIR/strict_d${D}.csv"
  LOG="$PENROOT/$OUTDIR/strict_d${D}.log"
  if [ "$D" -eq 2 ] && [ -f "$CSV1" ]; then
    :
  else
    if ! run_logged "$LOG" bash -lc "cd \"$PENROOT/engine\" && \"$CABAL_BIN\" run ab-initio -- --strict --window \"$D\" --csv \"$CSV_ARG\""; then
      echo "  FAIL: strict d=$D command failed (see strict_d${D}.log)" | tee -a "$REPORT"
      FAILS=$((FAILS + 1))
      continue
    fi
  fi
  STEPS=$(tail -n +2 "$CSV" | wc -l | tr -d ' ')
  TOTAL_NU=$(tail -n +2 "$CSV" | awk -F, '{s+=$3} END{print s}')
  echo "  d=$D: $STEPS steps, total nu=$TOTAL_NU" | tee -a "$REPORT"
done

NU_D1=$(tail -n +2 "$PENROOT/$OUTDIR/strict_d1.csv" | awk -F, '{s+=$3} END{print s}')
NU_D2=$(tail -n +2 "$PENROOT/$OUTDIR/strict_d2.csv" | awk -F, '{s+=$3} END{print s}')
NU_D3=$(tail -n +2 "$PENROOT/$OUTDIR/strict_d3.csv" | awk -F, '{s+=$3} END{print s}')
if [ "$NU_D2" -gt "$NU_D1" ] && [ "$NU_D2" -gt "$NU_D3" ]; then
  echo "  PASS: d=2 (nu=$NU_D2) > d=1 (nu=$NU_D1) and d=3 (nu=$NU_D3)" | tee -a "$REPORT"
else
  echo "  FAIL: d=2 not uniquely optimal (d1=$NU_D1, d2=$NU_D2, d3=$NU_D3)" | tee -a "$REPORT"
  FAILS=$((FAILS + 1))
fi
echo "" | tee -a "$REPORT"

echo "--- Profile 3: Acceptance Tests ---" | tee -a "$REPORT"
ACCEPTANCE_LOG="$PENROOT/$OUTDIR/acceptance.log"
if run_logged "$ACCEPTANCE_LOG" bash -lc "cd \"$PENROOT/engine\" && \"$CABAL_BIN\" run acceptance"; then
  PASS_COUNT=$(grep -c "PASS" "$ACCEPTANCE_LOG" || true)
  FAIL_COUNT=$(grep -c "FAIL:" "$ACCEPTANCE_LOG" || true)
  if [ "$FAIL_COUNT" -eq 0 ]; then
    echo "  PASS: $PASS_COUNT tests passed, 0 failed" | tee -a "$REPORT"
  else
    echo "  FAIL: $PASS_COUNT passed, $FAIL_COUNT failed" | tee -a "$REPORT"
    FAILS=$((FAILS + 1))
  fi
else
  PASS_COUNT=$(grep -c "PASS" "$ACCEPTANCE_LOG" || true)
  FAIL_COUNT=$(grep -c "FAIL:" "$ACCEPTANCE_LOG" || true)
  echo "  FAIL: acceptance command failed ($PASS_COUNT passed, $FAIL_COUNT failed)" | tee -a "$REPORT"
  FAILS=$((FAILS + 1))
fi
echo "" | tee -a "$REPORT"

{
  echo "============================================"
  if [ "$FAILS" -eq 0 ]; then
    echo "ALL 3 PROFILES PASS"
  else
    echo "$FAILS PROFILE(S) FAILED"
  fi
  echo "============================================"
  echo ""
  echo "Artifacts:"
  ls -1 "$PENROOT/$OUTDIR/"
} | tee -a "$REPORT"

exit "$FAILS"
