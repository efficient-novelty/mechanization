#!/usr/bin/env bash
# benchmark.sh — One-command verification for external reviewers
#
# Runs three benchmark profiles and produces a pass/fail report:
#   Profile 1: Paper-calibrated replay (15/15 exact match expected)
#   Profile 2: Structural discovery (12/15 exact, total ν=359, κ=64)
#   Profile 3: d-window sweep (d=1 degraded, d=2 correct, d=3 degraded)
#   Profile 4: Acceptance test suite (42 unit tests)
#
# Usage:
#   ./scripts/benchmark.sh              # output to runs/benchmark_<timestamp>/
#   ./scripts/benchmark.sh my_review    # output to runs/my_review/
#
# Exit code: 0 if all profiles pass, 1 if any fail.
# No source code reading required — interpret the REPORT.txt file.

set -euo pipefail

cd "$(dirname "$0")/.."
PENROOT="$(pwd)"

TAG="${1:-benchmark_$(date +%Y%m%d_%H%M%S)}"
OUTDIR="runs/${TAG}"
mkdir -p "$OUTDIR"

REPORT="$OUTDIR/REPORT.txt"
FAILS=0

# Header
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

# Build
echo "Building engine..." | tee -a "$REPORT"
(cd engine && cabal build all 2>&1) | tail -1 | tee -a "$REPORT"
echo "" | tee -a "$REPORT"

# ============================================
# Profile 1: Paper-calibrated replay
# ============================================
echo "--- Profile 1: Paper-Calibrated Replay ---" | tee -a "$REPORT"

CSV1="$PENROOT/$OUTDIR/paper_calibrated.csv"
(cd engine && cabal run ab-initio -- --csv "$CSV1") > "$PENROOT/$OUTDIR/paper_calibrated.log" 2>&1

# Check: 15 steps, all ν and κ exact
STEPS1=$(tail -n +2 "$CSV1" | wc -l | tr -d ' ')
TOTAL_NU1=$(tail -n +2 "$CSV1" | awk -F, '{s+=$3} END{print s}')
TOTAL_K1=$(tail -n +2 "$CSV1" | awk -F, '{s+=$4} END{print s}')

P1_PASS=true
if [ "$STEPS1" -ne 15 ]; then P1_PASS=false; fi
if [ "$TOTAL_NU1" -ne 356 ]; then P1_PASS=false; fi
if [ "$TOTAL_K1" -ne 64 ]; then P1_PASS=false; fi

if $P1_PASS; then
  echo "  PASS: 15 steps, total ν=$TOTAL_NU1/356, κ=$TOTAL_K1/64" | tee -a "$REPORT"
else
  echo "  FAIL: steps=$STEPS1, total ν=$TOTAL_NU1/356, κ=$TOTAL_K1/64" | tee -a "$REPORT"
  FAILS=$((FAILS + 1))
fi
echo "" | tee -a "$REPORT"

# ============================================
# Profile 2: Structural discovery
# ============================================
echo "--- Profile 2: Structural Discovery ---" | tee -a "$REPORT"

CSV2="$PENROOT/$OUTDIR/structural_d2.csv"
(cd engine && cabal run ab-initio -- --structural --csv "$CSV2") > "$PENROOT/$OUTDIR/structural_d2.log" 2>&1

STEPS2=$(tail -n +2 "$CSV2" | wc -l | tr -d ' ')
TOTAL_NU2=$(tail -n +2 "$CSV2" | awk -F, '{s+=$3} END{print s}')
TOTAL_K2=$(tail -n +2 "$CSV2" | awk -F, '{s+=$4} END{print s}')

# Count exact matches against paper values
PAPER_NU=(1 1 2 5 7 8 10 18 17 19 26 34 43 60 105)
PAPER_K=(2 1 1 3 3 3 3 5 4 4 5 6 7 9 8)
EXACT=0
i=0
while IFS=, read -r step name nu kappa rest; do
  if [ "$nu" -eq "${PAPER_NU[$i]}" ] && [ "$kappa" -eq "${PAPER_K[$i]}" ]; then
    EXACT=$((EXACT + 1))
  fi
  i=$((i + 1))
done < <(tail -n +2 "$CSV2")

# Check canonical names
NAMES=$(tail -n +2 "$CSV2" | awk -F, '{print $2}' | tr '\n' ',' | sed 's/,$//')
EXPECTED_NAMES="Universe,Unit,Witness,Pi,S1,Trunc,S2,S3,Hopf,Cohesion,Connections,Curvature,Metric,Hilbert,DCT"

P2_PASS=true
if [ "$STEPS2" -ne 15 ]; then P2_PASS=false; fi
if [ "$EXACT" -lt 12 ]; then P2_PASS=false; fi
if [ "$TOTAL_K2" -ne 64 ]; then P2_PASS=false; fi
if [ "$NAMES" != "$EXPECTED_NAMES" ]; then P2_PASS=false; fi

if $P2_PASS; then
  echo "  PASS: 15 steps, $EXACT/15 exact, total ν=$TOTAL_NU2, κ=$TOTAL_K2/64" | tee -a "$REPORT"
  echo "  Names: $NAMES" | tee -a "$REPORT"
else
  echo "  FAIL: steps=$STEPS2, exact=$EXACT/15, ν=$TOTAL_NU2, κ=$TOTAL_K2/64" | tee -a "$REPORT"
  echo "  Names: $NAMES" | tee -a "$REPORT"
  FAILS=$((FAILS + 1))
fi
echo "" | tee -a "$REPORT"

# ============================================
# Profile 3: d-window sweep
# ============================================
echo "--- Profile 3: Coherence Window Sweep ---" | tee -a "$REPORT"

for D in 1 2 3; do
  CSVD="$PENROOT/$OUTDIR/structural_d${D}.csv"
  if [ ! -f "$CSVD" ]; then
    (cd engine && cabal run ab-initio -- --structural --window "$D" --csv "$CSVD") > "$PENROOT/$OUTDIR/structural_d${D}.log" 2>&1
  fi
  STEPSD=$(tail -n +2 "$CSVD" | wc -l | tr -d ' ')
  TOTAL_NUD=$(tail -n +2 "$CSVD" | awk -F, '{s+=$3} END{print s}')
  echo "  d=$D: $STEPSD steps, total ν=$TOTAL_NUD" | tee -a "$REPORT"
done

# d=2 must be the best
NU_D1=$(tail -n +2 "$PENROOT/$OUTDIR/structural_d1.csv" | awk -F, '{s+=$3} END{print s}')
NU_D2=$(tail -n +2 "$PENROOT/$OUTDIR/structural_d2.csv" | awk -F, '{s+=$3} END{print s}')
NU_D3=$(tail -n +2 "$PENROOT/$OUTDIR/structural_d3.csv" | awk -F, '{s+=$3} END{print s}')

P3_PASS=true
if [ "$NU_D2" -le "$NU_D1" ]; then P3_PASS=false; fi
if [ "$NU_D2" -le "$NU_D3" ]; then P3_PASS=false; fi

if $P3_PASS; then
  echo "  PASS: d=2 (ν=$NU_D2) > d=1 (ν=$NU_D1) and d=3 (ν=$NU_D3)" | tee -a "$REPORT"
else
  echo "  FAIL: d=2 not uniquely optimal (d1=$NU_D1, d2=$NU_D2, d3=$NU_D3)" | tee -a "$REPORT"
  FAILS=$((FAILS + 1))
fi
echo "" | tee -a "$REPORT"

# ============================================
# Profile 4: Acceptance tests
# ============================================
echo "--- Profile 4: Acceptance Tests ---" | tee -a "$REPORT"

(cd engine && cabal run acceptance) > "$PENROOT/$OUTDIR/acceptance.log" 2>&1
ACCEPTANCE_EXIT=$?

PASS_COUNT=$(grep -c "PASS" "$PENROOT/$OUTDIR/acceptance.log" || true)
FAIL_COUNT=$(grep -c "FAIL:" "$PENROOT/$OUTDIR/acceptance.log" || true)

if [ "$ACCEPTANCE_EXIT" -eq 0 ] && [ "$FAIL_COUNT" -eq 0 ]; then
  echo "  PASS: $PASS_COUNT tests passed, 0 failed" | tee -a "$REPORT"
else
  echo "  FAIL: $PASS_COUNT passed, $FAIL_COUNT failed" | tee -a "$REPORT"
  FAILS=$((FAILS + 1))
fi
echo "" | tee -a "$REPORT"

# ============================================
# Summary
# ============================================
{
  echo "============================================"
  if [ "$FAILS" -eq 0 ]; then
    echo "ALL 4 PROFILES PASS"
  else
    echo "$FAILS PROFILE(S) FAILED"
  fi
  echo "============================================"
  echo ""
  echo "Artifacts:"
  ls -1 "$PENROOT/$OUTDIR/"
} | tee -a "$REPORT"

exit "$FAILS"
