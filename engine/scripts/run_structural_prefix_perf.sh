#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/structural_prefix_perf/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi

TIMEOUT_S="${TIMEOUT_S:-120}"
MAX_STEPS="${MAX_STEPS:-4}"
ABINITIO_BIN="${ABINITIO_BIN:-}"

TIMEOUT_CMD="timeout"
if ! command -v "$TIMEOUT_CMD" >/dev/null 2>&1; then
  if command -v gtimeout >/dev/null 2>&1; then
    TIMEOUT_CMD="gtimeout"
  else
    echo "Missing timeout command (timeout/gtimeout)." >&2
    exit 2
  fi
fi

mkdir -p "$OUT_DIR"

RUN_LOG="$OUT_DIR/structural_prefix.log"
BUILD_LOG="$OUT_DIR/structural_prefix_build.log"
REPORT_TMP="structural_prefix_report.csv"
REPORT_CSV="$OUT_DIR/prefix_report.csv"
CSV_OUT="$OUT_DIR/structural_prefix.csv"
rm -f "$RUN_LOG" "$BUILD_LOG" "$REPORT_CSV" "$CSV_OUT"

pushd "$ENGINE_DIR" >/dev/null
rm -f "$REPORT_TMP"

if [[ -z "$ABINITIO_BIN" ]]; then
  set +e
  cabal build exe:ab-initio > "$BUILD_LOG" 2>&1
  build_ec=$?
  set -e
  if [[ "$build_ec" -ne 0 ]]; then
    echo "Failed to build exe:ab-initio (see $BUILD_LOG)." >&2
    exit "$build_ec"
  fi
  ABINITIO_BIN="$(cabal list-bin exe:ab-initio)"
fi

start_epoch="$(date +%s)"
set +e
"$TIMEOUT_CMD" "$TIMEOUT_S" \
  "$ABINITIO_BIN" \
    --structural \
    --skip-validation \
    --max-steps "$MAX_STEPS" \
    --prefix-report "$REPORT_TMP" \
    --csv "$CSV_OUT" \
    > "$RUN_LOG" 2>&1
ec=$?
set -e
end_epoch="$(date +%s)"
duration_s="$((end_epoch - start_epoch))"

if [[ -f "$REPORT_TMP" ]]; then
  mv "$REPORT_TMP" "$REPORT_CSV"
fi
popd >/dev/null

python3 - "$REPORT_CSV" "$MAX_STEPS" "$ec" "$duration_s" "$TIMEOUT_S" > "$OUT_DIR/prefix_gate.txt" <<'PY'
import csv
import os
import sys

report_path = sys.argv[1]
max_steps = int(sys.argv[2])
exit_code = int(sys.argv[3])
duration_s = int(sys.argv[4])
timeout_s = int(sys.argv[5])

rows = []
if os.path.exists(report_path):
    with open(report_path, newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

by_step = {}
for row in rows:
    try:
        step = int(row.get("step", ""))
    except ValueError:
        continue
    by_step[step] = row

ok = True
reasons = []

if exit_code == 124:
    ok = False
    reasons.append(f"command timed out (timeout={timeout_s}s)")
elif exit_code != 0:
    ok = False
    reasons.append(f"command exit code={exit_code}")

for step in range(1, max_steps + 1):
    row = by_step.get(step)
    if row is None:
        ok = False
        reasons.append(f"missing step {step}")
        continue
    if row.get("status") != "selected":
        ok = False
        reasons.append(f"step {step} status={row.get('status')}")

print("pass" if ok else "fail")
print(f"duration_s={duration_s}")
print(f"timeout_s={timeout_s}")
print(f"exit_code={exit_code}")
print(f"rows={len(rows)}")
if not os.path.exists(report_path):
    print(f"missing_report={report_path}")
for reason in reasons:
    print(reason)
PY

echo "Wrote structural prefix performance artifacts to: $OUT_DIR"
