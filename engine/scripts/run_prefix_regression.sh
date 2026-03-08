#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/prefix_regression/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi

TIMEOUT_S="${TIMEOUT_S:-30}"
MAX_STEPS="${MAX_STEPS:-7}"
RTS_CORES="${RTS_CORES:--N}"
REQUIRE_SUCCESS_THROUGH="${REQUIRE_SUCCESS_THROUGH:-7}"
EXPECTED_NAMES="${EXPECTED_NAMES:-1:Universe 2:Unit 3:Witness 4:Pi 5:S1 6:Trunc 7:S2}"
USE_CABAL_RUN="${USE_CABAL_RUN:-0}"
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

RUN_LOG="$OUT_DIR/prefix_run.log"
BUILD_LOG="$OUT_DIR/prefix_build.log"
REPORT_TMP="strict_prefix_report.csv"
REPORT_CSV="$OUT_DIR/prefix_report.csv"
SUMMARY_CSV="$OUT_DIR/prefix_summary.csv"
rm -f "$RUN_LOG" "$BUILD_LOG" "$REPORT_CSV" "$SUMMARY_CSV"

pushd "$ENGINE_DIR" >/dev/null
rm -f "$REPORT_TMP"

RUN_DESC=""
if [[ "$USE_CABAL_RUN" != "1" ]]; then
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
  RUN_DESC="$ABINITIO_BIN --strict --phase1-shadow --max-steps $MAX_STEPS --prefix-report $REPORT_TMP +RTS $RTS_CORES -RTS"
else
  RUN_DESC="cabal run exe:ab-initio -- --strict --phase1-shadow --max-steps $MAX_STEPS --prefix-report $REPORT_TMP +RTS $RTS_CORES -RTS"
fi

start_epoch="$(date +%s)"
set +e
if [[ "$USE_CABAL_RUN" = "1" ]]; then
  "$TIMEOUT_CMD" "$TIMEOUT_S" \
    cabal run exe:ab-initio -- \
      --strict \
      --phase1-shadow \
      --max-steps "$MAX_STEPS" \
      --prefix-report "$REPORT_TMP" \
      +RTS "$RTS_CORES" -RTS \
      > "$RUN_LOG" 2>&1
else
  "$TIMEOUT_CMD" "$TIMEOUT_S" \
    "$ABINITIO_BIN" \
      --strict \
      --phase1-shadow \
      --max-steps "$MAX_STEPS" \
      --prefix-report "$REPORT_TMP" \
      +RTS "$RTS_CORES" -RTS \
      > "$RUN_LOG" 2>&1
fi
ec=$?
set -e
end_epoch="$(date +%s)"
duration_s="$((end_epoch - start_epoch))"

if [[ -f "$REPORT_TMP" ]]; then
  mv "$REPORT_TMP" "$REPORT_CSV"
fi
popd >/dev/null

python3 - "$REPORT_CSV" "$SUMMARY_CSV" <<'PY'
import csv
import os
import sys

report_path = sys.argv[1]
summary_path = sys.argv[2]

header = [
    "step",
    "status",
    "selected_name",
    "selected_nu",
    "selected_kappa",
    "selected_rho",
    "selected_source",
    "bar",
    "raw_candidates",
    "viable_candidates",
]

rows = []
if os.path.exists(report_path):
    with open(report_path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            try:
                step = int(row.get("step", ""))
            except ValueError:
                continue
            rows.append((step, row))

rows.sort(key=lambda x: x[0])

with open(summary_path, "w", newline="", encoding="utf-8") as out:
    writer = csv.writer(out)
    writer.writerow(header)
    for _, row in rows:
        writer.writerow([
            row.get("step", ""),
            row.get("status", ""),
            row.get("selected_name", ""),
            row.get("selected_nu", ""),
            row.get("selected_kappa", ""),
            row.get("selected_rho", ""),
            row.get("selected_source", ""),
            row.get("bar", ""),
            row.get("raw_candidates", ""),
            row.get("viable_candidates", ""),
        ])
PY

python3 - "$REPORT_CSV" "$REQUIRE_SUCCESS_THROUGH" "$EXPECTED_NAMES" "$ec" "$duration_s" "$TIMEOUT_S" > "$OUT_DIR/prefix_gate.txt" <<'PY'
import csv
import os
import sys

report_path = sys.argv[1]
require_success = int(sys.argv[2])
expected_spec = sys.argv[3].strip()
exit_code = int(sys.argv[4])
duration_s = int(sys.argv[5])
timeout_s = int(sys.argv[6])

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

if require_success > 0:
    for step in range(1, require_success + 1):
        row = by_step.get(step)
        if row is None:
            ok = False
            reasons.append(f"missing step {step}")
            continue
        if row.get("status") != "selected":
            ok = False
            reasons.append(f"step {step} status={row.get('status')}")

if expected_spec:
    for part in expected_spec.split():
        if ":" not in part:
            continue
        s_raw, expected_name = part.split(":", 1)
        try:
            step = int(s_raw)
        except ValueError:
            continue
        row = by_step.get(step)
        actual = "" if row is None else (row.get("selected_name") or "")
        if actual != expected_name:
            ok = False
            reasons.append(f"step {step} expected {expected_name} got {actual or '<none>'}")

observed = " ".join(
    f"{step}:{(row.get('selected_name') or '<none>')}"
    for step, row in sorted(by_step.items())
)

print("pass" if ok else "fail")
print(f"duration_s={duration_s}")
print(f"timeout_s={timeout_s}")
print(f"exit_code={exit_code}")
print(f"observed={observed}")
print(f"expected={expected_spec}")
print(f"rows={len(rows)}")
if not os.path.exists(report_path):
    print(f"missing_report={report_path}")
for reason in reasons:
    print(reason)
PY

cat > "$OUT_DIR/runtime.txt" <<TXT
exit_code=$ec
duration_s=$duration_s
timeout_s=$TIMEOUT_S
max_steps=$MAX_STEPS
report_file=$(basename "$REPORT_CSV")
summary_file=$(basename "$SUMMARY_CSV")
log_file=$(basename "$RUN_LOG")
TXT

cat > "$OUT_DIR/manifest.json" <<JSON
{
  "mode": "strict-prefix-regression",
  "timeout_s": $TIMEOUT_S,
  "max_steps": $MAX_STEPS,
  "rts_cores": "$RTS_CORES",
  "require_success_through": $REQUIRE_SUCCESS_THROUGH,
  "expected_names": "$EXPECTED_NAMES",
  "command": "$RUN_DESC",
  "artifacts": {
    "report_csv": "$(basename "$REPORT_CSV")",
    "summary_csv": "$(basename "$SUMMARY_CSV")",
    "gate_txt": "prefix_gate.txt",
    "runtime_txt": "runtime.txt",
    "log_txt": "$(basename "$RUN_LOG")",
    "build_log_txt": "$(basename "$BUILD_LOG")"
  }
}
JSON

echo "Wrote strict prefix regression artifacts to: $OUT_DIR"
if [[ "$(head -n 1 "$OUT_DIR/prefix_gate.txt")" != "pass" ]]; then
  echo "Prefix gate failed (see $OUT_DIR/prefix_gate.txt)." >&2
  exit 1
fi
