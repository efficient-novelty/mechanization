#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <run_dir> [main|pr]" >&2
  exit 2
fi

RUN_DIR="$1"
MODE="${2:-pr}"

fail() {
  echo "[phase1-evidence-check] ERROR: $*" >&2
  exit 1
}

require_file() {
  local f="$1"
  [[ -f "$f" ]] || fail "missing required file: $f"
}

require_contains() {
  local f="$1"
  local needle="$2"
  grep -Fq "$needle" "$f" || fail "expected '$needle' in $f"
}

require_regex() {
  local f="$1"
  local pattern="$2"
  grep -Eq "$pattern" "$f" || fail "expected pattern /$pattern/ in $f"
}

[[ -d "$RUN_DIR" ]] || fail "run dir does not exist: $RUN_DIR"

require_zero_failed() {
  local log="$1"
  local label="$2"
  local line
  line=$(grep -E 'Results:' "$log" | tail -n 1 || true)
  [[ -n "$line" ]] || fail "missing Results line in $label log: $log"
  echo "$line" | grep -Eq 'failed,[[:space:]]*0 failed|0 failed' || fail "$label reports failures: $line"
}

require_file "$RUN_DIR/env.txt"
require_file "$RUN_DIR/acceptance-core.log"
require_file "$RUN_DIR/acceptance-mbtt-fast.log"
require_file "$RUN_DIR/abinitio_mbtt_shadow6.log"
require_file "$RUN_DIR/abinitio_mbtt_shadow6.csv"
require_file "$RUN_DIR/phase1-shadow-ladder.log"
require_file "$RUN_DIR/ladder/ladder_status.csv"
require_file "$RUN_DIR/manifest.json"
require_file "$RUN_DIR/summary.md"

require_regex "$RUN_DIR/manifest.json" '"contract"[[:space:]]*:[[:space:]]*"docs/phase1_evidence_contract.md"'
require_regex "$RUN_DIR/manifest.json" '"mbtt_shadow_ladder"'
require_contains "$RUN_DIR/ladder/ladder_status.csv" 'step,status,exit_code,csv_rows'
require_contains "$RUN_DIR/summary.md" 'Phase 1 Evidence Summary'
require_regex "$RUN_DIR/summary.md" 'core:[[:space:]]+Results:'
require_regex "$RUN_DIR/summary.md" 'mbtt_fast:[[:space:]]+Results:'

require_zero_failed "$RUN_DIR/acceptance-core.log" "acceptance-core"
require_zero_failed "$RUN_DIR/acceptance-mbtt-fast.log" "acceptance-mbtt-fast"

shadow_rows=$(tail -n +2 "$RUN_DIR/abinitio_mbtt_shadow6.csv" | wc -l | tr -d ' ' )
[[ "$shadow_rows" -ge 1 ]] || fail "expected abinitio_mbtt_shadow6.csv to contain at least one row"

if [[ "$MODE" == "main" ]]; then
  require_file "$RUN_DIR/acceptance-mbtt-full.log"
  require_regex "$RUN_DIR/summary.md" 'mbtt_full:[[:space:]]+Results:'
  require_file "$RUN_DIR/abinitio_mbtt_structural.log"
  require_file "$RUN_DIR/abinitio_mbtt_structural.csv"
  require_zero_failed "$RUN_DIR/acceptance-mbtt-full.log" "acceptance-mbtt-full"
  full_rows=$(tail -n +2 "$RUN_DIR/abinitio_mbtt_structural.csv" | wc -l | tr -d ' ' )
  [[ "$full_rows" -ge 1 ]] || fail "expected abinitio_mbtt_structural.csv to contain at least one row"
  require_file "$RUN_DIR/phase1-shadow-ladder-main.log"
  require_file "$RUN_DIR/ladder-main/ladder_status.csv"
  require_file "$RUN_DIR/ladder-main/ladder_gate.txt"
  require_contains "$RUN_DIR/ladder-main/ladder_gate.txt" 'pass'
  require_regex "$RUN_DIR/manifest.json" '"mbtt_shadow_ladder_main_gate"'
fi

echo "[phase1-evidence-check] OK: $RUN_DIR ($MODE)"
