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

[[ -d "$RUN_DIR" ]] || fail "run dir does not exist: $RUN_DIR"

require_file "$RUN_DIR/env.txt"
require_file "$RUN_DIR/acceptance-core.log"
require_file "$RUN_DIR/acceptance-mbtt-fast.log"
require_file "$RUN_DIR/abinitio_mbtt_shadow6.log"
require_file "$RUN_DIR/abinitio_mbtt_shadow6.csv"
require_file "$RUN_DIR/phase1-shadow-ladder.log"
require_file "$RUN_DIR/ladder/ladder_status.csv"
require_file "$RUN_DIR/manifest.json"

require_contains "$RUN_DIR/manifest.json" '"contract": "docs/phase1_evidence_contract.md"'
require_contains "$RUN_DIR/manifest.json" '"mbtt_shadow_ladder"'
require_contains "$RUN_DIR/ladder/ladder_status.csv" 'step,status,exit_code,csv_rows'

if [[ "$MODE" == "main" ]]; then
  require_file "$RUN_DIR/acceptance-mbtt-full.log"
  require_file "$RUN_DIR/abinitio_mbtt_structural.log"
  require_file "$RUN_DIR/abinitio_mbtt_structural.csv"
  require_file "$RUN_DIR/phase1-shadow-ladder-main.log"
  require_file "$RUN_DIR/ladder-main/ladder_status.csv"
  require_file "$RUN_DIR/ladder-main/ladder_gate.txt"
  require_contains "$RUN_DIR/ladder-main/ladder_gate.txt" 'pass'
  require_contains "$RUN_DIR/manifest.json" '"mbtt_shadow_ladder_main_gate"'
fi

echo "[phase1-evidence-check] OK: $RUN_DIR ($MODE)"
