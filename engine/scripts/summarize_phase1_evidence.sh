#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <run_dir> [main|pr]" >&2
  exit 2
fi

RUN_DIR="$1"
MODE="${2:-pr}"
OUT="$RUN_DIR/summary.md"

[[ -d "$RUN_DIR" ]] || { echo "run dir missing: $RUN_DIR" >&2; exit 1; }

extract_result_line() {
  local log="$1"
  if [[ -f "$log" ]]; then
    grep -E 'Results:' "$log" | tail -n 1 || true
  fi
}

ladder_tail() {
  local csv="$1"
  if [[ -f "$csv" ]]; then
    tail -n +2 "$csv" | tr '\n' ';' | sed 's/;$/\n/'
  fi
}

core_result="$(extract_result_line "$RUN_DIR/acceptance-core.log")"
mbtt_fast_result="$(extract_result_line "$RUN_DIR/acceptance-mbtt-fast.log")"
mbtt_full_result="$(extract_result_line "$RUN_DIR/acceptance-mbtt-full.log")"

shadow_rows=0
if [[ -f "$RUN_DIR/abinitio_mbtt_shadow6.csv" ]]; then
  shadow_rows=$(tail -n +2 "$RUN_DIR/abinitio_mbtt_shadow6.csv" | wc -l | tr -d ' ')
fi

full_rows=0
if [[ -f "$RUN_DIR/abinitio_mbtt_structural.csv" ]]; then
  full_rows=$(tail -n +2 "$RUN_DIR/abinitio_mbtt_structural.csv" | wc -l | tr -d ' ')
fi

ladder_status="$(ladder_tail "$RUN_DIR/ladder/ladder_status.csv")"
ladder_main_status="$(ladder_tail "$RUN_DIR/ladder-main/ladder_status.csv")"
ladder_gate="n/a"
if [[ -f "$RUN_DIR/ladder-main/ladder_gate.txt" ]]; then
  ladder_gate="$(tr -d '\n' < "$RUN_DIR/ladder-main/ladder_gate.txt")"
fi

cat > "$OUT" <<MD
# Phase 1 Evidence Summary

- mode: $MODE
- run_dir: $RUN_DIR

## Acceptance lanes
- core: ${core_result:-"(result line not found)"}
- mbtt_fast: ${mbtt_fast_result:-"(result line not found)"}
MD

if [[ "$MODE" == "main" ]]; then
  cat >> "$OUT" <<MD
- mbtt_full: ${mbtt_full_result:-"(result line not found)"}
MD
fi

cat >> "$OUT" <<MD

## Ab-initio lanes
- shadow_csv_rows: $shadow_rows
MD

if [[ "$MODE" == "main" ]]; then
  cat >> "$OUT" <<MD
- full_csv_rows: $full_rows
MD
fi

cat >> "$OUT" <<MD

## Ladder telemetry
- ladder_status_rows: ${ladder_status:-"(none)"}
MD

if [[ "$MODE" == "main" ]]; then
  cat >> "$OUT" <<MD
- ladder_main_status_rows: ${ladder_main_status:-"(none)"}
- ladder_main_gate: $ladder_gate
MD
fi

echo "Wrote summary: $OUT"
