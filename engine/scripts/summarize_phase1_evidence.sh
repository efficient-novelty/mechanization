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

extract_canonical_telemetry() {
  local csv="$1"
  if [[ -f "$csv" ]]; then
    python3 - "$csv" <<'PY2'
import csv, sys
path = sys.argv[1]
with open(path, newline='', encoding='utf-8') as f:
    rows = list(csv.DictReader(f))
if not rows:
    print('raw=n/a canonical=n/a dedupe=n/a best_key=n/a')
    raise SystemExit(0)
row = rows[-1]
print(f"raw={row.get('raw_candidates','n/a')} canonical={row.get('canonical_candidates','n/a')} dedupe={row.get('dedupe_ratio','n/a')} best_key={row.get('best_canonical_key','n/a')}")
PY2
  else
    echo "raw=n/a canonical=n/a dedupe=n/a best_key=n/a"
  fi
}


extract_p4_kappa_telemetry() {
  local csv="$1"
  if [[ -f "$csv" ]]; then
    python3 - "$csv" <<'PY3'
import csv, sys
path = sys.argv[1]
with open(path, newline='', encoding='utf-8') as f:
    rows = list(csv.DictReader(f))
if not rows:
    print('bit_kappa=n/a ast_nodes=n/a canonical_key=n/a decoded_name=n/a decode_confidence=n/a decode_status=n/a')
    raise SystemExit(0)
row = rows[-1]
print(f"bit_kappa={row.get('bit_kappa','n/a')} ast_nodes={row.get('ast_nodes','n/a')} canonical_key={row.get('canonical_key','n/a')} decoded_name={row.get('decoded_name?','n/a')} decode_confidence={row.get('decode_confidence','n/a')} decode_status={row.get('decode_status','n/a')}")
PY3
  else
    echo "bit_kappa=n/a ast_nodes=n/a canonical_key=n/a decoded_name=n/a decode_confidence=n/a decode_status=n/a"
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

shadow_canon="$(extract_canonical_telemetry "$RUN_DIR/abinitio_mbtt_shadow6.csv")"
full_canon="$(extract_canonical_telemetry "$RUN_DIR/abinitio_mbtt_structural.csv")"
shadow_p4="$(extract_p4_kappa_telemetry "$RUN_DIR/abinitio_mbtt_shadow6.csv")"
full_p4="$(extract_p4_kappa_telemetry "$RUN_DIR/abinitio_mbtt_structural.csv")"

ladder_status="$(ladder_tail "$RUN_DIR/ladder/ladder_status.csv")"
ladder_main_status="$(ladder_tail "$RUN_DIR/ladder-main/ladder_status.csv")"
ladder_gate="n/a"
if [[ -f "$RUN_DIR/ladder-main/ladder_gate.txt" ]]; then
  ladder_gate="$(tr -d '\n' < "$RUN_DIR/ladder-main/ladder_gate.txt")"
fi

phase3_report_status="n/a"
if [[ -f "$RUN_DIR/phase3/native_nu/report.md" ]]; then
  phase3_report_status="$(grep -E '^- status:' "$RUN_DIR/phase3/native_nu/report.md" | head -n 1 | sed 's/^- status:[[:space:]]*//')"
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
- shadow_canonical_telemetry: $shadow_canon
- shadow_kappa_telemetry: $shadow_p4
MD

if [[ "$MODE" == "main" ]]; then
  cat >> "$OUT" <<MD
- full_csv_rows: $full_rows
- full_canonical_telemetry: $full_canon
- full_kappa_telemetry: $full_p4
MD
fi

cat >> "$OUT" <<MD

## Ladder telemetry
- ladder_status_rows: ${ladder_status:-"(none)"}

## Phase 3 native-ν lane
- native_nu_report_status: ${phase3_report_status:-"(unknown)"}
MD

if [[ "$MODE" == "main" ]]; then
  cat >> "$OUT" <<MD
- ladder_main_status_rows: ${ladder_main_status:-"(none)"}
- ladder_main_gate: $ladder_gate
MD
fi

echo "Wrote summary: $OUT"
