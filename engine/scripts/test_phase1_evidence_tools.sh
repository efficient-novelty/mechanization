#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

mk_fixture() {
  local dir="$1"
  mkdir -p "$dir/ladder" "$dir/ladder-main"
  cat > "$dir/env.txt" <<TXT
mode=test
TXT
  cat > "$dir/acceptance-core.log" <<TXT
Results: 52 passed, 0 failed, 52 total
TXT
  cat > "$dir/acceptance-mbtt-fast.log" <<TXT
Results: 5 passed, 0 failed, 5 total
TXT
  cat > "$dir/acceptance-mbtt-full.log" <<TXT
Results: 5 passed, 0 failed, 5 total
TXT
  cat > "$dir/abinitio_mbtt_shadow6.log" <<TXT
ok
TXT
  cat > "$dir/abinitio_mbtt_shadow6.csv" <<TXT
col
1
TXT
  cat > "$dir/abinitio_mbtt_structural.log" <<TXT
ok
TXT
  cat > "$dir/abinitio_mbtt_structural.csv" <<TXT
col
1
TXT
  cat > "$dir/phase1-shadow-ladder.log" <<TXT
ok
TXT
  cat > "$dir/phase1-shadow-ladder-main.log" <<TXT
ok
TXT
  cat > "$dir/ladder/ladder_status.csv" <<TXT
step,status,exit_code,csv_rows
1,ok,0,1
TXT
  cat > "$dir/ladder-main/ladder_status.csv" <<TXT
step,status,exit_code,csv_rows
1,ok,0,1
TXT
  cat > "$dir/ladder-main/ladder_gate.txt" <<TXT
pass
TXT
  cat > "$dir/manifest.json" <<TXT
{"contract":"docs/phase1_evidence_contract.md","mbtt_shadow_ladder":"x","mbtt_shadow_ladder_main_gate":"y"}
TXT
}

FIXTURE="$WORK_DIR/fixture"
mk_fixture "$FIXTURE"

"$ROOT_DIR/engine/scripts/summarize_phase1_evidence.sh" "$FIXTURE" pr >/dev/null
"$ROOT_DIR/engine/scripts/verify_phase1_evidence.sh" "$FIXTURE" pr >/dev/null
"$ROOT_DIR/engine/scripts/summarize_phase1_evidence.sh" "$FIXTURE" main >/dev/null
"$ROOT_DIR/engine/scripts/verify_phase1_evidence.sh" "$FIXTURE" main >/dev/null

echo "phase1 evidence tools self-check: OK"
