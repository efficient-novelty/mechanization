#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

mk_fixture() {
  local dir="$1"
  mkdir -p "$dir/ladder" "$dir/ladder-main" "$dir/phase3/native_nu"
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
step,name,nu,kappa,rho,bar,delta,source,candidates,raw_candidates,canonical_candidates,dedupe_ratio,best_canonical_key,k_desugar,k_entry,k_bitcost,canonical_key,bit_kappa,ast_nodes,decoded_name?
1,Universe,1,1,1.0000,0.5000,1,REF,1,1,1,1.0000,abc123,1,1,4,abc123,4,1,Universe
TXT
  cat > "$dir/abinitio_mbtt_structural.log" <<TXT
ok
TXT
  cat > "$dir/abinitio_mbtt_structural.csv" <<TXT
step,name,nu,kappa,rho,bar,delta,source,candidates,raw_candidates,canonical_candidates,dedupe_ratio,best_canonical_key,k_desugar,k_entry,k_bitcost,canonical_key,bit_kappa,ast_nodes,decoded_name?
1,Universe,1,1,1.0000,0.5000,1,REF,1,1,1,1.0000,abc123,1,1,4,abc123,4,1,Universe
TXT
  cat > "$dir/phase1-shadow-ladder.log" <<TXT
ok
TXT
  cat > "$dir/phase1-shadow-ladder-main.log" <<TXT
ok
TXT
  cat > "$dir/phase3-native-nu.log" <<TXT
ok
TXT
  cat > "$dir/phase3/native_nu/native_nu_trace_summary.csv" <<TXT
step,name,nu_total,node_trace_count,trace_line_count,required_keys
1,Universe,1,1,10,true
TXT
  cat > "$dir/phase3/native_nu/report.md" <<TXT
# Phase 3 Native Nu Evidence Report

- status: pass
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
{"contract":"docs/phase1_evidence_contract.md","lanes":{"core":"cabal run acceptance-core","mbtt_fast":"cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates 50","mbtt_full":"cabal run acceptance-mbtt (main branch only)","abinitio_mbtt_shadow":"cabal run ab-initio -- --structural --phase1-shadow --mbtt-max-candidates 200 --csv abinitio_mbtt_shadow6.csv","abinitio_mbtt_full":"cabal run ab-initio -- --structural --mbtt-first --mbtt-max-candidates 200 --csv abinitio_mbtt_structural.csv (main branch only)","mbtt_shadow_ladder":"TIMEOUT_S=45 MAX_CANDS=20 STEPS='1 2 3' engine/scripts/run_phase1_shadow_ladder.sh runs/phase1_ci/<run-id>/ladder","mbtt_shadow_ladder_main_gate":"TIMEOUT_S=90 MAX_CANDS=20 STEPS='1 2 3 4 5 6' REQUIRE_SUCCESS_THROUGH=6 engine/scripts/run_phase1_shadow_ladder.sh runs/phase1_ci/<run-id>/ladder-main (main branch only)","phase3_native_nu_evidence":"STEPS='1 2 3 4 5 6' engine/scripts/run_phase3_native_nu_evidence.sh runs/phase1_ci/<run-id>/phase3/native_nu"}}
TXT
}

FIXTURE="$WORK_DIR/fixture"
mk_fixture "$FIXTURE"

"$ROOT_DIR/engine/scripts/check_phase1_manifest_schema.sh" "$FIXTURE" pr >/dev/null
"$ROOT_DIR/engine/scripts/summarize_phase1_evidence.sh" "$FIXTURE" pr >/dev/null
"$ROOT_DIR/engine/scripts/verify_phase1_evidence.sh" "$FIXTURE" pr >/dev/null
"$ROOT_DIR/engine/scripts/check_phase1_manifest_schema.sh" "$FIXTURE" main >/dev/null
"$ROOT_DIR/engine/scripts/summarize_phase1_evidence.sh" "$FIXTURE" main >/dev/null
"$ROOT_DIR/engine/scripts/verify_phase1_evidence.sh" "$FIXTURE" main >/dev/null

echo "phase1 evidence tools self-check: OK"
