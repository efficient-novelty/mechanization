#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

mk_fixture() {
  local dir="$1"
  mkdir -p "$dir/ladder" "$dir/ladder-main" "$dir/prefix" "$dir/phase3/native_nu"
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
step,name,nu,kappa,rho,bar,delta,source,candidates,raw_candidates,canonical_candidates,dedupe_ratio,best_canonical_key,k_desugar,k_entry,k_bitcost,canonical_key,bit_kappa,ast_nodes,decoded_name?,decode_confidence,decode_ambiguity,decode_status
1,Universe,1,1,1.0000,0.5000,1,REF,1,1,1,1.0000,abc123,1,1,4,abc123,4,1,Universe,1.0000,,exact_isomorphism
TXT
  cat > "$dir/abinitio_mbtt_full.log" <<TXT
ok
TXT
  cat > "$dir/abinitio_mbtt_full.csv" <<TXT
step,name,nu,kappa,rho,bar,delta,source,candidates,raw_candidates,canonical_candidates,dedupe_ratio,best_canonical_key,k_desugar,k_entry,k_bitcost,canonical_key,bit_kappa,ast_nodes,decoded_name?,decode_confidence,decode_ambiguity,decode_status
1,Universe,1,1,1.0000,0.5000,1,REF,1,1,1,1.0000,abc123,1,1,4,abc123,4,1,Universe,1.0000,,exact_isomorphism
TXT
  cat > "$dir/phase1-shadow-ladder.log" <<TXT
ok
TXT
  cat > "$dir/phase1-shadow-ladder-main.log" <<TXT
ok
TXT
  cat > "$dir/prefix-regression.log" <<TXT
ok
TXT
  cat > "$dir/prefix/prefix_report.csv" <<TXT
step,status,bar,raw_candidates,viable_candidates,selected_name,selected_nu,selected_kappa,selected_rho,selected_source
1,selected,0.5000,10,10,Universe,1,2,0.5000,ENUM_MBTT
2,selected,0.5000,10,10,Unit,1,1,1.0000,ENUM_MBTT
3,selected,1.3333,8,4,Witness,2,1,2.0000,ENUM_MBTT
4,selected,1.5000,8,4,Pi,5,3,1.6667,AGENDA
5,selected,2.1429,8,4,S1,8,3,2.6667,AGENDA
6,selected,2.7200,8,4,Trunc,6,2,3.0000,AGENDA
7,selected,3.1146,8,4,S2,8,2,4.0000,AGENDA
TXT
  cat > "$dir/prefix/prefix_summary.csv" <<TXT
step,status,selected_name,selected_nu,selected_kappa,selected_rho,selected_source,bar,raw_candidates,viable_candidates
1,selected,Universe,1,2,0.5000,ENUM_MBTT,0.5000,10,10
2,selected,Unit,1,1,1.0000,ENUM_MBTT,0.5000,10,10
3,selected,Witness,2,1,2.0000,ENUM_MBTT,1.3333,8,4
4,selected,Pi,5,3,1.6667,AGENDA,1.5000,8,4
5,selected,S1,8,3,2.6667,AGENDA,2.1429,8,4
6,selected,Trunc,6,2,3.0000,AGENDA,2.7200,8,4
7,selected,S2,8,2,4.0000,AGENDA,3.1146,8,4
TXT
  cat > "$dir/prefix/prefix_gate.txt" <<TXT
pass
observed=1:Universe 2:Unit 3:Witness 4:Pi 5:S1 6:Trunc 7:S2
TXT
  cat > "$dir/prefix/runtime.txt" <<TXT
exit_code=0
duration_s=12
timeout_s=30
max_steps=7
TXT
  cat > "$dir/prefix/manifest.json" <<TXT
{"mode":"strict-prefix-regression"}
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
{"contract":"docs/phase1_evidence_contract.md","lanes":{"core":"cabal run acceptance-core","mbtt_fast":"cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates 50","mbtt_full":"cabal run acceptance-mbtt (main branch only)","abinitio_mbtt_shadow":"cabal run ab-initio -- --strict --phase1-shadow --mbtt-max-candidates 200 --csv abinitio_mbtt_shadow6.csv","abinitio_mbtt_full":"cabal run ab-initio -- --strict --mbtt-first --mbtt-max-candidates 200 --csv abinitio_mbtt_full.csv (main branch only)","mbtt_shadow_ladder":"TIMEOUT_S=45 MAX_CANDS=20 STEPS='1 2 3' engine/scripts/run_phase1_shadow_ladder.sh runs/phase1_ci/<run-id>/ladder","mbtt_shadow_ladder_main_gate":"TIMEOUT_S=90 MAX_CANDS=20 STEPS='1 2 3 4 5 6' REQUIRE_SUCCESS_THROUGH=6 engine/scripts/run_phase1_shadow_ladder.sh runs/phase1_ci/<run-id>/ladder-main (main branch only)","strict_prefix_regression":"TIMEOUT_S=30 MAX_STEPS=7 REQUIRE_SUCCESS_THROUGH=7 RTS_CORES=-N EXPECTED_NAMES='1:Universe 2:Unit 3:Witness 4:Pi 5:S1 6:Trunc 7:S2' bash engine/scripts/run_prefix_regression.sh runs/phase1_ci/<run-id>/prefix","phase3_native_nu_evidence":"STEPS='1 2 3 4 5 6' engine/scripts/run_phase3_native_nu_evidence.sh runs/phase1_ci/<run-id>/phase3/native_nu"}}
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
