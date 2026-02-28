#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WF="$ROOT_DIR/.github/workflows/pen-engine.yml"

[[ -f "$WF" ]] || { echo "Missing workflow: $WF" >&2; exit 1; }

check_step_once() {
  local step="$1"
  local count
  count=$(grep -F -- "- name: ${step}" "$WF" | wc -l | tr -d ' ')
  if [[ "$count" -ne 1 ]]; then
    echo "Workflow consistency failure: expected step '${step}' exactly once, found $count" >&2
    exit 1
  fi
}

check_step_once "Self-check evidence tooling scripts"
check_step_once "Check Phase-5 decoder fixtures"
check_step_once "Check Phase-5 decode non-interference"
check_step_once "Check Phase-6 bridge schema payloads"
check_step_once "Check Phase-6 Agda harness contract"
check_step_once "Lane P6-V4 — agda-bridge determinism (required on PR/main)"
check_step_once "Check Phase-6 verification split sign-off"
check_step_once "Check Phase-7 default mode migration contract"
check_step_once "Check Phase-7 CI migration + rollback lanes"
check_step_once "Lane D1-rollback — legacy-generator shadow smoke (required on PR/main)"
check_step_once "Check Phase-1 repository hygiene"
check_step_once "Check for unresolved merge markers in Phase-1 files"
check_step_once "Build evidence summary (PR/main)"
check_step_once "Verify evidence artifact contract (PR/main)"
check_step_once "Lane P2-V4 — canonical differential gate (required on PR/main)"
check_step_once "Lane P2-V5 — canonical quality parity gate (required on PR/main)"
check_step_once "Lane P3-V5 — native-nu bounded evidence lane (required on PR/main)"
check_step_once "Publish Phase-2/P3 gate reports in job output"
check_step_once "Upload artifacts"

echo "phase1/phase2 workflow consistency check: OK"
