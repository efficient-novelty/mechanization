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
check_step_once "Check Phase-1 repository hygiene"
check_step_once "Check for unresolved merge markers in Phase-1 files"
check_step_once "Build evidence summary (PR/main)"
check_step_once "Verify evidence artifact contract (PR/main)"
check_step_once "Upload artifacts"

echo "phase1 workflow consistency check: OK"
