#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TARGETS=(
  ".github/workflows/pen-engine.yml"
  "docs/phase1_evidence_contract.md"
  "MBTT_FIRST_AUTONOMOUS_SYNTHESIS_ROADMAP.md"
  "engine/scripts/run_phase1_shadow.sh"
  "engine/scripts/run_phase1_shadow_ladder.sh"
  "engine/scripts/run_phase1_evidence_bundle.sh"
  "engine/scripts/summarize_phase1_evidence.sh"
  "engine/scripts/verify_phase1_evidence.sh"
  "engine/scripts/test_phase1_evidence_tools.sh"
)

for f in "${TARGETS[@]}"; do
  [[ -f "$f" ]] || continue
  if rg -n '^(<<<<<<<|=======|>>>>>>>)' "$f" >/dev/null; then
    echo "Conflict markers found in $f" >&2
    rg -n '^(<<<<<<<|=======|>>>>>>>)' "$f" >&2
    exit 1
  fi
done

echo "phase1 conflict marker check: OK"
