#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ROADMAP="$ROOT_DIR/MBTT_FIRST_AUTONOMOUS_SYNTHESIS_ROADMAP.md"
REPORT="$ROOT_DIR/docs/reports/p7_v4_pruning_signoff.md"

[[ -f "$ROADMAP" ]] || { echo "missing roadmap: $ROADMAP" >&2; exit 1; }
[[ -f "$REPORT" ]] || { echo "missing report: $REPORT" >&2; exit 1; }

python3 - "$ROADMAP" "$REPORT" <<'PY'
import sys
roadmap = open(sys.argv[1], encoding='utf-8').read()
report = open(sys.argv[2], encoding='utf-8').read()

required_roadmap = [
    '## Phase 7 — Migration, Hardening, and Cleanup (Weeks 12–14) ✅ COMPLETE',
    '- [x] **P7-V3 — Search-space reduction evidence publication**',
    '- [x] **P7-WP4 — Search-space reduction telemetry + phase sign-off**',
]
for token in required_roadmap:
    if token not in roadmap:
        raise SystemExit(f'missing roadmap token: {token}')

required_report = [
    'Phase 7 V4 Pruning Sign-off',
    'typed validity rate',
    'compositional narrowing',
    'P7-WP4 is complete',
]
for token in required_report:
    if token not in report:
        raise SystemExit(f'missing report token: {token}')

print('phase7 pruning sign-off check: OK')
PY
