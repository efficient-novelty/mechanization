#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WF="$ROOT_DIR/.github/workflows/pen-engine.yml"
ROADMAP="$ROOT_DIR/MBTT_FIRST_AUTONOMOUS_SYNTHESIS_ROADMAP.md"
REPORT="$ROOT_DIR/docs/reports/p7_v3_ci_migration_report.md"

[[ -f "$WF" ]] || { echo "missing workflow: $WF" >&2; exit 1; }
[[ -f "$ROADMAP" ]] || { echo "missing roadmap: $ROADMAP" >&2; exit 1; }
[[ -f "$REPORT" ]] || { echo "missing report: $REPORT" >&2; exit 1; }

python3 - "$WF" "$ROADMAP" "$REPORT" <<'PY'
import sys
wf = open(sys.argv[1], encoding='utf-8').read()
roadmap = open(sys.argv[2], encoding='utf-8').read()
report = open(sys.argv[3], encoding='utf-8').read()

required_wf = [
    'Check Phase-7 CI migration + rollback lanes',
    'Lane D1-rollback — legacy-generator shadow smoke (required on PR/main)',
    '--legacy-generator --phase1-shadow',
    'abinitio_legacy_shadow',
]
for t in required_wf:
    if t not in wf:
        raise SystemExit(f'missing workflow token: {t}')

required_roadmap = [
    '- [x] **P7-WP3 — CI/acceptance migration and rollback checks**',
    'docs/reports/p7_v3_ci_migration_report.md',
]
for t in required_roadmap:
    if t not in roadmap:
        raise SystemExit(f'missing roadmap token: {t}')

required_report = [
    'Phase 7 V3 CI Migration Report',
    'legacy-generator shadow smoke',
    'P7-WP3 is complete',
]
for t in required_report:
    if t not in report:
        raise SystemExit(f'missing report token: {t}')

print('phase7 ci migration check: OK')
PY
