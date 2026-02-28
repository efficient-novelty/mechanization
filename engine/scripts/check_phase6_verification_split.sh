#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WF="$ROOT_DIR/.github/workflows/pen-engine.yml"
ROADMAP="$ROOT_DIR/MBTT_FIRST_AUTONOMOUS_SYNTHESIS_ROADMAP.md"
REPORT="$ROOT_DIR/docs/reports/p6_v4_verification_split_signoff.md"

[[ -f "$WF" ]] || { echo "missing workflow: $WF" >&2; exit 1; }
[[ -f "$ROADMAP" ]] || { echo "missing roadmap: $ROADMAP" >&2; exit 1; }
[[ -f "$REPORT" ]] || { echo "missing report: $REPORT" >&2; exit 1; }

python3 - "$WF" "$ROADMAP" "$REPORT" <<'PY'
import sys
wf = open(sys.argv[1], encoding='utf-8').read()
roadmap = open(sys.argv[2], encoding='utf-8').read()
report = open(sys.argv[3], encoding='utf-8').read()

required_wf = [
    'Check Phase-6 bridge schema payloads',
    'Check Phase-6 Agda harness contract',
    'Lane P6-V4 — agda-bridge determinism (required on PR/main)',
    'Check Phase-6 verification split sign-off',
]
for t in required_wf:
    if t not in wf:
        raise SystemExit(f'missing workflow step: {t}')

required_roadmap = [
    '- [x] **P6-WP4 — Phase sign-off and verification split audit**',
    '- [x] `cabal run agda-bridge -- --check` deterministic with new metadata.',
    '- [x] Agda test suite validates updated bridge schema.',
    '- [x] Discovery-vs-verification separation is explicit: Haskell proposes, Agda independently checks encoded claims.',
]
for t in required_roadmap:
    if t not in roadmap:
        raise SystemExit(f'missing roadmap marker: {t}')

required_report = [
    'Phase 6 V4 Verification Split Sign-off',
    'Haskell discovery engine proposes anonymous MBTT candidates',
    'Agda-facing bridge payload schema and harness validate exported claims',
    'P6-WP4 is complete',
]
for t in required_report:
    if t not in report:
        raise SystemExit(f'missing report marker: {t}')

print('phase6 verification split check: OK')
PY
