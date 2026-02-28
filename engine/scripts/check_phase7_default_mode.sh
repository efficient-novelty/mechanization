#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ABINITIO="$ROOT_DIR/engine/src/RunAbInitio.hs"
ROADMAP="$ROOT_DIR/MBTT_FIRST_AUTONOMOUS_SYNTHESIS_ROADMAP.md"
REPORT="$ROOT_DIR/docs/reports/p7_v2_default_flip_report.md"

[[ -f "$ABINITIO" ]] || { echo "missing file: $ABINITIO" >&2; exit 1; }
[[ -f "$ROADMAP" ]] || { echo "missing file: $ROADMAP" >&2; exit 1; }
[[ -f "$REPORT" ]] || { echo "missing file: $REPORT" >&2; exit 1; }

python3 - "$ABINITIO" "$ROADMAP" "$REPORT" <<'PY'
import sys
abinitio = open(sys.argv[1], encoding='utf-8').read()
roadmap = open(sys.argv[2], encoding='utf-8').read()
report = open(sys.argv[3], encoding='utf-8').read()

required_abinitio = [
    'cfgLegacyGenerator',
    'legacyGenerator = "--legacy-generator" `elem` args',
    'mbttFirstFinal = phase1Shadow || mbttFirstFlag || not legacyGenerator',
    'WARNING: --legacy-generator is deprecated in Phase 7',
]
for t in required_abinitio:
    if t not in abinitio:
        raise SystemExit(f'missing RunAbInitio token: {t}')

required_roadmap = [
    '- [x] **P7-WP2 — Default-path flip + legacy fallback guardrails**',
    'docs/reports/p7_v2_default_flip_report.md',
]
for t in required_roadmap:
    if t not in roadmap:
        raise SystemExit(f'missing roadmap token: {t}')

required_report = [
    'Phase 7 V2 Default Flip Report',
    '--legacy-generator',
    'P7-WP2 is complete',
]
for t in required_report:
    if t not in report:
        raise SystemExit(f'missing report token: {t}')

print('phase7 default-mode check: OK')
PY
