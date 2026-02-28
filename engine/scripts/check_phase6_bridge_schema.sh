#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
AGDA_EXPORT="$ROOT_DIR/engine/src/AgdaExport.hs"
RUN_BRIDGE="$ROOT_DIR/engine/src/RunAgdaBridge.hs"

[[ -f "$AGDA_EXPORT" ]] || { echo "missing file: $AGDA_EXPORT" >&2; exit 1; }
[[ -f "$RUN_BRIDGE" ]] || { echo "missing file: $RUN_BRIDGE" >&2; exit 1; }

python3 - "$AGDA_EXPORT" "$RUN_BRIDGE" <<'PY'
import sys
agda_export = open(sys.argv[1], encoding='utf-8').read()
run_bridge = open(sys.argv[2], encoding='utf-8').read()

required_export_tokens = [
    'exportAllVerificationPayloads',
    'canonical_key',
    'kappa_bit',
    'kappa_desugared',
    'anonymous_ast',
    'nu_claim',
    'nu_total',
]
for t in required_export_tokens:
    if t not in agda_export:
        raise SystemExit(f'missing token in AgdaExport.hs: {t}')

required_bridge_tokens = [
    'exportAllVerificationPayloads',
    'exportAllSteps ++ exportAllVerificationPayloads',
]
for t in required_bridge_tokens:
    if t not in run_bridge:
        raise SystemExit(f'missing token in RunAgdaBridge.hs: {t}')

print('phase6 bridge schema check: OK')
PY
