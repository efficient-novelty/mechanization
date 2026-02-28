#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: $0 <run_dir> [main|pr]" >&2
  exit 2
fi

RUN_DIR="$1"
MODE="${2:-pr}"
MANIFEST="$RUN_DIR/manifest.json"

[[ -f "$MANIFEST" ]] || { echo "Missing manifest: $MANIFEST" >&2; exit 1; }

python - "$MANIFEST" "$MODE" <<'PY'
import json, sys
manifest_path, mode = sys.argv[1], sys.argv[2]
with open(manifest_path, 'r', encoding='utf-8') as f:
    data = json.load(f)

if data.get('contract') != 'docs/phase1_evidence_contract.md':
    raise SystemExit(f"invalid contract field in {manifest_path}")

lanes = data.get('lanes')
if not isinstance(lanes, dict):
    raise SystemExit(f"missing lanes object in {manifest_path}")

required = [
    'core',
    'mbtt_fast',
    'mbtt_full',
    'abinitio_mbtt_shadow',
    'abinitio_mbtt_full',
    'mbtt_shadow_ladder',
    'phase3_native_nu_evidence',
]
for k in required:
    v = lanes.get(k)
    if not isinstance(v, str) or not v.strip():
        raise SystemExit(f"missing/empty lane '{k}' in {manifest_path}")

if mode == 'main':
    v = lanes.get('mbtt_shadow_ladder_main_gate')
    if not isinstance(v, str) or not v.strip():
        raise SystemExit(f"missing/empty lane 'mbtt_shadow_ladder_main_gate' in {manifest_path}")

print(f"phase1 manifest schema check: OK ({mode})")
PY
