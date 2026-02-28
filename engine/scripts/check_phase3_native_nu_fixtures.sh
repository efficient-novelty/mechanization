#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FIX_DIR="$ROOT_DIR/engine/testdata/phase3_native_nu"
FIX_JSON="$FIX_DIR/fixtures.json"
FIX_SHA="$FIX_DIR/fixtures.sha256"

[[ -f "$FIX_JSON" ]] || { echo "missing fixture file: $FIX_JSON" >&2; exit 1; }
[[ -f "$FIX_SHA" ]] || { echo "missing fixture hash file: $FIX_SHA" >&2; exit 1; }

expected_sha="$(tr -d '[:space:]' < "$FIX_SHA")"
actual_sha="$(sha256sum "$FIX_JSON" | awk '{print $1}')"

if [[ "$expected_sha" != "$actual_sha" ]]; then
  echo "fixture hash mismatch" >&2
  echo "  expected: $expected_sha" >&2
  echo "  actual:   $actual_sha" >&2
  exit 1
fi

python3 - "$FIX_JSON" <<'PY'
import json, sys
path = sys.argv[1]
with open(path, encoding='utf-8') as f:
    data = json.load(f)

if data.get('version') != 1:
    raise SystemExit('fixtures version must be 1')

eq_cases = data.get('equivalence_cases', [])
ctrl_cases = data.get('control_cases', [])
if len(eq_cases) < 2:
    raise SystemExit('need at least 2 equivalence cases')
if len(ctrl_cases) < 1:
    raise SystemExit('need at least 1 control case')

required_types = {'alpha_rename', 'canonical_rewrite'}
seen_types = {c.get('type') for c in eq_cases}
missing = sorted(required_types - seen_types)
if missing:
    raise SystemExit(f'missing equivalence case types: {missing}')

if not any(c.get('expect_equal') is False for c in ctrl_cases):
    raise SystemExit('need at least one control case with expect_equal=false')

print('phase3 native-nu fixture schema check: OK')
PY

echo "phase3 native-nu fixture hash check: OK"
