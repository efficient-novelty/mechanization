#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SRC="$ROOT_DIR/engine/src/RunAbInitio.hs"

[[ -f "$SRC" ]] || { echo "missing source file: $SRC" >&2; exit 1; }

python3 - "$SRC" <<'PY'
import re, sys
path = sys.argv[1]
text = open(path, encoding='utf-8').read()

if 'import MBTTDecode' not in text:
    raise SystemExit('MBTTDecode import missing in RunAbInitio.hs')

marker = '-- ============================================\n-- CSV Output\n-- ============================================'
idx_marker = text.find(marker)
if idx_marker < 0:
    raise SystemExit('CSV output marker not found')

needle_occ = [m.start() for m in re.finditer(r'decodeCanonicalNameWithKey\s*\(', text)]
if len(needle_occ) != 1:
    raise SystemExit(f'expected exactly one decodeCanonicalNameWithKey call, found {len(needle_occ)}')
if needle_occ[0] < idx_marker:
    raise SystemExit('decodeCanonicalNameWithKey used before CSV output section (may interfere with selection)')

status_occ = [m.start() for m in re.finditer(r'\bdecodeStatus\b', text)]
if len(status_occ) < 2:
    raise SystemExit('decodeStatus definition/use not found as expected')
if any(pos < idx_marker for pos in status_occ):
    raise SystemExit('decodeStatus referenced before CSV output section')

sel_block = text[text.find('selectBest cs'):text.find('knownNames :: [String]')]
if 'decode' in sel_block.lower():
    raise SystemExit('decode-related identifier appears in selection block')

print('phase5 decode non-interference check: OK')
PY
