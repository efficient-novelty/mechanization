#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"

STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/phase1_shadow/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi
MAX_STEPS="${MAX_STEPS:-1}"
MAX_CANDS="${MAX_CANDS:-5}"

mkdir -p "$OUT_DIR"


extract_canonical_json() {
  local csv="$1"
  python3 - "$csv" <<'PY2'
import csv, json, sys
path = sys.argv[1]
with open(path, newline='', encoding='utf-8') as f:
    rows = list(csv.DictReader(f))
if not rows:
    print(json.dumps({"raw_candidates": None, "canonical_candidates": None, "dedupe_ratio": None, "best_canonical_key": None}))
    raise SystemExit(0)
row = rows[-1]
out = {
  "raw_candidates": row.get("raw_candidates"),
  "canonical_candidates": row.get("canonical_candidates"),
  "dedupe_ratio": row.get("dedupe_ratio"),
  "best_canonical_key": row.get("best_canonical_key"),
}
print(json.dumps(out))
PY2
}

pushd "$ENGINE_DIR" >/dev/null

cabal run acceptance-core > "$OUT_DIR/acceptance-core.log" 2>&1

# Bounded MBTT shadow replay preset
cabal run ab-initio -- \
  --structural \
  --phase1-shadow \
  --max-steps "$MAX_STEPS" \
  --mbtt-max-candidates "$MAX_CANDS" \
  --csv phase1_shadow.csv \
  > "$OUT_DIR/abinitio-shadow.log" 2>&1

mv phase1_shadow.csv "$OUT_DIR/phase1_shadow.csv"
shadow_canon_json="$(extract_canonical_json "$OUT_DIR/phase1_shadow.csv")"

cat > "$OUT_DIR/manifest.json" <<JSON
{
  "contract": "docs/phase1_evidence_contract.md",
  "mode": "phase1-shadow-local",
  "max_steps": $MAX_STEPS,
  "max_candidates": $MAX_CANDS,
  "commands": {
    "core": "cabal run acceptance-core",
    "shadow": "cabal run ab-initio -- --structural --phase1-shadow --max-steps $MAX_STEPS --mbtt-max-candidates $MAX_CANDS --csv phase1_shadow.csv"
  },
  "canonical_telemetry": $shadow_canon_json
}
JSON

popd >/dev/null

echo "Wrote phase1 shadow artifacts to: $OUT_DIR"
