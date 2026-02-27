#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/phase1_shadow_ladder/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi

TIMEOUT_S="${TIMEOUT_S:-60}"
MAX_CANDS="${MAX_CANDS:-20}"
STEPS="${STEPS:-1 2 3 4 5 6}"

mkdir -p "$OUT_DIR"

pushd "$ENGINE_DIR" >/dev/null

echo "step,status,exit_code,csv_rows" > "$OUT_DIR/ladder_status.csv"

for s in $STEPS; do
  log="$OUT_DIR/step_${s}.log"
  csv="$OUT_DIR/step_${s}.csv"
  set +e
  timeout "$TIMEOUT_S" cabal run ab-initio -- \
    --structural \
    --phase1-shadow \
    --max-steps "$s" \
    --mbtt-max-candidates "$MAX_CANDS" \
    --csv "$(basename "$csv")" \
    > "$log" 2>&1
  ec=$?
  set -e

  if [[ -f "$(basename "$csv")" ]]; then
    mv "$(basename "$csv")" "$csv"
  fi

  if [[ $ec -eq 0 ]]; then
    rows=$(tail -n +2 "$csv" | wc -l | tr -d ' ')
    echo "$s,ok,$ec,$rows" >> "$OUT_DIR/ladder_status.csv"
  else
    rows=0
    [[ -f "$csv" ]] && rows=$(tail -n +2 "$csv" | wc -l | tr -d ' ')
    echo "$s,fail,$ec,$rows" >> "$OUT_DIR/ladder_status.csv"
  fi
done

cat > "$OUT_DIR/manifest.json" <<JSON
{
  "contract": "docs/phase1_evidence_contract.md",
  "mode": "phase1-shadow-ladder",
  "timeout_s": $TIMEOUT_S,
  "max_candidates": $MAX_CANDS,
  "steps": "$STEPS",
  "command_template": "cabal run ab-initio -- --structural --phase1-shadow --max-steps <N> --mbtt-max-candidates $MAX_CANDS --csv step_<N>.csv"
}
JSON

popd >/dev/null

echo "Wrote phase1 shadow ladder artifacts to: $OUT_DIR"
