#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/phase1_bundle/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi

MBTT_FAST_MAX_CANDS="${MBTT_FAST_MAX_CANDS:-50}"
SHADOW_MAX_CANDS="${SHADOW_MAX_CANDS:-20}"
SHADOW_MAX_STEPS="${SHADOW_MAX_STEPS:-6}"
LADDER_TIMEOUT_S="${LADDER_TIMEOUT_S:-45}"
LADDER_STEPS="${LADDER_STEPS:-1 2 3}"
RUN_MAIN_GATES="${RUN_MAIN_GATES:-0}"
MAIN_LADDER_TIMEOUT_S="${MAIN_LADDER_TIMEOUT_S:-90}"
MAIN_LADDER_MAX_CANDS="${MAIN_LADDER_MAX_CANDS:-20}"
MAIN_LADDER_STEPS="${MAIN_LADDER_STEPS:-1 2 3 4 5 6}"
MAIN_LADDER_GATE="${MAIN_LADDER_GATE:-6}"

mkdir -p "$OUT_DIR"

status_file="$OUT_DIR/lane_status.csv"
echo "lane,status,exit_code" > "$status_file"

run_lane() {
  local lane="$1"
  local logfile="$2"
  shift 2

  set +e
  "$@" > "$logfile" 2>&1
  local ec=$?
  set -e

  if [[ $ec -eq 0 ]]; then
    echo "$lane,ok,$ec" >> "$status_file"
  else
    echo "$lane,fail,$ec" >> "$status_file"
  fi
  return $ec
}

failed=0
pushd "$ENGINE_DIR" >/dev/null

run_lane "acceptance_core" "$OUT_DIR/acceptance-core.log" \
  cabal run acceptance-core || failed=1

run_lane "acceptance_mbtt_fast" "$OUT_DIR/acceptance-mbtt-fast.log" \
  cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates "$MBTT_FAST_MAX_CANDS" || failed=1

run_lane "abinitio_shadow" "$OUT_DIR/abinitio-shadow.log" \
  cabal run ab-initio -- --structural --phase1-shadow --max-steps "$SHADOW_MAX_STEPS" --mbtt-max-candidates "$SHADOW_MAX_CANDS" --csv phase1_bundle_shadow.csv || failed=1
if [[ -f phase1_bundle_shadow.csv ]]; then
  mv phase1_bundle_shadow.csv "$OUT_DIR/abinitio-shadow.csv"
fi

set +e
TIMEOUT_S="$LADDER_TIMEOUT_S" MAX_CANDS="$SHADOW_MAX_CANDS" STEPS="$LADDER_STEPS" \
  "$ROOT_DIR/engine/scripts/run_phase1_shadow_ladder.sh" "$OUT_DIR/ladder" \
  > "$OUT_DIR/phase1-shadow-ladder.log" 2>&1
ec=$?
set -e
if [[ $ec -eq 0 ]]; then
  echo "shadow_ladder,ok,$ec" >> "$status_file"
else
  echo "shadow_ladder,fail,$ec" >> "$status_file"
  failed=1
fi

if [[ "$RUN_MAIN_GATES" == "1" ]]; then
  run_lane "acceptance_mbtt_full" "$OUT_DIR/acceptance-mbtt-full.log" \
    cabal run acceptance-mbtt || failed=1

  run_lane "abinitio_mbtt_full" "$OUT_DIR/abinitio-mbtt-full.log" \
    cabal run ab-initio -- --structural --mbtt-first --mbtt-max-candidates "$MAIN_LADDER_MAX_CANDS" --csv phase1_bundle_mbtt_full.csv || failed=1
  if [[ -f phase1_bundle_mbtt_full.csv ]]; then
    mv phase1_bundle_mbtt_full.csv "$OUT_DIR/abinitio-mbtt-full.csv"
  fi

  set +e
  TIMEOUT_S="$MAIN_LADDER_TIMEOUT_S" MAX_CANDS="$MAIN_LADDER_MAX_CANDS" STEPS="$MAIN_LADDER_STEPS" REQUIRE_SUCCESS_THROUGH="$MAIN_LADDER_GATE" \
    "$ROOT_DIR/engine/scripts/run_phase1_shadow_ladder.sh" "$OUT_DIR/ladder-main" \
    > "$OUT_DIR/phase1-shadow-ladder-main.log" 2>&1
  ec=$?
  set -e
  if [[ $ec -eq 0 ]]; then
    echo "shadow_ladder_main_gate,ok,$ec" >> "$status_file"
  else
    echo "shadow_ladder_main_gate,fail,$ec" >> "$status_file"
    failed=1
  fi
fi

cat > "$OUT_DIR/manifest.json" <<JSON
{
  "contract": "docs/phase1_evidence_contract.md",
  "mode": "phase1-evidence-bundle",
  "run_main_gates": $RUN_MAIN_GATES,
  "mbtt_fast_max_candidates": $MBTT_FAST_MAX_CANDS,
  "shadow": {
    "max_steps": $SHADOW_MAX_STEPS,
    "max_candidates": $SHADOW_MAX_CANDS
  },
  "ladder": {
    "timeout_s": $LADDER_TIMEOUT_S,
    "steps": "$LADDER_STEPS"
  },
  "main_ladder_gate": {
    "timeout_s": $MAIN_LADDER_TIMEOUT_S,
    "max_candidates": $MAIN_LADDER_MAX_CANDS,
    "steps": "$MAIN_LADDER_STEPS",
    "require_success_through": $MAIN_LADDER_GATE
  }
}
JSON

popd >/dev/null

if [[ $failed -ne 0 ]]; then
  echo "One or more Phase 1 bundle lanes failed. See $status_file" >&2
  exit 1
fi

echo "Wrote phase1 evidence bundle to: $OUT_DIR"
