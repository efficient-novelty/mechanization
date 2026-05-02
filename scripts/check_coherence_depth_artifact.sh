#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
AGDA_DIR="$ROOT/agda"

to_windows_path() {
  local path="$1"
  if command -v cygpath >/dev/null 2>&1; then
    cygpath -w "$path"
  elif [[ "$path" =~ ^/mnt/([a-zA-Z])/(.*)$ ]]; then
    local drive="${BASH_REMATCH[1]}"
    local rest="${BASH_REMATCH[2]//\//\\}"
    printf '%s:\\%s\n' "${drive^^}" "$rest"
  elif [[ "$path" =~ ^/([a-zA-Z])/(.*)$ ]]; then
    local drive="${BASH_REMATCH[1]}"
    local rest="${BASH_REMATCH[2]//\//\\}"
    printf '%s:\\%s\n' "${drive^^}" "$rest"
  else
    printf '%s\n' "$path"
  fi
}

run_agda() {
  local target="$1"
  echo "==> agda --transliterate $target"
  if command -v agda >/dev/null 2>&1; then
    (cd "$AGDA_DIR" && agda --transliterate "$target")
  elif command -v powershell.exe >/dev/null 2>&1; then
    local win_agda_dir
    win_agda_dir="$(to_windows_path "$AGDA_DIR")"
    powershell.exe -NoProfile -ExecutionPolicy Bypass -Command \
      "Set-Location -LiteralPath '$win_agda_dir'; agda --transliterate '$target'"
  else
    echo "ERROR: agda not found on PATH and powershell.exe fallback unavailable" >&2
    return 127
  fi
}

require_no_postulates() {
  local found=0
  local paths=(
    "$AGDA_DIR/Core"
    "$AGDA_DIR/Metatheory"
    "$AGDA_DIR/Geometry/Clutching.agda"
    "$AGDA_DIR/ObligationGraph"
    "$AGDA_DIR/PEN.agda"
    "$AGDA_DIR/Test/MetatheorySmoke.agda"
    "$AGDA_DIR/Test/PresentationInvariance"
    "$AGDA_DIR/Test/ClutchingSmoke.agda"
    "$AGDA_DIR/Test/Fibonacci.agda"
  )

  echo "==> scanning theorem-facing modules for postulate"
  local rg_bin=""
  if command -v rg >/dev/null 2>&1; then
    rg_bin="$(command -v rg)"
  fi

  if [ -n "$rg_bin" ] && [ -x "$rg_bin" ]; then
    if rg -n "\\bpostulate\\b" "${paths[@]}"; then
      found=1
    fi
  else
    if grep -R -n -E "\\bpostulate\\b" "${paths[@]}"; then
      found=1
    fi
  fi

  if [ "$found" -ne 0 ]; then
    echo "ERROR: theorem-facing postulate found" >&2
    return 1
  fi

  echo "==> auxiliary postulates outside theorem boundary"
  if [ -f "$AGDA_DIR/Test/BridgePayloadContract.agda" ]; then
    if [ -n "$rg_bin" ] && [ -x "$rg_bin" ]; then
      rg -n "\\bpostulate\\b" "$AGDA_DIR/Test/BridgePayloadContract.agda" || true
    else
      grep -n -E "\\bpostulate\\b" "$AGDA_DIR/Test/BridgePayloadContract.agda" || true
    fi
  fi
}

run_agda "PEN.agda"
run_agda "Test/MetatheorySmoke.agda"
run_agda "Test/PresentationInvariance/Smoke.agda"
run_agda "Metatheory/PresentationEquivalence.agda"
run_agda "Metatheory/MuInvariance.agda"
run_agda "Metatheory/RawStructuralSyntax.agda"
run_agda "Metatheory/RawStructuralTyping.agda"
run_agda "Test/PresentationInvariance/RebundleRecord.agda"
run_agda "Test/PresentationInvariance/SplitShell.agda"
run_agda "Test/PresentationInvariance/CurryUncurry.agda"
run_agda "Test/PresentationInvariance/TransparentAlias.agda"
run_agda "Test/PresentationInvariance/DuplicateTrace.agda"
run_agda "Geometry/Clutching.agda"
run_agda "Test/ClutchingSmoke.agda"
run_agda "Test/Fibonacci.agda"

if [ -f "$AGDA_DIR/Test/SurfaceBridgeSmoke.agda" ]; then
  run_agda "Test/SurfaceBridgeSmoke.agda"
fi

if [ -f "$AGDA_DIR/Test/ActiveBasisExamples.agda" ]; then
  run_agda "Test/ActiveBasisExamples.agda"
fi

if [ -f "$AGDA_DIR/Test/SparseRecurrenceSmoke.agda" ]; then
  run_agda "Test/SparseRecurrenceSmoke.agda"
fi

require_no_postulates

echo "coherence-depth artifact check passed"
