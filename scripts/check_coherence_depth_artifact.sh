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
    "$AGDA_DIR/CaseStudies"
    "$AGDA_DIR/ObligationGraph"
    "$AGDA_DIR/PEN.agda"
    "$AGDA_DIR/Test/MetatheorySmoke.agda"
    "$AGDA_DIR/Test/SurfaceBridgeSmoke.agda"
    "$AGDA_DIR/Test/ActiveBasisExamples.agda"
    "$AGDA_DIR/Test/SparseRecurrenceSmoke.agda"
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

run_case_study_audit() {
  local runs_dir="$ROOT/runs/coherence_depth_case_studies"
  local audit_script="$ROOT/scripts/coherence_depth_audit.py"
  echo "==> python scripts/coherence_depth_audit.py runs/coherence_depth_case_studies"
  if command -v python3 >/dev/null 2>&1; then
    python3 "$audit_script" "$runs_dir"
  elif command -v python >/dev/null 2>&1; then
    python "$audit_script" "$runs_dir"
  elif command -v powershell.exe >/dev/null 2>&1; then
    local win_root
    win_root="$(to_windows_path "$ROOT")"
    powershell.exe -NoProfile -ExecutionPolicy Bypass -Command \
      "Set-Location -LiteralPath '$win_root'; python scripts/coherence_depth_audit.py runs/coherence_depth_case_studies"
  else
    echo "ERROR: python not found on PATH and powershell.exe fallback unavailable" >&2
    return 127
  fi
}

run_agda "PEN.agda"
run_agda "Test/MetatheorySmoke.agda"
run_agda "Test/PresentationInvariance/Smoke.agda"
run_agda "Metatheory/PresentationEquivalence.agda"
run_agda "Metatheory/MuInvariance.agda"
run_agda "Metatheory/RawStructuralSyntax.agda"
run_agda "Metatheory/RawStructuralTyping.agda"
run_agda "Metatheory/SurfaceNormalizationBridge.agda"
run_agda "Metatheory/SurfaceToHornImage.agda"
run_agda "Metatheory/FiniteInterfaceBasis.agda"
run_agda "Metatheory/GlobalActionSemantics.agda"
run_agda "Metatheory/ActiveBasisContract.agda"
run_agda "Metatheory/SparseDependencyRecurrence.agda"
run_agda "Metatheory/FullCouplingEnvelope.agda"
run_agda "CaseStudies/Common.agda"
run_agda "CaseStudies/UniverseExtension.agda"
run_agda "CaseStudies/GlobalModality.agda"
run_agda "CaseStudies/PromotedInterface.agda"
run_agda "CaseStudies/SparseDatatype.agda"
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
run_case_study_audit

echo "coherence-depth artifact check passed"
