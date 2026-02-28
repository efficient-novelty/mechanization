#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

TARGETS=(
  "runs/phase1_ci"
  "runs/phase1_bundle"
)

for t in "${TARGETS[@]}"; do
  if [[ -d "$t" ]]; then
    rm -rf "$t"
    echo "Removed $t"
  fi
done

echo "Phase-1 generated artifact cleanup complete."
