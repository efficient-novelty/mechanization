#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

# Ensure generated local smoke artifacts are not tracked.
violations=0
while IFS= read -r f; do
  case "$f" in
    runs/phase1_shadow/.gitkeep|runs/phase1_shadow_ladder/.gitkeep)
      ;;
    runs/phase1_shadow/*|runs/phase1_shadow_ladder/*)
      echo "Tracked generated Phase-1 smoke artifact: $f" >&2
      violations=1
      ;;
  esac
done < <(git ls-files runs)

if [[ "$violations" -ne 0 ]]; then
  echo "Phase-1 repo hygiene check failed." >&2
  exit 1
fi

echo "phase1 repo hygiene check: OK"
