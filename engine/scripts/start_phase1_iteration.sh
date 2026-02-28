#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

usage() {
  cat <<USAGE
Usage: $0 [--dry-run] [--allow-dirty] [branch-name]

Starts a new Phase-1 iteration branch from latest origin/main:
  1) git fetch origin
  2) git checkout main
  3) git reset --hard origin/main
  4) git clean -fd
  5) git checkout -b <branch-name>

Defaults:
  branch-name: codex/phase1-<UTC timestamp>

Options:
  --dry-run      Print commands without executing them.
  --allow-dirty  Skip the clean-working-tree safety check.
USAGE
}

dry_run=0
allow_dirty=0
branch_name=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run)
      dry_run=1
      shift
      ;;
    --allow-dirty)
      allow_dirty=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      if [[ -n "$branch_name" ]]; then
        echo "Unexpected extra argument: $1" >&2
        usage >&2
        exit 2
      fi
      branch_name="$1"
      shift
      ;;
  esac
done

if [[ -z "$branch_name" ]]; then
  stamp="$(date -u +%Y%m%dT%H%M%SZ)"
  branch_name="codex/phase1-${stamp}"
fi

run() {
  if [[ "$dry_run" -eq 1 ]]; then
    echo "+ $*"
  else
    "$@"
  fi
}

if [[ "$allow_dirty" -ne 1 ]]; then
  if [[ -n "$(git status --porcelain)" ]]; then
    echo "Working tree is dirty. Commit/stash changes or use --allow-dirty." >&2
    exit 1
  fi
fi

if ! git remote get-url origin >/dev/null 2>&1; then
  if [[ "$dry_run" -eq 1 ]]; then
    echo "[dry-run] Missing remote 'origin' (commands shown only)." >&2
  else
    echo "Missing remote 'origin'. Configure it before starting a new iteration." >&2
    exit 1
  fi
fi

run git fetch origin
run git checkout main
run git reset --hard origin/main
run git clean -fd
run git checkout -b "$branch_name"

echo "Started Phase-1 branch: $branch_name"
