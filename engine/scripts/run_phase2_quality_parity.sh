#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE_DIR="$ROOT_DIR/engine"
STAMP="$(date -u +%Y%m%dT%H%M%SZ)"
OUT_DIR_INPUT="${1:-$ROOT_DIR/runs/phase2_parity/$STAMP}"
if [[ "$OUT_DIR_INPUT" = /* ]]; then
  OUT_DIR="$OUT_DIR_INPUT"
else
  OUT_DIR="$ROOT_DIR/$OUT_DIR_INPUT"
fi

MAX_STEPS="${MAX_STEPS:-2}"
MAX_CANDS="${MAX_CANDS:-20}"

mkdir -p "$OUT_DIR"

pushd "$ENGINE_DIR" >/dev/null

CABAL_BIN="$(command -v cabal || true)"
[[ -n "$CABAL_BIN" ]] || { echo "cabal not found in PATH" >&2; exit 127; }
$CABAL_BIN build ab-initio >/dev/null
ABINITIO_BIN="$($CABAL_BIN list-bin ab-initio)"

"$ABINITIO_BIN" --structural --phase1-shadow --max-steps "$MAX_STEPS" --mbtt-max-candidates "$MAX_CANDS" --csv p2_v5_on.csv \
  > "$OUT_DIR/canonical_on.log" 2>&1
"$ABINITIO_BIN" --structural --phase1-shadow --max-steps "$MAX_STEPS" --mbtt-max-candidates "$MAX_CANDS" --no-canonical-quotient --csv p2_v5_off.csv \
  > "$OUT_DIR/canonical_off.log" 2>&1

mv p2_v5_on.csv "$OUT_DIR/p2_v5_on.csv"
mv p2_v5_off.csv "$OUT_DIR/p2_v5_off.csv"

python3 - "$OUT_DIR/p2_v5_on.csv" "$OUT_DIR/p2_v5_off.csv" "$OUT_DIR/report.md" "$MAX_STEPS" <<'PY'
import csv, sys
on_csv, off_csv, out_md, max_steps = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])

def read_rows(path):
    with open(path, newline='', encoding='utf-8') as f:
        return list(csv.DictReader(f))

on = read_rows(on_csv)
off = read_rows(off_csv)
if len(on) < max_steps or len(off) < max_steps:
    raise SystemExit(f"expected at least {max_steps} rows in both parity CSVs")

exp = {
    1: ("Universe", 1, 2),
    2: ("Unit", 1, 1),
}

parity_rows = []
all_parity = True
golden_ok = True
for i in range(max_steps):
    a, b = on[i], off[i]
    step = int(a["step"])
    same = (a["name"], a["nu"], a["kappa"]) == (b["name"], b["nu"], b["kappa"])
    all_parity = all_parity and same
    if step in exp:
        en, ev, ek = exp[step]
        golden_ok = golden_ok and (a["name"] == en and int(a["nu"]) == ev and int(a["kappa"]) == ek)
    parity_rows.append((step, a["name"], a["nu"], a["kappa"], b["name"], b["nu"], b["kappa"], same))

md = [
    "# Phase 2 V5 Quality Parity Report",
    "",
    f"- max_steps: {max_steps}",
    f"- parity_on_off: {str(all_parity).lower()}",
    f"- golden_prefix_ok (steps 1..2): {str(golden_ok).lower()}",
    "",
    "## Per-step parity",
    "",
    "| step | on_name | on_nu | on_kappa | off_name | off_nu | off_kappa | parity |",
    "|---:|---|---:|---:|---|---:|---:|:---:|",
]
for r in parity_rows:
    md.append(f"| {r[0]} | {r[1]} | {r[2]} | {r[3]} | {r[4]} | {r[5]} | {r[6]} | {'yes' if r[7] else 'no'} |")

with open(out_md, 'w', encoding='utf-8') as f:
    f.write("\n".join(md) + "\n")

if not (all_parity and golden_ok):
    raise SystemExit("quality parity check failed")
PY

popd >/dev/null

echo "Wrote P2-V5 parity artifacts to: $OUT_DIR"
