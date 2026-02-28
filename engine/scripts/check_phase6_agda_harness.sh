#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HARNESS="$ROOT_DIR/agda/Test/BridgePayloadContract.agda"
AGDA_EXPORT="$ROOT_DIR/engine/src/AgdaExport.hs"

[[ -f "$HARNESS" ]] || { echo "missing file: $HARNESS" >&2; exit 1; }
[[ -f "$AGDA_EXPORT" ]] || { echo "missing file: $AGDA_EXPORT" >&2; exit 1; }

python3 - "$HARNESS" "$AGDA_EXPORT" <<'PY'
import sys
harness = open(sys.argv[1], encoding='utf-8').read()
export_hs = open(sys.argv[2], encoding='utf-8').read()

required_harness_tokens = [
    'module Test.BridgePayloadContract where',
    'record NuClaim : Set where',
    'record BridgePayload : Set where',
    'canonical-key',
    'kappa-bit',
    'kappa-desugared',
    'anonymous-ast',
    'nu-claim',
    'CanonicalKeySound',
    'NuClaimWellFormed',
    'DecodeNonInterference',
    'record ContractWitness (p : BridgePayload) : Set where',
]
for token in required_harness_tokens:
    if token not in harness:
        raise SystemExit(f'missing token in BridgePayloadContract.agda: {token}')

# Ensure Agda-side harness tracks schema fields emitted by bridge exporter.
field_pairs = [
    ('canonical_key', 'canonical-key'),
    ('kappa_bit', 'kappa-bit'),
    ('kappa_desugared', 'kappa-desugared'),
    ('anonymous_ast', 'anonymous-ast'),
    ('nu_claim', 'nu-claim'),
]
for export_field, harness_field in field_pairs:
    if export_field not in export_hs:
        raise SystemExit(f'missing exporter field in AgdaExport.hs: {export_field}')
    if harness_field not in harness:
        raise SystemExit(f'missing harness field in BridgePayloadContract.agda: {harness_field}')

print('phase6 agda harness contract check: OK')
PY
