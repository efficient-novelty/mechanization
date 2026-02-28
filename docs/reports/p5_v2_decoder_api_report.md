# Phase 5 V2 Decoder API Report

Date: 2026-02-28  
Scope: Complete P5-WP2 by introducing a deterministic post-hoc decode API boundary and fixture corpus.

## Change implemented

- Added `engine/src/MBTTDecode.hs` with Phase-5 decoder schema:
  - `drCanonicalName`
  - `drCanonicalKey`
  - `drDecodedLabel`
  - `drConfidence`
  - `drAmbiguity`
  - `drNonInterfering`
- Added deterministic decode entrypoints:
  - `decodeCanonicalName`
  - `decodeCanonicalNameWithKey`
- Wired `MBTTDecode` into `engine/pen-engine.cabal` so the module is compile-visible in core executables.
- Added fixture corpus under `engine/testdata/phase5_decode/`:
  - `fixtures.tsv`
  - `fixtures.sha256`
- Added `engine/scripts/check_phase5_decode_fixtures.sh` to validate:
  - fixture hash lock,
  - fixture schema,
  - decoder behavior via a `runghc` harness against fixture expectations.

## Validation commands

```bash
engine/scripts/check_phase5_decode_fixtures.sh
engine/scripts/check_no_conflict_markers.sh
engine/scripts/check_phase1_workflow_consistency.sh
engine/scripts/test_phase1_evidence_tools.sh
```

## Observed outcomes

- Phase-5 fixture schema check: OK.
- Phase-5 fixture behavior check: OK.
- Phase-5 fixture hash check: OK.
- Existing evidence/workflow guards remain green.

## Conclusion

P5-WP2 is complete: decoder API boundary is present and deterministic, fixtures are hash-locked, and behavior is validated by scriptable checks.
