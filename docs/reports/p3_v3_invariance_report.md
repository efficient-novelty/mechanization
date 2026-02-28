# Phase 3 V3 Invariance Report

Date: 2026-02-28  
Scope: Native ν invariance under alpha-equivalent telescope renaming and canonical-expression rewrites.

## Validation target

Close out roadmap item **P3-V3 — Alpha/canonical invariance evidence** by demonstrating:

1. `computeNativeNu` is invariant for `(nnTotal, nnTrace)` under TeleEntry alpha-renaming.
2. `computeNativeNu` is invariant for `(nnTotal, nnTrace)` under per-entry `canonicalizeExpr` rewrites.
3. A non-equivalent negative control is distinguishable.

## Acceptance tests added/exercised

- `J11. [MBTT] NativeNu invariant under TeleEntry alpha-renaming`
- `J12. [MBTT] NativeNu invariant under canonicalized expression rewrite`
- `J13. [MBTT] NativeNu distinguishes non-equivalent control`

Command run:

```bash
cd engine && cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates 80
```

Observed outcome:

- `J11` PASS
- `J12` PASS
- `J13` PASS
- MBTT suite summary: `12 passed, 0 failed`

## Conclusion

P3-V3 invariance evidence is satisfied for the current native-ν implementation:

- Alpha-equivalent renaming and canonical rewrites preserve native ν outputs for tested fixtures.
- The harness distinguishes non-equivalent controls, demonstrating test sensitivity.
