# Phase 3 WP1 Invariance Harness Report

Date: 2026-02-28

## Scope

This report closes **P3-WP1 — Invariance harness + fixture corpus** by adding
and validating a deterministic fixture corpus for native-ν invariance work.

## Added artifacts

- Fixture corpus: `engine/testdata/phase3_native_nu/fixtures.json`
- Deterministic hash lock: `engine/testdata/phase3_native_nu/fixtures.sha256`
- Fixture verifier: `engine/scripts/check_phase3_native_nu_fixtures.sh`

## Fixture content summary

- Equivalence cases:
  - `alpha_rename_step6` (`expect_equal=true`)
  - `canonical_rewrite_step9` (`expect_equal=true`)
- Control case:
  - `negative_control_universe_vs_var` (`expect_equal=false`)

## Validation commands

```bash
engine/scripts/check_phase3_native_nu_fixtures.sh
cd engine && cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates 80
```

## Observed outcome

- Fixture verifier confirms:
  - hash lock matches,
  - required equivalence/control case types are present,
  - negative control expectation exists.
- MBTT acceptance run confirms harness sensitivity remains active (`J13` passes,
  demonstrating non-equivalent control distinction).

## Conclusion

P3-WP1 is complete: a deterministic fixture corpus exists, hash-locked for
reproducibility, with explicit equivalent and non-equivalent control coverage.
