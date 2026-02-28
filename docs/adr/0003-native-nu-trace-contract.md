# ADR-0003: Phase-3 Native ν API and Trace Schema Contract (P3-V1 Freeze)

- **Status:** Accepted
- **Date:** 2026-02-28
- **Owners:** PEN engine maintainers
- **Related roadmap item:** `P3-V1 — Native ν API + trace schema freeze`
- **Related code:** `engine/src/MBTTNu.hs`, `engine/src/TelescopeEval.hs`, `engine/src/AcceptanceSuite.hs`

## Context

Phase 3 introduces native ν extraction from anonymous MBTT telescopes. We need
an explicit API boundary and a stable trace schema so downstream evaluators,
acceptance tests, and CI evidence lanes can evolve without cross-module drift.

## Decision (frozen contract for P3-V1)

1. **Native ν API boundary**
   - `computeNativeNu :: Telescope -> Library -> [(Int, Int)] -> NativeNuResult`
     is the sole evaluator-facing entrypoint for native ν extraction.
   - `TelescopeEval` `EvalStructural` must route through `computeNativeNu`.

2. **Native ν result shape**
   - `NativeNuResult` fields are frozen for V1:
     - `nnNuG`, `nnNuH`, `nnNuC`, `nnTotal`, `nnTrace`.
   - `nnTotal` must match the scalar ν used for selection.

3. **Trace schema (V1)**
   - `nnTrace` is a list of `key=value` strings that must include:
     - `source=`
     - `nu_g=`
     - `nu_h=`
     - `nu_c=`
     - `bonus_distributive=`
     - `bonus_universe_poly=`
     - `bonus_infinitesimal_shift=`
     - `nu_total=`

4. **Semantic baseline**
   - V1 native ν delegates to the current structural AST computation backend.
   - This freeze is an interface/trace contract, not a semantic rewrite.

## Validation hooks

- Acceptance coverage adds:
  - native total parity with structural total;
  - required trace-key presence checks for schema stability.

## Consequences

- We get a stable seam for later node-level explainability and invariance work.
- Future changes to trace detail can append fields without removing V1 keys.
- Evaluator call sites remain stable while Phase 3 internals are upgraded.

## Amendment (P3-V2 extension)

- Added node-level provenance lines to `nnTrace` in the form
  `node=<entry/path>|ctor=<Ctor>` plus aggregate `node_trace_count=`.
- This is an additive extension and remains compatible with the V1 required
  key set (no V1 keys removed).

## Amendment (P3-V3 invariance evidence)

- Added acceptance evidence for invariance under:
  - TeleEntry alpha-renaming (binder-label changes only), and
  - canonical expression rewrites (`canonicalizeExpr` per entry).
- Added a negative control asserting non-equivalent candidates remain
  distinguishable by native ν outputs.
- Evidence report: `docs/reports/p3_v3_invariance_report.md`.
