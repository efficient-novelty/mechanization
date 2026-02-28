# ADR-0002: Phase-2 MBTT Canonicalization Contract (P2-V1 Freeze)

- **Status:** Accepted
- **Date:** 2026-02-27
- **Owners:** PEN engine maintainers
- **Related roadmap item:** `P2-V1 — Canonicalization semantics freeze`
- **Related code:** `engine/src/MBTTCanonical.hs`, `engine/src/RunAbInitio.hs`

## Context

Phase 2 introduces canonicalization/quotienting so the search stack can collapse
structurally equivalent MBTT candidates before expensive evaluation. To avoid
drift across enumerator, acceptance, and evidence tooling lanes, we need a
frozen semantic contract for what canonicalization *does* and *does not*
normalize in this phase.

## Decision (frozen contract for P2-V1)

For Phase-2 V1, canonicalization is defined as follows:

1. **Structural recursive normalization only**
   - `canonicalizeExpr` recursively canonicalizes children while preserving
     the original constructor and child ordering.
   - No rewriting that changes expression meaning/shape is performed.

2. **Canonical key derivation**
   - `canonicalKeyExpr` and `canonicalKeySpec` are derived from canonicalized
     AST encodings and hashed to stable 32-bit hex keys.
   - Key stability is required across repeated canonicalization passes.

3. **Reduction boundaries (explicitly out-of-scope in V1)**
   - No beta/eta reduction.
   - No binder reindexing/de Bruijn alpha-normalization.
   - No commutativity/permutation normalization.

4. **Search quotienting behavior**
   - Deduplication by canonical key keeps first-seen representative to preserve
     deterministic upstream enumeration order.

## Canonical-equivalence examples (V1 semantics)

The following equivalent forms must map to one canonical form/key under V1:

1. **Idempotence equivalence**
   - `e`
   - `canonicalizeExpr e`
   - `canonicalizeExpr (canonicalizeExpr e)`

   These are canonically equivalent by contract (`canonicalizeExpr` idempotent).

2. **Source-path equivalence (same AST discovered via different paths)**
   - Candidate A emitted by exhaustive MBTT enumeration with AST `t`
   - Candidate B emitted by another search branch with AST `t`

   Both map to the same `canonicalKeySpec` because the canonical encoding of
   their `teType` sequences is identical.

## Non-goals deferred to later Phase-2 deliveries

- Alpha-equivalence collapsing for binder-renamed terms.
- Definitional-equality quotienting (beta/eta).
- Symmetry/permutation class collapsing where mathematically valid.

These are deferred to later Phase-2 deliveries (P2-V2+), so V1 can stabilize
behavior and telemetry first.

## Consequences

- We get deterministic, reproducible key-based dedupe immediately.
- We do **not** yet merge alpha-equivalent but syntactically distinct terms.
- Acceptance invariants can be anchored to idempotence/key stability now, with
  stronger equivalence classes added in subsequent Phase-2 milestones.
