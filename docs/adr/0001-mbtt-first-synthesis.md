# ADR-0001: MBTT-First Autonomous Synthesis

**Status:** Accepted
**Date:** 2026-02-25
**Deciders:** Halvor

## Context

PEN's current synthesis engine uses a **template-first** architecture:

1. `Generator.hs` proposes candidates from 9 human-curated category templates
   (Foundation, Former, HIT, Suspension, Map, Algebra, Modal, Axiom, Synthesis),
   each gated by hand-written prerequisite rules.
2. Evaluation modules (`StructuralNu`, `UniformNu`, etc.) score those
   pre-categorized candidates.
3. MBTT telescope encoding (`Telescope.hs`, `Kolmogorov.hs`) is used for
   audit and reporting, not as the primary search space.

This works — the engine discovers 15/15 structures with 12/15 exact ν match —
but has structural limitations:

- **Category leakage:** The search space is partitioned by semantic labels
  before scoring. This means the engine cannot discover a structure that
  doesn't fit a pre-defined template category.
- **Template fragility:** Adding new candidate families requires hand-writing
  generator rules, prerequisite gates, and recognizer patterns.
- **Scoring entanglement:** Some ν computation paths implicitly depend on
  knowing the candidate's category label, weakening the claim of autonomous
  discovery.
- **κ inconsistency:** Multiple κ measures coexist (clause count, AST node
  count, bit-length) with no single primary.

## Decision

Transition to an **MBTT-first** architecture where:

1. **Typed MBTT terms are the native search space.** A new enumerator directly
   emits well-typed, anonymous MBTT programs under bit-budget and depth bounds.
   No category templates, no semantic labels in the search loop.

2. **Canonicalization before scoring.** Alpha-normalization, definitional
   equality reduction, and safe permutation symmetry collapse syntactic
   duplicates before they reach the scorer.

3. **Native ν extraction from anonymous ASTs.** The spectral decomposition
   (ν_G, ν_H, ν_C) is computed directly from MBTT term structure — introduction
   rules, elimination rules, computation rules — without knowing what the
   term "is."

4. **MBTT bit-length as primary κ.** The Kolmogorov bit-length of the MBTT
   encoding becomes the primary complexity measure. Clause count and AST
   node count become secondary telemetry.

5. **Post-hoc semantic decoding only.** Human-readable names ("Pi", "S1",
   "Cohesion") are attached after selection, purely for reporting. Decoding
   must be provably non-interfering with the optimization loop.

## Constraints (Invariant Contracts)

These invariants must hold throughout the migration and in the final system:

### C1: Search-Space Independence from Semantic Labels

> Candidate enumeration and scoring must not depend on semantic names or
> category labels. Given the same library state, the set of candidates
> generated and their scores must be identical regardless of whether
> names/categories are available.

**Verification:** Remove all name/category fields from library entries;
re-run synthesis; assert identical winner sequence.

### C2: Canonicalization Idempotence

> For any MBTT term `t`, `canon(canon(t)) = canon(t)`.

**Verification:** Property test over generated terms.

### C3: κ Monotonicity by Bit Budget

> If term `t` fits within bit-budget `B`, then `bitKappa(t) ≤ B`. Increasing
> the bit budget never causes a previously enumerable term to become
> non-enumerable.

**Verification:** For all terms enumerated at budget B, assert they are also
enumerated at budget B+1 (modulo ordering).

### C4: Post-hoc Decoding Non-Interference

> The selection winner at each step must be identical whether the decoder is
> enabled or disabled. Formally: `select(candidates, ν, κ) = select(candidates, ν, κ)`
> is trivially true because the decoder is not an input to `select`.

**Verification:** Compile-time module boundary (decoder imports evaluator
output, not vice versa) + runtime contract test.

## Consequences

### What changes

- New modules: `MBTTEnum.hs`, `MBTTCanonical.hs`, `MBTTDecode.hs`.
- `RunAbInitio.hs` gains `--mbtt-first` flag (additive; legacy path retained).
- `StructuralNu.hs` extended with anonymous-term entry point.
- CSV schema extended with `bit_kappa`, `canonical_key`, `decoded_name?`.
- MCTS node expansion uses canonical representatives.

### What stays

- `Telescope.hs` data types (TeleEntry, MBTTExpr) — they become the
  primary representation, not just audit.
- `TelescopeCheck.hs` — well-formedness checking, now on the critical path.
- PEN bar formula, Fibonacci integration latency, d-Bonacci sequences.
- Agda bridge output format (extended, not replaced).
- Acceptance suite (extended with MBTT-first invariant checks).

### Rollback strategy

- `--legacy-generator` flag preserves template-first path indefinitely.
- Dual-run comparison harness (`compare_runs.sh`) validates MBTT-first
  against template-first before default switch.
- Phase 7 deprecation only after equivalence demonstrated across seed sweep.

## References

- `MBTT_FIRST_AUTONOMOUS_SYNTHESIS_ROADMAP.md` — full phased plan
- `runs/phase0_baseline/` — pre-migration performance snapshot
- `engine/src/Telescope.hs` — existing MBTT data types
- `engine/src/Kolmogorov.hs` — existing bit-length encoding
