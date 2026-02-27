# MBTT-First Autonomous Synthesis — Roadmap and Backlog

## Goal

Transition PEN from a two-phase architecture (human-curated candidate templates + mechanical evaluation) to an MBTT-first architecture where typed MBTT terms are the native search space and semantic labels are attached only after global selection.

---

## Architectural Target State

### Current (template-first)
1. `Generator`/`Enumerate` proposes category-scoped candidates (Foundation, HIT, Modal, etc.).
2. Evaluation (`InferenceNu`, `UniformNu`, `StructuralNu`, PEN bar) scores those named candidates.
3. MBTT/Kolmogorov encoding is mostly audit/reporting.

### Target (MBTT-first)
1. Typed MBTT synthesis loop enumerates bounded, well-typed anonymous programs (`Π`, `Σ`, atoms, `Lib(i)`).
2. Canonicalization/quotienting collapses alpha-equal/definitional/permutation-equivalent terms.
3. Native novelty extraction computes `ν_G, ν_H, ν_C` directly from MBTT structure.
4. PEN optimization selects by `ρ = ν / κ` where `κ` is MBTT bit-length (primary); clause count is secondary telemetry.
5. Post-hoc semantic decoding maps winning anonymous terms to human interpretations.

---

## Implementation Principles

- Keep legacy path runnable behind feature flags until parity acceptance passes.
- Introduce MBTT-first as an additive pipeline first; remove template-first generator only after equivalence and stability criteria are met.
- Treat all names/categories as metadata: they must not influence search order.
- Preserve reproducibility: deterministic enumeration order, canonicalization hash stability, and seed-locked MCTS.
- Strengthen contracts with explicit invariants in both Haskell tests and Agda-facing artifacts.

---

## Phased Roadmap (10–14 weeks)

## Phase 0 — Baseline and Contracts (Week 1) ✓ COMPLETE

**Completed:** 2026-02-25 | **Git:** 8e2f548 (main)

### Deliverables
- [x] Snapshot current performance/correctness baseline for structural mode and acceptance suite.
  - `runs/phase0_baseline/` — REPORT.txt, 6 CSV profiles (structural d1/d2/d3, paper-calibrated, 2 ablations), acceptance log.
  - Summary: 15/15 structures, 12/15 exact ν match, κ=64/64, d=2 optimal, 46/46 acceptance.
- [x] Add architecture decision record (ADR) documenting MBTT-first constraints.
  - `docs/adr/0001-mbtt-first-synthesis.md` — Context, Decision, Constraints (C1–C4), Consequences.
- [x] Define invariant contracts for:
  - [x] C1: search-space independence from semantic labels — I1 (steps 1-14 name-free; step 15 known gap via capability flag gating), I2 (classification name-free).
  - [x] C2: canonicalization idempotence — I3 (κ determinism; full canonicalization deferred to Phase 2).
  - [x] C3: κ monotonicity by bit budget — I4 (weak monotonicity with telescope size).
  - [x] C4: post-hoc decoding non-interference — I5 (scoring order invariance), I6 (bar name-free).

### Exit criteria
- [x] Baseline CSV/log artifacts in `runs/phase0_baseline/`.
- [x] ADR in `docs/adr/0001-mbtt-first-synthesis.md`.
- [x] 6 new contract tests (I1–I6) in acceptance suite, 52/52 passing.

### Known gap documented
- **I1 canary:** Step 15 (DCT) ν drops from 103→88 when library names are scrambled, because `telescopeToCandidate` gates `leHasTemporalOps` on `name=="DCT"`. This capability-flag name-dependence is the primary target for Phase 3 (native ν extraction from anonymous terms). The canary test will fail-positive when the gap is closed.

---

## Phase 1 — Typed MBTT Enumerator Core (Weeks 2–4) ↻ IN PROGRESS

**Last updated:** 2026-02-25

### Current implementation status
- [x] Added `engine/src/MBTTEnum.hs` with typed MBTT expression generation (`Pi`, `Sigma`, atoms, `Lib` refs), explicit budget-split enumeration, deterministic ordering, and candidate cost payload (`bitKappa`, clause count, AST nodes).
- [x] Added Phase-1 acceptance coverage in `engine/src/RunAcceptance.hs` (J1–J5): grammar coverage, well-formedness, determinism, reference telescope recovery (steps 1–4), and bit-cost ordering.
- [x] Wired `MBTTEnum` into build targets via `engine/pen-engine.cabal`.
- [x] Integrate `--mbtt-first` flag into `RunAbInitio` as a selectable search path (Phase A can now enumerate via `MBTTEnum`; optional `--mbtt-max-candidates` cap added for bounded MBTT sessions).
- [x] Add shadow-run controls in `RunAbInitio` (`--phase1-shadow` preset (expands to `--mbtt-first`, `--max-steps`, `--skip-validation`, `--mbtt-shadow-profile`, `--skip-mcts`)) so Phase-1 MBTT evidence can run bounded first-stage checks without paying full Phase 0 validation cost.
- [x] Added local Phase-1 shadow artifact helper `engine/scripts/run_phase1_shadow.sh` to run `acceptance-core` + bounded `ab-initio --phase1-shadow` and write `runs/phase1_shadow/<run>/` manifest/log/CSV bundles.
- [x] Added local Phase-1 ladder helper `engine/scripts/run_phase1_shadow_ladder.sh` to run step-horizon ladders (1..6) with per-step timeouts and emit `ladder_status.csv` for hardware-capability evidence.
- [x] Added local Phase-1 evidence bundle helper `engine/scripts/run_phase1_evidence_bundle.sh` to mirror CI lane structure in one command and emit per-lane status telemetry (`lane_status.csv`) + replay manifest.
- [ ] Run parity/acceptance in CI with Haskell toolchain enabled and archive artifacts under `runs/phase1_*`.

### Key learnings so far
- Exhaustive budget-split enumeration resolves the under-generation bias from previous single-best-child shortcuts for compound nodes.
- Structural modal/temporal gating from library capabilities can be expressed without direct semantic-name checks inside the enumerator itself.

### Active blockers
- Local environment now has `ghc`/`cabal` installed (Ubuntu packages). Acceptance has been split into `acceptance-core` (A–I) and `acceptance-mbtt` (J-lane) so local/CI workflows can run bounded MBTT checks independently; full-budget MBTT still requires a larger runner.

### Immediate next step (evidence hardening)
- [x] Formalized an **evidence contract** for autonomy claims in `docs/phase1_evidence_contract.md`:
  - lane definitions (`acceptance-core`, bounded/full `acceptance-mbtt`, `ab-initio --mbtt-first`),
  - required pass/fail gates per lane,
  - reproducibility metadata (commit SHA, flags, seed/window, machine profile).
- [x] Formalized a **CI artifact policy** for `runs/phase1_*` and wired it into `.github/workflows/pen-engine.yml`:
  - mandatory artifacts (acceptance logs, MBTT lane logs, ab-initio CSV/report, manifest),
  - retention/naming conventions (PR 14d, main 30d),
  - replay instructions via manifest + contract doc.
- [x] Added a CI **shadow-ladder telemetry lane** (`engine/scripts/run_phase1_shadow_ladder.sh`) for bounded horizon evidence (`STEPS=1 2 3`, per-step timeout, `ladder_status.csv`) so resource limits are measured explicitly rather than inferred from abrupt runner kills.
- [x] Added a CI **main-branch ladder gate lane** (`REQUIRE_SUCCESS_THROUGH=6`) to enforce that shadow replay horizons 1..6 complete on provisioned runners before treating the six-stage criterion as satisfied.
- [x] Added CI **artifact-contract verification** (`engine/scripts/verify_phase1_evidence.sh`) so required lane evidence files and main-branch gate artifacts are validated before upload.
- [x] Added CI/local **human-readable evidence summarization** (`engine/scripts/summarize_phase1_evidence.sh`) producing `summary.md` from lane outputs before verification/upload.
- [x] Added CI workflow-summary publishing (`$GITHUB_STEP_SUMMARY`) plus stricter evidence checks requiring acceptance logs to report zero failures before artifact upload.
- [x] Added CI evidence-tooling self-check (`engine/scripts/test_phase1_evidence_tools.sh`) to prevent summarize/verify contract drift.
- [x] Added CI merge-marker guard (`engine/scripts/check_no_conflict_markers.sh`) to fail fast if unresolved conflict blocks are introduced in Phase-1 workflow/docs/scripts.
- [x] Added Phase-1 artifact hygiene controls (`.gitignore` + `engine/scripts/clean_phase1_artifacts.sh`) so generated evidence outputs (including local shadow/ladder smoke folders) are kept in CI artifacts instead of creating repeated repository merge noise.
- [x] Added CI repo-hygiene guard (`engine/scripts/check_phase1_repo_hygiene.sh`) to fail fast if generated local smoke artifacts become tracked again.
- [x] Added CI workflow-consistency guard (`engine/scripts/check_phase1_workflow_consistency.sh`) to ensure key Phase-1 evidence steps appear exactly once (avoids accidental duplicate/missing step edits).
- [x] Strengthened evidence verification so `summary.md` must contain concrete acceptance `Results:` lines for required lanes (prevents placeholder-only summaries from passing).
- [x] Added local iteration bootstrap helper (`engine/scripts/start_phase1_iteration.sh`) to force fresh-`main` branch starts and reduce recurring merge-conflict churn.

### Scope
Build a new typed enumerator that directly emits well-typed MBTT ASTs under bit-budget and depth bounds.

### Haskell workstream
- Add `engine/src/MBTTEnum.hs`:
  - grammar constructors for `Pi`, `Sigma`, primitives, `LibRef`;
  - budget-aware generation;
  - type-directed pruning via inhabitation/context checks;
  - deterministic ordering (for reproducibility).
- Add candidate identity type in `Telescope.hs` or new `MBTTCandidate.hs`:
  - anonymous term id;
  - provenance (budget, depth, seed);
  - cost payload (`bitKappa`, `nodeCount`, `clauseCount`).
- Integrate into `RunAbInitio.hs` with `--mbtt-first` gate.

### Tests
- Unit: grammar coverage for each constructor family.
- Property: every emitted term type-checks under `TelescopeCheck`.
- Regression: deterministic candidate stream given fixed seed/budget.

### Exit criteria
- [ ] `--mbtt-first` can enumerate and evaluate at least first 6 canonical stages in shadow mode (now operationally supported via `--max-steps 6` and CI lane `REQUIRE_SUCCESS_THROUGH=6`; pending first green run with archived ladder gate artifact).
- [x] Enumerator-specific acceptance checks (J1–J5) are implemented and tracked in `RunAcceptance`.
- [ ] Phase-1 benchmark artifacts committed in `runs/phase1_*` per formal CI artifact policy (including ladder telemetry `ladder/ladder_status.csv` and lane logs).

---

## Phase 2 — Canonicalization and Quotienting (Weeks 4–6)

### Scope
Normalize MBTT candidates before scoring to avoid syntactic duplicates.

### Haskell workstream
- Add `engine/src/MBTTCanonical.hs`:
  - alpha-normalization (de Bruijn / stable binder ordering);
  - definitional equality reduction (beta/eta + existing telescope reductions);
  - permutation symmetry canonical ordering for commutative-equivalent substructures where valid.
- Add canonical hash and quotient cache (`HashMap CanonKey CandidateRep`).
- Thread canonical representatives into MCTS node expansion.

### Tests
- Property: canonicalization is idempotent.
- Property: alpha-equivalent terms share canonical key.
- Differential: search frontier size reduced without loss of best-ρ candidates at fixed budget.

### Exit criteria
- Duplicate rate reduced by agreed threshold (target ≥40% at medium budget).
- No regression in discovered best score for benchmark seeds.

---

## Phase 3 — Native ν Extraction from Anonymous Terms (Weeks 6–8)

### Scope
Compute `ν_G`, `ν_H`, `ν_C` directly from MBTT AST behavior, not semantic labels.

### Haskell workstream
- Extend `StructuralNu.hs`/`InferenceNu.hs` with MBTT-term entry point:
  - derive eliminator/introduction/computation interaction signatures from anonymous term;
  - produce spectral components and total novelty;
  - emit explainability trace tied to AST nodes.
- Build compatibility adapter so legacy reports can still print named fields when decoding exists.

### Tests
- Golden: selected existing canonical terms produce expected ν decomposition within tolerance.
- Property: ν extraction invariant under alpha-equivalence and canonical rewrites.
- Consistency: no dependence on label metadata in evaluator inputs.

### Exit criteria
- Native ν path is default under `--mbtt-first`; old label-dependent path disabled in that mode.

---

## Phase 4 — PEN Optimization with MBTT κ Primary (Weeks 8–10)

### Scope
Shift optimizer objective to bit-length-first complexity in MBTT space.

### Haskell workstream
- Update scoring interfaces in `Synthesis.hs`, `RunAbInitio.hs`, and evaluator bridge:
  - primary `κ = bitKappa` from MBTT encoding;
  - secondary telemetry: clause count / node count;
  - unchanged bar constraint + `ρ` formula.
- Update CSV/report schema:
  - include `bit_kappa`, `ast_nodes`, `canonical_key`, `decoded_name?`.
- Confirm step-13 target object emerges as unlabeled maximal-ρ term (`ν=46`, ~138 bits expected band).

### Tests
- Regression: structural run still yields 15-step growth with bar compliance.
- Golden: step-13 target checks (`ν=46`, winner near expected bit complexity).
- Ablation: if clause-count becomes primary again, sequence quality degrades (sanity check).

### Exit criteria
- MBTT-primary scoring stable across seed sweep and window settings.

---

## Phase 5 — Post-hoc Semantic Decoding (Weeks 10–11)

### Scope
Attach mathematical interpretation after optimization only.

### Haskell workstream
- Add `engine/src/MBTTDecode.hs`:
  - pattern/constraint-based decoder from anonymous AST to semantic descriptors;
  - confidence score and ambiguity handling.
- Integrate decoding into reporting layer only (not candidate generation/scoring).
- Extend Agda bridge comments to include decoded interpretation + confidence.

### Tests
- Unit: known anonymous representatives decode to expected identities.
- Contract: removing decoder must not change selected winners.

### Exit criteria
- Reports show both anonymous winner id and optional decoded interpretation.

---

## Phase 6 — Agda Alignment and Formal Contracts (Weeks 11–12)

### Scope
Synchronize Agda artifacts with MBTT-first candidate provenance and invariants.

### Agda workstream
- Extend bridge output metadata fields for canonical key + bit κ provenance.
- Add proof obligations (or machine-checked skeletons) for:
  - canonicalization soundness assumptions,
  - invariance of ν under alpha-equivalence,
  - non-interference of decoding with selection.
- Add focused tests in `agda/Test` for new bridge record fields.

### Exit criteria
- `cabal run agda-bridge -- --check` deterministic with new metadata.
- Agda test suite validates updated bridge schema.

---

## Phase 7 — Migration, Hardening, and Cleanup (Weeks 12–14)

### Scope
Make MBTT-first default, retain rollback, deprecate template-first components.

### Workstream
- Promote `--mbtt-first` to default; add `--legacy-generator` fallback.
- Deprecate category template generation paths with warnings.
- Update docs, scripts, CI matrix, and acceptance suite.
- Performance pass: quotient cache sizing, parallel enumeration, memory profiling.

### Exit criteria
- CI green with MBTT-first default.
- Legacy path marked deprecated with sunset issue created.

---

## Backlog (Epics and Tickets)

## Epic A — Enumerator Foundation
- **A1 (P0):** MBTT grammar AST extension and pretty-printer parity.
- **A2 (P0):** Typed budgeted enumerator with deterministic order.
- **A3 (P1):** Enumerator telemetry (branching factor, prune reasons).
- **A4 (P1):** Seeded parallel chunking without order drift.

## Epic B — Canonicalization/Quotient Engine
- **B1 (P0):** Alpha normalization + canonical binder strategy.
- **B2 (P0):** Definitional equality normalizer.
- **B3 (P1):** Permutation symmetry reduction for safe operator classes.
- **B4 (P1):** Canonical hash cache and collision tests.

## Epic C — Native Novelty Pipeline
- **C1 (P0):** AST-to-rule extraction API (`ν_G, ν_H, ν_C`).
- **C2 (P0):** Label-independent evaluator contract enforcement.
- **C3 (P1):** Explainability traces from AST nodes to novelty increments.
- **C4 (P2):** Cross-check with `UniformNu` on sampled corpus.

## Epic D — MBTT-Primary Optimization
- **D1 (P0):** Scoring schema change (`bitKappa` primary).
- **D2 (P0):** PEN bar compatibility checks with new κ.
- **D3 (P1):** Step-13 winner validation harness.
- **D4 (P1):** Budget/seed robustness sweep automation.

## Epic E — Post-hoc Decoder
- **E1 (P1):** Decoder core with confidence scores.
- **E2 (P1):** Report/UI wiring (non-interfering).
- **E3 (P2):** Ambiguity clustering + top-k candidate explanations.

## Epic F — Agda/Bridge Integration
- **F1 (P1):** Bridge schema extension for canonical metadata.
- **F2 (P1):** Agda stubs updated with MBTT provenance comments.
- **F3 (P2):** Proof skeletons for canonicalization and non-interference assumptions.

## Epic G — Quality, Performance, and Ops
- **G1 (P0):** Acceptance suite expansion for MBTT-first invariants.
- **G2 (P1):** CI job split: fast invariant checks vs full discovery.
- **G3 (P1):** Profiling and memory budget envelope.
- **G4 (P2):** Failure triage dashboard from run artifacts.

---

## Definition of Done (Program-Level)

A release is “MBTT-first complete” when all are true:
1. MBTT-first path is default and deterministic.
2. Search uses typed MBTT enumeration (no semantic template priors).
3. Canonicalization eliminates semantic duplicates before scoring.
4. Native ν extraction is label-free and invariant under canonical equivalence.
5. PEN optimization uses MBTT bit-length as primary κ.
6. Post-hoc decoding is present and provably non-interfering with selection.
7. CI + acceptance + Agda bridge checks are green under default mode.

---

## Risks and Mitigations

- **Risk:** Enumeration explosion from richer MBTT space.  
  **Mitigation:** aggressive type-directed pruning, quotient cache, progressive widening controls.

- **Risk:** Over-normalization merges semantically distinct terms.  
  **Mitigation:** staged canonicalization flags, conservative symmetry classes, differential oracle tests.

- **Risk:** ν extraction drift vs legacy expectations.  
  **Mitigation:** dual-run comparison harness and tolerance-bounded goldens.

- **Risk:** Decoder leakage into scoring path.  
  **Mitigation:** compile-time module boundary + contract tests that disable decoder.

---

## Suggested Execution Order (first sprint cut)

1. A1 → A2 → G1 baseline tests.
2. B1 + B2 minimal canonicalization.
3. C1 native ν extraction MVP.
4. D1 scoring migration.
5. Shadow-run comparison harness (legacy vs MBTT-first) before default switch.

This gives a thin vertical slice that proves feasibility early while preserving rollback.
