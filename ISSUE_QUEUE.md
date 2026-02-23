# PEN Engine Issue Queue (P0 / P1 / P2)

This queue translates current architecture findings and roadmap notes into executable work items with clear acceptance criteria.

## Priority Definitions

- **P0**: Blocks scientific validity or introduces claim-risk in current paper narrative.
- **P1**: Improves discovery quality, runtime, and reproducibility but does not block core claim.
- **P2**: Scale-up, polish, and long-horizon verification tasks.

---

## P0 — Claim Integrity and Reproducibility

### P0.1 — Canonicalize strict-vs-paper semantics across docs and CLI output
**Problem**: Documentation drift around what strict mode computes vs what paper-calibrated mode computes.

**Tasks**
- Update `physics_creation.md` mode descriptions and historical notes to reflect current evaluator behavior.
- Update `CODEBASE_GUIDE.md` mode table so it matches `RunAbInitio` + `TelescopeEval` exactly.
- Add a concise “mode legend” in `RunAbInitio` output header (paper-calibrated vs strict-computed).

**Acceptance criteria**
- No contradictory statements remain across `physics_creation.md`, `CODEBASE_GUIDE.md`, and code comments.
- A reviewer can map each mode to exact ν/κ computation path in one hop.
- Running `ab-initio` and `ab-initio -- --strict` prints explicit evaluator mode labels.

---

### P0.2 — Evidence matrix for claims (ordering, numerics, d=2 scaling)
**Problem**: Claim boundaries are not centralized; difficult to audit what is demonstrated vs assumed.

**Tasks**
- Add `CLAIMS_EVIDENCE.md` with 3 sections:
  1. Sequence ordering claim,
  2. ν/κ numeric claim,
  3. d=2/Fibonacci schedule claim.
- For each claim, include: module(s), command(s), expected artifact/log snippet, and known caveats.

**Acceptance criteria**
- Every major claim in paper abstract/sequence table has a corresponding evidence row.
- At least one command per claim can be run in CI/local to reproduce evidence.
- Caveats section explicitly lists unresolved assumptions.

---

### P0.3 — Deterministic replication harness for ab-initio runs
**Problem**: Hard to compare regressions across changes without stable run outputs.

**Tasks**
- Add script (`scripts/repro_ab_initio.sh`) that runs both modes with fixed seeds and captures logs.
- Emit machine-readable summary (`.json` or `.csv`) with step, name, ν, κ, ρ, bar, source.
- Add diff helper (`scripts/compare_runs.py` or shell equivalent) for baseline vs candidate run.

**Acceptance criteria**
- Two repeated runs on unchanged code produce identical summary files.
- Summary includes all 15 steps and source attribution (ENUM/MCTS/REF).
- A single command generates artifacts under a predictable output folder.

---

### P0.4 — Formalize κ policy as a first-class configuration
**Problem**: κ mismatch is acknowledged but not operationalized as explicit policy.

**Tasks**
- Introduce `KappaMode` (e.g., `PaperKappa`, `EntryKappa`, `BitCostKappa`, `HybridSuspFloor`).
- Thread it through evaluation and reporting layers.
- Default remains current publication-compatible behavior, with explicit strict options.

**Acceptance criteria**
- CLI flag selects κ mode and prints chosen mode in run header.
- Report table includes `κ_used` and at least one auxiliary κ metric column.
- Unit tests verify suspension κ floor policy behavior.

---

## P1 — Search Quality and Throughput

### P1.1 — Strengthen rollout validity checks in MCTS
**Problem**: Rollouts can still produce low-quality/ill-typed candidates, weakening MCTS efficiency.

**Tasks**
- Integrate `TelescopeCheck` in rollout completion path (not just enum pipeline).
- Add rejection counters by failure class and expose them in `MCTSStats`.
- Penalize repeatedly invalid action paths.

**Acceptance criteria**
- MCTS stats print valid vs rejected rollout counts.
- Invalid rollout ratio decreases relative to current baseline.
- Best-candidate quality (minimal overshoot among viable) improves or matches baseline at κ>3 steps.

---

### P1.2 — Implement progressive widening in MCTS
**Problem**: Child expansion can be too broad early, diluting visits.

**Tasks**
- Add widening schedule as function of node visits.
- Expand top-priority actions first; delay long-tail actions.
- Add config knobs and ablation logging.

**Acceptance criteria**
- Configurable widening parameters available in `MCTSConfig`.
- Ablation run demonstrates equal/better best reward at same iteration budget.
- Node-visit distribution shows less shallow over-expansion.

---

### P1.3 — Separate ENUM and MCTS budget regimes cleanly
**Problem**: ENUM can dominate where MCTS should contribute, obscuring value of AI search.

**Tasks**
- Define non-overlapping budget windows (e.g., ENUM κ≤k₀, MCTS κ>k₀).
- Add per-step source contribution report (winner and runner-up by source).
- Add optional “MCTS-only beyond threshold” experiment mode.

**Acceptance criteria**
- Run summary includes per-step source comparison table.
- MCTS contribution is measurable (wins or near-wins) in designated regime.
- Experiment mode produces reproducible outputs with fixed seed.

---

### P1.4 — Add tri-metric reporting for κ diagnostics
**Problem**: Hard to reason about κ mismatch without side-by-side numbers.

**Tasks**
- Report `κ_entry`, `κ_bitcost`, and `κ_used` for each selected step.
- Add aggregate table and divergence stats from paper κ.

**Acceptance criteria**
- End-of-run report includes all three κ metrics for 15 steps.
- Divergence summary highlights largest offenders (e.g., suspensions).
- Documentation explains interpretation of each κ metric.

---

## P2 — Verification Expansion and Productization

### P2.1 — Agda Rosetta bridge for selected canonical steps
**Problem**: Machine-checked pathway from MBTT telescope discovery to Agda artifacts is still manual/TODO.

**Tasks**
- Define export format from discovered telescopes to Agda stubs.
- Implement for a minimal subset (Universe, Pi, S1, DCT skeleton).
- Add checker script that validates exported files typecheck (where environment supports).

**Acceptance criteria**
- Export command generates deterministic Agda files for selected steps.
- Typecheck pass documented for supported environment.
- Unsupported env paths fail with clear warning (not silent).

---

### P2.2 — CI pipeline for reproducibility + drift detection
**Problem**: No automated guardrails for doc/code/claim drift.

**Tasks**
- Add CI job(s) for strict and paper-calibrated regression summaries.
- Add doc consistency check (mode tables and key claim tags).
- Upload run artifacts for PR comparison.

**Acceptance criteria**
- CI publishes per-PR summary artifacts.
- Failing regression is visible when sequence order or key metrics change unexpectedly.
- Doc consistency check fails on known contradiction patterns.

---

### P2.3 — Public benchmark pack (for external review)
**Problem**: External reviewers need a concise, repeatable benchmark set.

**Tasks**
- Create `benchmarks/` with 3 canonical experiment profiles:
  1. Paper-calibrated replay,
  2. Strict-computed discovery,
  3. MCTS ablation.
- Provide expected output signatures and run-time envelope.

**Acceptance criteria**
- One-command benchmark runner produces all three reports.
- Outputs include version hash + config snapshot.
- Reviewer can validate profile pass/fail without reading source code.

---

## Suggested sequencing (next 4 weeks)

1. **Week 1 (P0)**: P0.1 + P0.2.
2. **Week 2 (P0/P1)**: P0.3 + P0.4.
3. **Week 3 (P1)**: P1.1 + P1.2.
4. **Week 4 (P1/P2)**: P1.3 + P1.4, then start P2.1.

## Definition of done for this queue

- P0 items complete and merged.
- Strict/paper mode behavior is auditable from docs + logs + code without ambiguity.
- Repro harness artifacts are stable and versioned.
