# PEN Engine Issue Queue (P0 / P1 / P2)

This queue translates current architecture findings and roadmap notes into executable work items with clear acceptance criteria.

## Priority Definitions

- **P0**: Blocks scientific validity or introduces claim-risk in current paper narrative.
- **P1**: Improves discovery quality, runtime, and reproducibility but does not block core claim.
- **P2**: Scale-up, polish, and long-horizon verification tasks.

---

## P0 — Claim Integrity: Desugared Kappa + Validation

### P0.2 — Implement desugared kappa (Clause Count of Desugared AST)
**Problem**: Raw `teleKappa` (entry count) is a compression artifact that makes suspensions degenerate (k=1 for Susp(S1) despite 4 implicit core judgments). The `max 3` suspension floor is a hack.

**Tasks**
- Define `CoreJudgment` data type: Formation, Introduction, Elimination, Computation, PathAttach.
- Implement `desugarTelescope :: Telescope -> [CoreJudgment]` that expands macro-like entries.
  - `Susp(Lib i)` desugars to Formation + north + south + meridian (4 judgments).
  - Native HITs desugar to their explicit clauses.
  - Pi/Sigma desugars to Pi-formation + Lam-intro + App-elim (3 judgments).
- Implement `desugaredKappa :: Telescope -> Int` = `length . desugarTelescope`.
- Remove the suspension kappa floor (`max 3 (teleKappa tele)`) from `strictKappa`.
- Wire desugared kappa into `TelescopeEval.hs`.

**Acceptance criteria**
- Suspension kappa floor is eliminated entirely.
- Native S1 gets kappa=3, Susp(S0) gets kappa>=3 (desugared).
- All 15 reference telescopes have principled, documented desugared kappa values.
- No more `max 3` hack in `strictKappa`.

---

### P0.3 — Validate Structural Mode Results + Commit Results Table
**Problem**: StructuralNu mode (`--structural`) discovers all 15 steps with steps 1-10 matching
paper values exactly. Steps 11-14 overcount due to axiomaticNuC scaling. Need to document results
and validate against paper narrative.

**Tasks**
- Document the structural mode results table in physics_creation.md.
- Verify that the discovered sequence ordering is correct for all 15 steps.
- Analyze the axiomaticNuC overcount for steps 11-14 and decide if paper v values should be updated.
- Run d=1 and d=3 stress tests with structural mode.
- Compare StructuralNu results with strict (UniformNu) results side by side.

**Acceptance criteria**
- Results table committed to physics_creation.md with all 15 steps.
- Steps 1-15 discovered in correct order (ACHIEVED: all 15 canonical names).
- Each step's v and kappa are fully justified by AST analysis (no paper tables).
- DCT ν=103 > 100, meta-theorems documented.
- Axiomatic step overcount (steps 11-14) documented with explanation.

---

### P0.4 — Deterministic replication harness for ab-initio runs
**Problem**: Hard to compare regressions across changes without stable run outputs.

**Tasks**
- Add script (`scripts/repro_ab_initio.sh`) that runs both modes with fixed seeds and captures logs.
- Emit machine-readable summary (`.json` or `.csv`) with step, name, v, kappa, rho, bar, source.
- Add diff helper (`scripts/compare_runs.py` or shell equivalent) for baseline vs candidate run.

**Acceptance criteria**
- Two repeated runs on unchanged code produce identical summary files.
- Summary includes all 15 steps and source attribution (ENUM/MCTS/REF).
- A single command generates artifacts under a predictable output folder.

---

## P1 — Search Quality and Repurposing

### P1.1 — Repurpose UniformNu as post-hoc analysis (not fitness function)
**Problem**: UniformNu is useful for measuring library coupling / amplification factor but should not be the core fitness function.

**Tasks**
- Remove UniformNu from the optimization loop (replaced by StructuralNu in P0.1).
- Keep UniformNu as a post-hoc analysis pass: compute and report schema counts for each discovered step.
- Use UniformNu output to validate that discovered structures have expected compositional reach.
- Report both StructuralNu (v used for selection) and UniformNu (v_amplification for analysis) in output tables.

**Acceptance criteria**
- UniformNu is not called during the selection loop.
- Post-hoc analysis table shows both metrics side by side.
- Documentation explains the distinct roles of each metric.

---

### P1.2 — Strengthen rollout validity checks in MCTS
**Problem**: Rollouts can still produce low-quality/ill-typed candidates, weakening MCTS efficiency.

**Tasks**
- Integrate `TelescopeCheck` in rollout completion path (not just enum pipeline).
- Add rejection counters by failure class and expose them in `MCTSStats`.
- Penalize repeatedly invalid action paths.

**Acceptance criteria**
- MCTS stats print valid vs rejected rollout counts.
- Invalid rollout ratio decreases relative to current baseline.
- Best-candidate quality (minimal overshoot among viable) improves or matches baseline at kappa>3 steps.

---

### P1.3 — Implement progressive widening in MCTS
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

### P1.4 — Formalize kappa policy as a first-class configuration
**Problem**: Multiple kappa metrics exist; need explicit policy selection.

**Tasks**
- Introduce `KappaMode` (e.g., `DesugaredKappa`, `EntryKappa`, `BitCostKappa`).
- Thread it through evaluation and reporting layers.
- Default is `DesugaredKappa` (from P0.2).

**Acceptance criteria**
- CLI flag selects kappa mode and prints chosen mode in run header.
- Report table includes `kappa_used` and auxiliary kappa metric columns.
- Unit tests verify each kappa mode's behavior.

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
  2. Strict-computed discovery (StructuralNu),
  3. MCTS ablation.
- Provide expected output signatures and run-time envelope.

**Acceptance criteria**
- One-command benchmark runner produces all three reports.
- Outputs include version hash + config snapshot.
- Reviewer can validate profile pass/fail without reading source code.

---

## Suggested sequencing

1. **Week 1 (P0)**: P0.1 (StructuralNu) + P0.2 (Desugared Kappa).
2. **Week 2 (P0)**: P0.3 (Strict Mode 2.0 validation) + P0.4 (Replication harness).
3. **Week 3 (P1)**: P1.1 (Repurpose UniformNu) + P1.4 (Kappa policy).
4. **Week 4 (P1/P2)**: P1.2 + P1.3 (MCTS improvements), then start P2.1.

## Definition of done for this queue

- P0 items complete and merged.
- StructuralNu is the definitive fitness function for the core search loop.
- Desugared kappa eliminates all artificial floors.
- Strict mode discovers Steps 1-14 (minimum 1-13) in correct order.
- Repro harness artifacts are stable and versioned.
