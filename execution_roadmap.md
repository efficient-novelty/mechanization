# Execution Roadmap: Fully Autonomous 15-Step PEN Discovery

## 1. Goal

Deliver a claim-grade implementation where the 15-step Generative Sequence is discovered by the PEN loop without step-indexed reference injection, name-priority selection, or paper-value substitution in discovery mode.

## 2. Target Claim (Engineering Form)

A run is claim-grade only if all conditions hold:

1. Candidate pool contains only generated candidates (no injected `referenceTelescope step`).
2. Winner selection is one objective across all modes: minimal overshoot with fixed deterministic tie-breakers.
3. Discovery scoring uses no paper lookup tables (`paperNuByName`, `paperKappaByName`).
4. Library insertion uses only the selected candidate output (no step fallback to `genesisLibrarySteps`).
5. Discovery path has no dependency on step-indexed target assets.
6. CI proves the discovery executable respects 1-5.

## 3. Scope Boundary

In scope:

- `engine/src/RunAbInitio.hs`
- `engine/src/TelescopeEval.hs`
- `engine/src/Telescope.hs`
- `engine/src/Main.hs`
- `engine/src/Generator.hs` and/or MBTT discovery entry path
- CI/tests and reporting scripts
- manuscript updates in `pen_paper.tex`

Out of scope:

- Removal of benchmark/reference assets entirely. They remain for comparison lanes, but are hard-separated from discovery lanes.

## 4. Workstreams and PR Plan

## Phase 0: Baseline Freeze and Traceability

### PR-0.1: Baseline snapshot

Tasks:

- Capture current discovery outputs and logs for all existing modes.
- Save reproducibility artifacts under `runs/baseline_target_aware/`.

Acceptance criteria:

- Baseline run artifacts and command transcript committed.

---

## Phase 1: Hard De-Targeting of Discovery Loop

### PR-1.1: Remove reference-candidate injection from discovery mode

Code touchpoints:

- `engine/src/RunAbInitio.hs` around current reference evaluation and `rawCandidates` assembly.

Tasks:

- Delete `referenceTelescope step` injection from discovery candidate pool.
- Keep reference evaluation only in explicit benchmark mode.

Acceptance criteria:

- Discovery run candidate list never contains source `REF`.
- Existing benchmark lane can still compare against references.

### PR-1.2: Unify selection objective

Code touchpoints:

- `engine/src/RunAbInitio.hs` selection block (`selectBest`, `useMaxRho`, `kappaFirst`, canonical tier).

Tasks:

- Replace mode-dependent selection ranking with one ranking function:
  - Primary: minimal overshoot (`rho - bar`, non-negative by admissibility filter).
  - Tie-break 1: lower `kappa`.
  - Tie-break 2: stable canonical key hash (or deterministic expression encoding).
- Move alternative rankers behind explicit ablation flag names that are disallowed for claim-grade runs.

Acceptance criteria:

- One default ranking path in all discovery modes.
- Claim-grade run fails fast if ablation flags are enabled.

### PR-1.3: Remove paper fallback insertion

Code touchpoints:

- `engine/src/RunAbInitio.hs` insertion block currently using `genesisLibrarySteps` fallback.

Tasks:

- Ensure inserted entry is always derived from selected candidate.
- Keep paper fallback only in replay/benchmark executable, not discovery.

Acceptance criteria:

- Discovery insertion path has zero dependency on `genesisLibrarySteps`.

---

## Phase 2: Decouple Evaluation From Canonical Target Knowledge

### PR-2.1: Make strict structural evaluation default for discovery

Code touchpoints:

- `engine/src/RunAbInitio.hs` mode parsing and default mode.
- `engine/src/TelescopeEval.hs` `EvalMode` use in discovery path.

Tasks:

- Default discovery mode to strict paper-independent mode.
- Require explicit `--paper-calibrated-benchmark` for paper-backed scoring.

Acceptance criteria:

- Default discovery run does not call `effectiveNu/effectiveKappa`.

### PR-2.2: Remove name-prerequisite ladder from discovery gating

Code touchpoints:

- `engine/src/TelescopeEval.hs` `detectCanonicalName`, `hasPrerequisites`.

Tasks:

- Split naming into post-hoc semantic labeling only.
- Replace name-chain prerequisites with generic structural/capability constraints in generation/checking layers.

Acceptance criteria:

- Discovery scoring and selection are invariant under renaming of candidates.

### PR-2.3: Eliminate canonical-name-conditioned capability assignment in discovery

Code touchpoints:

- `engine/src/TelescopeEval.hs` `telescopeToCandidate` capability assignment.

Tasks:

- Compute capability flags from AST and library structure only.
- Keep canonical names only for reporting, not for unlocking capabilities.

Acceptance criteria:

- If names are scrambled post hoc, discovery ordering does not change.

---

## Phase 3: Search Policy Neutralization

### PR-3.1: Remove step-index-shaped search parameters

Code touchpoints:

- `engine/src/RunAbInitio.hs` `mctsKappaEst` and any step banding.

Tasks:

- Replace step-index heuristics with state-derived policy (for example, based on last accepted kappa and candidate frontier statistics).
- Ban `genesisLibrarySteps` lookups in discovery search scheduling.

Acceptance criteria:

- Discovery path has no `step -> expected kappa` table.

### PR-3.2: Isolate staged generator path from claim-grade executable

Code touchpoints:

- `engine/src/Generator.hs`, `engine/src/Main.hs`, discovery entrypoint.

Tasks:

- Keep `Generator.hs` as benchmark/smoke path only.
- Claim-grade executable uses MBTT-first typed generation only.

Acceptance criteria:

- Claim-grade executable imports no hardcoded staged candidate names.

---

## Phase 4: CI Leakage Guards and Claim-Grade Tests

### PR-4.1: Static leakage guards

Tasks:

- Add CI check that claim-grade modules must not reference:
  - `referenceTelescope`
  - `genesisLibrarySteps`
  - `paperNuByName`
  - `paperKappaByName`
  - any `REF` source insertion

Acceptance criteria:

- CI fails on any forbidden symbol use in claim-grade path.

### PR-4.2: Runtime invariance tests

Tasks:

- Add tests:
  - Name scrambling invariance (ordering unchanged).
  - No-reference candidate source invariant.
  - Strict mode no-paper-lookup trace assertion.
  - Single-objective ranker path assertion.

Acceptance criteria:

- All invariance tests pass in CI.

### PR-4.3: End-to-end discovery test

Tasks:

- Add one deterministic claim-grade run test that checks:
  - 15 realized steps
  - bar clearance each step
  - reproducible output hash/log signature

Acceptance criteria:

- CI publishes `runs/claim_grade_latest/REPORT.txt` and machine-readable JSON.

---

## Phase 5: Paper and Artifact Alignment

### PR-5.1: Rewrite claim language to match implementation class

Code touchpoints:

- `pen_paper.tex` sections currently describing reference injection and bounded local optimality.

Tasks:

- If Phases 1-4 complete, upgrade text to true autonomous discovery over bounded typed manifold without target injection.
- Clearly separate benchmark/replay lanes from claim-grade discovery lane.

Acceptance criteria:

- No contradictions between paper claims and executable behavior.

### PR-5.2: Add claim-grade appendix

Tasks:

- Document exact command, seed policy, constraints, and CI artifact paths.
- Include a "target-knowledge exclusion checklist" in the manuscript appendix.

Acceptance criteria:

- Independent reviewer can reproduce claim-grade run from repository docs only.

## 5. Delivery Milestones

Milestone M1 (De-targeted loop):

- Complete Phase 1.
- Output: discovery run with no reference injection and no paper fallback insertion.

Milestone M2 (Evaluator neutrality):

- Complete Phase 2.
- Output: discovery ordering invariant to post-hoc naming.

Milestone M3 (Policy-neutral search):

- Complete Phase 3.
- Output: no step-index target schedule in discovery.

Milestone M4 (Claim-grade CI):

- Complete Phase 4.
- Output: automated leakage guards + deterministic 15-step run artifact.

Milestone M5 (Paper lock):

- Complete Phase 5.
- Output: manuscript claim language fully aligned with implementation.

## 6. Definition of Done

Done means all are true:

1. Claim-grade executable passes CI leakage guards.
2. Deterministic run discovers and constructs all 15 steps from empty library.
3. No target tables/references are used in discovery generation, scoring, or selection.
4. Reproducibility artifact is generated automatically in CI.
5. Paper text and repository docs describe the same claim class.

## 7. Immediate Next Actions (first 3 PRs)

1. Implement PR-1.1 and PR-1.3 together in `RunAbInitio.hs`.
2. Implement PR-1.2 (single ranker) and add a test for ranking path.
3. Implement PR-2.1 (strict default discovery mode) and trace assertion for no paper lookup.
