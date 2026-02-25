# PEN — Principle of Efficient Novelty

## What This Project Is

A mathematical theory (with computational engine and formal proofs) proposing
that mathematical structures emerge in a deterministic order governed by
efficiency optimization. Starting from an empty library, the model produces the
**Generative Sequence**: 15 structures — from dependent types through spheres,
cohesion, and differential geometry to the Dynamical Cohesive Topos — with
Fibonacci-governed integration costs.

The core claim: intensional type theory (HoTT) has Coherence Window $d = 2$,
which forces Fibonacci cost scaling, and the Golden Ratio $\varphi$ is the
dominant eigenvalue of mathematical evolution.

## Project Layout

```
PEN/
├── pen_unified.tex          # THE PAPER — single authoritative write-up
├── pen_unified.pdf          # Compiled paper
├── engine/                  # Haskell synthesis engine (~3,000 lines)
│   ├── pen-engine.cabal     # Build config (GHC 9.6+, cabal 2.4+)
│   └── src/                 # 20 Haskell modules (see below)
├── agda/                    # Cubical Agda formal proofs
│   ├── PEN.agda             # Main module
│   ├── Saturation/          # Abstraction barrier proofs (steps 8-9)
│   ├── Core/                # Fibonacci sequence, Δ/τ definitions
│   ├── ObligationGraph/     # Interface/recurrence proofs
│   └── Experiments/         # Circle, sphere, torus elimination traces
├── scripts/                 # Automation scripts
│   ├── benchmark.sh         # One-command 4-profile verification
│   ├── gen_latex_table.sh   # Generate §1 LaTeX table from engine
│   ├── repro_ab_initio.sh   # Multi-mode replication harness
│   └── compare_runs.sh      # Diff two run directories
├── .github/workflows/       # CI pipeline
│   └── pen-engine.yml       # Build + acceptance + structural + paper-calibrated
├── sweep_figure.py          # Sensitivity analysis (2601-cell weight sweep)
├── paper_improvement_plan.md    # Status tracker — what's done, what's next
└── remaining_priority2_plan.md  # Detailed plan for inference-rule algorithm
```

## Key Concepts

- **Generative Capacity** ($\nu$): Novelty metric. Defined as the marginal
  expansion of the library's operational repertoire — the count of derivation
  schemas (type-inhabitation patterns classifiable as Introduction, Elimination,
  Computation) added by a candidate structure. For foundational steps (1–9),
  these coincide with core inference rules. For library specification steps
  (10–14), these are the derivation schemas natively unlocked by the API spec.
- **Spectral Decomposition**: $\nu$ projects onto three orthogonal axes —
  Grammar ($\nu_G$, Intro), Capability ($\nu_C$, Elim), Homotopy ($\nu_H$,
  Comp) — with approximately equal weight. This is an empirical observation
  about the Generative Sequence, not a parameter choice or derived theorem.
- **Construction Effort** ($\kappa$): Clause count of the candidate's
  specification. For library specifications, this is the Kolmogorov complexity
  (AST node count) of the API spec.
- **Efficiency** ($\rho = \nu / \kappa$): Selection score.
- **Integration Latency** ($\Delta_n = F_n$): Fibonacci-governed cost of
  sealing a structure against the library. Arises from PEN's **maximal
  interface density** assumption: each candidate must seal against the entire
  exported interface of the past $d$ layers. This is a modeling choice of
  the PEN framework (analogous to universal coupling in physics), not a
  native requirement of HoTT.
- **Selection Bar**: $\text{Bar}_n = \Phi_n \cdot \Omega_{n-1}$ — the
  rising threshold each candidate must clear.
- **Coherence Window** ($d$): Depth of historical context for obligations.
  $d = 2$ for HoTT (gives Fibonacci), $d = 1$ for extensional (stagnates).
- **Library Specifications vs. Defined Terms**: Steps 10–14 are library
  specifications (non-derivable API packages, $\nu > 0$), not defined terms
  (derivable from prior structures, $\nu = 0$). The derivability criterion
  resolves the scoring methodology: e.g., ℕ's addition is derivable from
  rec_ℕ ($\nu = 0$), but a metric's Hodge star is not derivable from
  cohesion ($\nu > 0$).

## The Haskell Engine

### Building and Running

```bash
cd engine && cabal build all
cabal run ab-initio -- --structural                # Publication-grade discovery (15/15)
cabal run ab-initio -- --structural --csv out.csv  # With CSV output
cabal run ab-initio                                # Paper-calibrated replay
cabal run ab-initio -- --structural --window 1     # d=1 stress test
cabal run ab-initio -- --structural --window 3     # d=3 stress test
cabal run ab-initio -- --structural --no-canonical-priority  # Ablation
cabal run ab-initio -- --structural --max-rho      # Ablation (max ρ)
cabal run acceptance                               # 46-test regression suite
cabal run agda-bridge                              # Generate 15 Agda stubs
cabal run agda-bridge -- --step 5 --stdout         # Preview single step
cabal run agda-bridge -- --check                   # Verify determinism
cabal run pen-engine                               # Full 10-phase analysis
cabal run uniform-nu                               # Uniform novelty computation
./scripts/benchmark.sh                             # Full 4-profile verification
./scripts/gen_latex_table.sh table.tex             # Generate LaTeX table
```

### Module Architecture

**Layer 1 — Type System:**
- `Types.hs` — AST: `TypeExpr` (TUnit, TVoid, TArrow, TProd, TCoprod, TOmega,
  TSusp, THIT, TFlat, TSharp, TNext, TEventually, TInf, TTangent, ...)
- `Inhabitation.hs` — Conservative inhabitation checker (19 rules). Returns
  `Inhabited Witness | NotInhabited Reason | Unknown`. Never falsely positive.
- `Enumerate.hs` — Type expression enumeration up to complexity bound.
- `Equivalence.hs` — Confluent rewrite system for HoTT isomorphisms.

**Layer 2 — Novelty Computation:**
- `Cluster.hs` — Schema-based clustering: library atoms → L, candidate → X,
  count distinct non-trivial schemas + homotopy bonus.
- `GenuineNu.hs` — Dispatches $\nu$ by structure category (Foundation → hardcoded,
  HIT → Cluster, Modal → operator count, DCT → lattice tensor product).
- `UniformNu.hs` — Before/after type inhabitation comparison with adjoint
  completion. Works for all 15/15 steps (adjoint completion closes the
  Extensional Boundary at steps 3–4).
- `Independence.hs` — Trivial schema filter (derivable for any inhabited type).
- `ProofRank.hs` — Schema enumeration, schematization, normalization.
- `Capability.hs` — Hand-tuned capability rules (older approach, for comparison).
- `KappaNu.hs` — Paper reference values (`paperNu`, `paperKappa`) + old Kolmogorov κ.
- `Kolmogorov.hs` — MBTT-based Conditional Kolmogorov Complexity $\kappa(X \mid \mathcal{B})$.
  Prefix-free binary encoding with Elias Gamma library pointers. All 15 steps specified.
- `ExactNu.hs` — Exact depth-1/2/3 oracle using all library atoms.

**Layer 3 — Candidate Generation:**
- `Generator.hs` — Generates candidates from 9 categories (Foundation, Former,
  HIT, Suspension, Map, Algebra, Modal, Axiom, Synthesis), gated by prerequisites.
- `HITEnum.hs` — Parametric HIT enumeration by cost.
- `TheoryState.hs` — Library state, type former unlocking chain.

**Layer 4 — Ab Initio Synthesis (primary):**
- `Telescope.hs` — Core data types (TeleEntry, Telescope, MBTTExpr), structural
  analysis, reference telescopes, desugared κ, classification.
- `TelescopeGen.hs` — Type-directed generator, structural action gating.
- `TelescopeEval.hs` — Classification, naming, EvalMode dispatch, evaluation bridge.
- `TelescopeCheck.hs` — Conservative well-formedness checker.
- `StructuralNu.hs` — AST rule extraction (ν_G + ν_H + ν_C), meta-theorem detectors.
- `MCTS.hs` — Monte Carlo Tree Search (full UCT, progressive widening).
- `RunAbInitio.hs` — Ab initio engine (structural / paper-calibrated / strict modes).
- `RunAcceptance.hs` — 46-test acceptance suite.
- `AgdaExport.hs` — MBTT telescope → Cubical Agda translation.
- `RunAgdaBridge.hs` — Agda Rosetta bridge CLI (generates 15 stub files).

**Layer 5 — Legacy Selection Loop:**
- `Synthesis.hs` — Original discovery loop (predates telescope-based ab initio).
- `Simulation.hs` — Paper-mode replay using hardcoded values (for validation).
- `CoherenceWindow.hs` — $d$-Bonacci sequences ($d=1$: constant, $d=2$: Fibonacci).

**Layer 6 — Support:**
- `Parallel.hs` — 8-core parallel map utilities.
- `Manifest.hs` — JSON manifest loader (stubbed).

### Evaluation Modes

The ab initio engine (`RunAbInitio.hs`) supports three evaluation modes:
- **EvalPaperCalibrated** — Uses paper ν/κ for canonical names (replay/comparison).
- **EvalStrictComputed** — Uses UniformNu + strictKappa (no paper lookups).
- **EvalStructural** — Uses StructuralNu (AST rule extraction, publication-grade).

The `--structural` flag selects the publication-grade mode. All scoring, bar
computation, MCTS rollout guidance, and library insertion are paper-independent.

The uniform algorithm (`UniformNu.hs`) runs as a separate executable.

### Key Data Types

```haskell
-- engine/src/Types.hs
data LibraryEntry = LibraryEntry
  { leName :: String, leConstructors :: Int, lePathDims :: [Int]
  , leHasLoop :: Bool, leIsTruncated :: Maybe Int }

-- engine/src/KappaNu.hs
paperNu  :: Int -> Int   -- paperNu 3 = 2 (Witness), paperNu 16 = 105 (DCT)
paperKappa :: Int -> Int  -- paperKappa 3 = 1, paperKappa 16 = 8
```

## The Agda Proofs

### Building

```bash
cd agda
agda --cubical --safe PEN.agda          # Type-check everything
agda --cubical --safe Test/Fibonacci.agda  # Run Fibonacci validation
```

Requires Agda 2.6.4+ with the `cubical` library.

### What's Proved

- **Fibonacci recurrence** ($\Delta_{n+1} = \Delta_n + \Delta_{n-1}$ for $d=2$)
  — `ObligationGraph/Recurrence.agda`
- **Cumulative sum** ($\tau_n = F_{n+2} - 1$) — `Core/Sequence.agda`
- **Abstraction barrier at step 8** — Group B obligations discharged from
  opaque $L_7$ record, no PropTrunc import — `Saturation/AbstractionBarrier.agda`
- **Abstraction barrier at step 9** — Hopf obligations from opaque $L_8$
  record — `Saturation/AbstractionBarrier9.agda`
- **Obligation traces** for $S^1$, $S^2$, $T^2$, Hopf — `Experiments/`

## The Paper

`pen_unified.tex` is the single authoritative document. Key sections:

| Section | Content |
|---------|---------|
| §1 | Generative Sequence table (15 structures) |
| §2 | The Model: state, candidates, dual-cost, **Generative Capacity**, library API remark, selection |
| §3 | Coherence Window Theorems ($d=1$ extensional, $d=2$ intensional), maximal coupling remark |
| §4 | Complexity Scaling Theorem ($\Delta_n = F_n$) |
| §5 | Combinatorial Novelty Theorem (superlinear growth, Combinatorial Schema Synthesis), Gödelian Horizon |
| §6 | **Spectral Decomposition** (Empirical Observation, equal-weight property, sweep) |
| §7 | Computational verification (engine, 10 rejected candidates, schema canonicality, uniform $\nu$) |
| §8 | Discussion (proved/assumed/open, limitations, falsifiability) |
| §9 | Metamathematical Boundaries (algorithmic criticality, discrete inefficiency, Gödelian Horizon, adjoint completion) |

Build: `pdflatex pen_unified.tex` (run twice for cross-references).

## Current Status and Next Steps

See `MBTT_FIRST_AUTONOMOUS_SYNTHESIS_ROADMAP.md` for the current research focus. Update this file on an ongoing basis to reflect the overall status, progress, learnings, blockers and critical problems to solve.

## Conventions

- The paper uses $\nu_G$ (grammar/syntactic), $\nu_H$ (homotopy/topological),
  $\nu_C$ (capability/logical) for the spectral projections. The engine
  uses these same names in comments but computes them differently depending
  on mode.
- Genesis steps are numbered 1–15 in the paper. The engine's `KappaNu.hs`
  uses indices 1–16 (index 10 = Lie groups, which are absorbed, not realized;
  indices 11–16 = paper steps 10–15).
- $d$ always means the Coherence Window depth (default 2). Do not confuse
  with enumeration depth in the schema-counting algorithm.
