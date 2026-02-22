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
├── sweep_figure.py          # Sensitivity analysis (2601-cell weight sweep)
├── paper_improvement_plan.md    # Status tracker — what's done, what's next
└── remaining_priority2_plan.md  # Detailed plan for inference-rule algorithm
```

## Key Concepts

- **Generative Capacity** ($\nu$): Novelty metric. Defined as the count of
  atomic inference rules (Introduction, Elimination, Computation) added to
  the library's derivation logic by a candidate structure.
- **Spectral Decomposition**: $\nu$ projects onto three orthogonal axes —
  Grammar ($\nu_G$, Intro), Capability ($\nu_C$, Elim), Homotopy ($\nu_H$,
  Comp) — with approximately equal weight. This is an emergent property,
  not a parameter choice.
- **Construction Effort** ($\kappa$): Definitional complexity of a candidate.
- **Efficiency** ($\rho = \nu / \kappa$): Selection score.
- **Integration Latency** ($\Delta_n = F_n$): Fibonacci-governed cost of
  sealing a structure against the library.
- **Selection Bar**: $\text{Bar}_n = \Phi_n \cdot \Omega_{n-1}$ — the
  rising threshold each candidate must clear.
- **Coherence Window** ($d$): Depth of historical context for obligations.
  $d = 2$ for HoTT (gives Fibonacci), $d = 1$ for extensional (stagnates).

## The Haskell Engine

### Building and Running

```bash
cd engine && cabal build
cabal run pen-engine        # Full 10-phase analysis + synthesis
cabal run uniform-nu        # Uniform novelty computation (all 15 steps)
cabal run pen-engine -- --window 1   # Stress-test with d=1 (stagnation)
cabal run pen-engine -- --window 3   # Stress-test with d=3
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
- `UniformNu.hs` — Before/after type inhabitation comparison. Works for 13/15
  steps; fails at Witness and Π/Σ (misses Elimination rules).
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

**Layer 4 — Selection Loop:**
- `Synthesis.hs` — Main discovery loop: compute bar, generate candidates,
  evaluate $\rho$, select minimal overshoot. Implements the 5 PEN axioms.
- `Simulation.hs` — Paper-mode replay using hardcoded values (for validation).
- `CoherenceWindow.hs` — $d$-Bonacci sequences ($d=1$: constant, $d=2$: Fibonacci).

**Layer 5 — Support:**
- `Parallel.hs` — 8-core parallel map utilities.
- `Manifest.hs` — JSON manifest loader (stubbed).

### Simulation Modes

The engine supports multiple evaluation modes in `Simulation.hs`:
- **PaperMode** — Replays hardcoded $\nu$/$\kappa$ from the paper table.
- **CapabilityMode** — Uses hand-tuned capability rules.
- **ComputedMode** — Uses Kolmogorov-based novelty.

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
| §2 | The Model: state, candidates, dual-cost, **Generative Capacity**, selection |
| §3 | Coherence Window Theorems ($d=1$ extensional, $d=2$ intensional) |
| §4 | Complexity Scaling Theorem ($\Delta_n = F_n$) |
| §5 | Combinatorial Novelty Theorem (superlinear growth, Combinatorial Schema Synthesis) |
| §6 | **Spectral Decomposition** (equal-weight property, 12.7% island, sweep) |
| §7 | Computational verification (engine, schema canonicality, uniform $\nu$) |
| §8 | Discussion (proved/assumed/open, limitations, falsifiability) |

Build: `pdflatex pen_unified.tex` (run twice for cross-references).

## Current Status and Next Steps

See `paper_improvement_plan.md` for the full tracker. In brief:

**Completed:**
1. Integration Trace Principle — proved, machine-checked at steps 8-9.
2. Generative Capacity reframing — novelty is now a single intrinsic metric;
   Spectral Decomposition is an emergent property.
3. Inference-rule counter — 15/15 exact match, all steps verified.
4. DCT singularity resolved — ν(DCT) = 105 via Combinatorial Schema Synthesis
   (replaces incorrect Lattice Tensor Product formula of 150).
5. Kolmogorov κ formalized — κ(X|B) is now Conditional Kolmogorov Complexity
   via MBTT encoding. S³ ambiguity resolved (13 bits suspension vs 23 bits native).
   Divergence theorem strengthened with Logarithmic Effort Growth.

**Next research steps (priority order):**
1. Theoretical derivation of $d = 2$ via adjoint functor argument.
2. Tangent Topos hypothesis (why the sequence terminates).

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
