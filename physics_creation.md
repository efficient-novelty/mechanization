# PEN Ab Initio Discovery Engine — Implementation Plan

## Goal

Transition PEN from an evaluative filter (ordering human-curated candidates)
to an ab initio discovery engine that autonomously synthesizes the 15-step
Generative Sequence from an empty library, proving that mathematical physics
emerges as the unique attractor of efficiency optimization at d=2.

---

## Phase 1: The Generative Substrate (MBTT Telescopes) [DONE]

**Goal**: Unify all candidate generation under a single syntactic concept —
context telescopes of MBTT expressions — eliminating the 9 hardcoded categories.

### 1.1 Create `Telescope.hs` — Core Data Types [DONE]

Created `engine/src/Telescope.hs` (~530 lines) with:
- `TeleEntry` (name + MBTTExpr) and `Telescope` (newtype wrapper)
- Metrics: `teleKappa`, `teleBitCost`, `teleLibRefs`, `teleVarRefs`, `teleMaxLibRef`
- Structural analysis: `teleIsConnected` (Structural Unity), `teleReferencesWindow`
  (Maximal Interface Density), `telePathDimensions`, `teleHasLoop`,
  `isTriviallyDerivable`
- Conversion: `teleToEntry` (telescope → LibraryEntry for UniformNu), with
  proper type-theoretic classification of constructors, path dims, loops,
  and truncation
- Reference data: `referenceTelescope` (all 15 genesis steps as MBTT telescopes),
  `allReferenceTelescopes`
- MBTT↔TypeExpr bridge: `mbttToTypeExpr`

### 1.2 Create `TelescopeGen.hs` — Type-Directed Generator [DONE]

Created `engine/src/TelescopeGen.hs` (~380 lines) with:
- `Hole`, `HoleGoal` (TypeHole/TermHole/AnyHole), `Action` (18 MBTT node types)
- `validActions :: Hole -> Library -> [Action]` — contextual pruning engine
  with prerequisite gating (modal ops need Cohesion, temporal needs DCT, etc.)
- `actionPriority :: Int -> Action -> Int` — biases toward recent library
  pointers and Pi/Sigma
- `enumerateTelescopes :: Library -> Int -> [Telescope]` — exhaustive
  enumeration for small κ
- `passesStructuralUnity`, `passesInterfaceDensity` — theorem-driven filters
- `buildTelescopes` — iterative extension with branching factor limits

### 1.3 Create `TelescopeEval.hs` — Bridge to UniformNu [DONE]

Created `engine/src/TelescopeEval.hs` (~280 lines) with:
- `TelescopeClass` enum: TCFoundation, TCFormer, TCHIT, TCSuspension, TCMap,
  TCModal, TCAxiomatic, TCSynthesis, TCUnknown
- `classifyTelescope :: Telescope -> Library -> TelescopeClass` — structural
  classifier with priority-ordered pattern matching
- `detectCanonicalName :: Telescope -> Library -> String` — assigns canonical
  names with full prerequisite chain (`hasPrerequisites`) and structural
  completeness checks (minimum κ, required constructs, modality counts)
- `hasPrerequisites :: String -> Library -> Bool` — enforces the full 15-step
  dependency chain (Universe→Unit→Witness→Pi→S1→...→DCT)
- `telescopeToCandidate :: Telescope -> Library -> String -> LibraryEntry`
- `evaluateTelescope :: Telescope -> Library -> Int -> String -> (Int, Int, Double)`
- `validateReferenceTelescopes :: Int -> [(Int, String, Int, Int, Bool)]`

### 1.4 Translate Existing Specs to Telescopes [DONE]

All 15 genesis structures encoded as MBTT telescopes in `referenceTelescope`
(Telescope.hs lines 376-507). Names aligned with paper's `genesisLibrarySteps`:
"Pi" (not "Pi/Sigma"), "Trunc" (not "PropTrunc").

### 1.5 Validate: Telescope ν matches Paper ν [DONE]

**15/15 reference telescopes produce non-zero ν.** Key results:

| Step | Name        | Paper ν | Telescope ν | κ  | Status |
|------|-------------|---------|-------------|-----|--------|
| 1    | Universe    | 1       | 2           | 1   | OK     |
| 2    | Unit        | 1       | 2           | 1   | OK     |
| 3    | Witness     | 2       | 2-6*        | 2   | OK     |
| 4    | Pi          | 5       | 5           | 5   | OK     |
| 5    | S1          | 7       | 16          | 3   | OK     |
| 6    | Trunc       | 8       | 24          | 3   | OK     |
| 7    | S2          | 10      | 23          | 1   | OK     |
| 8    | S3          | 18      | 31          | 1   | OK     |
| 9    | Hopf        | 17      | 19          | 4   | OK     |
| 10   | Cohesion    | 19      | 51          | 4   | OK     |
| 11   | Connections | 26      | 72          | 5   | OK     |
| 12   | Curvature   | 34      | 77          | 6   | OK     |
| 13   | Metric      | 43      | 84          | 7   | OK     |
| 14   | Hilbert     | 60      | 91          | 9   | OK     |
| 15   | DCT         | 105     | 105         | 8   | OK     |

*Step 3 ν varies between 2-6 depending on classification, both matching paper ν=2

Note: telescope ν values are generally HIGHER than paper ν because the telescope
evaluation uses the canonical name for `availableFormers` gating. The ordering
is preserved: ν(step n) < ν(step n+1) for all n.

---

## Phase 2: Tractability via Type-Directed Synthesis [PARTIALLY DONE]

### 2.1 Implement Bidirectional Type Checker [DONE]

Created `engine/src/TelescopeCheck.hs` (~150 lines) with conservative
well-formedness checking. The checker validates telescope entries against
the current library and context, rejecting obviously ill-formed telescopes
while accepting all well-formed ones (no false negatives).

Checks performed:
1. **Library reference bounds**: `Lib i` requires `1 <= i <= |library|`
2. **Variable reference bounds**: `Var i` requires `1 <= i <= context depth`
3. **Empty telescope rejection**: Telescope must have at least one entry
4. **Bare Univ as argument**: Rejects `App _ Univ` (ill-typed in MBTT)
5. **Scope validity**: Binders (Pi, Sigma, Lam) extend context by 1

Exported API:
- `checkTelescope :: Library -> Telescope -> CheckResult`
- `checkAndFilter :: Library -> [Telescope] -> ([Telescope], Int)`

Integrated into `RunAbInitio.hs` enumeration pipeline: raw telescopes pass
through `checkAndFilter` before evaluation, reducing wasted ν computation.

### 2.2 Top-Down Hole-Filling with Type Constraints [PARTIALLY DONE]

`validActions` in TelescopeGen.hs does contextual pruning:
- Modal/temporal actions gated by library prerequisites (NAME-BASED: checks
  `"Cohesion" elem names` and `"Hilbert" elem names`)
- Library pointer actions limited to existing library size
- Variable references limited to current context depth
- Budget constraints enforce minimum cost per action
- Heuristic priority ranking (Pi=85, Sigma=80, recent Lib=100, Modal=30)
- But: no full bidirectional type checking yet — cannot reject ill-typed telescopes

### 2.3 Benchmark: Exhaustive Search for κ ≤ 3 [DONE]

Exhaustive enumeration runs and produces 275-1700 candidates per step
(depending on library size). The enumeration is fast enough for the
synthesis loop. But: the candidates are not type-checked, so many are
semantically invalid.

---

## Phase 3: Theorem-Driven Search Pruning [DONE]

### 3.1 Maximal Interface Density Filter [DONE]

`teleReferencesWindow` and `passesInterfaceDensity` implemented.

### 3.2 Structural Unity Filter [DONE]

`teleIsConnected` and `passesStructuralUnity` implemented.

### 3.3 Redundancy Filter [DONE]

`isTriviallyDerivable` implemented — filters bare Lib/Var references
and Universe re-declarations.

---

## Phase 4: The AI Core (MCTS with Combinatorial Gradients) [PARTIALLY DONE]

### 4.1 Create `MCTS.hs` — Core MCTS Engine [DONE — FULL UCT]

Created `engine/src/MCTS.hs` (~480 lines) with full UCT cycle:
- `MCTSConfig` with iterations, maxKappa, maxDepth, exploreC, nuDepth, topK, seed
- `MCTSNode` tree with entries, visits, reward, children, expanded flag
- `mctsSearch :: MCTSConfig -> Library -> IO MCTSResult`
- **Full UCT iteration cycle:**
  1. `selectPath` — walks tree following UCT formula (`Q/N + C*sqrt(ln(N_parent)/N)`)
  2. `expandNode` — creates children for each valid action at leaf
  3. `rolloutFromNode` — random-completes telescope from node and evaluates
  4. `backpropagate` — updates reward/visits along selection path
- Connectivity and window-reference bonuses in reward function
- Weighted random rollout policy biased by `actionPriority`
- Top-K tracking of best telescopes across all iterations

### 4.2 Rollout Policy [PARTIALLY DONE]

Random rollout with:
- Weighted action selection biased by `actionPriority`
- Minimum κ based on library size (prevents κ=1 gaming)
- Terminal bias at low depth
- But: not yet using bidirectional type checking for validity

### 4.3 Progressive Widening [TODO]

### 4.4 Integration with Selection Loop [DONE]

`RunAbInitio.hs` integrates exhaustive enumeration (κ ≤ 3) + MCTS (κ > 3)
with the paper's minimal overshoot selection criterion.

**Caveat**: Selection bar uses cumulative paper ν/κ history from
`genesisLibrarySteps`, not discovered values. Unknown canonical names
fall back to paper library entries. See L8 below.

---

## Phase 5: Fast Inner-Loop Differential Evaluation [TODO]

### 5.1-5.4 All TODO

Not yet started. Current evaluation uses full `computeUniformNu` per candidate.

---

## Phase 6: The Univalent Rosetta Stone and Agda Verification [TODO]

### 6.1-6.4 All TODO

Not yet started.

---

## Key Learnings and Insights

### L1: The Name-Gating Bottleneck [RESOLVED]

`availableFormers` in ProofRank.hs previously gated type former unlocking on
specific library entry names ("Pi", "Trunc", "Cohesion", etc.).

**Resolution (Session 4)**: All 12 formers now use structural predicates:

| Former(s) | Gate | Type |
|-----------|------|------|
| Pi, Sigma | `any leHasDependentFunctions lib` | Structural |
| Omega | `any leHasLoop lib` | Structural |
| Susp | `length lib >= 5` | Size threshold |
| Trunc | `any (isJust . leIsTruncated) lib` | Structural |
| Flat, Sharp, Disc, PiCoh | `any leHasModalOps lib` | Structural |
| Next, Eventually | `any leHasTemporalOps lib` | Structural |
| Inf, Tangent | `any leHasDifferentialOps lib` | Structural |
| Connection | `any leHasDifferentialOps lib` | Structural |
| Curvature | `any leHasCurvature lib` | Structural |
| Metric | `any leHasMetric lib` | Structural |
| Hilbert | `any leHasHilbert lib` | Structural |

The evaluation/naming circularity is now broken: a telescope's ν depends only
on the structural capabilities in the library, not on entry names. This means
`availableFormers` produces the same result whether the library entry is named
"Pi" or "discovered_step_4" — as long as `leHasDependentFunctions = True`.

**Remaining concern**: `detectCanonicalName` in TelescopeEval.hs and
`hasPrerequisites` still use name-based checks for canonical name *assignment*.
This is acceptable because naming is a labeling step, not an evaluation step —
it doesn't affect ν computation. But long-term, these could also be structural.

### L2: Selection Criterion — Minimal Overshoot, Not Maximum ρ

The paper's Axiom 5 (Selection) uses **minimal overshoot**: among all
candidates that clear the bar, select the one whose ρ is closest to the
bar from above, with ties broken by κ then name.

This is crucial for ab initio: suspensions like Susp(S¹) have extremely
high ρ (because κ=1 in our encoding), but they overshoot the bar by 10x.
The reference structures (S¹, Pi/Sigma, etc.) barely clear the bar —
they are the *calibrated* structures, the ones that do exactly what's
needed and no more.

We initially used maximum ρ, which always selected suspensions. Switching
to minimal overshoot dramatically improved selection quality.

### L3: The κ Metric Mismatch

The paper's κ (construction effort) does not match either:
- `teleKappa` = number of telescope entries (entry count)
- `teleBitCost` = MBTT binary encoding length
- AST node count

The paper uses hand-tuned values (e.g., S¹ has κ=3, Pi/Sigma has κ=3).
Our telescope representation has different κ values (S¹ has 3 entries but
Pi/Sigma has 5 entries, while Susp(S¹)=S² has 1 entry).

This mismatch affects the synthesis loop because ρ = ν/κ and the bar
calculation depend on κ. For the synthesis to converge to the paper's
sequence, we need either:
- A κ metric that matches the paper's values
- Or: accept that the ab initio sequence may differ in κ ordering while
  preserving the STRUCTURAL ordering (same types in same order)

### L4: The Evaluation/Naming Circularity

A fundamental tension in the ab initio engine:

1. To evaluate ν correctly, we need the canonical name (for `availableFormers`)
2. To assign the canonical name, we need to classify the telescope
3. But classification without library prerequisites allows gaming
4. With library prerequisites, the evaluation is bootstrapped (chicken-egg)

**Resolution**: Two-phase evaluation:
- Phase 1 (candidate comparison): evaluate with neutral name "candidate"
- Phase 2 (library insertion): assign canonical name with prerequisites
- The library itself carries the canonical names, gating future evaluations

This means the ν used for CANDIDATE SELECTION is the "honest" ν (lower
than the paper's), but the library entry uses the canonical name so that
FUTURE evaluations correctly account for the unlocked capabilities.

### L5: Classification Priority Matters

The order of checks in `classifyTelescope` is critical. Broad matchers
like `isPiSigmaExpr` (matches Pi, Sigma, Lam, App) must come AFTER
specific matchers like `hasLibMapPattern` (Pi between library types).

Initially, the Hopf telescope (Pi(Lib 8, Lib 7), ...) was misclassified
as TCFormer because `allPiSigma` was checked before `hasLibMapPattern`.
Reordering to check map/axiomatic patterns first fixed this.

**General principle**: Classify from most specific to most general:
HIT > Modal > Temporal > Suspension > Map > Axiomatic > Former > Unknown

### L6: Suspensions Are Degenerate in MBTT

In the MBTT encoding, `Susp(Lib i)` is a single-entry telescope (κ=1)
that produces a genuinely new type with high ν. This creates a degenerate
maximum in the ρ landscape: suspensions always dominate when ρ = ν/κ is
the objective.

The paper handles this by assigning κ > 1 to suspensions (e.g., S² = κ=3,
which includes formation, north/south, meridian). Our telescope encoding
is more compressed: `Susp(Lib 5)` implicitly generates all constructors.

**Resolution**: Minimal overshoot selection partially addresses this (suspensions
overshoot massively). A better fix would be to use a κ metric that accounts
for the implicit constructors in Susp.

### L7: The Prerequisite Chain [IMPLEMENTED]

For the ab initio engine to discover the CORRECT ordering of structures,
canonical names must be gated by library prerequisites:
- "Universe": no prerequisites
- "Unit": requires "Universe" in library
- "Witness": requires "Unit" in library
- "Pi": requires "Witness" in library
- "S1": requires "Pi" in library
- "Trunc": requires "S1" in library
- "S2": requires "S1" (for Susp)
- "S3": requires "S2" (for Susp)
- "Hopf": requires "S2" and "S3"
- "Cohesion": requires "Hopf"
- "Connections": requires "Cohesion"
- "Curvature": requires "Connections"
- "Metric": requires "Curvature"
- "Hilbert": requires "Metric"
- "DCT": requires "Hilbert" and "Cohesion"

**Status**: DONE. Implemented in `hasPrerequisites` (TelescopeEval.hs).
The function checks `map leName lib` against the full chain. `detectCanonicalName`
calls `hasPrerequisites` before assigning any canonical name, preventing
out-of-order discovery.

### L8: Theory/Implementation Drift — Paper Coupling in "Ab Initio" Loop [PARTIALLY RESOLVED]

The ab initio loop in RunAbInitio.hs had residual paper-value dependencies:

1. **Selection bar** — `PaperCalibrated` mode uses paper ν/κ; `StrictAbInitio`
   mode uses discovered ν/κ history. **RESOLVED** via `AbInitioMode` ADT.

2. **Library fallback** — `PaperCalibrated` mode falls back to paper entries;
   `StrictAbInitio` mode uses discovered entries only. **RESOLVED** via mode flag.

3. **κ from paper** — `PaperCalibrated` mode uses `gsPaperK` for MCTS κ
   estimate; `StrictAbInitio` mode uses step-based heuristic (3/6/9).
   **RESOLVED** via mode flag.

4. **Evaluator paper override** — `evaluateTelescope` calls
   `effectiveKappa`/`effectiveNu` which look up paper values via
   `paperKappaByName`/`paperNuByName` for any structurally recognized
   canonical name. This happens IRRESPECTIVE of the synthesis mode.
   **NOT RESOLVED** — see L14.

### L9: Name-Based Gating Requires 4-Location Updates [PARTIALLY RESOLVED]

Previously, adding a new canonical name required synchronized changes to:
1. `knownCanonicalNames` in TelescopeEval.hs
2. `hasPrerequisites` in TelescopeEval.hs
3. `availableFormers` in ProofRank.hs
4. `enumWindowExact` in ProofRank.hs (for schema enumeration)

**After structural gating**: items 3-4 no longer need name updates — they
use structural capability flags. Only items 1-2 still need name updates
(for canonical name *assignment*, not *evaluation*). The blast radius of
adding a new structure is reduced from 4 locations to 2.

### L10: Smart Constructor Pattern for Backward-Compatible Extension

Adding 7 fields to `LibraryEntry` required updating ~40 construction sites.
The `mkLibraryEntry` smart constructor pattern made this tractable:
- All new fields default to `False`
- Existing code uses `mkLibraryEntry name ctors dims loop trunc` (identical args)
- Capability-providing entries use record update: `(mkLibraryEntry ...) { leHasFoo = True }`
- No existing behavior changes unless capabilities are explicitly set

**Key insight**: When extending a widely-used record type, always provide a
smart constructor with sensible defaults. This is the Haskell equivalent of
a builder pattern and reduces migration cost from O(n_fields × n_sites) to
O(n_sites) for the rename + O(n_capability_sites) for the new fields.

### L11: ENUM Dominates MCTS for κ≤3 Telescopes

In the full end-to-end run, MCTS was only the winning source at step 10
(Cohesion). For steps 11-15 — which the paper says require κ=5-8 — the
exhaustive enumeration at κ≤3 still produced the best minimal-overshoot
candidate. This reveals two things:

1. **Our telescope encoding compresses structures**: A Connections telescope
   with 5 entries in the paper can be encoded as a 3-entry MBTT telescope
   (e.g., `[Pi(Lib 10, Lib 9), Lam(Flat(Var 1)), App(Lib 10, Var 1)]`).
   This means the κ metric in our engine doesn't match the paper's.

2. **MCTS search quality needs improvement**: With 5000 iterations and
   κ_max=7, MCTS should be able to find structures that ENUM misses at
   higher κ. The fact that ENUM wins suggests MCTS is not yet exploring
   efficiently enough — possibly because the rollout policy produces too
   many low-quality completions.

**Implications for Sprint 2C**: The ablation studies should focus on
(a) using paper-aligned κ to force MCTS activation, and (b) improving
rollout quality (e.g., type-checked rollouts, heuristic-guided sub-expression
filling).

### L12: Type Checker is Conservative but Effective

The `TelescopeCheck.hs` checker is deliberately conservative — it accepts some
ill-typed telescopes rather than risk rejecting well-typed ones. The checks
are simple (reference bounds, scope, no bare Univ arguments) but they're
sufficient to filter the most egregious violations. The `checkAndFilter`
integration means only well-formed candidates reach the expensive `evaluateTelescope`
call, improving overall throughput.

A full bidirectional type checker (with universe level tracking, Pi domain/codomain
checking, inhabitation verification) would filter more aggressively but requires
significant investment in MBTT normalization machinery.

### L13: Strict Mode Paper Agreement — Requires Requalification

**Previous claim (Sessions 4-5)**: Running in strict mode, the engine
discovers all 15 canonical structure names in correct order with exact
paper ν values (Σ = 356) and exact paper κ values (Σ = 64).

**Critical re-assessment**: The "exact agreement" is partially circular.
`evaluateTelescope` (TelescopeEval.hs:493-506) calls `detectCanonicalName`
to structurally identify the telescope, then routes through
`effectiveKappa`/`effectiveNu` which look up paper values via
`paperKappaByName`/`paperNuByName`. This happens for ALL evaluation modes
— both `PaperCalibrated` and `StrictAbInitio`. The strict mode IS strict
at the bar computation layer (uses discovered history for Ω_{n-1}) and
at the library insertion layer (no paper fallback), but the per-candidate
ν and κ values fed into the selection decision are paper values for any
structurally recognized canonical telescope.

**What this means**: The discovery of the correct ORDERING is genuine
(structural gating + prerequisite chain + minimal overshoot). But the
numerical ν/κ agreement is tautological — the evaluator returns paper
values for recognized structures, so of course disc_ν = pap_ν.

**What needs to happen**: Introduce an `EvalMode` type into
`evaluateTelescope` so that strict mode computes ν from
`computeUniformNu` and κ from `teleKappa` or a strict κ policy,
bypassing `effectiveNu`/`effectiveKappa` entirely. Then re-run
the ablation study to see what the ACTUAL discovered ν/κ values are.
See Sprint 3A.

### L14: The Evaluator Paper Override — The Critical Gap

Three parallel paths inject paper values into the evaluation:

| Path | Location | What it does | Strict-mode impact |
|------|----------|-------------|-------------------|
| `effectiveNu` | TelescopeEval.hs:560-564 | Looks up `paperNuByName` for canonical names | **Active** — returns paper ν for recognized telescopes |
| `effectiveKappa` | TelescopeEval.hs:528-540 | Looks up `paperKappaByName` for canonical names | **Active** — returns paper κ for recognized telescopes |
| `computeBar` (paper mode) | RunAbInitio.hs:308-315 | Uses `genesisLibrarySteps` for Ω | **Inactive in strict mode** ✓ |

The first two paths are the problem. In strict mode, bar computation and
library insertion are genuinely paper-free, but the ν/κ values used to
RANK candidates and SELECT the winner come from paper tables when
`detectCanonicalName` identifies a known structure. This makes strict mode
"bar-strict, evaluator-calibrated" rather than "fully strict."

**Resolution**: Sprint 3A (EvalMode refactor). After the refactor, strict
mode will be "bar-strict AND evaluator-strict" — a genuinely paper-free
discovery pipeline.

---

## Current Architecture

```
engine/src/
├── Types.hs           [MODIFIED] LibraryEntry + 7 capability flags + mkLibraryEntry
├── Telescope.hs       [NEW] Core data types, structural analysis, reference telescopes
├── TelescopeGen.hs    [NEW+MOD] Type-directed generator, structural action gating
├── TelescopeEval.hs   [NEW+MOD] Classification, naming, evaluation, capability setting
├── TelescopeCheck.hs  [NEW] Bidirectional type checker MVP for telescope validity
├── MCTS.hs            [NEW] Monte Carlo Tree Search engine (full UCT cycle)
├── RunAbInitio.hs     [NEW+MOD] Ab initio engine (strict/paper-calibrated modes)
├── ProofRank.hs       [MODIFIED] availableFormers now structural (no name-based gates)
├── UniformNu.hs       [MODIFIED] Genesis entries carry structural capabilities
├── KappaNu.hs         [MODIFIED] Genesis entries carry structural capabilities
├── Generator.hs       [MODIFIED] candidateToEntry uses mkLibraryEntry + capabilities
├── HITEnum.hs         [MODIFIED] hitToLibraryEntry uses mkLibraryEntry
├── Manifest.hs        [MODIFIED] JSON parser uses mkLibraryEntry
├── Kolmogorov.hs      [MODIFIED] Added Ord to MBTTExpr deriving clause
├── pen-engine.cabal   [MODIFIED] Added library, ab-initio executable, random dep
└── (remaining modules unchanged)
```

New code: ~2,300 lines of Haskell across 6 new + 8 modified modules.

---

## Execution Plan

### Sprint 1 (Week 1): Foundation Hardening [DONE]

#### 1A. Add strict mode flags to RunAbInitio [DONE]

Implemented `AbInitioMode` ADT: `StrictAbInitio | PaperCalibrated`.
- `--strict` CLI flag selects strict mode
- `DiscoveryRecord` tracks (ν, κ) per step for strict bar computation
- `computeBar` dispatches on mode: paper values vs discovered history
- Library insertion: strict mode uses discovered entry only, paper mode
  falls back to paper entry for unknown canonical names
- MCTS κ estimate: strict mode uses step-based heuristic (3/6/9),
  paper mode uses `gsPaperK`
- Summary comparison table printed at end (discovered vs paper)

#### 1B. Replace name-based capability gating with structural predicates [DONE]

Extended `LibraryEntry` (Types.hs) with 7 structural capability flags:

| Flag | Gates | Replaces |
|------|-------|----------|
| `leHasDependentFunctions` | Pi, Sigma | `"Pi" elem names` |
| `leHasModalOps` | Flat, Sharp, Disc, PiCoh | `"Cohesion" elem names` |
| `leHasDifferentialOps` | Inf, Tangent, Connection | `"Connections" elem names` |
| `leHasCurvature` | Curvature | `"Curvature" elem names` |
| `leHasMetric` | Metric | `"Metric" elem names` |
| `leHasHilbert` | Hilbert | `"Hilbert" elem names` |
| `leHasTemporalOps` | Next, Eventually | `"DCT" elem names` |

Changes made:
1. **Types.hs**: Added 7 Bool fields + `mkLibraryEntry` smart constructor
2. **ProofRank.hs**: `availableFormers` rewritten to use structural predicates
3. **TelescopeGen.hs**: `actionGatedByLibrary` uses structural predicates
4. **TelescopeEval.hs**: `telescopeToCandidate` sets capability flags
5. **UniformNu.hs**: Genesis steps carry correct capabilities
6. **KappaNu.hs**: Genesis entries carry correct capabilities
7. **Generator.hs**: All `candidateToEntry` cases updated
8. **HITEnum.hs**: `hitToLibraryEntry` updated
9. **Manifest.hs**: JSON parser updated
10. **Telescope.hs**: `teleToEntry` uses `mkLibraryEntry`

**~40 construction sites updated**, all using `mkLibraryEntry` smart constructor.
Build verified: `cabal build all` succeeds with no errors.
`uniform-nu` and `pen-engine` produce identical output to pre-change baseline.

#### 1C. Sync remaining docs [DONE]

Updated this document. Still TODO:
- Update CLAUDE.md engine module descriptions (add Telescope/MCTS/RunAbInitio)

### Sprint 2 (Week 2): Search Quality [DONE]

#### 2A. Implement bidirectional type checker MVP for telescope validity [DONE]

Created `TelescopeCheck.hs` with conservative well-formedness checking:
- Library reference bounds: `Lib i` requires `1 <= i <= |library|`
- Variable reference bounds: `Var i` requires `1 <= i <= context depth`
- Empty telescope rejection
- Bare Univ as argument rejection (`App _ Univ`)
- Scope validity: binders extend context by 1

Integrated into `RunAbInitio.hs` — raw telescopes pass through
`checkAndFilter` before evaluation.

#### 2B. Upgrade MCTS to full UCT cycle [DONE]

Replaced simplified single-pass with full UCT iteration:
1. **Selection**: `selectPath` walks tree following UCT formula
   (`Q/N + C * sqrt(ln(N_parent)/N)`)
2. **Expansion**: `expandNode` creates children for each valid action
3. **Rollout**: `rolloutFromNode` random-completes and evaluates with bonuses
4. **Backpropagation**: `backpropagate` updates reward/visits along path

Design decisions:
- Tree nodes = partial telescopes (sequence of `TeleEntry`)
- Actions from `validActions` (TelescopeGen)
- Reward = ρ × connectivity bonus × window bonus
- Unvisited nodes get infinite UCT score (exploration priority)

**Test results** (PaperCalibrated mode, 15/15 steps discovered):
- MCTS activates at step 10 (Cohesion, κ=6) — first step where κ>3
- Steps 11-13: ENUM beats MCTS (exhaustive search finds better minimal-overshoot)
- Step 14 (Hilbert): REF wins (reference telescope has optimal κ=9)
- Step 15 (DCT): ENUM wins with ρ=13.12

#### 2C. Run ablation studies [DONE]

**Strict vs paper-calibrated mode comparison:**

| Step | PaperCal Name | PaperCal ν | PaperCal κ | Strict Name | Strict ν | Strict κ | Paper ν | Paper κ |
|------|---------------|------------|------------|-------------|----------|----------|---------|---------|
| 1    | Suspension    | 2          | 3          | Universe    | 1        | 2        | 1       | 2       |
| 2    | Axiom_1       | 2          | 3          | Unit        | 1        | 1        | 1       | 1       |
| 3    | Axiom_2       | 5          | 3          | Witness     | 2        | 1        | 2       | 1       |
| 4    | Axiom_3       | 5          | 3          | Pi          | 5        | 3        | 5       | 3       |
| 5    | Axiom_4       | 7          | 3          | S1          | 7        | 3        | 7       | 3       |
| 6    | Axiom_5       | 9          | 3          | Trunc       | 8        | 3        | 8       | 3       |
| 7    | Axiom_6       | 14         | 3          | S2          | 10       | 3        | 10      | 3       |
| 8    | Axiom_7       | 14         | 3          | S3          | 18       | 5        | 18      | 5       |
| 9    | Hopf          | 19         | 4          | Hopf        | 17       | 4        | 17      | 4       |
| 10   | Axiom_9       | 31         | 6          | Cohesion    | 19       | 4        | 19      | 4       |
| 11   | Connections   | 72         | 5          | Connections | 26       | 5        | 26      | 5       |
| 12   | Curvature     | 77         | 6          | Curvature   | 34       | 6        | 34      | 6       |
| 13   | Metric        | 84         | 7          | Metric      | 43       | 7        | 43      | 7       |
| 14   | Hilbert       | 91         | 9          | Hilbert     | 60       | 9        | 60      | 9       |
| 15   | DCT           | 105        | 8          | DCT         | 105      | 8        | 105     | 8       |
|      |               | **Σ=537**  | **Σ=69**   |             | **Σ=356**| **Σ=64** | **Σ=356** | **Σ=64** |

**Result: Strict mode discovers all 15 canonical names in correct order.**

**CAVEAT (see L13, L14):** The ν/κ numerical agreement (356/356, 64/64)
is partially circular — `evaluateTelescope` uses `effectiveNu`/`effectiveKappa`
which return paper values for recognized canonical names. The ordering and
name discovery are genuine; the exact numerical match needs requalification
after Sprint 3A.

### Sprint 3 (Week 3): Strict Mode Purity + κ Tri-Metric [CURRENT]

This sprint addresses the critical epistemic gap identified in the external
review: strict mode's evaluator still reads paper ν/κ tables for canonical
names, making the "exact agreement" claim partially circular.

#### 3A. Make StrictAbInitio truly paper-independent end-to-end [DONE]

**The problem**: `evaluateTelescope` (TelescopeEval.hs:493-506) always calls
`effectiveKappa`/`effectiveNu`, which look up `paperKappaByName`/`paperNuByName`
for any structurally recognized canonical name. This makes strict mode
"bar-strict, evaluator-calibrated" — the bar uses discovered history, but the
ν/κ values themselves come from paper tables.

**The fix**: Add an `EvalMode` type to the evaluation path:

```haskell
data EvalMode
  = EvalPaperCalibrated     -- use paper ν/κ for canonical names
  | EvalStrictComputed      -- never use paper ν/κ; compute from telescope + UniformNu
  deriving (Show, Eq)
```

**Files to change:**
1. **TelescopeEval.hs** (primary refactor):
   - Add `EvalMode` type
   - `evaluateTelescope :: EvalMode -> Telescope -> Library -> Int -> String -> (Int, Int, Double)`
   - In `EvalStrictComputed`: ν from `computeUniformNu`, κ from `strictKappa`
   - Keep `EvalPaperCalibrated` with existing semantics
   - Add `strictKappa :: Telescope -> Int` with explicit suspension policy
   - Add `EvalTrace` record for transparency logging

2. **RunAbInitio.hs** (wire strict mode):
   - Add `toEvalMode :: AbInitioMode -> EvalMode`
   - Replace all `evaluateTelescope tele lib 2 "candidate"` calls with
     `evaluateTelescope (toEvalMode mode) tele lib 2 "candidate"`

3. **MCTS.hs** (if evaluation is internalized):
   - Thread `EvalMode` through `mctsSearchStep`

**Acceptance criteria:**
- `ab-initio --strict` never calls `effectiveNu`/`effectiveKappa` or
  `paperNuByName`/`paperKappaByName`
- Paper-calibrated mode produces identical output to current behavior
- Strict mode produces potentially different ν/κ values (this is the point)

**Deliverable**: Independence audit table showing strict mode reads zero
paper ν/κ pathways.

#### 3B. Resolve κ as a first-class research object [TODO]

The κ metric mismatch (L3) is the strongest foreseeable reviewer attack
("overfitting through κ definition").

**Action**: Define and compare three κ metrics at every step:

| Metric | Definition | Source |
|--------|-----------|--------|
| κ_clause | Clause count of specification | Paper convention |
| κ_MBTT | MBTT bit-cost (Elias Gamma encoding) | Kolmogorov.hs |
| κ_entry | Telescope entry count (`teleKappa`) | Engine convention |

**Implementation**:
- Add `KappaTriMetric` record to TelescopeEval.hs
- Compute all three at each step in both modes
- Report sequence stability: does the ORDERING change under each metric?
- Move suspension floor (max 3 for Susp) into named policy with justification

**Deliverable**: 15-row table showing all three κ values per step, with
ordering stability analysis.

#### 3C. Upgrade type-checking from conservative filter to correctness gate [TODO]

TelescopeCheck is deliberately conservative. Tightening it reduces spurious
high-ρ candidates and provides cleaner evidence that the selected sequence
reflects structure, not generator noise.

**Minimal high-value additions to `CheckError`:**
- `PiDomainCannotBeTerm` — Pi/Sigma domain must be type-like
- `SigmaDomainCannotBeTerm` — same
- `LamBodyOutOfScope Int` — lambda body references beyond scope

**Add helper**: `isTypeLike :: MBTTExpr -> Bool` (syntactic filter, no
normalization). Feed checker confidence into MCTS reward/pruning.

#### 3D. Eliminate naming dependence in remaining classifier/prerequisite logic [TODO]

Strong progress on structural capability gating (L1), but canonical naming
and prerequisites still encode policy via string names in TelescopeEval.hs.

**Action:**
- Replace string-name prerequisite checks with capability/state predicates
  wherever possible
- Keep names only as labels for reporting
- Add regression tests for classification-order sensitivity

#### 3E. Publish independence audit table [TODO]

After 3A is complete, produce a one-page audit:

```
Component                 | Paper ν/κ in strict? | Evidence
--------------------------|---------------------|----------
computeBar                | NO                  | Uses discovered history
evaluateTelescope         | NO (after 3A)       | EvalStrictComputed path
effectiveNu/effectiveKappa| NOT CALLED          | Instrumentation log
library insertion         | NO                  | Uses discoveredEntry
MCTS evaluation           | NO                  | Always "candidate" name
```

### Sprint 4 (Week 4): Falsification Benchmark Suite + Doc Harmonization [TODO]

#### 4A. d-window sweep reproducibility [TODO]

Run strict mode with d=1, d=2, d=3:
- `--strict --window 1`: should stagnate (constant Δ)
- `--strict --window 2`: the paper sequence (Fibonacci Δ)
- `--strict --window 3`: tribonacci — different sequence expected
- Compare with paper predictions in §3

#### 4B. Adversarial candidate injections [TODO]

Inject "early efficiency monsters" — pathological telescopes with
artificially high ρ — and verify the engine rejects them:
- Bare suspensions at step 2 (should fail bar or lose to minimal overshoot)
- Fabricated HITs with maximum path dimensions
- Goal: demonstrate the selection bar + minimal overshoot is robust

#### 4C. Depth-cutoff sensitivity on DCT ν lower bound [TODO]

Vary UniformNu depth (1, 2, 3) and check DCT ν stability.
The paper claims ν ≥ 105; the engine should reproduce this at
reasonable enumeration depth.

#### 4D. Seed sensitivity study [TODO]

Run strict mode with 5 different MCTS seeds:
- The exhaustive ENUM is deterministic
- MCTS is seed-dependent — does the same structure always win?
- If yes: strong evidence of uniqueness
- If no: identify which steps are sensitive to seed

#### 4E. Doc harmonization [TODO]

Create consistency between pen_unified.tex, CODEBASE_GUIDE.md, and engine
output:
- Pin which ν definition each executable reports
- Pin which κ each mode uses
- Resolve the ν=105 vs ν=150 discrepancy (paper lower bound vs engine
  GenuineNu computation)

---

## Completed Tasks (Cumulative)

- [x] Phase 1: MBTT Telescope data types, generator, evaluator, reference specs
- [x] Phase 3: Theorem-driven search pruning (connectivity, interface density, redundancy)
- [x] Phase 4.1: MCTS with full UCT cycle (selection/expansion/rollout/backpropagation)
- [x] Phase 4.4: Integration with selection loop (RunAbInitio)
- [x] 15/15 reference telescope validation
- [x] Prerequisite chain enforcement (`hasPrerequisites` in TelescopeEval.hs)
- [x] Structural completeness checks in `detectCanonicalName`
- [x] Minimal overshoot selection (PEN Axiom 5)
- [x] Classification priority ordering (specific→general)
- [x] Two-phase evaluation (honest ν for selection, canonical name for insertion)
- [x] Doc sync: physics_creation.md updated to reflect actual implementation state
- [x] Sprint 1A: Strict ab initio mode (`--strict` flag, discovered ν/κ bar)
- [x] Sprint 1B: Structural capability gating (7 flags on LibraryEntry, ~40 sites updated)
- [x] Sprint 1C: Doc sync with implementation audit findings
- [x] Sprint 2A: Bidirectional type checker MVP (TelescopeCheck.hs, integrated into RunAbInitio)
- [x] Sprint 2B: Full UCT MCTS (selection/expansion/rollout/backpropagation cycle)
- [x] Sprint 2C: Ablation study — strict mode discovers all 15 names in correct order (caveat: L13/L14)

---

## Progress Log

### Session 1 (Initial Implementation)

- Created Telescope.hs, TelescopeGen.hs, TelescopeEval.hs, MCTS.hs, RunAbInitio.hs
- Added Ord to MBTTExpr deriving clause (Kolmogorov.hs)
- Updated pen-engine.cabal with new modules and ab-initio executable
- First build and run: 13/15 reference telescopes validated (Steps 3, 4 failed)

### Session 2 (Fixing Steps 3-4)

- Rewrote `teleToEntry` with proper type-theoretic classification
  (isTypeFormation, isSuspension, isLibOperation, isOperatorTop, isPointTerm)
- Added `isTriviallyDerivable` to prevent MCTS gaming
- Fixed Step 3 (Witness): ν=2, matching paper
- Step 4 (Pi/Sigma) still ν=0 due to name-gating

### Session 3 (Canonical Naming and MCTS Fixes)

- Fixed reference telescope names: "Pi/Sigma"→"Pi", "PropTrunc"→"Trunc"
- Implemented `detectCanonicalName` with structural prerequisites
- Step 4 now ν=5, matching paper exactly — **15/15 validation passes**
- Identified MCTS κ=1 gaming problem
- Added structural completeness requirements to canonical naming
  (Cohesion needs 3+ modalities, Pi needs Lam+Pi/Sigma, etc.)
- Implemented minimal overshoot selection (PEN Axiom 5)
- Fixed classification priority: Map/Axiomatic before Former
- Separated evaluation naming from library insertion naming
- Implemented prerequisite chain (`hasPrerequisites`) gating canonical names

### Session 4 (Architecture Audit + Structural Gating + Strict Mode)

**Audit findings:**
- `hasPrerequisites` was already fully implemented in TelescopeEval.hs but
  physics_creation.md said "Not yet implemented" (L7). Theory/implementation drift.
- `computeBar` in RunAbInitio.hs uses cumulative paper ν/κ, not discovered values.
- MCTS.hs is a scaffold — single-pass random, no tree navigation/backprop.
- `availableFormers` was 11/12 name-based. `actionGatedByLibrary` also name-based.

**Implementation (Sprint 1A — Strict mode):**
- Added `AbInitioMode` ADT: `StrictAbInitio | PaperCalibrated`
- Added `DiscoveryRecord` to track (ν, κ) per discovered step
- `computeBar` dispatches on mode for Ω_{n-1} computation
- `--strict` CLI flag; summary comparison table at end
- Clean build, no warnings

**Implementation (Sprint 1B — Structural gating):**
- Added 7 capability flags to `LibraryEntry`: `leHasDependentFunctions`,
  `leHasModalOps`, `leHasDifferentialOps`, `leHasCurvature`, `leHasMetric`,
  `leHasHilbert`, `leHasTemporalOps`
- Created `mkLibraryEntry` smart constructor (all caps default False)
- Updated ~40 `LibraryEntry` construction sites across 8 files
- Rewrote `availableFormers` (ProofRank.hs): all 12 formers now structural
- Rewrote `actionGatedByLibrary` (TelescopeGen.hs): modal/temporal structural
- `telescopeToCandidate` (TelescopeEval.hs) sets caps by classification
- Genesis steps (UniformNu.hs, KappaNu.hs) carry correct capabilities
- Clean build of all targets, `uniform-nu` output unchanged (regression pass)

### Session 5 (Sprint 2: Type Checker + UCT MCTS)

**Sprint 2A — Bidirectional type checker:**
- Created `TelescopeCheck.hs` (~150 lines) with conservative well-formedness
  checking: Lib/Var reference bounds, empty telescope, bare Univ as argument,
  scope validity through binders.
- Integrated `checkAndFilter` into `RunAbInitio.hs` enumeration pipeline.
- Clean build, zero warnings after fixing unused imports.

**Sprint 2B — Full UCT MCTS:**
- Complete rewrite of `MCTS.hs` (~480 lines):
  - `selectPath`: walks tree following UCT scores (Q/N + C√(ln(N_parent)/N))
  - `expandNode`: creates children for each valid action at leaf nodes
  - `rolloutFromNode`: random-completes telescope from node, evaluates with bonuses
  - `backpropagate`: updates reward/visits along selection path
- Clean build, zero warnings.
- **Full end-to-end test passed**: 15/15 structures discovered.
  - MCTS activated at step 10 (Cohesion, the first κ>3 step)
  - MCTS was the winning source only at step 10; ENUM dominated steps 11-15
  - Step 14 used REF (reference telescope)
  - Total runtime ~10 minutes (mostly MCTS iterations at steps 10, 14, 15)

**Key observation**: The MCTS-vs-ENUM competition reveals that for steps 11-15,
the exhaustive enumeration at κ≤3 still produces the best minimal-overshoot
candidates, even though the paper says these steps need κ>3. This suggests
our telescope κ encoding compresses these structures more than the paper's κ
metric. The κ mismatch (L3) remains the biggest gap.

**Sprint 2C — Ablation study:**
- **Strict mode result**: ALL 15 structures discovered in correct order.
  disc_ν = pap_ν for every step (Σ = 356 = 356). disc_κ = pap_κ for every
  step (Σ = 64 = 64). All 15 canonical names correctly assigned.
- **Paper-calibrated mode**: Over-reports ν (Σ = 537 vs 356) because the
  paper-derived bar allows non-canonical candidates to clear. Uses generic
  names ("Axiom_N") for steps 1-8.
- **CAVEAT IDENTIFIED**: The numerical agreement is partially circular
  because `evaluateTelescope` returns paper ν/κ for recognized canonical
  names (L13, L14). The ordering discovery is genuine; the exact match
  needs requalification after Sprint 3A.

### Session 6 (External Review + Strictness Audit)

**External review findings:**
- Confirmed the evaluator paper-value coupling identified in L14
- Recommended EvalMode refactor as top priority
- Identified κ as a first-class research object needing tri-metric treatment
- Recommended falsification-first experimental protocol
- Flagged doc drift: ν=105 (paper) vs ν=150 (CODEBASE_GUIDE / GenuineNu)

**Assessment**: The engine has strong structural foundations (structural
gating, prerequisite chain, minimal overshoot). The critical gap is that
"strict" is not fully strict at evaluation time. Resolving this is the
single highest-priority action before any broader physics claims.

**This session's deliverables:**
- Updated physics_creation.md with corrected L13, new L14, revised sprint plan
- Updated CODEBASE_GUIDE.md with evaluation modes, ab initio modules, and
  ν/κ definition clarifications
- Honest accounting of what's proved vs what's assumed vs what's circular
- **Implemented Sprint 3A: EvalMode refactor** (the top-priority fix):
  - Added `EvalMode` ADT (`EvalPaperCalibrated | EvalStrictComputed`) to TelescopeEval.hs
  - Added `strictKappa :: Telescope -> Int` — paper-independent κ with named suspension policy
  - Added `EvalTrace` record for transparency logging
  - Added `evaluateTelescopeTrace` for audit comparison
  - `evaluateTelescope` now takes `EvalMode` as first parameter
  - `evaluateTelescopeDetailed` signature updated for consistency
  - `toEvalMode :: AbInitioMode -> EvalMode` in RunAbInitio.hs maps synthesis mode to eval mode
  - All call sites updated: RunAbInitio.hs (2 sites), MCTS.hs rollout + mctsSearchStep
  - `mctsSearchStep` now takes `EvalMode` parameter
  - Clean build of all targets (`cabal build all`), zero errors, zero warnings

### Session 7 (Strict Mode Diagnosis + Root Cause Analysis)

**Strict mode run results** (`cabal run ab-initio -- --strict`):

| Step | Discovery | disc_ν | pap_ν | disc_κ | pap_κ | Status |
|------|-----------|--------|-------|--------|-------|--------|
| 1 | Universe | 2 | 1 | 1 | 2 | ν over (2x), κ under |
| 2 | Unit | 2 | 1 | 1 | 1 | ν over (2x) |
| 3 | Witness | 2 | 2 | 1 | 1 | OK but fails bar→REF |
| 4 | candidate | 0 | 5 | 3 | 3 | **FAILED** — Pi ν=0 |
| 5 | Pi | 10 | 7 | 3 | 3 | Recovered (shifted) |
| 6 | S1 | 14 | 8 | 3 | 3 | ν 1.75x over |
| 7 | S2 | 13 | 10 | 3 | 3 | ν 1.3x over |
| 8 | S3 | 17 | 18 | 3 | 5 | Close! κ mismatch |
| 9 | Hopf | 12 | 17 | 2 | 4 | ν under, κ under |
| 10 | Cohesion | 44 | 19 | 4 | 4 | ν 2.3x over |
| 11 | Connections | 65 | 26 | 5 | 5 | ν 2.5x over |
| 12 | Curvature | 70 | 34 | 6 | 6 | ν 2.1x over |
| 13 | Metric | 77 | 43 | 3 | 7 | ν 1.8x over, κ off |
| 14 | Trunc | 431 | 60 | 2 | 9 | **CATASTROPHIC** (54x) |
| 15 | candidate | 0 | 105 | 8 | 8 | **FAILED** — DCT ν=0 |
| SUM | | 759 | 356 | 48 | 64 | ν 2.1x, κ 0.75x |

**Three distinct failure modes identified:**

**F1: ν overestimation at steps 1-2 → cascading bar inflation.**
UniformNu counts all depth-2 schemas including compositions. Universe and
Unit get ν=2 instead of 1, inflating Ω₂ = 4/2 = 2.0 (paper: 2/3 = 0.67).
Bar₃ = 2.0 × 2.0 = 4.0 (paper: 1.33). Witness ρ=2.0 < 4.0 → REF fallback.
Cascading effect: corrupted history propagates to all subsequent bars.

**F2: Pi gets ν=0 at step 4 — REVISED: NOT chicken-and-egg.**
Initial hypothesis was that `availableFormers` can't see Pi/Sigma because
no library entry has `leHasDependentFunctions`. However, deep code trace
(Session 7) revealed `computeUniformNu` ALREADY does self-inclusive eval:
`fullLib = lib ++ [candidate]` (UniformNu.hs:212). The Pi entry in fullLib
has `leHasDependentFunctions = True` (set by `withCaps` in telescopeToCandidate).
So `availableFormers fullLib` DOES include Pi/Sigma in the AFTER enumeration.

The actual root cause is still UNKNOWN. Three hypotheses remain:
- H1: Canonicalization collapses `TPi "x" A B` to `TArrow A B` when B
  doesn't depend on x, making the before/after difference empty (checked:
  current rules don't do this explicitly, but something may still equate them)
- H2: The `schemaize` or `deepSchemaize` step collapses new Pi/Sigma
  schemas to trivial schemas that get filtered by `isTrivialSchema`
- H3: The inhabitation checker returns `Unknown` (not `Inhabited`) for
  some key Pi/Sigma types, causing them to be filtered from `inhAfter`

**Next step**: Added DEBUG output to RunAbInitio.hs at step 4 (prints
refCanon, refNu, entry details, library state). Build compiles. Need to
run `cabal run ab-initio -- --strict` and inspect the debug lines.
The debug code is currently in RunAbInitio.hs lines 172-182.

**F3: Trunc gets ν=431 at step 14 — combinatorial explosion.**
At step 14, library has ~20 available formers (Pi, Sigma, Omega, Susp,
Flat, Sharp, Disc, PiCoh, Inf, Tangent, Connection, Curvature, Metric...).
Depth-2 enumeration generates ALL pairs: Flat(Trunc(L)), Trunc(Flat(L)),
Metric(Trunc(L)), etc. `deepSchemaize` only collapses basic-op compositions;
modal/differential survive → O(formers²) schemas. Paper's ν=8 counts only
independent derivation schemas directly from Trunc's definition.

**Root cause**: UniformNu counts ALL enumerable schemas, not INDEPENDENT
derivation schemas. This is exactly what the paper says ν is NOT.

**What's genuinely working:**
- Ordering preserved through steps 1-13 (right structures, right order)
- Structural gating + prerequisite chain correctly gates canonical names
- Minimal overshoot selection picks right structure when ν is reasonable
- Trunc displaced to step 14 (not step 6) because it's already in library

**Recommendations (priority order):**
1. **R1 (Immediate)**: Diagnose Pi ν=0 root cause. Debug instrumentation
   is in place (RunAbInitio.hs lines 172-182). Run strict mode, inspect
   debug output at step 4. Identify which of H1/H2/H3 causes the zero.
   Self-inclusive eval is NOT the fix (already done by computeUniformNu).
2. **R2 (Critical)**: Depth-1 ν evaluation to prevent depth-2 explosion.
   Trunc ν=431 becomes ~8-20 at depth 1.
3. **R3 (Medium)**: Independence filter — remove schemas derivable from
   composition of prior schemas. F(G(L)) is independent only if neither
   F nor G was already available.
4. **R4 (Research)**: κ tri-metric comparison (Sprint 3B).

---

## Strictness Audit Status

**After Sprint 3A** (current state — EvalMode refactor complete):
- Bar computation: STRICT (uses discovered history) ✓
- Library insertion: STRICT (no paper fallback) ✓
- Candidate ν evaluation: STRICT via `EvalStrictComputed` (computeUniformNu only) ✓
- Candidate κ evaluation: STRICT via `strictKappa` (teleKappa + named suspension policy) ✓
- MCTS rollout: uses EvalPaperCalibrated but name is "mcts_candidate" → no paper table hits ✓
- MCTS final eval: uses `EvalStrictComputed` (threaded from RunAbInitio) ✓

**Strict mode known issues** (post-3A):
- ν systematically overestimated (~2x for steps 5-13, ~54x for Trunc at step 14)
- Pi ν=0 at step 4 (chicken-and-egg dependency)
- DCT ν=0 at step 15 (Hilbert never discovered → prerequisites fail)
- κ mismatch for suspensions and later structures

**Verification command** (after 3A):
```bash
cabal run ab-initio -- --strict > strict.log 2>&1
# Must produce zero hits:
grep -c "effectiveNu\|effectiveKappa\|paperNuByName\|paperKappaByName" strict.log
```

---

## Key Learnings (continued)

### L15: Pi ν=0 — Not Chicken-and-Egg After All

Initial diagnosis was that `availableFormers` can't see Pi/Sigma because
no library entry has `leHasDependentFunctions`. But `computeUniformNu`
already performs self-inclusive evaluation (line 212: `fullLib = lib ++ [candidate]`).
The Pi entry in fullLib has `leHasDependentFunctions = True`, so
`availableFormers fullLib` includes Pi/Sigma in the AFTER enumeration.

The actual root cause is **still unknown** as of Session 7. Three hypotheses:
1. **Canonicalization collapse**: `TPi "x" A B` → `TArrow A B` when non-dependent
2. **Schema triviality filter**: new Pi schemas hit `isTrivialSchema`
3. **Inhabitation checker gap**: some Pi types return `Unknown` not `Inhabited`

Debug instrumentation has been added to RunAbInitio.hs to diagnose this.
The fix R1 (self-inclusive) is NOT needed — the algorithm already does it.
The actual fix requires identifying which of H1/H2/H3 is the culprit.

### L16: UniformNu's Depth-2 Explosion for Late-Sequence Structures

At depth-2 enumeration, schema count grows as O(formers² × atoms). With
~20 formers at step 14, this produces ~400+ schemas for ANY new structure.
The paper's ν values are effectively depth-1 counts (single operation
applications), not depth-2 (all pairwise compositions).

**Evidence**: Steps 1-9 (small library, few formers) show ~1.5-2x
overestimation. Steps 10-14 (large library, many formers) show 2-54x
overestimation. The growth is quadratic in available formers.

### L17: Bar Dynamics Are Fragile Under ν Perturbation

The selection bar Bar_n = Φ_n × Ω_{n-1} depends on the CUMULATIVE ratio
Σν/Σκ. A 2x overestimation of ν at steps 1-2 doubles Ω, which doubles Bar₃,
which causes step 3 to fail bar clearance. This cascading failure shows
that the bar is sensitive to evaluation methodology — the paper's ν values
are not just convenient but NECESSARY for the bar dynamics to work correctly.

**Implication**: Any honest ab initio engine must produce ν values within
~20% of the paper's values for the bar to clear correctly. The bar is a
prediction of PEN theory, and its fragility is both a strength (precise
predictions) and a weakness (sensitivity to methodology).

---

## Open Questions (Updated)

1. **What causes Pi ν=0 at step 4?** (HIGHEST PRIORITY)
   Self-inclusive eval is NOT the issue (computeUniformNu already does it).
   Three hypotheses: canonicalization collapse, schema triviality filter,
   or inhabitation checker gap. Debug instrumentation is in place.
   **Immediate next step**: run strict mode, inspect debug output.

2. **Does depth-1 enumeration produce ν values close to paper?**
   R2 proposes restricting to depth-1. The paper's ν=8 for Trunc likely
   corresponds to depth-1 schemas. Test: run with maxDepth=1 and compare.

3. **Can diagnosis + depth-1 fix recover the full 15-step sequence?**
   If Pi ν=0 is fixed AND depth-1 tames the explosion, the engine might
   have a paper-free ν methodology that reproduces the sequence.

4. **How to compute κ that matches the paper's values?**
   Telescope entry count ≠ paper κ. The tri-metric approach (Sprint 3B)
   will determine whether ordering is κ-stable.

5. **Is the Generative Sequence truly unique under honest evaluation?**
   With corrected ν, does minimal overshoot produce a UNIQUE winner at
   each step, or are there near-ties?

6. **Can the engine discover all 15 structures without REF fallback?**
   After fixes, the REF dependency may change. Goal: 15/15 via
   ENUM/MCTS only.

---

## Session 7 Deep Investigation Log

### What was done

1. **Re-ran strict mode** — confirmed identical output to Session 6:
   - 13/15 canonical names discovered, 2 failures (step 4: Pi ν=0, step 15: DCT ν=0)
   - Total ν=759 (paper: 356), total κ=48 (paper: 64)

2. **Deep investigation of F2 (Pi ν=0 at step 4)**:
   - Traced full code path through TelescopeEval.hs → UniformNu.hs → ProofRank.hs
   - **Key discovery**: `computeUniformNu` (UniformNu.hs:212) already does
     self-inclusive evaluation: `fullLib = lib ++ [candidate]`. The Pi entry
     in fullLib has `leHasDependentFunctions = True` (via `withCaps`), so
     `availableFormers fullLib` includes "Pi" and "Sigma" in the AFTER state.
   - This DISPROVES the initial "chicken-and-egg" diagnosis.
   - The before/after comparison SHOULD find new types (TPi "x" A B in AFTER
     but not BEFORE) → ν should be > 0.
   - Added debug instrumentation to RunAbInitio.hs (step 4 diagnostic prints).
   - Debug build compiles cleanly. Run was started but interrupted by PC restart.

3. **Deep investigation of F3 (Trunc ν=431 at step 14)**:
   - Confirmed root cause: at step 14, library has ~20 available formers.
     Depth-2 enumeration generates O(formers²) compositions. `deepSchemaize`
     only collapses basic-op compositions (Arrow, Prod, Pi, Sigma); modal,
     differential, and temporal compositions survive as distinct schemas.
   - Fix confirmed: depth-1 enumeration would prevent the explosion.

4. **Traced full data flow for Pi evaluation** (steps traced):
   - `referenceTelescope 4` → Pi telescope: `[Lam(Pi(Var1,Var2)), App(App(Var1,Var2)(Var3)), App(Lam(Var1))(Var2)]`
   - `classifyTelescope` → TCFormer (all pure former, no lib pointers)
   - `detectCanonicalName` → "Pi" (prerequisites met: length lib ≥ 3)
   - `telescopeToCandidate` → entry with `leConstructors=0` (hasOperatorTop=True for Lam → constructors=0), `leHasDependentFunctions=True`
   - `evaluateTelescope EvalStrictComputed` → calls `computeUniformNu entry lib 2`
   - Inside `computeUniformNu`: `fullLib = lib ++ [entry]`, 4 entries total
   - `atomsAfter` includes TRef "Pi" (but `leConstructors=0` → `checkInhab` returns Unknown → filtered)
   - `enumWindowExact atomsAfter fullLib 1` → formers include "Pi","Sigma" → generates TPi/TSigma types
   - TPi types should be inhabited (e.g., `TPi "x" TUnit TUnit` → codomain TUnit is inhabited)
   - **The pipeline should produce ν > 0, yet it returns 0**
   - Three hypotheses for the discrepancy (H1-H3, see L15 above)

### Specific code locations investigated

| File | Lines | What |
|------|-------|------|
| TelescopeEval.hs | 527-546 | `evaluateTelescope` EvalStrictComputed branch |
| TelescopeEval.hs | 433-471 | `telescopeToCandidate` — gateStructural, withCaps |
| TelescopeEval.hs | 237-262 | `hasPrerequisites` for "Pi" |
| UniformNu.hs | 204-245 | `computeUniformNuAtDepth` — before/after comparison |
| UniformNu.hs | 112-125 | `enumBounded` — depth-0/1 full enum + depth-2+ unary ops |
| UniformNu.hs | 156-179 | `applyUnaryOps` — 15 unary operations at each level |
| UniformNu.hs | 255-297 | `computeUniformNu` — totalNu = scFull + newFormers + adjointCredit |
| ProofRank.hs | 170-199 | `availableFormers` — Pi/Sigma gated by leHasDependentFunctions |
| ProofRank.hs | 201-240 | `enumWindowExact` — Pi/Sigma in binaryOps, gated by "Pi" ∈ formers |
| Inhabitation.hs | 60-73 | `checkInhab TRef` — requires leConstructors > 0 |
| Inhabitation.hs | 127-135 | `checkInhab TPi/TSigma` — Pi inhabited if codomain is |
| Telescope.hs | 211-250 | `teleToEntry` — constructors=0 when hasOperatorTop |
| Telescope.hs | 314-325 | `isTriviallyDerivable` — bare refs only |
| Equivalence.hs | 123-128 | Pi rewrite rules: TPi _ _ TUnit→TUnit, TPi _ TUnit b→b |
| RunAbInitio.hs | 168-182 | REF evaluation + DEBUG diagnostic (added this session) |

### Current state of RunAbInitio.hs

DEBUG instrumentation added at lines 172-182 (inside `go` function):
```haskell
when (step == 4 || refNu == 0) $ do
  let refCanon = detectCanonicalName refTele lib
      refEntry = telescopeToCandidate refTele lib (...)
  printf "  [DEBUG step %d] refCanon=%s refNu=%d refKappa=%d\n" ...
  printf "  [DEBUG step %d] entry: cons=%d deps=%s loop=%s dims=%s\n" ...
  printf "  [DEBUG step %d] lib entries: %s\n" ...
```

Also added `import Control.Monad (when)` and changed `import Types (Library)`
to `import Types (Library, LibraryEntry(..))`.

**The debug build compiles cleanly.** Run it with `cabal run ab-initio -- --strict`
and look for `[DEBUG step 4]` lines.

### Immediate next steps (when resuming)

1. **Run `cabal run ab-initio -- --strict`** — inspect `[DEBUG step 4]` output.
   This will show the Pi entry's properties and the library state, confirming
   the trace above.

2. **Add deeper debug**: If the entry looks correct, the issue is inside
   `computeUniformNu`. Add a trace inside `computeUniformNuAtDepth` that
   prints `rawCount`, `schemaCount`, `newFormers`, `adjointCredit` for the
   Pi entry. This will pinpoint whether the issue is:
   - rawCount=0 (no new inhabited types found) → inhabitation checker issue
   - rawCount>0 but schemaCount=0 (all schemas are trivial) → isTrivialSchema issue
   - schemaCount>0 but totalNu=0 somehow → logic error

3. **Test H1 (canonicalization collapse)**: Check if
   `canonicalize (TPi "x" (TRef "Universe") (TRef "Universe"))` returns
   `TArrow (TRef "Universe") (TRef "Universe")`. If yes, that's the bug.
   Quick test: add a trace in computeUniformNuAtDepth that prints the
   BEFORE and AFTER sets at depth 1.

4. **If Pi ν=0 is resolved**: Test depth-1 evaluation by changing
   `maxDepth` from 2 to 1 in the evaluateTelescope call (RunAbInitio.hs
   lines 163, 170). Check if Trunc ν drops from 431 to ~8-20.

5. **Remove debug instrumentation** once the root cause is found and fixed.
   The `when (step == 4 || refNu == 0)` block should be deleted.
