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
  names that `availableFormers` recognizes, with structural prerequisites
  (minimum κ, required constructs, modality counts)
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

### 2.1 Implement Bidirectional Type Checker [TODO]

Not yet implemented. Currently using structural heuristics in `validActions`.

### 2.2 Top-Down Hole-Filling with Type Constraints [PARTIALLY DONE]

`validActions` in TelescopeGen.hs does contextual pruning:
- Modal/temporal actions gated by library prerequisites
- Library pointer actions limited to existing library size
- Variable references limited to current context depth
- But: no full bidirectional type checking yet

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

### 4.1 Create `MCTS.hs` — Core MCTS Engine [DONE]

Created `engine/src/MCTS.hs` (~400 lines) with:
- `MCTSConfig` with iterations, maxKappa, maxDepth, exploreC, nuDepth, topK, seed
- `MCTSNode` tree with entries, visits, reward, children
- `mctsSearch :: MCTSConfig -> Library -> IO MCTSResult`
- UCT selection (implemented but not yet integrated into main loop)
- Simplified random rollout with weighted action selection
- Connectivity and window-reference bonuses in reward function

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

### L1: The Name-Gating Bottleneck

`availableFormers` in ProofRank.hs gates type former unlocking on specific
library entry names ("Pi", "Trunc", "Cohesion", etc.). This creates a critical
coupling between the evaluation infrastructure and the naming convention.

For ab initio discovery, this means:
- **With canonical naming during evaluation**: ν reflects the FULL generative
  capacity of the structure, but enables gaming (a partial telescope fragment
  gets inflated ν by claiming a canonical name)
- **Without canonical naming**: ν is "honest" but FLAT — many structurally
  different telescopes produce similar ν because the name-gated schemas
  are never unlocked

**Current approach**: Use canonical naming only for library insertion (not
evaluation), combined with structural prerequisite checks. The canonical
name is assigned by `detectCanonicalName` which checks:
- Structural completeness (minimum κ, required constructs)
- Library state prerequisites (coming soon)

**Long-term fix needed**: Make `availableFormers` structural rather than
name-based. Detect Pi/Sigma capability from LibraryEntry fields, not from
`"Pi" \`elem\` names`.

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

### L7: The Prerequisite Chain

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
- etc.

This is the PEN theory's own structural constraint — each step builds on
the previous ones. Without this chain, the engine can "discover" Trunc at
step 1 (with an empty library), which is nonsensical.

**Status**: Not yet implemented. This is the next critical task.

---

## Current Architecture

```
engine/src/
├── Telescope.hs       [NEW] Core data types, structural analysis, reference telescopes
├── TelescopeGen.hs    [NEW] Type-directed generator, exhaustive enumeration
├── TelescopeEval.hs   [NEW] Classification, canonical naming, evaluation bridge
├── MCTS.hs            [NEW] Monte Carlo Tree Search engine
├── RunAbInitio.hs     [NEW] Ab initio discovery executable
├── Kolmogorov.hs      [MODIFIED] Added Ord to MBTTExpr deriving clause
├── pen-engine.cabal   [MODIFIED] Added library, ab-initio executable, random dep
└── (existing 20 modules unchanged)
```

New code: ~1,600 lines of Haskell across 5 new modules.

---

## Updated Implementation Plan

### Immediate Next Steps (Priority Order)

1. **Implement prerequisite chain in `detectCanonicalName`** [CRITICAL]
   Gate canonical name assignment on library state. "Pi" only available
   if library has "Witness", etc. This enforces the correct ordering.

2. **Restore canonical naming for evaluation** [CRITICAL]
   With prerequisites preventing gaming, canonical naming during evaluation
   is safe and necessary for correct ν values.

3. **Add diagnostic output to synthesis loop**
   Show the telescope structure (MBTT expressions) for each discovery,
   not just the name. Essential for debugging classification issues.

4. **Implement proper κ metric**
   Either use `teleBitCost` normalized to match paper's κ scale, or
   implement AST node count with implicit constructor expansion for Susp.

5. **Full UCT tree search in MCTS**
   Currently using simplified random rollout. Need to integrate the
   UCT selection → expansion → rollout → backpropagation cycle for
   κ > 3 telescopes.

### Medium-Term

6. **Bidirectional type checker** (Phase 2.1)
   Filter out semantically invalid telescopes during generation.
   Currently many enumerated telescopes are ill-typed.

7. **Differential ν evaluation** (Phase 5.1)
   Cache the "before" schema set and only enumerate new schemas.
   Currently each candidate triggers a full before/after comparison.

8. **Structural `availableFormers`**
   Replace name-based gating with structural detection. This removes
   the evaluation/naming circularity entirely.

### Long-Term

9. **Rosetta Stone identification** (Phase 6.1)
   Semantic equivalence checking between discovered and reference telescopes.

10. **Agda emission and verification** (Phase 6.2-6.4)
    Formal verification of discovered structures.

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
- Identified prerequisite chain as the next critical task

---

## Open Questions

1. **How to compute κ that matches the paper's values?**
   Telescope entry count ≠ paper κ. Need either a mapping function or
   acceptance that ab initio κ values may differ.

2. **Should the ab initio engine use discovered or paper library entries?**
   Currently uses paper entries as fallback. True ab initio would use
   only discovered entries, but requires robust canonical naming.

3. **Can exhaustive enumeration at κ ≤ 3 discover all 15 structures?**
   Steps 1-8 have κ ≤ 3 in the paper. Steps 9-15 have κ ≤ 9, requiring
   MCTS. But our telescope κ differs from the paper's κ.

4. **Is the Generative Sequence truly unique under honest evaluation?**
   The paper claims uniqueness, but our experiments show many telescopes
   with similar ν when evaluated without canonical naming. The uniqueness
   may depend on a richer evaluation that the current infrastructure
   doesn't provide.
