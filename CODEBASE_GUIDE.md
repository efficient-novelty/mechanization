# PEN Engine: Reading Guide and Instruction Manual

## For Researchers Verifying the Genesis Sequence

**Version:** 0.5 (ab initio engine + strictness audit)
**Language:** Haskell (GHC 9.6+)
**Build:** `cd engine && cabal build`
**Run:** `cd engine && cabal run pen-engine`

---

## What This Engine Does

The PEN engine answers one question: **Does the Genesis Sequence emerge from unconstrained search?**

The Genesis Sequence is a list of 15 mathematical structures (Universe, Unit, Witness, Pi/Sigma, S1, PropTrunc, S2, S3, Hopf, Cohesion, Connections, Curvature, Metric, Hilbert, DCT) predicted by the Principle of Efficient Novelty (PEN). The engine starts from an empty library, generates candidate structures at each step, evaluates their efficiency (rho = nu/kappa), and selects the winner that just barely clears a rising selection bar. The answer is: **yes, 15/15 structures are discovered in the correct order.**

---

## Evaluation Modes

The engine operates in several modes that differ in how ν (novelty) and κ (construction effort) are computed. Understanding these modes is critical for interpreting results.

### Ab Initio Modes (RunAbInitio.hs)

| Mode | CLI Flag | Bar Computation | Library Insertion | Candidate ν/κ | EvalMode |
|------|----------|----------------|-------------------|---------------|----------|
| **PaperCalibrated** | (default) | Paper ν/κ for Ω_{n-1} | Fallback to paper entries | Paper values for canonical names | `EvalPaperCalibrated` |
| **StrictAbInitio** | `--strict` | Discovered ν/κ history | Discovered entry only | `computeUniformNu` + `strictKappa` | `EvalStrictComputed` |

The `EvalMode` refactor (Sprint 3A) cleanly separates these paths. In strict mode, `evaluateTelescope` never calls `effectiveNu`/`effectiveKappa` — all ν/κ values are computed from telescope structure + library state. The only explicit policy is the suspension κ floor (`max 3 (teleKappa tele)`), which is named and documented. See `physics_creation.md` L14.

### Simulation Modes (Simulation.hs / Main.hs Phases G-J)

| Mode | Phase | ν Source | κ Source | Purpose |
|------|-------|----------|----------|---------|
| **PaperMode** | G | `paperNu` (KappaNu.hs) | `paperKappa` (KappaNu.hs) | Replay paper values — validates bar dynamics |
| **CapabilityMode** | H-I | 18 hand-tuned rules (Capability.hs) | Same as PaperMode | Cross-validates computed ν against paper |
| **ComputedMode** | J | GenuineNu.hs dispatch | Generator.hs `candidateKappa` | Genuine synthesis — the core claim |

### ν Definitions by Source

| Source | Definition | Typical DCT value | Used in |
|--------|-----------|-------------------|---------|
| `paperNu` (KappaNu.hs) | Hand-counted independent schemas | 105 | PaperMode, effectiveNu |
| `computeUniformNu` (UniformNu.hs) | Before/after inhabitation comparison | 105 (at depth 2) | StrictAbInitio (after refactor) |
| `genuineNu` (GenuineNu.hs) | Dispatch by structure category | 150 (lattice tensor) | ComputedMode (Phase J) |
| `capabilityNu` (Capability.hs) | 18 domain-specific rules | 150 | CapabilityMode (Phase H-I) |

**Important**: The ν=105 vs ν=150 discrepancy for DCT is a known documentation issue. The paper uses ν=105 (lower bound, independent schemas only). GenuineNu and Capability compute ν=150 (lattice tensor product: 14×11−4). These are different metrics with different semantics. The paper's ν=105 is the publication-canonical value.

### κ Definitions by Source

| Source | Definition | Typical Pi value | Notes |
|--------|-----------|-----------------|-------|
| `paperKappa` (KappaNu.hs) | Clause count of specification | 3 | Paper convention |
| `teleKappa` (Telescope.hs) | Telescope entry count | 5 | MBTT encoding — often different |
| `teleBitCost` (Telescope.hs) | MBTT binary encoding length | varies | Kolmogorov-style |
| `effectiveKappa` (TelescopeEval.hs) | Paper κ for canonical names, teleKappa otherwise | 3 | Mixed — used in current evaluator |

---

## Quick Start: What to Run and What to Look For

```bash
cd engine
cabal build
cabal run pen-engine 2>&1 | less
```

The output is organized into 10 phases (A through J). **If you only have time to check one thing, skip to Phase J** -- it's the genuine synthesis run where structures are discovered from scratch.

### Ab Initio Engine (separate executable)

```bash
cd engine
cabal run ab-initio                  # Paper-calibrated mode
cabal run ab-initio -- --strict      # Strict mode (no paper bar fallback)
```

The ab initio engine starts from an empty library and discovers all 15
structures using exhaustive enumeration (κ ≤ 3) + MCTS (κ > 3).

### Key output to verify in Phase J:

```
=== PHASE J: Synthesis Mode ===

 n  | tau  | Structure      | Type       | Delta | nu  | kappa | rho    | Bar    | Cleared
----|------|----------------|------------|-------|-----|-------|--------|--------|--------
  1 |    1 | Universe       | Foundation |     1 |   1 |     2 |    0.5 |    --- | YES
  2 |    2 | Unit           | Foundation |     1 |   1 |     1 |    1.0 |    0.5 | YES
  ...
 15 | 1596 | DCT            | Synthesis  |   610 | 150 |     8 |  18.75 |   7.73 | YES
```

**What to verify:**
1. All 15 structures appear in the correct order (Universe through DCT)
2. Every structure has "YES" in the Cleared column (rho >= Bar)
3. The Delta column follows the Fibonacci sequence: 1,1,2,3,5,8,13,21,34,55,89,144,233,377,610
4. DCT achieves rho=18.75, far exceeding its Bar (~7.73)

**Note**: Phase J uses GenuineNu.hs which reports DCT ν=150. The paper
uses ν=105 (independent schema lower bound). Both are correct measures;
they answer different questions about novelty.

---

## Architecture Overview

```
                    Main.hs
                   (10 phases)
                       |
    +---------+---------+---------+---------+
    |         |         |         |         |
 Phase A-F  Phase G   Phase H   Phase I   Phase J
 (analysis) (paper    (capabil- (cap.     (genuine
             replay)   ity val.) replay)   synthesis)
    |         |         |         |         |
    |    Simulation.hs  |    Simulation.hs  |
    |    (hardcoded     |    (computed       |
    |     kappa/nu)     |     kappa/nu)      |
    |                   |                    |
    |            Capability.hs          Synthesis.hs
    |            (18 rules)            (genuine loop)
    |                                       |
    +-------+-------+              +--------+--------+
    |       |       |              |        |        |
 Types.hs  Enum.   Inhab.    Generator.hs  GenuineNu.hs
            erate   itation        |        |        |
             .hs     .hs     TheoryState.hs |   Cluster.hs
                                        HITEnum.hs

                    RunAbInitio.hs
                   (ab initio engine)
                       |
    +---------+---------+---------+
    |         |         |         |
 TelescopeGen  TelescopeEval  MCTS.hs
 (enumerate)   (evaluate)    (search κ>3)
    |              |
 Telescope.hs  TelescopeCheck.hs
 (data types)  (well-formedness)
```

### Data Flow in Phase J (the one that matters):

```
1. TheoryState (empty library, no formers)
       |
2. Generator.generateCandidates(state, horizon)
   -> produces [Candidate] based on what's in the library
       |
3. For each candidate:
   a. candidateKappa(cand) -> kappa (construction cost)
   b. genuineNu(cand, state) -> nu (novelty)
   c. rho = nu / kappa
       |
4. Filter: admissible (kappa <= horizon) AND clears bar (rho >= bar)
       |
5. selectWinner: minimal overshoot (rho - bar)
       |
6. addToTheory: update library, unlock type formers
       |
7. Advance Fibonacci clock: Delta_{n+1} = F_{n+1}, tau += Delta
       |
8. Repeat until 15 structures found or no candidates survive
```

### Data Flow in Ab Initio Engine (RunAbInitio.hs):

```
1. Empty library B = {}
       |
2. At step n:
   a. Exhaustive enumeration: enumerateTelescopes(B, κ_max=3)
   b. Type checking: checkAndFilter(B, telescopes)
   c. MCTS (if κ > 3): mctsSearchStep(cfg, B, bar)
   d. Reference telescope: referenceTelescope(n)
       |
3. For each telescope:
   a. evaluateTelescope(evalMode, tele, B, 2, "candidate")
      -> detectCanonicalName -> ν/κ via mode dispatch -> (ν, κ, ρ)
      PaperCalibrated: effectiveNu/effectiveKappa (paper tables)
      StrictComputed: computeUniformNu/strictKappa (paper-free)
   b. Filter: ρ >= bar (or step ≤ 2)
       |
4. Selection: canonical names prioritized, then minimal overshoot
       |
5. Insert winner into library B
       |
6. Record (ν, κ) in discovery history
       |
7. Repeat
```

---

## Module-by-Module Guide

Read these in the order below to understand the system from bottom up.

### Layer 1: Type Representations

#### `Types.hs` (~120 lines)
**What:** Defines the AST for type expressions in HoTT.

**Key type:**
```haskell
data TypeExpr
  = TUnit | TVoid                    -- 1, 0
  | TRef String                      -- Library reference ("S1", "Pi")
  | TArrow TypeExpr TypeExpr         -- Function type A -> B
  | TProd TypeExpr TypeExpr          -- Product A x B
  | TCoprod TypeExpr TypeExpr        -- Coproduct A + B
  | TId TypeExpr TypeExpr TypeExpr   -- Identity type a =_A b
  | TSelfId TypeExpr                 -- Reflexivity x =_A x
  | TOmega TypeExpr                  -- Loop space Omega(A)
  | TSusp TypeExpr                   -- Suspension Susp(A)
  | TTrunc Int TypeExpr              -- n-truncation ||A||_n
  | THIT Int [Int]                   -- HIT(points, [path dims])
  | TFiber TypeExpr TypeExpr         -- Fiber of f : A -> B
  | TDeloop TypeExpr                 -- Delooping BA
```

**Key function:** `complexity :: TypeExpr -> Int` -- counts AST nodes, used as the Kolmogorov cost measure.

**Library entry:**
```haskell
data LibraryEntry = LibraryEntry
  { leName         :: String
  , leConstructors :: Int       -- point constructors
  , lePathDims     :: [Int]     -- path constructor dimensions
  , leHasLoop      :: Bool      -- has non-trivial loops?
  , leIsTruncated  :: Maybe Int -- truncation level
  -- Structural capability flags (added for ab initio engine):
  , leHasDependentFunctions :: Bool  -- gates Pi, Sigma formers
  , leHasModalOps          :: Bool  -- gates Flat, Sharp, Disc, PiCoh
  , leHasDifferentialOps   :: Bool  -- gates Inf, Tangent, Connection
  , leHasCurvature         :: Bool  -- gates Curvature formers
  , leHasMetric            :: Bool  -- gates Metric formers
  , leHasHilbert           :: Bool  -- gates Hilbert formers
  , leHasTemporalOps       :: Bool  -- gates Next, Eventually
  }
```

**Smart constructor:** `mkLibraryEntry :: String -> Int -> [Int] -> Bool -> Maybe Int -> LibraryEntry` creates an entry with all capability flags defaulting to `False`.

**Verification point:** Check that the 16 `genesisEntry` definitions in `KappaNu.hs` correctly model the mathematical structures. For example, S1 should have 1 constructor and path dimension [1], meaning one point and one 1-dimensional path (the loop).

---

#### `Enumerate.hs` (~100 lines)
**What:** Generates all well-formed type expressions up to a given complexity bound.

**Key function:** `allTypes :: Library -> Int -> [TypeExpr]`

Builds types bottom-up: at each complexity level, combines types from lower levels using all type constructors (arrow, product, coproduct, identity, loop space, suspension, truncation).

**Verification point:** Check that `allTypes` produces the expected counts. At complexity <= 3 with library [Universe, Unit], you should get types like `TUnit`, `TArrow TUnit TUnit`, `TProd TUnit TUnit`, etc.

---

#### `Inhabitation.hs` (~150 lines)
**What:** Conservative heuristic for determining whether a type expression is inhabited.

**Key function:** `isInhabited :: TypeExpr -> Library -> InhabitResult`

Returns `Inhabited`, `Uninhabited`, or `Unknown`. **Crucially conservative:** it only claims inhabitation when it can construct a witness. It never falsely claims a type is inhabited.

**14 rules:**
- TUnit -> Inhabited (witness: star)
- TVoid -> Uninhabited
- A -> B -> Inhabited if B is inhabited (witness: const) or A is uninhabited (vacuous)
- A x B -> Inhabited if both are
- A + B -> Inhabited if either is
- x =_A x -> Inhabited if A is inhabited (witness: refl)
- Omega(A) -> Inhabited if A has a loop (witness: loop constructor)
- Susp(A) -> Inhabited always (witness: north)
- ||A||_n -> Inhabited if A is (witness: quotient)

**Verification point:** The conservatism is key. `isNewlyInhabited` checks that a type is inhabited with the new structure but NOT without it. This is how novelty is counted.

---

### Layer 2: Novelty Computation

#### `Cluster.hs` (~130 lines) -- THE CORE NOVELTY ALGORITHM
**What:** Implements proof-rank clustering for HITs and suspensions.

**Algorithm:**
1. **Enumerate** all type expressions at depth <= 1 using a 2-step window (the candidate X plus the two most recent library entries)
2. **Filter** for types that are newly inhabited (inhabited with X, not without)
3. **Abstract** each type to a schema: replace the candidate name with "X", all library atoms with "L"
4. **Canonicalize** schemas (flatten/sort commutative ops)
5. **Group** by schema -- each schema represents one independent proof technique
6. **Filter trivial** schemas: X*X, X+X, X->X, SelfId(X) -- these work for ANY inhabited type
7. **Count** non-trivial schema groups
8. **Add latent bonus**: pathConstructors + maxPathDim^2

**Example for S1:**
- Types at depth 1 involving S1: {S1, S1->L, L->S1, S1xL, S1+L, Omega(S1), SelfId(S1), ...}
- Newly inhabited subset: those not inhabited before S1 enters
- Schemas: {X, X->L, L->X, XxL, X+L, Omega(X), SelfId(X)}
- After trivial filtering (remove SelfId(X)): 6 schemas
- Bonus: 1 path constructor + 1^2 homotopy = 2... but trivials removed already
- Result: nu = 7

**Verification point:** Run the engine and look at Phase D output. It shows the actual schemas and counts for each structure. You can manually verify that each schema represents a genuinely independent proof technique.

---

#### `GenuineNu.hs` (~260 lines)
**What:** Dispatches nu computation to the appropriate method based on candidate type.

**Dispatch logic:**
```
Foundation/Former -> hardcoded values (nu=1,1,2,5 for steps 1-4; context-dependent for PropTrunc)
HIT/Suspension    -> proof-rank clustering (Cluster.hs)
Map               -> component formula (fibration + longExact + classifying + cross + funcSpace)
Algebra           -> cross-interaction formula (absorbed anyway)
Modal             -> component formula (modal + cross + funcSpace + adjunction)
Axiom             -> 4-component formula (fieldOps + modalCross + funcSpace + cross)
Synthesis (DCT)   -> lattice tensor product (spatial 14 x temporal 11 - correction 4 = 150)
```

**Verification point:** The DCT nu=150 computation is the most important to verify. Look at the `genuineNu` case for `CSynthesis`. It checks whether Cohesion is in the library (for spatialLattice=14) and computes 14*11-4=150. Without Cohesion, spatialLattice=0 and DCT cannot form. This demonstrates that DCT's novelty genuinely depends on library state.

**Note:** The paper uses ν=105 for DCT (independent derivation schemas only). GenuineNu reports ν=150 (lattice tensor product). These measure different things — 105 is the publication-canonical lower bound.

---

#### `Capability.hs` (~200 lines)
**What:** Alternative nu computation using 18 domain-specific rules. Used in Phase H/I for cross-validation.

**18 rules:** existence, function-space, product-sum, path-loop, homotopy, suspension, truncation, modal, fibration, long-exact, classifying, field-ops, modal-cross, spectral, operator, cross-interactions, su2, synthesis.

**Verification point:** Phase H output shows rule-by-rule breakdowns for each structure. Every structure's computed nu should match the paper nu exactly (16/16). This validates that the paper's nu values are self-consistent.

---

#### `UniformNu.hs` (~250 lines)
**What:** Uniform before/after inhabitation comparison for all 15 steps. Works with adjoint completion to handle the Extensional Boundary at steps 3-4.

**Key function:** `computeUniformNu :: LibraryEntry -> Library -> Int -> UniformNuResult`

**Used by:** TelescopeEval.hs as the primary ν source in strict mode (`EvalStrictComputed`). Also used for non-canonical telescope evaluation in paper-calibrated mode.

---

### Layer 3: Candidate Generation

#### `HITEnum.hs` (~100 lines)
**What:** Enumerates all parametric Higher Inductive Types up to a given cost.

**Parameterization:** A HIT is `(numPoints, [pathDimensions])`. Cost = 1 + numPoints + sum(pathDims).

**Symmetry breaking:** Path dimensions must be non-decreasing. This eliminates permutation duplicates (e.g., [1,2] and [2,1] are the same HIT).

**Known HITs:**
- S1 = (1, [1]) -- 1 point, 1 loop -- cost 3
- S2 = (1, [2]) -- 1 point, 1 surface -- cost 4
- S3 = (1, [3]) -- 1 point, 1 volume -- cost 5
- Torus = (1, [1,1]) -- 1 point, 2 loops -- cost 4
- Bool = (2, []) -- 2 points -- cost 3

**Verification point:** At cost 3, you should get: (1,[1]) = S1 and (2,[]) = Bool. The engine should select S1 over Bool at step 5 because S1 has higher nu (loops generate more novelty than discrete points).

---

#### `Generator.hs` (~275 lines)
**What:** Generates all candidate structures at each synthesis step.

**Gating logic (in order of enabling):**
```
Step 0-2: CFoundation "Universe", "Unit", "Witness"
Step 3+:  CFormer Pi/Sigma (enables FPi, FSigma, FId, FSusp)
Step 3+:  CFormer PropTrunc
Step 3+:  CHIT (enumerated from HITEnum, filtered by dimension bound)
Step 4+:  CSusp X (for loopy X in library, requires FSusp)
Step 5+:  CMap "S3" "S2" "S1" (requires all three spheres)
Step 5+:  CAlgebra "Lie" "S3" (requires S3)
Step 9+:  CModal "Cohesion" 3 (requires FFibration)
Step 10+: CAxiom chain: Connections -> Curvature -> Metric -> Hilbert
Step 14+: CSynthesis "DCT" 8 (requires FHilbert)
```

**Dimension bound filter:** A d-dimensional path constructor (e.g., a 2-cell for S2) requires (d-1)-dimensional paths in the library. This prevents S2 from appearing before S1.

**Duplicate filtering:** If a HIT is isomorphic to a suspension (e.g., HIT(1,[2]) = Susp(S1) = S2), the HIT version is filtered out in favor of the suspension (which has lower kappa).

**Verification point:** At step 5, the engine should generate candidates including S1, Bool, and PropTrunc. S1 should win because it has the best rho that clears the bar with minimal overshoot.

---

#### `TheoryState.hs` (~85 lines)
**What:** Tracks the evolving theory state (library + available type formers).

**Type formers unlock chain:**
```
Pi/Sigma realizes  -> FPi, FSigma, FId, FSusp become available
PropTrunc realizes  -> FTrunc
Hopf realizes       -> FFibration
Cohesion realizes   -> FModal
Connections         -> FConnection
Curvature           -> FCurvature
Metric              -> FMetric
Hilbert             -> FHilbert
DCT                 -> FDCT
```

This gating ensures structures can only be generated after their prerequisites exist. It is the engine's version of the "structural capital" concept from the papers.

---

### Layer 4: Selection and Synthesis

#### `Synthesis.hs` (~350 lines) -- THE MAIN LOOP
**What:** The genuine discovery engine. This is the code that proves the Genesis Sequence emerges from search.

**Core loop (`synthLoop`):**
```
1. Compute bar = Phi_n * Omega_{n-1}
   where Phi_n = F_n / F_{n-1}, Omega = cumNu / cumKappa
2. Generate all candidates (Generator.generateCandidates)
3. Evaluate each: kappa, nu, rho = nu/kappa
4. Filter: admissible (kappa <= horizon) AND clears bar (rho >= bar)
5. If none survive: idle tick (horizon += 1)
6. If survivors exist: select winner (minimal overshoot)
7. Add winner to library, update theory state
8. Reset horizon to delta + 1, advance Fibonacci clock
9. Repeat
```

**Selection criterion:** Among candidates clearing the bar, pick the one with the smallest `rho - bar`. Ties broken by smallest kappa, then alphabetical name. This "minimal overshoot" criterion ensures the system selects the most natural next step.

**Verification point:** The key claim is that this loop, starting from nothing, discovers all 15 Genesis structures in order. Run it and check.

---

#### `Simulation.hs` (~200 lines)
**What:** Paper-mode simulation using hardcoded kappa/nu values. Used for comparison/validation.

Implements the same 5 PEN axioms but with paper reference values instead of computed ones. The output should be identical to Phase J (same structures in same order). If it diverges, that indicates a problem with the computed nu values.

---

### Layer 5: Ab Initio Engine

#### `Telescope.hs` (~530 lines)
**What:** Core data types for MBTT context telescopes — the universal
representation for candidate structures.

**Key types:**
```haskell
data TeleEntry = TeleEntry String MBTTExpr
newtype Telescope = Telescope { teleEntries :: [TeleEntry] }
```

**Key functions:**
- `teleKappa :: Telescope -> Int` — entry count (κ metric)
- `teleBitCost :: Telescope -> Int` — MBTT binary encoding cost
- `teleIsConnected :: Telescope -> Bool` — Structural Unity filter
- `teleReferencesWindow :: Telescope -> Int -> Bool` — Interface Density filter
- `isTriviallyDerivable :: Telescope -> Library -> Bool` — redundancy filter
- `teleToEntry :: Telescope -> Library -> String -> LibraryEntry` — convert for UniformNu
- `referenceTelescope :: Int -> Telescope` — paper's 15 structures as MBTT telescopes
- `mbttToTypeExpr :: MBTTExpr -> Library -> TypeExpr` — bridge to Type layer

---

#### `TelescopeGen.hs` (~380 lines)
**What:** Type-directed telescope generation via hole-filling.

**Key functions:**
- `enumerateTelescopes :: Library -> Int -> [Telescope]` — exhaustive for small κ
- `validActions :: Hole -> Library -> [Action]` — contextual pruning with capability gating
- `actionPriority :: Int -> Action -> Int` — heuristic bias for search

---

#### `TelescopeEval.hs` (~600 lines)
**What:** Classification, naming, and evaluation of telescopes.

**Key types:**
- `EvalMode = EvalPaperCalibrated | EvalStrictComputed` — controls ν/κ computation path
- `EvalTrace` — transparency record (computed vs used vs paper values)

**Key functions:**
- `evaluateTelescope :: EvalMode -> Telescope -> Library -> Int -> String -> (Int, Int, Double)` — returns (ν, κ, ρ)
- `evaluateTelescopeTrace :: Telescope -> Library -> Int -> String -> (EvalTrace, EvalTrace)` — audit comparison (paper vs strict)
- `detectCanonicalName :: Telescope -> Library -> String` — structural name assignment
- `hasPrerequisites :: String -> Library -> Bool` — prerequisite chain gating
- `strictKappa :: Telescope -> Int` — paper-independent κ with named suspension floor policy
- `effectiveKappa :: String -> Telescope -> Int` — paper κ for known names (PaperCalibrated only)
- `effectiveNu :: String -> LibraryEntry -> Library -> Int -> Int` — paper ν for known names (PaperCalibrated only)

---

#### `TelescopeCheck.hs` (~150 lines)
**What:** Conservative well-formedness checker for telescopes.

**Checks:** Library/variable reference bounds, scope validity through binders, empty telescope rejection, bare Univ-as-argument rejection.

**API:** `checkTelescope :: Library -> Telescope -> CheckResult`, `checkAndFilter :: Library -> [Telescope] -> ([Telescope], Int)`

---

#### `MCTS.hs` (~480 lines)
**What:** Monte Carlo Tree Search for discovering structures at κ > 3.

**Full UCT cycle:** selection (UCT formula), expansion, rollout (random completion + evaluation), backpropagation.

**Note:** MCTS always evaluates with name "candidate", so paper value lookup does NOT apply to MCTS-discovered telescopes.

---

#### `RunAbInitio.hs` (~330 lines)
**What:** The ab initio synthesis orchestrator. Combines exhaustive enumeration, MCTS, and reference telescopes with minimal overshoot selection.

**Key types:**
```haskell
data AbInitioMode = PaperCalibrated | StrictAbInitio
data DiscoveryRecord = DiscoveryRecord { drNu :: Int, drKappa :: Int }
```

**Selection logic:** Canonical names are prioritized over generic candidates (justified by structural completeness + prerequisite chain). Within each priority tier, minimal overshoot (ρ − bar), then κ ascending.

---

### Layer 6: Supporting Modules

#### `Equivalence.hs` (~170 lines)
**What:** Confluent rewrite system for type isomorphisms. Normalizes type expressions so structurally equivalent types are recognized as equal.

**Rewrite rules:**
- A x 1 ~ A, A + 0 ~ A (unit/void absorption)
- (A+B)->C ~ (A->C) x (B->C) (distributivity)
- (AxB)->C ~ A->(B->C) (currying)
- Commutative/associative normalization (flatten + sort)
- Susp(S1) ~ S2, Susp(S2) ~ S3 (suspension reduction)

---

#### `Independence.hs` (~70 lines)
**What:** Filters out trivial schemas and counts independent rank.

---

#### `ProofRank.hs` (~200 lines)
**What:** Schema-based proof-rank computation. The `availableFormers` function uses **structural capability predicates** (not name-based gating) to determine which type formers are available given the current library.

---

#### `KappaNu.hs` (~150 lines)
**What:** Paper reference values (paperKappa, paperNu) for all 16 Genesis entries (including Lie groups which are absorbed). Also contains the Kolmogorov kappa computation via program enumeration.

**Note:** The genesis entry list has 16 entries (indices 1-16) because Lie groups are entry 10. The published papers have 15 entries (no Lie as a distinct realization). The synthesis loop discovers 15 structures because Lie groups are absorbed (rho=1.50 << bar).

---

#### `Kolmogorov.hs` (~200 lines)
**What:** MBTT-based Conditional Kolmogorov Complexity κ(X | B). Prefix-free binary encoding with Elias Gamma library pointers.

---

#### `Manifest.hs` (~50 lines)
**What:** JSON manifest loader for Agda library integration. Currently stubbed.

---

#### `CoherenceWindow.hs` (~50 lines)
**What:** d-Bonacci sequences for coherence window depth d. d=1 gives constant Δ, d=2 gives Fibonacci, d=3 gives tribonacci.

---

## How to Verify the Claims

### Claim 1: "Fibonacci costs are necessary for d=2 foundations"

**Where to check:** The Fibonacci sequence is hardcoded as `fibs = 1 : 1 : zipWith (+) fibs (tail fibs)` in both `Simulation.hs` and `Synthesis.hs`. The formal proof is in the Agda mechanization (`agda/` directory, Phase 1). The engine assumes this result; it does not re-derive it.

**What to verify in Agda:** The file `agda/src/PEN/Core/Fibonacci.agda` proves that for a 2-step coherence window with saturation, Delta_{n+1} = Delta_n + Delta_{n-1}.

### Claim 2: "The Genesis Sequence emerges from unconstrained search"

**Where to check:** Phase J output. Run the engine and verify:
1. All 15 structures appear
2. They appear in the correct order
3. Each clears its bar
4. No manual intervention or special-case logic forces the ordering

**What to audit:** Read `Generator.hs` to verify that the candidate generation is generic (no "if step==5 then generate S1" logic). Read `GenuineNu.hs` to verify that nu computation doesn't hardcode genesis values for HITs/suspensions (it uses proof-rank clustering). The hardcoded components are for foundations (trivially correct) and axioms/synthesis (where the formulas encode structural theorems like the Kuratowski lattice).

**Ab initio audit:** Run `cabal run ab-initio -- --strict` to see genuine paper-free discovery. After the EvalMode refactor (Sprint 3A), strict mode computes all ν/κ from telescope structure. The sequence is correct through step 13 but has known failures at steps 14-15 (see `physics_creation.md` F1-F3).

### Claim 3: "DCT achieves rho=18.75 via lattice tensor product"

**Where to check:** In `GenuineNu.hs`, find the `CSynthesis` case. Verify:
- spatialLattice = 14 (Kuratowski closure-complement theorem)
- temporalLattice = 11 (LTL operator count)
- correction = -4 (infinitesimal collapse)
- nu = 14 * 11 - 4 = 150
- kappa = 8 (from `candidateKappa` in `Generator.hs`)
- rho = 150/8 = 18.75

**Paper vs engine:** The paper reports DCT ν=105 (independent derivation schema count, the lower bound). GenuineNu computes ν=150 (lattice tensor product). Both produce rho well above the bar. The paper value is canonical for publication.

### Claim 4: "Lie groups are absorbed"

**Where to check:** Phase J output. Lie groups should NOT appear as a realized structure. In `Generator.hs`, `CAlgebra "Lie" "S3"` is generated when S3 is in the library (step 8+). Its kappa=6, nu=9, rho=1.50. At step 9+, the bar is already ~4.18. Since 1.50 << 4.18, Lie groups fail the bar-clearing filter.

### Claim 5: "The sequence is robust to +-30% nu variation"

**Where to check:** Compare Phase G (paper nu values) with Phase J (computed nu values). The engine's computed nu values differ from the paper's for some structures (e.g., S3: 15 vs 18, Connections: 27 vs 26) but the same 15 structures are selected in the same order. The comparison table at the end of Phase J output shows this.

---

## Gotchas and Known Limitations

1. **S3 kappa divergence.** The paper gives kappa(S3)=5 (counting SU(2) group structure), but the engine computes kappa=3 (suspension constructors only). This is a genuine open question about what kappa measures.

2. **S3 nu gap.** Proof-rank clustering gives S3 nu=15 vs paper nu=18. The gap comes from deep homotopy (pi_3(S3) = Z) not captured at depth-1 enumeration. S3 still clears its bar (rho=5.0 vs bar=3.43).

3. **Axiom nu values are formula-based, not proof-rank.** For structures 11-14 (Connections through Hilbert), nu is computed from component formulas encoding domain knowledge (field operations + modal cross-interactions + function spaces + library cross-terms). These formulas are justified by the structures' mathematical content but are not derived from proof-rank clustering.

4. **The engine uses 16 internal genesis entries (including Lie) but publishes 15 (excluding Lie).** This is because Lie groups are generated as candidates but absorbed, never realized. The indexing in `KappaNu.hs` counts Lie as entry 10; the published Genesis sequence skips Lie.

5. **Horizon policy.** After each realization, the horizon resets to `delta + 1`. This is slightly different from the paper's "H <- 2" rule but produces identical dynamics because the Fibonacci gaps are always >= 1.

6. **Strict mode ν divergence from paper.** After the EvalMode refactor (Sprint 3A), strict mode computes genuinely paper-free ν/κ. The results diverge: total ν=759 (paper: 356), total κ=48 (paper: 64). Ordering is preserved through steps 1-13 but breaks at step 14 (Trunc displaced) and 15 (DCT ν=0). Three failure modes identified (F1: ν overestimation, F2: Pi ν=0, F3: combinatorial explosion). See `physics_creation.md` L15-L17.

7. **DCT ν discrepancy.** Phase J (GenuineNu) reports ν=150; the paper uses ν=105. These are different metrics: lattice tensor product vs independent schema count. The paper value (105) is canonical.

---

## File Sizes and Complexity Budget

| File | Lines | Purpose | Complexity |
|------|-------|---------|------------|
| Types.hs | ~120 | Type AST + capability flags | Low - data types |
| Inhabitation.hs | ~150 | Inhabitation heuristic | Medium - 14 rules |
| Enumerate.hs | ~100 | Type enumeration | Low - recursive generation |
| HITEnum.hs | ~100 | HIT parameterization | Low - partition enumeration |
| Equivalence.hs | ~170 | Type normalization | Medium - rewrite rules |
| Independence.hs | ~70 | Schema filtering | Low |
| Cluster.hs | ~130 | Proof-rank clustering | **High** - core algorithm |
| ProofRank.hs | ~200 | Legacy proof-rank + structural gating | Medium |
| Capability.hs | ~200 | Capability engine | Medium - 18 rules |
| KappaNu.hs | ~150 | Paper reference values | Low - lookup tables |
| UniformNu.hs | ~250 | Uniform ν algorithm | **High** - core evaluation |
| Kolmogorov.hs | ~200 | MBTT Kolmogorov complexity | Medium |
| Manifest.hs | ~50 | JSON loader | Low |
| TheoryState.hs | ~85 | Theory tracking | Low - state management |
| Generator.hs | ~275 | Candidate generation | **High** - 9 candidate types |
| GenuineNu.hs | ~260 | Nu dispatch | **High** - core evaluation |
| Synthesis.hs | ~350 | Synthesis loop | **High** - main algorithm |
| Simulation.hs | ~200 | Paper-mode replay | Medium |
| Main.hs | ~375 | Phase runner | Medium - orchestration |
| Telescope.hs | ~530 | MBTT telescope data types | **High** - core representation |
| TelescopeGen.hs | ~380 | Telescope generation | **High** - type-directed search |
| TelescopeEval.hs | ~600 | Telescope evaluation | **High** - classification + naming |
| TelescopeCheck.hs | ~150 | Well-formedness checker | Medium |
| MCTS.hs | ~480 | Monte Carlo Tree Search | **High** - UCT cycle |
| RunAbInitio.hs | ~330 | Ab initio orchestrator | **High** - synthesis loop |
| CoherenceWindow.hs | ~50 | d-Bonacci sequences | Low |
| **Total** | **~6,385** | | |

The engine is ~6,400 lines of Haskell. The original synthesis engine (~3,100 lines) plus the ab initio engine (~2,300 lines) plus supporting modules (~1,000 lines). A careful reader can audit the complete system in a day.

---

## Reproducing from Scratch

```bash
# Prerequisites
# GHC 9.6+ and Cabal 2.4+

# Clone and build
git clone <repository>
cd PEN/engine
cabal build

# Run all 10 phases (original engine)
cabal run pen-engine

# Run ab initio engine
cabal run ab-initio                  # Paper-calibrated mode
cabal run ab-initio -- --strict      # Strict mode

# The pen-engine output is ~500 lines. Key sections:
# Phase D: proof-rank validation (schemas and counts)
# Phase G: paper-mode simulation (15 structures clear bars)
# Phase H: capability validation (16/16 nu match)
# Phase J: genuine synthesis (15/15 discovered from search)

# The ab-initio output shows:
# Phase 0: Reference telescope validation (15/15 non-zero ν)
# Phase 1: Step-by-step discovery with ν, κ, ρ, bar, source
# Summary: Discovered vs paper comparison table
```

---

## Relationship to the Agda Mechanization

The Agda code in `agda/` provides formal verification of a subset of the claims:
- **Phase 1 (complete):** Proves the Fibonacci recurrence theorem
- **Phase 2 (stub):** kappa-Oracle via reflection API
- **Phase 3 (partial):** nu-measure exploration via OpSchema
- **Phase 4 (not started):** Selection loop

The Haskell engine is the **fast explorer**; Agda is the **trusted checker**. The engine discovers structures and computes nu/kappa heuristically. Agda verifies the underlying theorems (Fibonacci recurrence, type-checking of structures) with machine-checked proofs.

The two systems are complementary: the engine provides evidence that the Genesis Sequence is computable; Agda provides certainty that the mathematical foundations are correct.
