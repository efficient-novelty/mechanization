# PEN Ab Initio Discovery Engine — Implementation Plan

## Goal

Transition PEN from an evaluative filter (ordering human-curated candidates)
to an ab initio discovery engine that autonomously synthesizes the 15-step
Generative Sequence from an empty library, proving that mathematical physics
emerges as the unique attractor of efficiency optimization at d=2.

---

## Phase 1: The Generative Substrate (MBTT Telescopes) [ACTIVE]

**Goal**: Unify all candidate generation under a single syntactic concept —
context telescopes of MBTT expressions — eliminating the 9 hardcoded categories.

### 1.1 Create `Telescope.hs` — Core Data Types [TODO]

New module `engine/src/Telescope.hs` with:

```haskell
-- A telescope entry: a named declaration with a type
data TeleEntry = TeleEntry
  { teName   :: String     -- bound name (c_1, c_2, ...)
  , teType   :: MBTTExpr   -- the type of this declaration
  } deriving (Show, Eq)

-- A telescope: ordered sequence of dependent declarations
-- Δ = [c₁ : A₁, c₂ : A₂(c₁), ..., cκ : Aκ(c₁...cκ₋₁)]
newtype Telescope = Telescope { teleEntries :: [TeleEntry] }
  deriving (Show, Eq)

-- Telescope length = κ (construction effort)
teleKappa :: Telescope -> Int
teleKappa = length . teleEntries

-- Total MBTT bit cost (for Kolmogorov audit)
teleBitCost :: Telescope -> Int
teleBitCost = sum . map (bitLength . teType) . teleEntries

-- Convert telescope to LibraryEntry (for UniformNu evaluation)
teleToEntry :: Telescope -> LibraryEntry

-- Convert telescope to TypeExpr (for inhabitation checking)
teleToTypeExprs :: Telescope -> Library -> [TypeExpr]
```

### 1.2 Create `TelescopeGen.hs` — Type-Directed Generator [TODO]

New module `engine/src/TelescopeGen.hs` with:

```haskell
-- A typed hole: a position in a partially-built telescope
data Hole = Hole
  { holeCtx   :: [TeleEntry]  -- context so far (bound variables)
  , holeGoal  :: HoleGoal     -- what we're trying to fill
  , holeDepth :: Int           -- depth in the AST
  }

data HoleGoal
  = TypeHole          -- ? : U  (generate a type)
  | TermHole MBTTExpr -- ? : A  (generate a term of type A)

-- The actions available at a hole (contextual pruning)
data Action
  = AUniv             -- U
  | AVar Int          -- Var(i) — reference a bound variable
  | ALib Int          -- Lib(i) — reference a library entry
  | APi               -- Pi(A,B) — open two sub-holes
  | ASigma            -- Sigma(A,B)
  | ALam              -- Lam(body)
  | AApp              -- App(f,x)
  | AId               -- Id(A,x,y)
  | ARefl             -- Refl(a)
  | ASusp             -- Susp(A)
  | ATrunc            -- Trunc(A)
  | APathCon Int      -- PathCon(d)
  | AFlat | ASharp | ADisc | AShape  -- Modal
  | ANext | AEventually               -- Temporal
  deriving (Show, Eq)

-- Generate valid actions at a typed hole (the core pruning engine)
validActions :: Hole -> Library -> [Action]

-- Expand a hole with an action, producing new sub-holes
expandHole :: Hole -> Action -> Library -> (MBTTExpr, [Hole])

-- Enumerate all complete telescopes up to length κ ≤ horizon
-- (exhaustive for small κ, returns generator for large κ)
enumerateTelescopes :: Library -> Int -> [Telescope]
```

### 1.3 Create `TelescopeEval.hs` — Bridge to UniformNu [TODO]

```haskell
-- Convert a raw telescope to the data structures UniformNu expects
telescopeToCandidate :: Telescope -> Library -> LibraryEntry

-- Fast ν evaluation for a telescope
evaluateTelescope :: Telescope -> Library -> Int -> (Int, Int, Double)
-- Returns (nu, kappa, rho = nu/kappa)
```

### 1.4 Translate Existing Specs to Telescopes [TODO]

Create a reference table mapping all 15 genesis structures to their telescope
representations (derived from Kolmogorov.hs's genesisSpecs). This serves as
ground truth for validation.

### 1.5 Validate: Telescope ν matches Paper ν [TODO]

Run the telescope evaluator on all 15 reference telescopes and verify that
the computed ν matches the paper's values (or the uniform algorithm's values).

---

## Phase 2: Tractability via Type-Directed Synthesis

**Goal**: Embed a bidirectional type-checker into the generator so that only
well-typed MBTT terms are ever constructed, pruning >99.9% of the search space.

### 2.1 Implement Bidirectional Type Checker [TODO]

New module `engine/src/MBTTCheck.hs`:

```haskell
-- Type-checking context
data TyCtx = TyCtx
  { tcBound :: [(String, MBTTExpr)]  -- bound variables with types
  , tcLib   :: Library                -- the historical library
  }

-- Bidirectional type checking
checkType :: TyCtx -> MBTTExpr -> MBTTExpr -> Bool   -- Γ ⊢ e : A
inferType :: TyCtx -> MBTTExpr -> Maybe MBTTExpr      -- Γ ⊢ e ⇒ A
```

### 2.2 Top-Down Hole-Filling with Type Constraints [TODO]

Extend `TelescopeGen.hs` to use the type-checker for contextual pruning:
- At `TypeHole`: only propose well-formed type expressions
- At `TermHole A`: only propose terms that inhabit A
- Library pointers: only propose `Lib(i)` when `Lib(i)` has the right type

### 2.3 Benchmark: Exhaustive Search for κ ≤ 3 [TODO]

Run exhaustive type-directed telescope generation for κ ∈ {1,2,3} and verify
that the first 5 genesis structures (Universe, Unit, Witness, Pi/Sigma, S1)
are discovered as the top-ρ candidates.

---

## Phase 3: Theorem-Driven Search Pruning

**Goal**: Hardcode PEN's structural theorems as admissibility filters.

### 3.1 Maximal Interface Density Filter [TODO]

If a telescope of length κ > 1 contains zero `Lib(n)` or `Lib(n-1)` pointers
(where n = current library size), prune it. Forces building on the bleeding
edge of the library.

### 3.2 Structural Unity Filter [TODO]

Build a dependency graph for the telescope entries. If entry c_i is never
referenced by any c_j (j > i), the telescope is disconnected — reject it.
This prevents axiom packing.

### 3.3 Redundancy Filter [TODO]

If a telescope's entries are all derivable from the existing library
(ν = 0 for each entry independently), reject it.

---

## Phase 4: The AI Core (MCTS with Combinatorial Gradients)

**Goal**: For κ ∈ [4, 9], use Monte Carlo Tree Search to navigate the
telescope space, guided by efficiency gradients.

### 4.1 Create `MCTS.hs` — Core MCTS Engine [TODO]

```haskell
-- MCTS tree node
data MCTSNode = MCTSNode
  { nodeState   :: PartialTelescope  -- partially built telescope
  , nodeVisits  :: !Int
  , nodeReward  :: !Double           -- cumulative reward
  , nodeChildren :: Map Action MCTSNode
  }

-- UCT selection
uctSelect :: MCTSNode -> Double -> Action

-- MCTS loop
mctsSearch :: Library -> Int -> Int -> IO Telescope
-- mctsSearch lib maxKappa numIterations
```

### 4.2 Rollout Policy [TODO]

When a partially-built telescope needs evaluation:
1. Complete it with random valid actions (biased toward library pointers)
2. Evaluate ρ = ν/κ via the fast evaluator
3. Backpropagate the reward

### 4.3 Progressive Widening [TODO]

For nodes with many valid actions, use progressive widening:
- Start with the highest-priority actions (Lib pointers, Pi, Sigma)
- Expand to less likely actions (PathCon, Modal) only after sufficient visits

### 4.4 Integration with Selection Loop [TODO]

Replace `Generator.hs` in `Synthesis.hs` with the MCTS generator:
- For each step n, run MCTS to find the top-k telescopes
- Evaluate each via UniformNu
- Select the one that clears the bar with minimal overshoot

---

## Phase 5: Fast Inner-Loop Differential Evaluation

**Goal**: Make ν computation fast enough for thousands of MCTS rollouts/second.

### 5.1 Create `DiffNu.hs` — Differential Schema Enumeration [TODO]

Instead of full before/after comparison, only enumerate schemas that contain
at least one symbol from the new telescope. This reduces enumeration from
O(|B|^d) to O(κ · |B|^(d-1)).

### 5.2 Schema Cache [TODO]

Cache the "before" schema set across MCTS iterations (it only changes when
a new structure is realized, not between candidates).

### 5.3 Adjoint Completion Fast Path [TODO]

Use Lemma 9.4 to skip elimination schema enumeration entirely: count intro
schemas and double them (modulo adjoint arity).

### 5.4 Benchmark: ≥1000 evaluations/second [TODO]

Profile and optimize until the differential evaluator achieves ≥1000 ν
evaluations per second on a single core.

---

## Phase 6: The Univalent Rosetta Stone and Agda Verification

**Goal**: When MCTS discovers a winning telescope, identify it and verify it.

### 6.1 Create `RosettaStone.hs` — Semantic Isomorphism Checker [TODO]

Maintain a dictionary of the 15 target specifications (from Appendix B).
When a winning telescope is generated, check structural equivalence
(up to β-reduction, Σ-reordering, and univalent isomorphism).

### 6.2 Create `AgdaEmit.hs` — Agda Metaprogramming [TODO]

Pretty-print winning MBTT ASTs into Cubical Agda syntax:
- Wrap in opaque record (Sealing Encapsulation)
- Generate module header with correct imports
- Output to `agda/Synthesized/StepN.agda`

### 6.3 Create `AgdaVerify.hs` — Automated Verification [TODO]

Invoke `agda --cubical --safe` on the generated files via a background daemon.
If Agda accepts the file, the discovery is formally verified.

### 6.4 End-to-End Validation [TODO]

Run the full pipeline from empty library through all 15 steps:
1. MCTS discovers a telescope
2. RosettaStone identifies it
3. AgdaEmit generates the Agda file
4. AgdaVerify type-checks it
5. Print: `[DISCOVERY] Step N: <Name> synthesized — Agda verified`

---

## Implementation Order

Phase 1 is the foundation — everything else builds on it. Within Phase 1,
the order is: 1.1 → 1.2 → 1.3 → 1.4 → 1.5.

After Phase 1 is validated, Phases 2-3 can proceed in parallel with Phase 5.
Phase 4 depends on Phases 2, 3, and 5. Phase 6 depends on Phase 4.

```
Phase 1 (Substrate)
  ├── Phase 2 (Type-Directed) ──┐
  ├── Phase 3 (Pruning) ────────┤
  └── Phase 5 (Fast Eval) ──────┴── Phase 4 (MCTS) ── Phase 6 (Verify)
```

---

## Progress Log

*(Entries added as items are completed)*
