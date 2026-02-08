# Research Plan: Constructing Genesis Objects from Primitives

## Status Quo

The PEN engine currently **pre-defines** all 16 Genesis structures as hardcoded
`LibraryEntry` and `StructureDesc` records. The simulation selects which
structures get realized and in what order, but never constructs or discovers
them. Every piece of metadata — names, constructors, path dimensions, κ values,
ν values — is manually specified.

The three "computed" ν methods (Shannon enumeration, proof-rank, K-novelty) all
fail to reproduce paper values. The Capability engine achieves exact agreement
but encodes per-structure domain knowledge, making it a principled lookup table
rather than a genuine computation.

**Goal**: Make the engine actually *construct* mathematical structures from
primitives, then evaluate them using a genuine novelty measure, so that the
Genesis sequence (or something close to it) *emerges* from search rather than
being pre-specified.

---

## 1. What "Construction from Primitives" Means

There are three levels of ambition, each substantially harder than the last:

### Level A: Type Synthesis (feasible near-term)

Given a fixed set of type formers (Unit, Void, Π, Σ, Id, Susp, Trunc, HIT),
enumerate all well-formed types up to some complexity bound and select the one
maximizing ρ = ν/κ at each step. The engine *discovers* that S¹ = HIT(1,[1])
is the optimal next structure after Pi/Sigma, etc.

**Scope**: Structures 1–8 (Universe through S³), possibly PropTrunc.

### Level B: Structure Discovery (medium-term research)

Go beyond individual types to discover *relationships* between types:
fibrations, group structures, exact sequences. The Hopf fibration isn't a type
but a specific map S³ → S² with fiber S¹. Lie groups aren't single types but
algebraic structures on existing spaces.

**Scope**: Structures 1–10 (through Lie groups).

### Level C: Framework Invention (long-term / open research)

Discover entirely new axiomatic extensions to the type theory itself: modalities
(Cohesion), connections, curvature, Hilbert space axioms. This is essentially
automated theory building — the engine would need to propose new inference rules
or axiom schemas, not just new types.

**Scope**: Structures 11–16 (Cohesion through DCT).

This plan focuses primarily on **Level A**, with design decisions that keep
the door open for Levels B and C.

---

## 2. Architecture Overview

```
                    ┌─────────────┐
                    │  Primitives  │  (Π, Σ, Id, HIT schema, Susp, Trunc, ...)
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │  Generator   │  Enumerate candidate structures
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │  Evaluator   │  Compute genuine κ and ν
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │  Selector    │  PEN axiom simulation (bar-clearing)
                    └──────┬──────┘
                           │
                    ┌──────▼──────┐
                    │  Library L   │  Accumulates realized structures
                    └─────────────┘
```

Each component needs redesign relative to the current engine:

---

## 3. Component Design

### 3.1 Primitives: A Richer Type Theory

The current `TypeExpr` AST is too shallow. It represents *types* but not
*structures-with-properties*. We need:

**3.1.1 Explicit HIT Specifications**

Replace the opaque `THIT Int [Int]` with a structured HIT definition:

```haskell
data HITDef = HITDef
  { hitPoints :: [(String, TypeExpr)]       -- Named point constructors + types
  , hitPaths  :: [(String, PathSpec)]       -- Named path constructors
  , hitHigher :: [(String, HigherPathSpec)] -- 2-cells, etc.
  }

data PathSpec = PathSpec
  { psSource :: Term    -- Source endpoint
  , psTarget :: Term    -- Target endpoint
  , psBaseType :: TypeExpr  -- Type the path lives in (usually self)
  , psDimension :: Int
  }
```

This lets the engine distinguish S¹ (one point `base`, one loop `loop : base = base`)
from S² (one point, one 2-cell `surf : refl = refl`) syntactically, and
enumerate HIT schemas parametrically.

**3.1.2 Type Former Availability as State**

Currently, type formers like Π and Σ are always syntactically available but
semantically gated by the library. The engine should track which type formers
are available as part of the evolving theory:

```haskell
data TheoryState = TheoryState
  { tsLibrary     :: Library
  , tsFormers     :: Set TypeFormer    -- {Pi, Sigma, Id, Susp, Trunc, ...}
  , tsAxioms      :: [Axiom]          -- E.g., univalence, funext, ...
  , tsModalities  :: [Modality]       -- (Level C)
  }

data TypeFormer = FPi | FSigma | FId | FSusp | FTrunc | FHIT
  deriving (Eq, Ord, Show)
```

Adding a type former (e.g., Π/Σ at step 4) should be a candidate action
alongside adding a type, and it should have its own κ and ν.

**3.1.3 Candidate Actions (not just types)**

Generalize from "add a type to the library" to:

```haskell
data Candidate
  = AddType TypeExpr HITDef        -- Add a concrete type
  | AddFormer TypeFormer           -- Add a type former (Π, Σ, Trunc, ...)
  | AddAxiom Axiom                 -- Add an axiom (univalence, ...)
  | AddStructure Structure         -- (Level B) Add algebraic structure
  | AddModality Modality           -- (Level C) Add modal operator
```

### 3.2 Generator: Bounded Enumeration of Candidates

**3.2.1 The Core Problem: Search Space Size**

The current enumerator at complexity ≤ 4 already produces ~2800 types for a
7-entry library. Most are trivially uninteresting (e.g., `(1 × 1) → (1 + 0)`).

The key insight is that we don't need to enumerate all types — we need to
enumerate *structurally distinct* candidates. The schema abstraction from
ProofRank.hs is relevant here: many concrete types represent the same
"proof technique."

**3.2.2 HIT Schema Enumeration**

For HITs specifically, enumerate schemas parametrically:

```
Cost 2: HIT(1 point, 0 paths)           -- discrete point (≈ Unit)
Cost 3: HIT(1 point, 1 path dim=1)      -- S¹
         HIT(2 points, 0 paths)          -- Bool / 2
Cost 4: HIT(1 point, 1 path dim=2)      -- S² candidate
         HIT(1 point, 2 paths dim=1)     -- figure-eight?
         HIT(2 points, 1 path dim=1)     -- interval
Cost 5: HIT(1 point, 1 path dim=3)      -- S³ candidate
         ...
```

The search space of HITs grows polynomially in cost (unlike the exponential
blowup from type former composition), making this tractable.

**3.2.3 Pruning Strategies**

1. **Normal-form filtering**: Only enumerate types in β-normal, η-long form.
   Skip `1 × A` (≃ A), `0 + A` (≃ A), `A → 1` (≃ 1), etc.

2. **Schema deduplication**: Two candidates with the same schema (after
   library-abstraction) are interchangeable. Only keep one representative.

3. **Monotone novelty**: If a candidate has ν = 0, skip it immediately. A type
   that doesn't enable anything new is never selected.

4. **Admissibility pre-filter**: Only enumerate up to complexity H (the current
   horizon). Don't waste time on candidates that can't be admitted.

5. **Symmetry breaking**: For HITs, impose a canonical ordering on path
   constructors (non-decreasing dimension) to avoid enumerating permutations.

### 3.3 Evaluator: Genuine κ and ν

This is the hardest component. The current system fails here because:
- Shannon ν overcounts (73 for Unit vs paper 1)
- Proof-rank undercounts some, overcounts others
- K-novelty overcounts massively

**3.3.1 κ: Kolmogorov Complexity**

The current approach (enumerate all programs, find shortest that produces the
target type) works in principle but hits combinatorial explosion at cost ~6.

Improvements:
- **Bidirectional search**: Search forward from library atoms AND backward from
  the target, meeting in the middle. Reduces search from O(b^d) to O(b^{d/2}).
- **Memoized decomposition**: For `Susp(S¹)`, recognize that if S¹ has cost 3,
  then `Susp(S¹)` has cost 4 = 1 + cost(S¹). Don't re-derive S¹.
- **Type-directed synthesis**: Use the target type's structure to guide search
  (e.g., a HIT must be built with PMakeHIT, a suspension with PSusp).

For Level A, the paper's κ values are small (1–9), so improving the enumerator
to handle cost ≤ 10 reliably may suffice.

**3.3.2 ν: The Central Research Question**

The paper's ν counts "independent mathematical capabilities unlocked." Making
this precise and computable is the crux of the research. Three approaches:

#### Approach 1: Enriched Inhabitation Counting (fix Shannon ν)

The Shannon ν overcounts because it counts every syntactically distinct newly
inhabited type. The fix is to count *semantically distinct* capabilities:

1. **Quotient by known isomorphisms**: `A × B ≃ B × A`, `A × 1 ≃ A`,
   `(A → B → C) ≃ (A × B → C)`, etc. Build an equivalence relation on types
   and count equivalence classes, not raw types.

2. **Quotient by derivability**: If `Ω(S¹)` is inhabited and this implies
   `Ω(S¹) → 1` is inhabited, don't count both — the second is derived from
   the first.

3. **Independence filtering**: Two newly inhabited types T₁, T₂ are
   *independent* if neither is derivable from the other (plus the library).
   Count the rank of the independence relation.

This is essentially making the proof-rank approach work correctly:

```haskell
genuineNu :: TypeExpr -> Library -> Int
genuineNu newType lib =
  let allNew = newlyInhabitedTypes newType lib maxComplexity
      grouped = quotientByIsomorphism allNew
      independent = filterIndependent grouped lib
  in length independent
```

The key missing piece is a sound isomorphism checker and derivability oracle.

#### Approach 2: Information-Theoretic ν (compression-based)

Instead of counting types, measure the *information gain* from adding X to the
library. This is closer to the Kolmogorov-complexity spirit:

```
ν(X | L) = Σ_T max(0, K(T | L) - K(T | L∪{X}))
```

Sum over all "interesting" types T the drop in Kolmogorov complexity from
adding X. This is what K-novelty attempts, but it overcounts because it sums
over too many types.

Fix: weight by the inverse of type complexity, or only count types whose
complexity drops below a threshold, or use schema-deduplication.

#### Approach 3: Capability Semantics (formalize the Capability engine)

The Capability engine's rules encode mathematical knowledge. Could we *derive*
these rules from the type theory?

For example, the "function-space" rule (ν += 2 when Π is available) could be
derived by:
1. Noticing that Π-types enable `X → L` and `L → X`
2. Checking that these are newly inhabited
3. Checking that they are independent

If we can mechanize steps 1–3 for each rule family, we get a principled
version of the Capability engine that generalizes to novel structures.

**3.3.3 Recommended Path: Hybrid Approach**

Combine Approaches 1 and 3:
- Use enriched inhabitation counting as the base
- Identify specific rule families that the counting should discover
- Validate that the counting produces the right answer for each rule
- Use the Capability engine's per-structure traces as regression tests

### 3.4 Selector: Existing Simulation with Genuine Values

The simulation loop (Simulation.hs) needs minimal changes — it already
abstracts over the evaluation strategy via `SimMode`. Add a new mode:

```haskell
data SimMode = PaperMode | ComputedMode | CapabilityMode | SynthesisMode
```

In `SynthesisMode`:
1. Generator produces candidate types/formers up to complexity H
2. Evaluator computes genuine κ and ν for each candidate
3. Selector applies the five PEN axioms as today
4. Winner is added to library (and theory state)

---

## 4. Concrete Implementation Phases

### Phase 1: Enriched Type Equivalence (2–3 weeks)

**Goal**: Build a robust isomorphism checker for types up to complexity ~8.

**Tasks**:
- Implement known HoTT isomorphisms as rewrite rules:
  - `A × 1 ≃ A`, `A + 0 ≃ A`, `A × 0 ≃ 0`
  - `A × B ≃ B × A`, `A + B ≃ B + A`
  - `(A × B) × C ≃ A × (B × C)`
  - `A → (B × C) ≃ (A → B) × (A → C)`
  - `(A + B) → C ≃ (A → C) × (B → C)`
  - `0 → A ≃ 1`, `A → 1 ≃ 1`, `1 → A ≃ A`
  - `Ω(A) ≃ (pt =_A pt)`, `Susp(0) ≃ 1`
  - `ΣS^n ≃ S^{n+1}` (suspension of spheres)
- Build canonical-form normalization (confluent rewrite system)
- Implement an equivalence checker: `typeEquiv :: TypeExpr -> TypeExpr -> Bool`
  that handles all of the above

**Deliverable**: `Equivalence.hs` module. Test against known equivalences.
Validate that the number of *equivalence classes* among newly inhabited types
is closer to paper ν than the raw count.

### Phase 2: Derivability Relation and Independence (3–4 weeks)

**Goal**: Given a set of newly inhabited types, compute the *rank* — the
number of independent generators.

**Tasks**:
- Define derivability: T₁ derives T₂ if, given an inhabitant of T₁ (plus L),
  we can construct an inhabitant of T₂. For the types we care about, this
  reduces to specific patterns:
  - From `A`, derive `A × 1`, `B → A`, `A + 0`, etc.
  - From `A × B`, derive `A` and `B` (projections)
  - From `A + B`, derive `A → C` and `B → C` implies `(A + B) → C`
  - From `A → B` and `A`, derive `B` (modus ponens)
- Build a derivability DAG over newly inhabited types
- Compute the set of generators (sources in the DAG)
- Count generators = ν

**Deliverable**: `Independence.hs` module. Integrate with the ν computation.
Compare `independenceNu` against paper values for structures 1–8.

### Phase 3: HIT Schema Enumeration (2–3 weeks)

**Goal**: Automatically enumerate HIT candidates so the engine can *discover*
S¹, S², S³ rather than having them pre-defined.

**Tasks**:
- Implement `HITDef` type with explicit point/path constructors
- Implement `enumerateHITs :: Int -> [HITDef]` that generates all HITs up to
  a given cost
- Implement κ for HITs: cost = 1 + points + Σ(path dimensions)
- Implement inhabitation for structured HITs (not just point-count heuristic)
- Implement loop detection: does a HIT have non-trivial loops?
- Connect to the evaluator: compute ν for a candidate HIT

**Deliverable**: `HITEnum.hs` module. Verify that the enumerator produces
S¹ = HIT(1,[1]) at cost 3, S² = HIT(1,[2]) at cost 4, and that these
have the highest ν among their cost-peers.

### Phase 4: Type Former Discovery (2 weeks)

**Goal**: Allow the engine to "discover" Π/Σ types and PropTrunc as candidates
alongside concrete types.

**Tasks**:
- Define `Candidate` ADT (AddType | AddFormer | ...)
- Define κ for type formers: adding Π/Σ has κ = 3 (matching paperKappa)
- Define ν for type formers: count how many new type schemas become available
  when Π/Σ are added to the theory
  - With Π: all function types `A → B` are now meaningful
  - With Σ: all product types `A × B` are now meaningful
  - Before Π/Σ: these are syntactically present but semantically inert
- Extend the simulation to handle `Candidate` instead of just `LibraryEntry`

**Deliverable**: Modified `Simulation.hs` and `Generator.hs`. Verify that when
all candidates (types + formers) compete, the engine selects Universe, Unit,
Witness, Π/Σ, S¹, PropTrunc, S², S³ in that order (or close to it).

### Phase 5: Integration and Validation (2–3 weeks)

**Goal**: Full synthesis pipeline: start from empty theory, let the engine
discover and realize structures via the PEN axioms.

**Tasks**:
- Implement `SynthesisMode` in the simulation
- Full pipeline: enumerate candidates → evaluate κ, ν → select winner → update
  theory state → repeat
- Compare discovered sequence against the Genesis sequence
- Instrument with detailed logging: for each tick, show all candidates
  considered, their κ and ν, why the winner was selected
- Performance optimization: the inner loop needs to be fast enough that the
  simulation completes in reasonable time (target: < 60 seconds for 8 steps)

**Deliverable**: Phase J in Main.hs. Side-by-side comparison of discovered
sequence vs Genesis sequence for steps 1–8.

### Phase 6: Beyond Level A (research directions)

**6a. Fibrations and Maps (Level B)**:
- Extend candidates to include maps between existing types (not just new types)
- A map `f : A → B` is a candidate if its fiber `Fib(f)` provides new structure
- The Hopf fibration would emerge as a specific map S³ → S² whose fiber ≃ S¹
- This requires representing and evaluating maps, not just types

**6b. Algebraic Structures (Level B)**:
- Extend candidates to include group/ring/module structures on existing types
- Lie groups would emerge as group structures on spheres
- Requires representing algebraic axioms (associativity, inverses, etc.)

**6c. Theory Extensions (Level C)**:
- Allow candidates that extend the type theory itself (new modalities, axioms)
- Cohesion would emerge as a candidate modality triple (♯, ♭, ʃ)
- Requires a meta-theory: the engine reasons about type theories, not just
  within a fixed type theory

---

## 5. Key Research Questions

### Q1: Can enriched inhabitation counting match paper ν for structures 1–8?

This is the most important near-term question. If yes, it validates the entire
approach. If no, we need to understand what the paper's ν is actually counting
and whether it can be mechanized at all.

**Hypothesis**: Isomorphism-quotiented, independence-filtered inhabitation
counting will produce values within ±20% of paper ν for structures 1–8.

**Test**: Implement Phases 1–2 and measure.

### Q2: Does the Genesis sequence emerge from unconstrained search?

If we enumerate all candidates up to the horizon and select by ρ = ν/κ with
bar-clearing, does the engine produce the same sequence as the paper?

**Hypothesis**: The first 6–7 structures will match exactly. Later structures
may diverge because the search space becomes too large for exact enumeration
and the ν computation becomes approximate.

**Test**: Implement Phase 5 and compare.

### Q3: How sensitive is the sequence to the ν counting method?

Small changes in how we count ν could change the ρ rankings and thus the
selection order. Is the Genesis sequence a robust attractor, or a fragile
consequence of specific counting conventions?

**Test**: Run the synthesis with multiple ν methods (Shannon, proof-rank,
enriched inhabitation, K-novelty) and compare sequences. If all methods
produce similar sequences, the Genesis sequence is robust.

### Q4: What is the minimal theory needed to derive the Genesis sequence?

Can we identify the smallest set of assumptions (primitives + evaluation rules)
that causes the Genesis sequence to emerge? This would strengthen the paper's
claim that the sequence is canonical.

### Q5: Where does Level A break down?

At what point does "enumerate types and evaluate novelty" stop working, and
why? The answer determines what Level B/C features are actually needed versus
nice-to-have.

---

## 6. Risk Analysis

| Risk | Impact | Likelihood | Mitigation |
|------|--------|------------|------------|
| ν computation doesn't converge to paper values | High | Medium | Multiple approaches (§3.3.2); accept ±20% as success |
| Search space too large for structures 5+ | High | Medium | Aggressive pruning (§3.2.3); schema deduplication |
| Isomorphism checker is unsound or incomplete | Medium | Medium | Conservative: only quotient by known-safe isos |
| κ computation too slow for inner loop | Medium | Low | Bidirectional search; memoization; cap at cost 10 |
| Discovered sequence diverges from Genesis | Medium | Medium | Treat divergence as a *finding*, not a failure |
| Type former discovery produces degenerate results | Low | Medium | Constrain to well-known type formers initially |

---

## 7. Success Criteria

### Minimum Viable Result
- Engine discovers structures 1–5 (Universe, Unit, Witness, Pi/Sigma, S¹)
  from primitives in the correct order
- Computed ν is within ±30% of paper values for these structures
- Total runtime < 5 minutes

### Target Result
- Engine discovers structures 1–8 (through S³) from primitives
- Computed ν matches paper values within ±15%
- Genesis sequence order matches exactly for 1–8
- Lie groups (structure 10) are correctly absorbed (low ρ)
- Total runtime < 10 minutes

### Stretch Result
- Engine discovers structures 1–10 from primitives
- ν matches within ±10%
- Clear explanation of *why* each structure is selected at each step
- Quantitative characterization of where Level A breaks down

---

## 8. Relationship to Existing Work

- **Automated Theorem Proving**: Techniques from ATP (resolution, superposition)
  could help with the derivability relation in Phase 2.
- **Program Synthesis**: The κ computation is a program synthesis problem.
  Techniques from SyGuS (Syntax-Guided Synthesis) may help.
- **Mathematical Discovery (AM, HR, Graffiti)**: Doug Lenat's AM and Simon
  Colton's HR attempted automated mathematical concept discovery. Our approach
  is more constrained (type-theoretic) but shares the goal.
- **Proof Assistants (Agda, Lean)**: The Agda mechanization in `agda/` provides
  ground truth. Lean's `aesop` tactic could inspire the derivability checker.
- **HoTT Libraries**: The HoTT-Agda and Lean-HoTT libraries enumerate exactly
  the constructions we want to discover. They provide validation data.

---

## 9. File Plan

| File | Purpose |
|------|---------|
| `src/Equivalence.hs` | Isomorphism checker and canonical-form normalization |
| `src/Independence.hs` | Derivability relation and independence rank |
| `src/HITEnum.hs` | Parametric HIT enumeration |
| `src/Generator.hs` | Candidate generation (types + formers + structures) |
| `src/TheoryState.hs` | Evolving theory state (available formers, axioms) |
| `src/GenuineNu.hs` | Genuine ν computation combining equivalence + independence |
| `src/Synthesis.hs` | SynthesisMode simulation driver |
| `src/Capability.hs` | (Existing) Reference capability engine for regression tests |
| `src/Simulation.hs` | (Modified) Add SynthesisMode |
| `src/Main.hs` | (Modified) Add Phase J: synthesis comparison |

---

## 10. Recommended Starting Point

**Start with Phase 1 (Equivalence) + Phase 3 (HIT Enumeration) in parallel.**

Phase 1 is the foundation for everything else: without a good equivalence
checker, every ν method will overcount. Phase 3 is the most satisfying early
win: making the engine discover S¹ from scratch is a concrete, testable
milestone.

A concrete first experiment:
1. Build `HITEnum.hs` to enumerate HITs up to cost 5
2. For each HIT, compute Shannon ν with the current (overcounting) method
3. Sort by ρ = ν/κ
4. Check whether S¹ = HIT(1,[1]) has the highest ρ among cost-3 candidates
   when evaluated against library = {U, 1, ★, Π/Σ}

If S¹ wins this competition, the basic approach works and the remaining effort
is in refining ν to get exact values. If it doesn't, we learn something
important about what the ν computation needs to capture.
