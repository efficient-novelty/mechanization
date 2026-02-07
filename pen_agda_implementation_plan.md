# PEN Mechanization in Cubical Agda: Implementation Plan

## Executive Summary

This plan has one goal: **produce a computation that a skeptical mathematician can inspect.** Everything else — the physics papers, the philosophy, the cosmological predictions — is downstream of whether this computation works.

The plan is organized in four phases, each delivering a self-contained, testable artifact. Each phase has a clear "done" criterion. You should not begin the next phase until the current one compiles and you understand what it's telling you.

**Estimated timeline:** 3–6 months for Phases 1–3, depending on your Agda fluency. Phase 4 is open-ended.

---

## Phase 0: Environment and Foundations (1–2 weeks)

### Goal
Get a working Cubical Agda environment with the standard library, and build fluency with the core primitives you'll need.

### Setup
```
# Install Agda 2.6.4+ with Cubical support
# Install cubical library from: https://github.com/agda/cubical

# Project structure
PEN/
  Core/
    Nat.agda          -- Natural numbers, Fibonacci
    Graph.agda        -- Finite directed graphs
    Sequence.agda     -- Indexed sequences, cumulative sums
  ObligationGraph/
    Interface.agda    -- The Disjoint Interface definition
    Recurrence.agda   -- The Fibonacci proof
  Oracle/
    Kappa.agda        -- The effort measure
    Nu.agda           -- The novelty measure
    Efficiency.agda   -- Selection dynamics
  Genesis/
    Candidates.agda   -- The candidate DSL
    Selection.agda    -- The selection loop
    Trace.agda        -- The output sequence
  Test/
    Fibonacci.agda    -- Unit tests for the recurrence
    KnownTypes.agda   -- ν/κ for known HoTT types
```

### Warm-up exercises (essential — don't skip)
Before touching PEN, implement these in Cubical Agda to build fluency:

1. **Define Fibonacci** as a function `fib : ℕ → ℕ` and prove `Σ fib(i) for i=1..n = fib(n+2) - 1`
2. **Define the Circle** S¹ as a HIT and prove `π₁(S¹) ≅ ℤ` (this is in the cubical library — study it, don't just import it)
3. **Define the Torus** T² and explore its eliminator — count the constructors and understand the pattern-matching structure
4. **Use Agda's reflection API** (`Agda.Builtin.Reflection`) to write a macro that inspects a type definition and counts its constructors

Exercise 4 is the most important. The entire Oracle module depends on reflection. If you can't programmatically inspect type definitions, the project won't work.

### Done criterion
All four exercises compile. You can write a macro that, given a type name, returns the number of constructors as a `ℕ`.

---

## Phase 1: The Fibonacci Recurrence (3–4 weeks)

### Goal
Prove that a generic 2-step coherence obligation produces Fibonacci costs. This is the cleanest result in the whole project and the one most likely to impress a type theorist.

### 1.1 Define Obligation Graphs

The key abstraction. An obligation graph for step n+1 is built from the "exported schemas" of the previous d layers.

```agda
-- Core/Graph.agda

-- A Schema is a finite set of obligation nodes
Schema : Type
Schema = Fin k  -- for some k : ℕ

-- An ObligationGraph at step n, with window d, is the
-- disjoint union of schemas from the previous d layers
ObligationGraph : (d : ℕ) → (history : Vec Schema n) → Type
ObligationGraph d history = ⊎ (last-d-schemas d history)
```

The critical design decision: **What is a Schema, concretely?** 

Option A (Abstract): A Schema is just a natural number (its cardinality). This makes the Fibonacci proof trivial but says nothing about actual type theory.

Option B (Concrete): A Schema is extracted from an actual Agda type definition via reflection. This is harder but connects to real mathematics.

**Recommendation:** Start with Option A for Phase 1. Get the abstract proof working. Phase 2 will build the bridge to Option B.

### 1.2 The Saturation Assumption (make it explicit)

This is the assumption that does the heavy lifting. State it as an axiom:

```agda
-- ObligationGraph/Interface.agda

-- SATURATION ASSUMPTION:
-- The integration cost of the next structure equals
-- the full cardinality of the available interface.
--
-- This is an AXIOM of the model, not a theorem.
-- The paper should be honest about this.

saturation : (n : ℕ) → Δ (n + 1) ≡ | Interface d n |
```

Making this an explicit, named axiom is philosophically important. It tells the reader exactly where the model's assumptions live.

### 1.3 Prove the Recurrence

```agda
-- ObligationGraph/Recurrence.agda

-- Main theorem: for d=2, Δ follows Fibonacci
fibonacci-recurrence : (n : ℕ) → n ≥ 2 →
  Δ (n + 1) ≡ Δ n + Δ (n - 1)

-- Corollary: with Δ(1) = Δ(2) = 1, we get Δ(n) = fib(n)
fibonacci-identification : (n : ℕ) →
  Δ n ≡ fib n

-- The Golden Schedule
golden-schedule : (n : ℕ) →
  τ n ≡ fib (n + 2) ∸ 1
```

The proofs should be straightforward given the definitions. The real work is getting the definitions right.

### 1.4 Generalize to arbitrary d

For bonus points, prove the general recurrence for arbitrary window depth:

```agda
-- For d=1: Δ(n+1) = Δ(n)          (constant — "stagnation")
-- For d=2: Δ(n+1) = Δ(n) + Δ(n-1) (Fibonacci)
-- For d=3: Tribonacci
-- For d=k: k-nacci

general-recurrence : (d : ℕ) → (n : ℕ) → n ≥ d →
  Δ d (n + 1) ≡ Σ[ j ← 0..d-1 ] Δ d (n - j)
```

This is cheap to prove once you have the d=2 case and it strengthens the paper significantly, because it shows d=2 is special (Fibonacci), not assumed.

### Done criterion
The module compiles. You have a machine-checked proof that d=2 + saturation ⟹ Fibonacci costs. The saturation assumption is an explicit, named axiom.

---

## Phase 2: The κ-Oracle (3–4 weeks)

### Goal
Build a computable function that takes an Agda type definition and returns a natural number measuring its "effort."

### 2.1 Define κ precisely

Before writing code, you need to decide exactly what κ counts. Here's a concrete proposal:

```
κ(X) = (number of point constructors)
      + (number of path constructors)
      + (number of higher path constructors)
      + (number of computation rules / β-reductions)
```

For standard HoTT types, this gives:

| Type | Points | Paths | Higher | Comp | κ |
|------|--------|-------|--------|------|---|
| Unit (𝟏) | 1 (★) | 0 | 0 | 0 | 1 |
| Bool (𝟐) | 2 (tt,ff) | 0 | 0 | 0 | 2 |
| S¹ | 1 (base) | 1 (loop) | 0 | 0 | 2 |
| S² | 1 (base) | 0 | 1 (surf) | 0 | 2 |
| Torus | 1 | 2 (p,q) | 1 (surf) | 0 | 4 |
| Σ-type | 1 (pair) | 0 | 0 | 2 (fst,snd) | 3 |
| Π-type | 1 (lam) | 0 | 0 | 1 (app) | 2 |

**Critical check:** Do these numbers, when plugged into ρ = ν/κ, produce a viable Genesis Sequence? You need to verify this on paper before coding.

### 2.2 Implement via Reflection

```agda
-- Oracle/Kappa.agda
open import Agda.Builtin.Reflection

-- Count constructors of a data type
count-constructors : Name → TC ℕ
count-constructors name = do
  data-type pars cs ← getDefinition name
    where _ → typeError (strErr "Not a data type" ∷ [])
  return (length cs)

-- For HITs, also count path constructors
-- This requires inspecting the types of constructors
-- to see which ones target a path type
classify-constructor : Type → TC ConstructorKind
classify-constructor ty = {- inspect return type;
  if it's an identity type / path type, it's a path constructor -}

-- The full κ measure
measure-kappa : Name → TC ℕ
measure-kappa name = do
  cs ← get-constructors name
  kinds ← mapM classify-constructor cs
  return (length kinds)  -- or weighted sum if you want
```

### 2.3 Validate against known types

Build a test suite:

```agda
-- Test/KnownTypes.agda

-- These should all pass:
_ : measure-kappa (quote ⊤)  ≡ 1  -- Unit
_ : measure-kappa (quote S¹) ≡ 2  -- Circle: base + loop
_ : measure-kappa (quote S²) ≡ 2  -- 2-sphere: base + surf
_ : measure-kappa (quote T²) ≡ 4  -- Torus: base + p + q + surf
```

If these numbers don't match your paper's κ values, either the measure or the paper needs to change. **Let the computation lead.**

### Done criterion
A function `measure-kappa : Name → TC ℕ` that compiles and returns defensible values for at least 10 known HoTT types.

---

## Phase 3: The ν-Measure (4–8 weeks)

### Goal
Define a computable novelty measure. **This is the hardest and most important phase.** If you get this right, the project succeeds. If you don't, you need to be honest about it.

### 3.1 The Problem

The paper claims ν counts "enabling power." The Inductive Exponentiality Theorem says ν ≤ 2^Δ (maps to Bool). But the actual ν values in the Genesis table are much smaller than 2^Δ. So you need an intermediate notion.

### 3.2 Three candidate definitions (explore all three)

**Definition A: Eliminator Branch Count**

The simplest option. Given a type X with k constructors, and a target type Y with m constructors, count the number of distinct functions X → Y definable by pattern matching:

```
ν_A(X) = Σ_{Y ∈ Library} |Y|^(constructors of X)
```

Problem: This depends on the whole library, making it context-dependent. Also m^k grows very fast.

**Definition B: New Type Formers Enabled**

Count how many new well-typed definitions become expressible after adding X to the library, that weren't expressible before. Concretely:

```
ν_B(X) = | { f : can define f using X and the library }
         \ { g : can define g using only the library } |
```

Problem: Not directly computable without enumerating all terms (impossible).

Approximation: Count the number of new *types* that become well-formed (not new terms). Specifically, count the new function types, product types, and identity types that can be formed using X and existing library types, up to some bounded complexity.

```agda
-- Oracle/Nu.agda

-- Count new type formers enabled by adding X to context
measure-nu : Name → Library → TC ℕ
measure-nu x lib = do
  -- Types formable before X
  before ← enumerate-types lib (depth 3)
  -- Types formable after X
  after ← enumerate-types (x ∷ lib) (depth 3)
  return (length after ∸ length before)
```

The depth bound is a parameter you'll need to tune. Start with depth 2 or 3.

**Definition C: Connectivity of the Dependency Graph**

The most structurally motivated option. Define ν as the number of new edges in the library's dependency graph that X creates:

```
ν_C(X) = | { (A, B) : A, B ∈ Library, and X enables a path A → B
              that didn't exist before } |
```

A "path" here means: you can define a function A → B using X as an intermediate type. This measures how much X *connects* the existing library.

Problem: Also requires enumeration, but the search space is more bounded (you're looking at pairs of existing types, not all possible terms).

### 3.3 The experimental protocol

This is crucial. **Do not pick a definition and then check if it gives the "right" Genesis Sequence.** That's circular.

Instead:

1. Implement all three definitions (or at least B and C).
2. Compute ν for the first 8 HoTT types in the standard order: Unit, Bool, S¹, Σ, Π, S², S³, Torus.
3. Compute ρ = ν/κ for each.
4. **Report what you find, regardless of whether it matches the paper.**

If the computed ν values produce a sequence where ρ clears the Fibonacci bar — great, the model works. If they don't — that's also a result. It means the model needs revision, and you report *that*.

### 3.4 The honesty checkpoint

After implementing ν, you'll be at a fork:

**Fork A: The model works.** The computed ν/κ values produce a viable sequence. Structures appear in an order that's recognizably mathematical. You have a genuine computational result. → Proceed to Phase 4, write the definitive paper.

**Fork B: The model partially works.** Some structures are selected correctly, others aren't. The Fibonacci timing holds but the specific sequence differs from the paper. → This is still interesting. Report it honestly. Analyze *why* the model diverges from the hand-tuned sequence. Revise the paper accordingly.

**Fork C: The model doesn't work.** The ν-measure produces values that don't sustain the selection dynamics. The bar kills everything after step 4. → This is the most informative outcome. It tells you the Saturation Assumption is too strong, or the ν/κ framework is wrong, or the selection dynamics need revision. Report it. It's still a contribution — "we tried to mechanize PEN and here's where it breaks" is a publishable result.

**Commit now to publishing whatever you find.** The value is in the computation, not in the confirmation.

### Done criterion
A function `measure-nu : Name → Library → TC ℕ` that compiles and returns values for at least 8 known HoTT types. A document recording the computed values and whether they produce a viable selection sequence.

---

## Phase 4: The Selection Loop (4–6 weeks, after Phase 3 succeeds)

### Goal
Wire everything together into a generative system that produces the Genesis Sequence (or something like it) from scratch.

### 4.1 The Candidate Generator

This replaces the "Mutator" from the paper. It proposes new types to add to the library.

```agda
-- Genesis/Candidates.agda

data CandidateRecipe : Type where
  -- Promote: take an existing type and add a path constructor
  AddPath : Name → Name → CandidateRecipe
  -- Bundle: take two types and form Σ or Π
  Bundle  : Name → Name → CandidateRecipe
  -- HIT: specify points and paths directly
  MakeHIT : (points : ℕ) → (paths : List PathSpec) → CandidateRecipe
```

**Key design question:** How wide is the search space? If you allow arbitrary HITs, it's infinite. You need a bounded enumeration strategy.

Recommendation: At each step, generate candidates from a small grammar:
- For each type X in the library, propose "X with one new path" (this discovers circles, spheres, etc.)
- For each pair (X, Y) in the library, propose Σ X (λ _ → Y) and X → Y
- For each pair of points in the same type, propose adding a path between them
- For each pair of parallel paths, propose adding a 2-path between them

This is a finite (though growing) candidate set at each step.

### 4.2 The Selection Loop

```agda
-- Genesis/Selection.agda

-- One evolution step
step : Library → TC (Maybe (Name × EfficiencyData))
step lib = do
  -- Generate candidates
  candidates ← generate-candidates lib
  -- Evaluate each
  scores ← mapM (λ c → evaluate c lib) candidates
  -- Compute bar
  let bar = structural-inflation lib * cumulative-baseline lib
  -- Select winner (highest ρ above bar)
  let winner = select-above-bar scores bar
  return winner

-- The full loop (n steps)
genesis : ℕ → TC Library
genesis zero    = return bootstrap-library
genesis (suc n) = do
  lib ← genesis n
  result ← step lib
  case result of λ where
    nothing       → return lib  -- halted
    (just (x , _)) → return (x ∷ lib)
```

### 4.3 The Output

The final artifact is a printed trace:

```
Step 1:  τ=1    Δ=1   Selected: Universe       ν=?  κ=?  ρ=?   Bar=---
Step 2:  τ=2    Δ=1   Selected: Unit           ν=?  κ=?  ρ=?   Bar=?
Step 3:  τ=4    Δ=2   Selected: ???            ν=?  κ=?  ρ=?   Bar=?
...
```

**The question marks are the whole point.** You don't know what will appear. Maybe the Circle shows up at step 5. Maybe it shows up at step 4. Maybe Bool appears instead. Whatever happens, *report it*.

### 4.4 Analysis

After running the loop, compare the output to:
1. The hand-tuned Genesis Sequence from the paper
2. The actual historical order of mathematical development
3. The curriculum order in standard HoTT textbooks

Any of these comparisons could be illuminating. Disagreements are as interesting as agreements.

### Done criterion
A program that, starting from an empty library, generates a sequence of at least 8 type-theoretic structures, each clearing a computable selection bar. The output is recorded and analyzed.

---

## Known Risks and Mitigations

### Risk 1: Agda's reflection API can't inspect HITs

The reflection API (`TC` monad) can inspect standard data types, but Higher Inductive Types in Cubical Agda have special status. You may not be able to programmatically inspect their path constructors.

**Mitigation:** Define a parallel DSL that *describes* HITs as data, and use that for measurement. Build the actual HITs separately and link them manually. This is less elegant but workable.

### Risk 2: The ν-measure is too expensive to compute

If ν requires enumerating terms up to some depth, the computation may be infeasible for complex types.

**Mitigation:** Start with the simplest definition (eliminator branch count) and see if it works. Only escalate to fancier definitions if the simple one fails.

### Risk 3: The candidate space is too large

Even with a bounded grammar, the number of candidates grows combinatorially with the library size.

**Mitigation:** Implement the generator lazily. At each step, generate at most 50-100 candidates. If the "right" structure isn't found, that's information about the model.

### Risk 4: Nothing interesting comes out

The model may produce a sequence that's mathematically valid but unrecognizable — a bunch of arbitrary types that clear the bar but don't correspond to known mathematics.

**Mitigation:** This is the most important possible outcome. It means either (a) the ν-measure is wrong, (b) the selection dynamics are wrong, or (c) the whole framework is wrong. Any of these is worth publishing. Do not bury negative results.

### Risk 5: You run out of motivation

This is real. The philosophical papers are more fun to write than Agda proofs.

**Mitigation:** Set a concrete schedule. Phase 1 should be done in January. Phase 2 by mid-February. Phase 3 by April. Block out dedicated time. Tell someone (a collaborator, a friend, Claude) your deadlines. The fact that you're an "incessantly curious" person with "pathological fearlessness" is an asset here — but curiosity needs to be channeled into persistence.

---

## What Success Looks Like

**Minimum viable result (Phase 1 only):** A machine-checked proof that d=2 + saturation gives Fibonacci costs. This is already a small but real contribution. It's a formalization of a combinatorial result in a proof assistant. Publishable at a workshop (e.g., TYPES, HoTT/UF).

**Good result (Phases 1–3):** The above, plus a computable ν/κ measure for standard HoTT types, plus an honest report of whether these values sustain the selection dynamics. This is a solid paper regardless of the outcome.

**Great result (Phases 1–4):** A generative system that, starting from nothing, produces a recognizable sequence of mathematical structures on a Fibonacci schedule. This would be genuinely remarkable and would earn the right to write the physics and philosophy papers with real authority behind them.

**The result that changes everything:** The system produces structures you *didn't expect* — types that the model selects for efficiency reasons that turn out to correspond to known mathematics you hadn't considered. This would be evidence that the model is discovering something, not just confirming what you put in.

---

## Appendix: Resources

### Cubical Agda
- [Cubical Agda documentation](https://agda.readthedocs.io/en/latest/language/cubical.html)
- [1lab](https://1lab.dev/) — excellent reference implementation of HoTT in Cubical Agda
- Vezzosi, Mörtberg, Abel: "Cubical Agda" (ICFP 2019)

### Reflection API
- [Agda reflection docs](https://agda.readthedocs.io/en/latest/language/reflection.html)
- Allais et al., "A Type and Scope Safe Universe of Syntaxes with Binding" (ICFP 2018)
- Van der Walt and Swierstra, "Engineering Proof by Reflection in Agda" (IFL 2012)

### HoTT standard library
- [agda/cubical](https://github.com/agda/cubical) on GitHub
- The Circle, Torus, Spheres, Pushouts, and Truncations are all defined here

### Key mathematical references for implementation
- HoTT Book, Chapter 6 (Higher Inductive Types) — for understanding the types you'll be measuring
- HoTT Book, Chapter 8 (Homotopy Theory of Type Theory) — for understanding π₁(S¹) etc.
- Licata and Shulman, "Calculating the Fundamental Group of the Circle in HoTT" — for implementation details
