# The ν Convergence Test

## Design Document — Phase 2 of the Maximum Impact Plan

---

## 1. What This Test Determines

The Genesis Sequence claims to be the *unique* output of an efficiency-maximizing process. For this to be true, the novelty measure ν must be canonical — there must be essentially one principled way to measure "enabling power," and different approaches to defining it must converge.

Currently, ν is computed three different ways depending on the structure type:
- **HITs and suspensions:** Proof-rank clustering (depth-1 type enumeration, schema abstraction, cluster counting)
- **Maps, modals, axioms:** Component-based structural formulas
- **DCT:** Lattice Tensor Product (14 × 11 − 4 = 150)

This piecewise definition is the single most damaging weakness of the theory. If someone asks "what is ν?" and the answer is "it depends on what kind of structure you're looking at," the theory has a free parameter dressed up as three different formulas.

The convergence test asks: **do different principled definitions of ν, applied uniformly to all structures, produce the same (or proportional) values?**

If yes: there is a canonical ν, and proof-rank is approximating it. The theory is about mathematics.
If no: ν has irreducible degrees of freedom. The Genesis Sequence depends on modeling choices. The theory is interesting but not fundamental.

---

## 2. The Four Candidate Measures

### Measure A: Proof-Rank (the current baseline)

**Definition:** ν_A(X | B) = the number of independent type schemas newly inhabited when X is added to library B, computed by:
1. Enumerate all types expressible at depth ≤ 1 using X and the library
2. Abstract each type to a schema (replace specific names with generic variables)
3. Filter trivially inhabited schemas
4. Count remaining independent clusters

**Strengths:** Domain-independent, computable, already implemented in the Haskell engine.

**Weaknesses:** The depth-1 cutoff is a parameter choice. The schema abstraction involves heuristic decisions (what counts as "trivial"? how to cluster?). The latent capability bonus is hand-tuned.

**Implementation status:** Complete (engine's `ProofRank.hs` and `Cluster.hs`).

### Measure B: Compression Drop (Kolmogorov-style)

**Definition:** ν_B(X | B) = |{Y in candidate grammar : K_B(Y) − K_{B∪{X}}(Y) ≥ 1}|

In words: the number of structures whose description complexity drops by at least 1 unit when X is added to the library.

This is the formal definition already in the paper (Definition 3.5). The question is whether it's computable and whether it matches proof-rank.

**To make it computable, we need:**

1. A **description language** for type-theoretic structures. This is the candidate grammar — the set of type expressions that the engine can form.

2. A **complexity measure** K_B(Y) = the minimum number of grammar tokens needed to express Y given library B.

3. An **enumeration** of all Y with K_B(Y) ≤ H (the effort horizon) and K_{B∪{X}}(Y) ≤ H.

**Concrete implementation plan:**

Define the description language as the type expression grammar already used by the engine, with tokens:
- Type names: each library type is 1 token
- Type formers: Π, Σ, =, each 1 token
- Application: 1 token
- Variables: 1 token each

Then K_B(Y) is the minimum token count to express Y using the types in B.

For each Genesis step n:
- Enumerate all type expressions with token count ≤ H (start with H = 3 or 4)
- For each expression Y, compute K_{B_{n-1}}(Y) and K_{B_n}(Y) where B_n = B_{n-1} ∪ {X_n}
- Count Y where the drop is ≥ 1
- This count is ν_B(X_n | B_{n-1})

**Key design decision:** The choice of token grammar determines the result. The test's value comes from trying 2-3 reasonable grammars and checking whether ν_B is robust across them.

**Candidate grammars:**
- **G1 (minimal):** Only types, Π, Σ, application. No identity types as primitives.
- **G2 (with identity):** Types, Π, Σ, =, application, transport.
- **G3 (with eliminators):** All of G2 plus pattern matching / recursors.

If ν_B is approximately the same under G1, G2, and G3, the measure is grammar-independent (a very strong signal).

### Measure C: Categorical Enabling Power

**Definition:** ν_C(X | B) = the number of new natural transformations, adjunctions, or equivalences that become expressible when X is added.

This is the most mathematically principled definition but the hardest to compute. It requires treating the library as a category and counting new morphisms in a functor category.

**Concrete operationalization:**

For the purposes of computation, we restrict to a countable version:

ν_C(X | B) = |{(f, T, U) : f is a new map T → U expressible using X, where T, U ∈ B ∪ {X}}|

where "new" means f is not definable without X.

This is closely related to proof-rank but counts *maps* rather than *types*. The difference matters: proof-rank counts "how many new types can I form?" while categorical enabling power counts "how many new morphisms exist between existing types?"

**Implementation plan:**

For each Genesis step n and each pair (T, U) of library types:
1. Enumerate maps T → U definable with token count ≤ H using B_{n-1}
2. Enumerate maps T → U definable with token count ≤ H using B_n
3. Count the new maps

Sum over all pairs (T, U), including T = X_n and U = X_n.

This is computationally heavier than Measure B (quadratic in |B| rather than linear) but feasible for the first 10 steps where |B| ≤ 10.

### Measure D: Homotopy-Theoretic Richness

**Definition:** ν_D(X | B) = the number of new homotopy-theoretic invariants that become computable or the rank of newly accessible homotopy/cohomology groups.

This is the most HoTT-specific measure and the most likely to explain the *specific* ν values for geometric structures (S¹, S², S³, Hopf).

**Concrete operationalization:**

For each Genesis structure X_n, count:
1. New homotopy groups π_k(X_n) that are non-trivial and not previously computable
2. New cohomology groups H^k(X_n; G) for coefficient groups G in the library
3. New fibration sequences involving X_n
4. New equivalences (e.g., ΩS² ≃ S¹ × ΩS³) that become provable

**This cannot be fully automated** — it requires mathematical knowledge about each structure. However, it can be systematically tabulated by a human with HoTT expertise.

**Implementation plan:**

For each of the first 10 Genesis structures, manually compile:
- The homotopy groups π_k(X_n) for k = 0, 1, 2, 3
- The new long exact sequences involving X_n
- The new equivalences involving X_n
- The new cohomology operations involving X_n

Count the total number of "new invariants" by a consistent counting scheme. Compare with ν values from the other measures.

---

## 3. The Test Protocol

### 3.1 Scope

Compute all four measures for the first 10 Genesis structures:

| n | Structure | Current ν | ν_A (proof-rank) | ν_B (compression) | ν_C (categorical) | ν_D (homotopy) |
|---|-----------|-----------|------------------|--------------------|--------------------|----------------|
| 1 | Universe U₀ | 1 | | | | |
| 2 | Unit type **1** | 1 | | | | |
| 3 | Witness ⋆ : **1** | 2 | | | | |
| 4 | Π/Σ types | 5 | | | | |
| 5 | Circle S¹ | 7 | | | | |
| 6 | Prop. truncation | 8 | | | | |
| 7 | Sphere S² | 10 | | | | |
| 8 | S³ ≅ SU(2) | 18 | | | | |
| 9 | Hopf fibration | 17 | | | | |
| 10 | Cohesion | 19 | | | | |

Stop at 10 rather than 15 because:
- Steps 11–14 (connections, curvature, metric, Hilbert) use component-based ν formulas that are hard to validate independently
- Step 15 (DCT) is the subject of Phase 3 (separate formalization)
- 10 data points are sufficient to detect convergence or divergence

### 3.2 Implementation Order

**Week 1–2: Measure A (baseline).**
Already implemented. Run the engine on steps 1–10 and extract ν_A values. These are the baseline that the other measures must match (or explain why they differ).

**Week 2–4: Measure B (compression drop).**
This is the most important comparison because it's the measure from the paper's formal definition.

Implementation steps:
1. Define the token grammar (start with G2: types, Π, Σ, =, application, transport)
2. Write an enumerator that generates all type expressions up to token count H
3. For each expression, compute its minimal token count under B_{n-1} and B_n
4. Count the drop
5. Repeat for H = 3, 4, 5 (to check horizon sensitivity)
6. Repeat for grammars G1 and G3 (to check grammar sensitivity)

**Deliverable:** A table of ν_B values under 3 grammars × 3 horizons = 9 configurations, for steps 1–10.

**Week 4–6: Measure C (categorical).**
Implementation steps:
1. For each pair of library types, enumerate maps up to token count H
2. Count new maps enabled by X_n
3. Sum over pairs

**Deliverable:** A table of ν_C values for steps 1–10.

**Week 6–8: Measure D (homotopy-theoretic).**
This is manual mathematical work, not programming.
1. For each Genesis structure, compile the known homotopy-theoretic invariants
2. Count new ones by a consistent scheme
3. Document the counting methodology

**Deliverable:** A table of ν_D values for steps 1–10, with full mathematical justification for each count.

### 3.3 Analysis Protocol

Once all four tables are filled, compute:

**Test 1: Rank-order agreement.**
For each pair of measures (A, B), (A, C), (A, D), (B, C), (B, D), (C, D):
- Compute the Spearman rank correlation of the ν values across steps 1–10
- If ρ_Spearman > 0.9 for all pairs: **strong convergence** (the ordering is measure-independent)
- If ρ_Spearman > 0.7 for most pairs: **moderate convergence** (ordering mostly preserved, some sensitivity)
- If ρ_Spearman < 0.7 for any pair: **divergence** (the measures disagree on ordering, which means the Genesis Sequence is measure-dependent)

**Test 2: Proportionality.**
For each pair of measures, fit a linear model ν_X = a · ν_Y + b.
- If b ≈ 0 and R² > 0.9 for all pairs: **strong proportionality** (the measures differ by a constant factor, meaning there's essentially one ν up to units)
- If R² > 0.8 but b ≠ 0: **affine relationship** (there's a systematic offset, suggesting different baselines but the same growth)
- If R² < 0.8: **non-proportional** (the measures capture genuinely different things)

**Test 3: Genesis Sequence stability.**
For each measure, run the full PEN selection loop using that measure's ν values.
- Does the same sequence of 10 structures emerge in the same order?
- If yes for all 4 measures: **the Genesis Sequence is robust** (it doesn't depend on which ν you use)
- If yes for 3 of 4: **mostly robust** (one measure is an outlier, identify which and why)
- If different sequences emerge: **the Genesis Sequence is measure-dependent** (this is the kill criterion)

**Test 4: Sensitivity within each measure.**
For Measure B: does ν_B change significantly across grammars G1–G3 and horizons H = 3–5?
- If ν_B is stable: the compression-drop measure is grammar-independent (a very strong result)
- If ν_B varies by > 30%: grammar dependence is real, and the theory has a hidden parameter

For Measure A: does ν_A change significantly if we use depth-2 enumeration instead of depth-1?
- If stable: depth cutoff doesn't matter (good)
- If changes significantly: the depth cutoff is a tuning parameter (bad)

---

## 4. What Each Outcome Means

### Outcome 1: Strong convergence (all measures agree on ordering and proportionality)

**Interpretation:** There IS a canonical ν. All four measures are approximating the same underlying quantity from different angles. Proof-rank is a computationally efficient proxy for something mathematically real.

**Action:** Identify the simplest measure that agrees with all others. Promote it to the official definition. Prove that the other measures are equivalent (or proportional) to it. This becomes a theorem in the paper.

**Impact on the theory:** Transformative. The Genesis Sequence becomes measure-independent — it's a fact about mathematics, not about your algorithm.

### Outcome 2: Moderate convergence (ordering preserved, values differ)

**Interpretation:** There is a canonical *ordering* of structures by enabling power, but the *magnitude* of ν depends on the measure. The Genesis Sequence is robust (same order for all measures), but the bar-clearing analysis depends on the specific ν values.

**Action:** Characterize the *class* of ν measures that preserve the Genesis Sequence ordering. The theory's claim becomes: "For any ν measure in this class, PEN produces the Genesis Sequence." The class itself becomes an interesting mathematical object.

**Impact on the theory:** Significant. Weaker than Outcome 1 but still powerful — it shows the sequence is an attractor under a broad class of measures.

### Outcome 3: Partial divergence (some measures agree, others don't)

**Interpretation:** There are at least two genuinely different notions of "enabling power," and they produce different sequences. The theory has real degrees of freedom.

**Action:** Identify which measures agree (probably A and B, since they both count types/expressions) and which diverge (probably D, which counts homotopy-theoretic invariants). Understand why: is the divergence at the geometric steps (5–9) or the bootstrap steps (1–4)?

**Impact on the theory:** The Genesis Sequence is not unique. The theory is interesting as a model of mathematical evolution but not fundamental as a claim about the nature of mathematics. The coherence window theorem (d = 2) and the scaling theorem (Fibonacci) are still valid — those don't depend on ν. But the specific sequence depends on a choice.

### Outcome 4: Full divergence (measures disagree on ordering)

**Interpretation:** "Enabling power" is not a well-defined concept — different reasonable formalizations give different answers. The Genesis Sequence is an artifact of proof-rank clustering specifically.

**Action:** Report the result honestly. The coherence window classification and the scaling theorem are independent contributions that stand on their own. The Genesis Sequence becomes an interesting computational observation rather than a theorem.

**Impact on the theory:** The scaling theorem and the d=2 classification survive. The Genesis Sequence and the DCT-as-endpoint claim do not. The research pivots to: "Why does this particular ν (proof-rank) produce a sequence matching known mathematics? Is that a coincidence or does it point to a deeper structure?"

---

## 5. The First 4 Steps: A Worked Preview

To illustrate the protocol, let's trace what each measure should give for the first four Genesis structures. This is the part you can verify by hand before implementing anything.

### Step 1: Universe U₀

**What U₀ enables:** The ability to form types. Before U₀, there is nothing.

- ν_A (proof-rank): 1. The schema `U₀` itself is the one new schema.
- ν_B (compression): 1. The expression `U₀` has K = 1. Before, K = ∞ (no types at all). One structure drops from ∞ to finite.
- ν_C (categorical): 1. The identity morphism id : U₀ → U₀ is the one new morphism. (There's nothing else to map to or from.)
- ν_D (homotopy): 1. π₀(U₀) becomes a meaningful question. (But the answer depends on univalence, which isn't available yet.)

**Expected:** All four agree: ν = 1. ✓

### Step 2: Unit type **1**

**What **1** enables:** A type with a canonical inhabitant. The simplest non-trivial type.

- ν_A: 1. The schema `**1**` is one new type. The schema `X : **1**` is trivial (equivalent to `X : U₀`). Maybe 1.
- ν_B: 1. The expression `**1**` drops from K = ∞ to K = 1. No other expression's complexity changes.
- ν_C: 1. The unique map `U₀ → **1**` (the terminal map). (Or: the map `**1** → U₀` sending ⋆ ↦ **1**. This might be 2.)
- ν_D: 1. π₀(**1**) = 1 (contractible). One new invariant.

**Expected:** Likely all agree: ν = 1. Possible disagreement at ν_C (1 vs 2 depending on whether you count maps in both directions).

### Step 3: Witness ⋆ : **1**

**What ⋆ enables:** A canonical term. Before, we had types but no terms. Now we can apply functions to something.

- ν_A: 2. From the engine: 2 new schemas. Plausibly: the term `⋆ : **1**` itself, plus the ability to *apply* dependent functions at a specific point.
- ν_B: Interesting. The expression `⋆` has K = 1. But also: any expression `f(⋆)` where `f : **1** → T` now has K = K(f) + 1, whereas before, without a term to apply to, K(f(⋆)) = ∞. So ν_B could be larger — one drop for `⋆` itself, plus one for each `f(⋆)` that becomes expressible. With only U₀ and **1** in the library, there's one such f (the identity **1** → **1**), so ν_B ≈ 2.
- ν_C: 2. The maps `⋆ : **1** → **1**` (as a global section) and `! : **1** → U₀` (sending ⋆ ↦ **1**). Or similar count.
- ν_D: 0 or 1. Having a term doesn't create new homotopy invariants per se. But it creates a "basepoint," which is needed for π₁ later. Depends on counting methodology.

**Expected:** Likely ν ∈ {1, 2, 3} depending on measure. This is the first step where measures might diverge. Watch carefully.

### Step 4: Π/Σ types

**What Π/Σ enables:** Dependent function types and dependent pair types. This is the infrastructure step that unlocks everything.

- ν_A: 5. From the engine. Plausibly: Π itself, Σ itself, λ-abstraction, application, and the pairing/projection operations.
- ν_B: Should be large. With Π and Σ, many new type expressions become expressible: `Π(x:**1**).U₀`, `Σ(x:**1**).**1**`, `**1** → **1**`, `**1** → U₀`, etc. Each is a type whose K drops from ∞ to finite. Counting carefully: this could easily be 5–10 depending on the horizon H.
- ν_C: Should be large. Π and Σ create an explosion of new morphisms: function types are exponential objects in the category, and Σ types are coproducts. The number of new maps between library types jumps significantly. Likely ≥ 5.
- ν_D: Arguably 0–2. Π and Σ don't create new homotopy-theoretic invariants directly — they're infrastructure, not geometric objects. They enable the *computation* of invariants that will come later. This is a philosophical question: does ν_D count "invariants that become computable" or "invariants that newly exist"?

**Expected:** This is where the test gets interesting. ν_A = 5, ν_B probably 5–10 (horizon-dependent), ν_C probably ≥ 5, ν_D probably 0–2. The homotopy measure might diverge here, which would be informative — it would mean the bootstrap phase is driven by combinatorial enabling power (measures A/B/C) rather than homotopy-theoretic richness (measure D). This is actually *expected*: the bootstrap phase creates infrastructure, not geometry.

---

## 6. The Critical Steps: Where Divergence Would Be Most Informative

### Step 5 (S¹) — The First Geometric Structure

This is where ν_D should start contributing. S¹ has π₁ = ℤ, it creates loop spaces, covering spaces, winding numbers. If ν_D suddenly jumps while ν_B stays modest, it suggests that the "right" ν for geometric structures is homotopy-theoretic, not combinatorial.

Current ν = 7. Prediction:
- ν_A = 7 (known)
- ν_B ≈ 5–10 (from compression drop — S¹ enables `ΩS¹`, loop spaces, etc.)
- ν_C ≈ 5–15 (from new maps — S¹ has non-trivial endomorphisms via winding number)
- ν_D ≈ 5–10 (π₁(S¹) = ℤ, universal cover, loop space, long exact sequence, Mayer-Vietoris...)

If all four are in the range 5–15, we have convergence. If one is dramatically different (e.g., ν_D = 20 while ν_B = 3), we have divergence.

### Step 8 (S³) — The Measure's Hardest Test

S³ has the richest homotopy theory of any Genesis structure: π₃(S³) = ℤ, but also π₃(S²) = ℤ (via the Hopf fibration, which comes next). The engine gives ν = 18 but with known ±15% uncertainty.

This is where the measures are most likely to diverge, and where divergence would be most informative:
- If ν_D >> ν_B for S³, it suggests that homotopy-theoretic richness is the "real" ν and proof-rank underestimates it.
- If ν_D << ν_B for S³, it suggests that combinatorial enabling power is the "real" ν and homotopy groups overcount.

### Step 9 (Hopf fibration) — Cross-Layer Interaction

The Hopf fibration is a *map*, not a type. Its ν is currently computed by a structural formula, not proof-rank. This is one of the three steps where the engine uses a piecewise ν definition.

The convergence test's value is especially high here: can we compute ν_B and ν_C for the Hopf fibration directly, without a special formula? If the compression-drop and categorical measures give ν ≈ 17 (the current value), the structural formula is validated. If they give a very different number, the structural formula is wrong and needs revision.

---

## 7. Implementation Architecture

### 7.1 The ν Calculator Module

Create a new Haskell module `NuConvergence.hs` that implements all four measures with a common interface:

```haskell
-- The common interface
data NuMeasure = ProofRank | CompressionDrop Grammar | Categorical | HomotopyManual

data NuResult = NuResult
  { nrStep      :: Int           -- Genesis step n
  , nrStructure :: String        -- Structure name
  , nrMeasure   :: NuMeasure     -- Which measure
  , nrValue     :: Int           -- The computed ν
  , nrDetails   :: [String]      -- What was counted (for auditing)
  }

computeNu :: NuMeasure -> Library -> LibraryEntry -> NuResult

-- Run all measures on a given step
convergenceTest :: Library -> LibraryEntry -> [NuResult]
convergenceTest lib entry =
  [ computeNu ProofRank lib entry
  , computeNu (CompressionDrop G1) lib entry
  , computeNu (CompressionDrop G2) lib entry
  , computeNu (CompressionDrop G3) lib entry
  , computeNu Categorical lib entry
  ]
  -- HomotopyManual values are entered from the external table
```

### 7.2 The Compression Drop Engine

The core computation:

```haskell
data Grammar = G1 | G2 | G3

-- Generate all type expressions up to token count H
enumerate :: Grammar -> Library -> Int -> [TypeExpr]

-- Compute minimum token count for expression under given library
minTokens :: Grammar -> Library -> TypeExpr -> Int  -- ∞ if not expressible

-- Compute ν_B
compressionDrop :: Grammar -> Int -> Library -> LibraryEntry -> Int
compressionDrop grammar horizon libBefore newEntry =
  let libAfter = addEntry libBefore newEntry
      exprs = enumerate grammar libAfter horizon
      drops = [ e | e <- exprs
              , let before = minTokens grammar libBefore e
              , let after  = minTokens grammar libAfter e
              , before - after >= 1
              ]
  in length drops
```

### 7.3 The Categorical Counter

```haskell
-- Count new maps between library types
categoricalNu :: Int -> Library -> LibraryEntry -> Int
categoricalNu horizon libBefore newEntry =
  let libAfter = addEntry libBefore newEntry
      allTypes = libraryTypes libAfter
      pairs = [(t, u) | t <- allTypes, u <- allTypes]
      countNew (t, u) =
        let mapsBefore = enumerateMaps horizon libBefore t u
            mapsAfter  = enumerateMaps horizon libAfter t u
        in length mapsAfter - length mapsBefore
  in sum [countNew p | p <- pairs]
```

### 7.4 Output Format

The test produces a CSV file and a summary report:

```
step,structure,measure,grammar,horizon,nu,details
1,Universe,ProofRank,-,-,1,"[U0]"
1,Universe,CompressionDrop,G1,3,1,"[U0]"
1,Universe,CompressionDrop,G2,3,1,"[U0]"
...
5,Circle,ProofRank,-,-,7,"[base,loop,Omega,...]"
5,Circle,CompressionDrop,G1,3,5,"[S1,base,loop,...]"
5,Circle,CompressionDrop,G2,3,8,"[S1,base,loop,path_comp,...]"
5,Circle,Categorical,-,3,12,"[id,const,wind1,wind2,...]"
5,Circle,HomotopyManual,-,-,8,"[pi1,cover,Omega,LES,...]"
```

---

## 8. Decision Criteria (From the Maximum Impact Plan)

| Finding | Conclusion | Action |
|---------|------------|--------|
| Spearman ρ > 0.9 for all pairs, R² > 0.9 | Canonical ν exists | Promote simplest measure; prove equivalence |
| Spearman ρ > 0.9 but R² < 0.9 | Canonical ordering but no canonical magnitude | Characterize the class of ν measures preserving the sequence |
| Spearman ρ > 0.7 for most pairs | Mostly robust with some sensitivity | Identify sensitive steps; analyze why |
| Spearman ρ < 0.7 for some pair | Genuine divergence | Report honestly; the sequence has degrees of freedom |
| Genesis Sequence changes under some measure | The sequence is measure-dependent | Kill criterion for the "uniqueness" claim |
| ν_B is grammar-independent (stable across G1-G3) | Compression drop is canonical | Very strong evidence for a canonical ν |
| ν_B varies > 30% across grammars | Grammar dependence is real | The theory has a hidden parameter in the grammar choice |

---

## 9. The Deeper Question This Test Answers

The convergence test is not just about ν values. It answers a philosophical question about the nature of mathematical structure:

**Is "enabling power" a natural kind?**

If four different formalizations of "how much does this structure enable?" converge to the same answer, then enabling power is a real property of mathematical structures — as real as cardinality or homotopy type. It exists independently of how you measure it.

If they diverge, then "enabling power" is like "beauty" or "importance" — a projection of a particular perspective onto the mathematical landscape. The theory would still be interesting (it shows that *one particular* notion of enabling power produces a remarkable sequence), but it would lose the claim to inevitability.

The test is designed to be decisive. By running four measures on 10 structures with multiple parameter settings, we generate enough data to distinguish convergence from coincidence. The protocol is falsifiable (clear thresholds for each outcome) and the outcomes are pre-registered (we decide what counts as convergence *before* seeing the results).

---

## 10. Timeline

| Week | Activity | Deliverable |
|------|----------|-------------|
| 1–2 | Extract ν_A baseline from engine; design grammar G1/G2/G3 | Baseline table; grammar specification |
| 2–3 | Implement expression enumerator and token counter | Working enumerator for G2 up to H = 4 |
| 3–4 | Implement compression drop (Measure B) | ν_B table for 3 grammars × 3 horizons × 10 steps |
| 4–5 | Implement categorical counter (Measure C) | ν_C table for 10 steps |
| 5–6 | Compile homotopy-theoretic invariants (Measure D) | ν_D table with mathematical justification |
| 6–7 | Run analysis: correlations, proportionality, sequence stability | Convergence report |
| 7–8 | Write up results; determine canonical ν (or characterize its non-uniqueness) | Section for the paper, or a re-evaluation of the theory |

---

## 11. What Happens After

**If convergence (Outcome 1 or 2):**
Proceed to Phase 3 (DCT formalization). The canonical ν gives you the tool to rigorously compute ν(DCT) and verify or refute the ν = 150 claim. The lattice tensor product formula (14 × 11 − 4) becomes something you can derive rather than assert.

**If divergence (Outcome 3 or 4):**
Pivot. The coherence window theorem and the scaling theorem are unaffected — they don't depend on ν. The paper becomes: "Foundations are classified by coherence window; d = 2 gives Fibonacci scaling; here is a computational model that produces an interesting sequence under a specific ν." Weaker but still publishable and still true.

Either way, you'll know.
