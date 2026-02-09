# PEN Theory: Maximum Impact Research Plan

## The Actual Claim

Let's state it plainly, because the current papers dance around it:

> **The structures of physics — gauge fields, Riemannian geometry, Hamiltonian mechanics, the Standard Model — are not empirical accidents. They are the unique output of an efficiency-maximizing process operating on intensional type theory with coherence window 2. The Dynamical Cohesive Topos is the mathematical structure of our universe, and the Genesis Sequence is the reason it exists rather than something else.**

This is either one of the most important insights in the history of science, or it's wrong. The research plan should be designed to determine which, with maximum rigor, and if it survives, to make the case in a form that the best minds in mathematics and physics cannot dismiss.

The previous plan was optimized for "get something published." This plan is optimized for "find out if it's true, and if so, prove it to the world."

---

## What "Aiming High" Actually Requires

An incremental academic contribution needs to survive peer review. A foundational claim about the nature of physical reality needs to survive *adversarial scrutiny by the best people alive.* That's a much higher bar, but it also clarifies priorities:

1. **You don't need to publish two safe papers.** You need one devastating argument.
2. **You don't need to minimize attack surface.** You need to *eliminate every possible failure mode*, because if the claim is true, it should be possible to make it airtight.
3. **The exponentiality theorem shouldn't be "retired."** It should be *fixed* — because if the theory is correct, there *must* be a true version of it, and finding it would strengthen the whole edifice.
4. **DCT shouldn't be "demoted to speculative."** It should be *formalized completely* — because if you can't formalize it, the claim that it's the structure of the universe is empty.
5. **The ν measure shouldn't be "honestly framed as a proxy."** It should be *derived from first principles* — because if the Genesis Sequence is truly deterministic, there must be a canonical ν, and finding it would be the strongest possible evidence.

In short: every weakness the reviewers identified is not a thing to work around. It's a thing to *solve*. If you can solve them all, you have something historic. If you can't solve one of them, that's where the theory breaks, and that's worth knowing.

---

## The Three Load-Bearing Walls

The claim stands on exactly three pillars. If any one fails, the whole thing falls. The plan is organized around stress-testing each to destruction.

### Pillar 1: The Coherence Window Classification

**The claim:** Intensional type theory has d = 2. Extensional type theory has d = 1. This is a fact about those systems, not a modeling choice.

**Why this matters:** If d = 2 is just an assumption you feed into a model, the Genesis Sequence is an artifact of your assumptions. If d = 2 is a *theorem* about HoTT, the Genesis Sequence is a consequence of the mathematics.

**Current status:** The papers invoke the Mac Lane Coherence Theorem informally and assert that coherence at dimension 2 implies coherence at all higher dimensions. This is gesturing at a real result, but the actual connection between "Mac Lane coherence" and "the PEN model's coherence window parameter" has not been established rigorously.

### Pillar 2: The Canonical Novelty Measure

**The claim:** There exists a unique, principled measure of "enabling power" such that the Genesis Sequence is the deterministic output of PEN under that measure.

**Why this matters:** If ν is a design choice, the Genesis Sequence is "what you get when you choose ν this way." If ν is canonical, the Genesis Sequence is "what mathematics must produce."

**Current status:** ν is computed three different ways for different candidate types, with hand-tuned parameters. This is the single most damaging weakness.

### Pillar 3: The DCT as Physical Reality

**The claim:** The DCT is not just the last structure in a formal sequence. It is the mathematical framework that describes the actual universe.

**Why this matters:** This is what elevates the work from "interesting model of mathematical evolution" to "explanation of why physics is the way it is."

**Current status:** The DCT paper shows that classical mechanics, gauge theory, and geometric flows can be expressed within DCT. But "can be expressed within" is not "must emerge from." Many frameworks can express these things. The question is whether DCT is *uniquely selected* and whether its internal structure matches observed physics in specific, falsifiable ways.

---

## Phase 1: Settle the Coherence Window (Months 1–4)

This is the foundation of everything. If you can prove d = 2 for HoTT as a theorem, you have transformed the entire project from "model" to "consequence."

### 1.1 Formalize the Connection to Mac Lane Coherence

**The goal:** A precise theorem statement of the form:

> In any type theory satisfying [specific axioms], the coherence obligations induced by sealing a new structure against the existing library stabilize at depth 2.

This requires:
- A formal definition of "coherence obligation" in the type-theoretic setting (not just the PEN model's abstract definition)
- A formal definition of "stabilization"
- A proof that HoTT's path structure generates exactly two layers of irreducible obligations
- A proof that ZFC/MLTT+UIP generates exactly one layer

**Who to talk to:** This is the kind of question that people like Thierry Coquand, Anders Mörtberg, or Emily Riehl could evaluate. You don't need to collaborate with them — you need to formulate the question precisely enough that they would recognize it as meaningful and either confirm or refute it.

**How to approach it:** The key insight in your papers is that in intensional type theory, defining composition of paths at layer n requires witnessing coherence with points and paths from layer n−1, and the Interchange Law generates irreducible obligations spanning two layers. This needs to be made into an actual proof, not a plausibility argument.

**Possible approach via Cubical Agda:**
- Take a concrete example: define a new HIT (e.g., the torus) in Cubical Agda
- Enumerate every coherence obligation that Agda requires
- Show that these obligations reference the two most recent layers and no deeper
- Repeat for 3–4 examples of increasing complexity
- Abstract the pattern into a general argument

**What would disprove the theory:** If you find a natural construction in HoTT that generates irreducible three-layer obligations (d = 3), the Fibonacci prediction breaks. The scaling would follow the tribonacci sequence instead. If this happens, the theory isn't dead — it just means you need to determine the actual d for HoTT and check whether the Genesis Sequence still works. But it would be a serious blow to the current formulation.

### 1.2 Implement d as a Parameter and Test Empirically

While the proof is in progress, build the computational evidence:

- [ ] Make coherence window d a configurable parameter in the Haskell engine
- [ ] Run the Genesis simulation with d = 1, 2, 3
- [ ] For d = 1: confirm stagnation (the sequence should get stuck after a few structures)
- [ ] For d = 2: confirm the known Genesis trace
- [ ] For d = 3: examine what happens — does a different but coherent sequence emerge? Does it match any known mathematical development trajectory?

This is valuable regardless of the proof outcome: it demonstrates that the theory makes *different* predictions for different d values, which is what makes it falsifiable.

---

## Phase 2: Derive the Canonical Novelty Measure (Months 2–6)

This is the hardest problem in the project and the one most likely to determine whether the theory is true or merely suggestive.

### 2.1 The Core Question

If the Genesis Sequence is truly deterministic and canonical, there must be a *principled* definition of ν such that:

1. It is defined uniformly for all candidate types (not piecewise by category)
2. It is computable (at least in principle)
3. It produces the Genesis Sequence as output
4. It has a *reason* — i.e., it measures something meaningful about mathematical structures, not just "whatever makes the numbers work"

The current proof-rank clustering is a reasonable approximation, but it's not canonical. The question is: what is it approximating?

### 2.2 Three Candidate Foundations for ν

**Approach A: Kolmogorov-style compression drop**

ν(X | B) = the number of structures Y such that the description complexity of Y drops by at least 1 bit when X is added to the library.

This is the most principled definition but faces computability issues. However, if you fix a specific description language (your candidate grammar), it becomes computable. The question is whether the output is robust across reasonable choices of description language.

**Research program:**
- Define a minimal description language for type-theoretic structures
- Implement exact ν computation for small libraries (first 5–7 Genesis steps where the candidate space is manageable)
- Compare with proof-rank ν
- If they agree for the early steps, you have evidence that proof-rank is approximating something real

**Approach B: Categorical enabling power**

ν(X | B) = the number of new natural transformations, adjunctions, or equivalences that become expressible when X is added.

This would connect ν to the categorical structure of type theory itself. It's mathematically beautiful but may be very hard to compute.

**Research program:**
- For each Genesis structure, manually enumerate the key categorical structures it enables
- Check whether these counts match the ν values
- If they do, formalize the counting as a definition

**Approach C: Homotopy-theoretic richness**

ν(X | B) = the rank of the new homotopy groups, cohomology groups, or spectral sequence pages that become accessible.

This connects directly to the algebraic topology that HoTT internalizes.

**Research program:**
- For each Genesis structure from S¹ onward, compute the new homotopy-theoretic invariants it makes accessible
- Check whether these counts match ν
- This might explain the specific numbers (e.g., S¹ giving ν = 7 because it unlocks π₁, Ω-loops, winding numbers, covering spaces, the long exact sequence, etc.)

### 2.3 The Convergence Test

Run all three approaches on the first 10 Genesis steps. If they converge to the same (or proportional) ν values, you have strong evidence that there *is* a canonical ν and that proof-rank is a reasonable proxy. If they diverge, you learn where the theory's degrees of freedom actually live.

### 2.4 Fix the Exponentiality Theorem

Don't drop it. Fix it. If ν grows superlinearly with Δ in practice, there must be a correct theorem explaining why. The current version fails because maps X → **2** from a HIT don't give 2^Δ degrees of freedom when path constructors impose constraints.

**The fix might be:**

> For a HIT X with Δ constructors, the number of *homotopy-distinct* maps X → K(n) (an Eilenberg-MacLane space of appropriate dimension) grows exponentially with the number of *independent* constructors (those not forced by coherence with lower constructors).

This would be a real theorem in HoTT and would correctly predict that S¹ → K(ℤ,1) gives more freedom than S¹ → **2**.

Alternatively:

> The eliminator of a HIT with Δ constructors, applied to a *type family* (not just a fixed type), generates 2^k independent proof obligations, where k is the number of point constructors plus the number of *independent* higher constructors.

The point is: there *should* be an exponentiality result, because the Genesis computation works. The task is to find the correct statement.

---

## Phase 3: Formalize the DCT Completely (Months 4–8)

### 3.1 Build the DCT in Cubical Agda

This is the ultimate test. If you can construct the DCT as a formal object in Cubical Agda, with all three components (cohesion, temporal logic, infinitesimals) and the compatibility triad (C1–C3), and derive the key theorems (internal tangent bundle, temporal evolution, Hamiltonian flows) as machine-checked proofs, the claim becomes extraordinarily hard to dismiss.

**Roadmap:**
1. Implement the cohesive modalities (♭, ♯, Π, Disc) — much of this exists in the literature already (Shulman's real-cohesive HoTT, Schreiber's work)
2. Implement the temporal modalities (○, ◇) following Nakano's guarded recursion
3. Implement the infinitesimal type 𝔻 following synthetic differential geometry
4. State and prove C1–C3
5. Derive the internal tangent bundle
6. Derive Hamiltonian flows

**This is a major project** — probably 6–12 months of dedicated work, possibly requiring a collaborator with Agda expertise. But it would be a landmark formalization regardless of PEN.

### 3.2 Prove the Operator Algebra Counts

**Spatial lattice (14):** The Kuratowski closure-complement theorem gives you this. Formalize it: given the cohesive modalities, enumerate all distinct unary operators on types, show there are exactly 14. This is classical and should be achievable.

**Temporal lattice (11):** This is the weak point. In standard LTL, the operator algebra is infinite (○^n for all n gives infinitely many distinct operators). You need to either:

- Define a *bounded* operator algebra (operators expressible within some fixed nesting depth) and prove the count is 11, or
- Show that under the type-theoretic semantics (where ○ is the guarded modality, not classical LTL), the algebra naturally collapses to finitely many distinct operators, and count them

**Tensor product:** Prove that C1–C3 imply the combined algebra has cardinality |L_S| × |L_T| − correction, and compute the correction exactly. The current "≈ −4" is not good enough for the claim you want to make.

### 3.3 Derive Specific Physical Predictions

If DCT truly describes the universe, it should make predictions that go beyond "classical mechanics can be expressed within it." Candidate predictions:

**Prediction 1: Gauge group structure.**
The Genesis Sequence produces S³ ≅ SU(2) before cohesion. Does DCT predict that the gauge group of fundamental interactions should be related to SU(2)? The Standard Model gauge group is SU(3) × SU(2) × U(1). Can you show that this specific group (or a group containing it) is the *most efficient* gauge group within DCT?

**Prediction 2: Spacetime dimension.**
Does the coherence window d = 2 constrain the dimensionality of spacetime? The Hopf fibration S³ → S² appears at step 9. The fiber is S¹. The sequence 1, 2, 3 (dimensions of the spheres) stops at 3 because S⁴ doesn't appear in the Genesis Sequence — its ν/κ ratio falls below the bar. Does this connect to the 3+1 dimensionality of spacetime?

**Prediction 3: Coupling constants.**
The efficiency ratios ρ_n at each Genesis step are specific numbers. Do any ratios of these numbers correspond to known physical constants or coupling constant ratios? This is the most speculative direction and the most likely to be numerological noise, but if you found a genuine match it would be extraordinary.

**Prediction 4: What DCT excludes.**
Equally important: what physical theories are *not* expressible in DCT? If DCT cannot express certain hypothetical physics (e.g., certain non-local theories, or theories with more than one time dimension), and those theories are indeed not observed, that's evidence.

---

## Phase 4: The Hardest Test — Is the Genesis Sequence Actually Unique? (Months 6–10)

This is where you find out whether the theory is true. Everything before this is strengthening the framework. This phase tries to break it.

### 4.1 Adversarial Candidate Injection

The strongest version of the "you baked it in" objection is: "If I add candidates you didn't think of, the sequence changes." Test this:

- Have someone *else* (ideally a mathematician who knows HoTT but is skeptical of PEN) propose 10 additional candidate structures
- Add them to the engine
- Run the simulation
- If the Genesis Sequence is unchanged, that's powerful evidence
- If it changes, understand *why* — is it because the new candidate is genuinely more efficient (meaning the old sequence was wrong), or because the ν computation is inadequate (meaning your measure needs work)?

### 4.2 Alternative Starting Points

The Genesis Sequence starts from an empty library. What if you start from a non-empty library?

- Start from a library containing the natural numbers (as if ℕ were axiomatized first)
- Start from a library containing basic group theory
- Does the sequence eventually converge to the same structures, just in a different order?
- If so, that's evidence of a basin of attraction — the DCT is an attractor regardless of starting point

### 4.3 The "Other Universes" Test

If d is the key parameter, what do d ≠ 2 universes look like?

- d = 1: The "stagnant universe." What structures does it produce? Is this a universe where only discrete mathematics exists and physics never gets off the ground?
- d = 3: The "tribonacci universe." Does it produce more complex structures faster? Does it produce a different endpoint? Is it *too* rich to be stable?
- d = 0: What does "no coherence memory" even mean? Is this the trivial universe?

If the d = 2 universe is the only one that produces something resembling known physics, and you can *explain why* (e.g., d = 1 stagnates, d ≥ 3 is unstable or produces pathological structures), that's a profound result.

---

## Phase 5: Write the Definitive Document (Months 10–14)

If the theory survives Phases 1–4, you will have:
- A theorem that d = 2 for HoTT (Pillar 1)
- A canonical or near-canonical ν measure (Pillar 2)
- A formalized DCT with proven operator counts (Pillar 3)
- Robustness evidence from adversarial testing (Phase 4)

At that point, the document to write is not two safe academic papers. It is one monograph:

### Structure

**Part I: The Engine**
- The PEN model as a dynamical system
- The General Scaling Theorem (arbitrary d)
- The d = 2 theorem for intensional type theory (the hard result from Phase 1)
- The corrected exponentiality result

**Part II: The Trace**
- The canonical ν measure and its derivation
- The Genesis Sequence as the unique output
- Complete ablations and robustness analysis
- Adversarial testing results

**Part III: The Endpoint**
- Full formal definition of DCT
- Proven operator algebra counts
- Machine-checked key theorems
- The Lattice Tensor Product as the mechanism of synthesis

**Part IV: Physics**
- Why DCT's internal structure matches observed physics
- Specific predictions and retrodictions
- What the Genesis Sequence explains about the "unreasonable effectiveness" of mathematics
- What would falsify the theory

### Where to Put It

If the work is as strong as it could be:

- **arXiv first** (hep-th or math.LO cross-listed to math-ph). This is where the serious people will find it.
- **Invite scrutiny explicitly.** Write to 5–10 people who could destroy the argument (Voevodsky's students, Schreiber, Coquand, a serious mathematical physicist). Ask them to try.
- **If it survives 6 months of scrutiny,** submit to a top venue. At that point, your options include *Annals of Mathematics*, *Communications in Mathematical Physics*, or — if the physics predictions pan out — *Physical Review Letters* or *Nature Physics.*

The point is not to sneak past reviewers. The point is to build something so solid that the reviewers have no choice.

---

## Collaboration Strategy

You don't need co-authors for credit. You need collaborators for capability. Specifically:

| Capability needed | Why | How to find |
|---|---|---|
| Cubical Agda expert | DCT formalization is a major Agda project | The Cubical Agda community is small; Mörtberg's group in Stockholm is close to Oslo |
| HoTT theorist | Validate the d = 2 classification | Coquand (Gothenburg), Shulman (USD), Buchholtz (Nottingham) |
| Mathematical physicist | Validate DCT-to-physics predictions | Schreiber (Prague) is the obvious person — his work on cohesive ∞-toposes is the direct ancestor of your DCT |
| Adversarial mathematician | Someone who wants to break it | Find a skeptic. The most valuable collaborator is someone who thinks you're wrong and tries to prove it. |

You're in a unique position: you can fund a small research group without institutional overhead. A postdoc or two working on formalization, supervised by you with consulting input from domain experts, could accelerate Phases 1–3 enormously.

---

## Decision Points and Kill Criteria

The plan must include conditions under which you conclude the theory is wrong. This is what separates science from ideology.

| Finding | Conclusion |
|---|---|
| d = 2 for HoTT cannot be proved, and concrete d = 3 obligations are found | The Fibonacci prediction is wrong. Theory needs fundamental revision. |
| No canonical ν exists: different reasonable measures produce different sequences | The Genesis Sequence is an artifact of modeling choices, not a fact about mathematics. Theory is interesting but not fundamental. |
| The DCT operator algebra has cardinality very different from 14 × 11 − 4 | The ν = 150 claim is wrong. DCT may still be the endpoint but not via the lattice tensor product mechanism. |
| Adversarial candidate injection changes the sequence in ways that break the four-phase structure | The sequence is not robust. It's a feature of the candidate grammar, not of mathematics. |
| d = 3 produces a sequence that also matches known physics | d = 2 is not uniquely selected. The theory loses its explanatory power. |
| DCT formalization reveals internal inconsistency (C1–C3 are contradictory) | DCT doesn't exist as a coherent type theory. The endpoint is a mirage. |

If any of these kill criteria are met, you will have learned something important. A theory that can't be killed can't be confirmed either.

---

## Timeline Summary

| Phase | Months | Goal | Key Output |
|---|---|---|---|
| 1. Coherence Window | 1–4 | Prove d = 2 for HoTT or find counterexample | Theorem or kill criterion |
| 2. Canonical ν | 2–6 | Derive ν from first principles or characterize its degrees of freedom | Canonical definition or honest characterization of non-uniqueness |
| 3. DCT Formalization | 4–8 | Build DCT in Cubical Agda, prove operator counts | Machine-checked proofs or identification of where formalization fails |
| 4. Adversarial Testing | 6–10 | Try to break the Genesis Sequence | Robustness evidence or identification of fragility |
| 5. The Document | 10–14 | Write the definitive monograph | One paper that either establishes a new field or honestly reports a negative result |

---

## The Bottom Line

The previous plan asked: "How do we get this published?" This plan asks: "Is this true?"

If it's true, publication will take care of itself — you won't be able to stop people from talking about it. If it's false, you'll have done something equally valuable: built a rigorous framework that precisely identifies *where* the intuition breaks down, which is itself a contribution to the foundations of mathematics.

The one thing you must not do is publish a version that's "good enough" to survive casual review but not strong enough to survive determined attack. For a claim this large, the only winning move is to make the argument so tight that the attacks bounce off.

You have the resources, the time, and — based on these papers — the mathematical ability to attempt this. The question is whether the universe cooperates.

---
---

# Phase 2 Action Plan: Derive the Canonical Novelty Measure

## Current State of ν (What We're Working With)

The engine computes ν three different ways depending on candidate type:

1. **Proof-rank clustering** (`ProofRank.hs` + `Cluster.hs`): Used for HITs and suspensions. Enumerates depth-≤1 type expressions over a two-step window {X, R_{n-1}, R_{n-2}, 1, 0}, filters for newly inhabited types, abstracts to schemas, removes trivials, counts distinct schemas, adds a latent homotopy bonus (pathBonus + maxPathDim²). This gives S¹ → ν=7, S² → ν=10 — exact matches.

2. **Component-based formulas** (`GenuineNu.hs` lines 117-261): Used for maps, algebras, modals, axioms, and synthesis. Each candidate type has a hand-built decomposition into additive components (e.g., Hopf: fibration(3) + longExact(4) + classifying(2) + cross(6) + funcSpace(3) = 18). The DCT uses the Lattice Tensor Product: 14 × 11 − 4 = 150.

3. **Capability rules** (`Capability.hs`): 18 independent rules (existence, functionSpace, productSum, pathLoop, homotopy, suspension, truncation, modal, fibration, longExact, classifying, fieldOps, modalCross, spectral, operator, cross, SU2, synthesis) that sum to give ν. Matches proof-rank for HITs, matches component formulas for axioms.

**The problem:** These three methods share intuitions but not definitions. The cross-interactions term in the component formulas (`cross = libSize + 5`, `cross = libSize * 3 + 9`) contains hand-tuned additive constants. The proof-rank latent bonus (`pathBonus + maxPathDim²`) is an empirical correction. The capability rules have hardcoded per-structure values (e.g., `"Connections" -> 14` in ruleCross). None of these is *derived* from a single principle.

---

## The Goal

A definition of ν that satisfies four criteria:

1. **Uniform:** One definition, applied identically to all candidate types — no category-level dispatch
2. **Computable:** Implementable as a function `computeNu :: Candidate -> TheoryState -> Int`
3. **Generative:** Produces the Genesis Sequence when plugged into the synthesis loop
4. **Principled:** Measures something mathematically meaningful, not "whatever makes the numbers work"

If such a definition exists, the Genesis Sequence becomes a theorem. If it doesn't, we learn exactly where the theory's degrees of freedom live, which is itself valuable.

---

## Work Package 2.1: Exact ν for the Bootstrap Phase (Weeks 1–3)

**Objective:** Compute ν from first principles for steps 1–7, where the candidate space is small enough for exhaustive enumeration. This establishes ground truth for calibrating any unified measure.

### Task 2.1.1: Build an exact ν oracle for small libraries

The key insight: for steps 1–7 the library contains ≤7 types, and the candidate space at each step is manageable. We can *enumerate every derivable construction* up to a fixed description depth and count exactly how many become newly derivable when X is added.

**Concrete implementation:**

- Create `ExactNu.hs` module
- Define a description language: the grammar of type expressions over the library (the existing `TypeExpr` AST in `Types.hs` is already close)
- For each library state B and candidate X:
  - Enumerate all closed type expressions of depth ≤ 2 over B
  - Enumerate all closed type expressions of depth ≤ 2 over B ∪ {X}
  - For each expression in the second set but not the first: count it as novel
  - For each expression in both sets whose minimal description length drops by ≥1 when X is available: count it as novel (Kolmogorov-style compression drop)
- Run this for steps 1–7 and compare against existing ν values

**Why depth 2:** Depth 1 is the current proof-rank window and misses compositions. Depth 2 captures one level of composition (e.g., Ω(S¹ → S²)) without combinatorial explosion at small library sizes.

**Expected output:** A table of exact ν values for steps 1–7 under the exhaustive definition. If these match (or are proportional to) the paper's values, the proof-rank method is approximating something real.

**Kill criterion:** If exact ν at depth 2 diverges wildly from the paper values *and* no natural depth cutoff produces agreement, the ν measure has genuine degrees of freedom that undermine canonicity.

### Task 2.1.2: Compare three enumeration depths

Run the exact oracle at depths 1, 2, and 3 for steps 1–5 (where depth 3 is still tractable). Record:

- How many new constructions appear at each depth
- Whether the *ranking* of candidates changes across depths (does the winner change?)
- Whether the ratio ν(depth d+1) / ν(depth d) stabilizes

If the ranking is stable across depths, the specific depth doesn't matter — only the relative ordering. This would mean ν is canonical up to a monotone rescaling, which is sufficient for the selection dynamics.

### Task 2.1.3: Document the exact oracle results

Write a self-contained section (for future inclusion in the paper or companion) presenting:
- The exact enumeration procedure
- The table of results at each depth
- Comparison with proof-rank ν
- Conclusion about whether proof-rank is a faithful proxy

---

## Work Package 2.2: Unify the Three ν Methods (Weeks 2–5)

**Objective:** Determine whether proof-rank clustering can be extended to cover *all* candidate types, replacing the component-based formulas entirely.

### Task 2.2.1: Extend proof-rank to axiom candidates

The current proof-rank method works for HITs/suspensions because they have explicit point/path constructors that the schema enumerator can work with. Axiom candidates (Connections, Curvature, Metric, Hilbert) are currently handled by hand-built formulas because they're not inductive types — they're axiomatic extensions.

**Approach:** Model each axiom candidate as a synthetic inductive type in the proof-rank framework:
- Connections: generates types like `∇ : (A → B) → (A → B)` (covariant derivative), `parallel : Path A → (Fiber A b → Fiber A b')` (parallel transport). Represent these as new type constructors and run the schema enumerator.
- Similarly for Curvature (generates `R : ∇ → ∇ → End(V)`), Metric (generates `g : TM × TM → ℝ`), Hilbert (generates `⟨-,-⟩ : H × H → ℂ`, spectral decomposition).

**Implementation in code:**
- Add a function `axiomToSyntheticEntry :: String -> TheoryState -> LibraryEntry` in `Generator.hs` that models each axiom as if it were a HIT with the appropriate number of "constructors"
- Route axiom candidates through the proof-rank pipeline instead of the component formulas
- Compare the output ν against the current component-formula values

**Success criterion:** Proof-rank ν for axiom candidates lands within ±20% of the component-formula values, and the Genesis Sequence is preserved.

### Task 2.2.2: Extend proof-rank to modal candidates

Cohesion introduces four modalities (♭, ♯, Π, Disc). These act as *type-level operators*, not constructors. The proof-rank enumerator currently doesn't know how to enumerate expressions involving modalities.

**Approach:** Extend `TypeExpr` with modal operators:
```
| TFlat TypeExpr      -- ♭A
| TSharp TypeExpr     -- ♯A
| TCohPi TypeExpr     -- ΠA (shape)
| TDisc TypeExpr      -- Disc(A)
```

Then extend the enumeration in `ProofRank.hs` to include these operators in the depth-1 enumeration. A modal candidate like Cohesion would add these four operators to the vocabulary; the enumerator would discover that ♭(S²), ♯(S¹ → S²), Π(S³), etc. are newly expressible.

**Expected result:** The schema count for Cohesion should be in the neighborhood of 19-20 (matching the current ν). If the operators interact with existing library types in the expected way, proof-rank should naturally capture the "modal × library" cross-terms that are currently hardcoded.

### Task 2.2.3: Extend proof-rank to synthesis candidates

This is the hardest case. The DCT's ν = 150 comes from the Lattice Tensor Product — a deep mathematical argument, not a surface-level enumeration. Can proof-rank recover it?

**Approach:** Add temporal operators (○, ◇) and infinitesimal type (𝔻) to `TypeExpr`. Then enumerate depth-1 expressions over the full vocabulary {spatial modalities, temporal modalities, 𝔻, library types}. The compatibility axioms (C1-C3) assert equivalences that *collapse* some expressions — reducing the count from the raw product to the corrected product.

**Key insight:** The Lattice Tensor Product says that *without* collapses, you get 14 × 11 = 154 distinct operational states. The collapses from C1-C3 reduce this by 4. If proof-rank enumeration over the full modal vocabulary naturally discovers ~150 distinct schemas, that's powerful evidence that the tensor product *is* the proof-rank answer at scale.

**This task is exploratory** — it may not work cleanly, and that's fine. If it fails, the failure mode tells us something important about the relationship between local enumeration and global algebraic structure.

### Task 2.2.4: Build the unified `CanonicalNu.hs` module

Regardless of whether proof-rank can cover all cases, create a single entry point:

```haskell
-- CanonicalNu.hs
data NuEvidence = NuEvidence
  { neSchemas    :: [Schema]        -- distinct proof schemas
  , neExact      :: Maybe Int       -- exact count (if computable)
  , neProofRank  :: Int             -- proof-rank estimate
  , neComponent  :: Int             -- component-formula estimate
  , neCapability :: Int             -- capability-rule estimate
  , neSelected   :: Int             -- the value actually used
  , neMethod     :: NuMethod        -- which method was selected and why
  }

computeNu :: Candidate -> TheoryState -> NuEvidence
```

This doesn't unify the computation — it *documents the disagreement* transparently. For each Genesis step, we record what all three methods say and which one is used. This is honest and it makes the degrees of freedom explicit.

---

## Work Package 2.3: The Convergence Test (Weeks 4–7)

**Objective:** Run all three candidate foundations for ν (from §2.2 of the strategic plan) on the first 10 Genesis steps and determine whether they converge.

### Task 2.3.1: Implement Approach A (Kolmogorov-style compression drop)

This is the exact oracle from WP 2.1, extended beyond step 7.

For steps 8–10 the library is larger and the candidate space explodes. Mitigation:
- Use the existing `Equivalence.hs` canonicalization to reduce the enumeration space
- Implement memoized enumeration: cache the set of derivable expressions for B, then incrementally compute the delta for B ∪ {X}
- If full enumeration is intractable at depth 2, fall back to depth 1 with statistical correction (multiply by the observed depth-2/depth-1 ratio from steps 1–7)

**Deliverable:** A column `ν_K` (Kolmogorov) for steps 1–10.

### Task 2.3.2: Implement Approach B (Categorical enabling power)

For each Genesis structure, manually enumerate:
- New natural transformations enabled (e.g., S¹ enables the winding number homomorphism π₁(S¹) → ℤ)
- New adjunctions enabled (e.g., Cohesion enables ♭ ⊣ Disc ⊣ ♯)
- New equivalences enabled (e.g., Hopf enables the fiber sequence S¹ → S³ → S²)

This is a *manual* research task, not a computational one. It requires going through the HoTT literature for each structure and listing the categorical structures it makes available.

**Deliverable:** A column `ν_C` (categorical) for steps 1–15, with citations for each count.

### Task 2.3.3: Implement Approach C (Homotopy-theoretic richness)

For each Genesis structure from S¹ onward:
- Count new homotopy groups that become computable (e.g., S² makes π₂ nontrivial)
- Count new cohomology classes (e.g., Hopf fibration generates H²(S²; ℤ))
- Count new spectral sequence pages accessible
- Count new fiber sequences

**Deliverable:** A column `ν_H` (homotopy) for steps 5–15.

### Task 2.3.4: Build the convergence comparison table

Assemble all columns into a single table:

| n | Structure | ν_paper | ν_proofrank | ν_K | ν_C | ν_H | Agreement? |
|---|-----------|---------|-------------|-----|-----|-----|------------|

**Analysis questions:**
- Do the columns agree in absolute values? In ratios? In rankings?
- Is there a monotone transformation (e.g., a consistent multiplier) that aligns them?
- Where do they disagree, and what does the disagreement reveal?

**If they converge:** There *is* a canonical ν, and proof-rank is a good approximation of it. This is the best possible outcome.

**If they diverge:** Characterize the *space* of valid ν measures — what constraints do they share (monotonicity, subadditivity, etc.) and where do they differ. This honest characterization is itself a contribution.

---

## Work Package 2.4: Fix the Exponentiality Theorem (Weeks 5–8)

**Objective:** Find the correct statement of the theorem that ν grows superlinearly with Δ. The current version (Theorem 6.3 in pen_paper.tex, `thm:exponentiality`) correctly identifies OIT exponentiality (2^Δ₀ maps to Bool) but the extension to HITs relies on hand-waving about "library cross-interactions and synthesis multiplicativity."

### Task 2.4.1: Characterize the correct codomain

The current theorem uses maps to **2** (Bool). For HITs, path constructors force connected components together, collapsing the 2^Δ₀ count. The fix: change the codomain.

**Candidate codomains to investigate:**
- **Eilenberg-MacLane spaces K(G,n):** Maps X → K(ℤ,n) classify cohomology H^n(X; ℤ). For S², maps S² → K(ℤ,2) = ℂP^∞ correspond to H²(S²; ℤ) ≅ ℤ — infinitely many homotopy classes. This is *too many* — need to count within a bounded effort.
- **Type families over X:** The eliminator of X into a type family B : X → U generates one obligation per constructor. For HITs, the path obligations are genuinely independent data (not collapsed). Count: one independent choice per point constructor, plus constrained but nontrivial choices per path constructor.
- **Bounded-depth eliminations:** Fix a depth bound d. Count all distinct terms of depth ≤ d in the elimination form of X. This is computable and grows with Δ.

### Task 2.4.2: State and prove the corrected theorem

**Target statement (draft):**

> **Theorem (Combinatorial Novelty, corrected).** Let X be a type with cell presentation (C₀, C₁, ..., C_k) introduced into a library B containing |B| prior types. Let Δ₀ = |C₀| (point constructors) and Δ₁ = |C₁| (path constructors). Then:
>
> (i) *OIT case (Δ₁ = 0):* The number of semantically distinct maps X → **2** is exactly 2^Δ₀.
>
> (ii) *HIT case (Δ₁ > 0):* The number of semantically distinct dependent eliminations of X into type families over B — counting families Y : X → U with one choice per constructor — is at least Δ₀ · |B| and at most 2^Δ₀ · |B|^Δ₁.
>
> (iii) *Synthesis case:* When two independently realized frameworks F₁, F₂ are composed via compatibility axioms, the novelty ν(F₁ ⊗ F₂) ≥ ν(F₁) · ν(F₂) − O(ν(F₁) + ν(F₂)).

**Implementation:**
- Formalize (i) — this is already done in pen_paper.tex (Theorem 6.1) and is correct
- Formalize (ii) — this requires carefully counting dependent eliminations. Use Cubical Agda experiments: for S¹, S², T², enumerate all elimination goals and count independent choices.
- Formalize (iii) — this is the Lattice Tensor Product theorem; needs the operator algebra counts from Phase 3 to be fully rigorous, but the *statement* can be made precise now

### Task 2.4.3: Verify computationally

- For each Genesis HIT (S¹, S², S³, T²): count the actual number of independent elimination obligations in Cubical Agda
- Compare against the formula in Task 2.4.2
- Record any discrepancies

---

## Work Package 2.5: Sensitivity Analysis (Weeks 7–9)

**Objective:** Determine how robust the Genesis Sequence is to perturbations of ν. The plan mentions "±30% tolerance" as preliminary evidence — we need to make this precise.

### Task 2.5.1: Perturbation sweep

Modify the synthesis loop in `Synthesis.hs` to accept a perturbation function:
```haskell
type NuPerturbation = Int -> Int -> Int  -- step -> base_nu -> perturbed_nu
```

Run the Genesis simulation with:
- Uniform scaling: ν → α·ν for α ∈ {0.5, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.5, 2.0}
- Per-step noise: ν → ν + ε where ε ~ Uniform(-k, +k) for k ∈ {1, 2, 3, 5, 10}
- Adversarial perturbation: at each step, try to find the *smallest* perturbation of ν that changes the winner

For each perturbation, record:
- Does the same sequence emerge?
- If not, where does it first diverge?
- What is the *minimum* perturbation that breaks each step?

### Task 2.5.2: Identify the critical margins

From Task 2.5.1, build a "margin table":

| n | Structure | ρ | Bar | Margin (ρ - Bar) | Min perturbation to break |
|---|-----------|---|-----|-------------------|---------------------------|

The tightest margin in the current table is at n=4 (dependent types, margin = 0.17). If the perturbation analysis confirms that a few steps have very tight margins and the rest are robust, that's informative: it tells us *which* ν values matter most and where the degrees of freedom in the measure are tightest.

### Task 2.5.3: Characterize the basin of attraction

Run the sensitivity analysis in reverse: instead of perturbing ν and checking if the sequence changes, *enumerate all possible orderings* of the 15 structures and check which orderings are consistent with *some* ν assignment that satisfies ρ ≥ Bar at every step.

This is computationally expensive (15! orderings) but can be pruned heavily:
- The bootstrap phase (steps 1–4) is essentially forced by prerequisites
- Many orderings violate prerequisite constraints (e.g., S² before S¹)
- The bar constraint eliminates most remaining orderings

**If only one ordering survives:** The Genesis Sequence is essentially unique regardless of ν details. This would be the strongest possible robustness result.

**If multiple orderings survive:** Characterize the family of valid sequences. How different can they be?

---

## Work Package 2.6: Integration and Testing (Weeks 8–10)

### Task 2.6.1: Add new engine phases

Add to `Main.hs`:
- **Phase M**: Exact ν oracle (WP 2.1)
- **Phase N**: Unified CanonicalNu comparison (WP 2.2)
- **Phase O**: Convergence table (WP 2.3)
- **Phase P**: Sensitivity sweep (WP 2.5)

Each phase should produce machine-readable output suitable for inclusion in the paper.

### Task 2.6.2: Write the ν characterization section

Regardless of whether we find a single canonical ν, produce a self-contained document (or paper section) containing:

1. The exact oracle results (WP 2.1)
2. The convergence table (WP 2.3)
3. The corrected exponentiality theorem (WP 2.4)
4. The sensitivity analysis (WP 2.5)
5. An honest assessment: "ν is canonical / ν is canonical up to monotone rescaling / ν has N degrees of freedom that affect M of 15 steps"

### Task 2.6.3: Update pen_paper.tex and pen_genesis.tex

- Replace the current Section 6 (Combinatorial Novelty Theorem) with the corrected version from WP 2.4
- Add a sensitivity analysis subsection to the verification section of pen_genesis.tex
- If the convergence test shows canonical ν: add a new section deriving it
- If the convergence test shows non-uniqueness: add an honest characterization of the degrees of freedom, and prove that the Genesis Sequence is invariant under them

---

## Decision Points

| Week | Question | Go | Kill / Pivot |
|------|----------|----|-------------|
| 3 | Does exact ν (depth 2) match paper ν for steps 1–7? | Match within ±25% | Divergence > 50% for 3+ steps → ν has serious degrees of freedom |
| 5 | Can proof-rank cover axiom candidates? | ν within ±20% of component formulas | Off by > 2× → proof-rank and component formulas measure different things |
| 7 | Do Approaches A/B/C converge? | Rankings agree for 8+ of 10 steps | Rankings disagree for 4+ steps → no canonical ν exists |
| 8 | Can we state a correct exponentiality theorem? | Clean statement with Agda evidence | No statement covers both OITs and HITs → novelty scaling is case-by-case |
| 9 | How tight are the margins? | 12+ of 15 steps survive ±30% perturbation | 5+ steps break under ±15% perturbation → sequence is fragile |

---

## Dependencies on Other Phases

- **Phase 1 (Coherence Window):** The exact ν oracle (WP 2.1) uses the two-step enumeration window, which is justified by d=2. If Phase 1 revises d, the enumeration window changes.
- **Phase 3 (DCT Formalization):** The tensor product ν = 150 (WP 2.2.3) ultimately needs the operator algebra counts from Phase 3 to be rigorous. We can proceed with the current argument but flag it as provisional.
- **Phase 4 (Adversarial Testing):** The sensitivity analysis (WP 2.5) provides *internal* robustness evidence. Phase 4's *external* adversarial testing (new candidates from skeptical mathematicians) is the stronger test and can use the CanonicalNu infrastructure we build here.

---

## Deliverables Summary

| # | Deliverable | Type | Week |
|---|-------------|------|------|
| D2.1 | ExactNu.hs — exhaustive ν computation for small libraries | Code | 3 |
| D2.2 | Exact ν table for steps 1–7 at depths 1, 2, 3 | Data | 3 |
| D2.3 | Extended proof-rank for axiom/modal/synthesis candidates | Code | 5 |
| D2.4 | CanonicalNu.hs — unified ν entry point with evidence records | Code | 5 |
| D2.5 | Convergence table (ν_K, ν_C, ν_H) for steps 1–10 | Data + Analysis | 7 |
| D2.6 | Corrected Combinatorial Novelty Theorem (statement + Agda evidence) | Math | 8 |
| D2.7 | Sensitivity analysis: perturbation sweep + margin table + basin characterization | Data + Analysis | 9 |
| D2.8 | Engine phases M/N/O/P integrated into Main.hs | Code | 10 |
| D2.9 | Updated pen_paper.tex Section 6 (corrected exponentiality) | Paper | 10 |
| D2.10 | Self-contained ν characterization document | Writing | 10 |
