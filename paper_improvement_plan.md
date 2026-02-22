# PEN Paper Improvement Plan

**Created:** 2026-02-22
**Last updated:** 2026-02-22

---

## Previously Completed (summary)

All items from prior review rounds are resolved and committed:

1. Integration Trace Principle — proved, machine-checked at steps 8–9.
2. Generative Capacity reframing — novelty is a single intrinsic metric; Spectral Decomposition is emergent.
3. Inference-rule counter — 15/15 exact match, all steps verified.
4. DCT singularity resolved — ν(DCT) = 105 via Combinatorial Schema Synthesis.
5. Kolmogorov κ formalized — κ(X|B) is Conditional Kolmogorov Complexity via MBTT encoding.
6. κ definition contradiction resolved — specification selection via minimal overshoot.
7. S³ ν_C = 3 anomaly justified — H-space elimination from selected specification.
8. DCT semantic audit grounded — 3 concrete schema examples with HoTT type signatures.
9. Extensional Boundary closed — adjoint completion in UniformNu.hs gives 15/15.
10. ν_H topological projection formalized — concrete 3×3 cubical Kan matrix.
11. Ontological tone calibrated — systematic overclaim removal.
12. ν_G contradiction resolved — Steps 10–14 have ν_G > 0.
13. Axiom packing defense, automation scope, candidate generation, claims scoping, nomenclature.

---

## Current Round: Feedback Items

### Issue 1: Fix Vector Field Type Error in Gödelian Horizon ✅ RESOLVED

**Priority: CRITICAL**
**Status:** Complete

#### The Problem

Lemma 5.6 (lem:internalization) claimed `fix : (X^D → X) → X` with the assertion that `X^D → X` is a "vector field." In SDG, a vector field is `X → X^D` (section of tangent bundle), not `X^D → X` (evaluation of infinitesimal curve).

#### Changes Made

1. **lem:internalization proof rewritten**: Now correctly frames a discrete-time dynamical system as `f : X → ○X ≃ X → X^D` (a vector field). Guarded corecursion unfolds this into a global trajectory `unfold_f : X → Stream X`. Löb's rule `fix : (○A → A) → A` is the elimination principle; the introduction `next : X → ○X` is the dynamical system.
2. **thm:end-of-history proof**: "differential fixpoint combinator" → "guarded corecursion principle"; flow description corrected.
3. **Gödelian Horizon discussion** (cor:confinement): Updated reference to "guarded corecursion principle."

#### Key Insight

The connected-component confinement argument is *strengthened* by the correction: a vector field `v : U → U^D` generates continuous deformations within the connected component of U, making the Gödelian Horizon argument more precise. The philosophical point (continuous flows can't cross discrete boundaries) is preserved with correct type signatures.

---

### Issue 2: Resolve "Axiom vs. Definition" Scoring Double Standard ✅ RESOLVED

**Priority: HIGH**
**Status:** Complete

#### The Problem

Steps 11–14 count derived theorems (Hodge star, Laplacian, Ricci) as "Elimination rules" (metric gets ν_C = 39), but ℕ is capped at ν ≤ 4 by counting only foundational constructors.

#### Changes Made

1. **New Remark 2.6 (rem:library-api)**: Explicit "Library extensions vs. foundational primitives" remark after Definition 2.4. PEN evaluates the optimal *mathematical library/API*, not just core judgment extensions. κ = clause count of API specification, ν = derivation schemas natively unlocked. Scoring is uniform via the Uniform Algorithm.
2. **rem:axiomatic-extensions rewritten**: "Axiomatic extensions" → "Library specifications." Key distinction is *derivability*: metric is not derivable from cohesion; addition IS derivable from rec_ℕ. Non-derivable specs have ν > 0; derivable definitions have ν = 0.
3. **ℕ rejection expanded**: Now addresses the double standard head-on. Every ℕ operation (plus, times, exp, prime factorization) is *derivable* from rec_ℕ → ν = 0. Even with generous uniform-algorithm scoring (ν ≤ 10), ν_H = 0 (no path constructors) means no superlinear novelty.
4. **Appendix B.4 updated**: "Derived inference rules" → "Derivation schemas"; explicit note that these are scored as patterns natively unlocked, not core inference rules.
5. **"Three Patterns" section**: "axiomatic extension" → "library specification" with cross-reference to rem:library-api.
6. **Section 7 candidate generation**: Updated to use "library specification" terminology.
7. **Discussion "Assumed" section**: Updated terminology throughout.

#### Key Insight: The Derivability Criterion

The critical distinction is not "foundational vs. definitional" but *derivable vs. non-derivable*. ℕ's rich API is entirely derivable from its recursor. The metric's API is irreducible: no combination of cohesive modalities produces a positive-definite bilinear form. This makes the scoring methodology consistent: ν counts non-derivable derivation schemas, applied uniformly.

---

### Issue 3: Clarify Integration Trace Principle as Systemic Constraint ✅ RESOLVED

**Priority: HIGH**
**Status:** Complete

#### The Problem

Theorem 4.1 assumes adding X_{n+1} requires sealing against the entire past d layers. In HoTT, defining S² doesn't require interaction clauses with PropTrunc.

#### Changes Made

1. **New Remark (rem:maximal-coupling)**: "Maximal interface density as a modeling assumption." Explicitly states this is PEN's library-quality policy, not a HoTT requirement. Three justifications:
   - Physical: unified theories require universal coupling (all fields couple to metric)
   - Combinatorial: complete API coverage requires interaction documentation
   - Predictive: without this coupling, no Fibonacci latency → no selection dynamics
2. **Discussion "Assumed" section**: New bullet point for maximal interface density as an explicit modeling assumption.

#### Key Insight

Making the maximal-coupling assumption explicit actually *strengthens* the paper. The Fibonacci latency is now a *consequence* of a clearly stated policy, not an implicit requirement smuggled in from the type theory. Reviewers can evaluate whether the policy is physically/mathematically motivated rather than debating whether HoTT requires it.

---

### Issue 4: Downgrade Epistemological Rhetoric ✅ RESOLVED

**Priority: MEDIUM**
**Status:** Complete

#### Changes Made

1. **Spectral Decomposition** (thm:spectral, thm:spectral-preview): `\begin{theorem}` → `\begin{observation}` (new "Empirical Observation" environment). Both instances updated.
2. **Topological Projection** (thm:topological-projection): Kept as Theorem but added "Framework dependence" paragraph at end of proof. Explicitly acknowledges dependence on CCHM cubical model (connection maps, Kan operations, E₂ degeneration). Conjectures d² scaling is robust but exact coefficient may differ in other cubical frameworks.
3. **Discussion "Proved" section**: Spectral Decomposition moved from categorical "proved" to "verified by exhaustive parameter sweep, not derived from first principles." Topological Projection caveat added about CCHM framework specificity.

---

### Issue 5: Demystify the "Theory-Guided" Candidate Pool ✅ RESOLVED

**Priority: MEDIUM**
**Status:** Complete

#### Changes Made

1. **Abstract softened**: "PEN autonomously generates the Generative Sequence" → "PEN filters and uniquely orders a landscape of candidate mathematical structures—the Generative Sequence."
2. **Rejected candidates table expanded**: 5 → 10 entries. Added:
   - Ordinal arithmetic (ν ≤ 6, κ = 5, ρ ≤ 1.20 vs bar 2.14)
   - Measure theory (ν ≈ 12, κ = 6, ρ ≈ 2.0 vs bar 4.46)
   - Elementary topos axioms (ν ≈ 15, κ = 8, ρ ≈ 1.9 vs bar 4.46)
   - Scheme theory (ν ≈ 18, κ = 7, ρ ≈ 2.6 vs bar 5.50)
   - Galois theory (ν ≈ 10, κ = 5, ρ ≈ 2.0 vs bar 5.50)
3. **Explanatory paragraphs**: Each new entry has a paragraph explaining the ν estimate and why it fails.
4. **Engine section**: "selection mechanism is autonomous" → "selection mechanism is deterministic"; added cross-reference to expanded rejected table.

#### Key Insight: The ν_H = 0 Barrier

All 5 new rejected candidates share a common feature: ν_H = 0. Discrete/algebraic structures lack path constructors and therefore cannot access the d² topological novelty mechanism. This means their novelty grows at most *linearly* with specification complexity, while the Fibonacci bar grows exponentially. The expanded table makes this structural argument vivid: regardless of how rich the algebraic structure, without homotopy content it cannot outpace geometric candidates.

---

## Key Theoretical Insights (accumulated)

### The Library/API Reframing (Issue 2)
PEN evaluates the optimal *mathematical library* to build, not merely extensions to the core type theory. The derivability criterion (non-derivable specifications have ν > 0; derivable definitions have ν = 0) provides a uniform scoring methodology applicable to both foundational steps and library specifications.

### The Maximal Coupling Policy (Issue 3)
The Fibonacci latency arises from PEN's explicit maximal-coupling policy, not from HoTT's typechecking requirements. This policy is physically motivated (universal coupling in unified theories) and is now an explicit assumption rather than an implicit consequence.

### The Specification Selection Principle
Different specifications of the same type yield different (ν, κ) pairs. Minimal overshoot selects the optimal level of richness. S³ carries its Lie group structure as a *prediction* of the dynamics.

### The Curry-Howard Adjoint
The Extensional Boundary is a structural feature of the Curry-Howard correspondence. The Adjoint Completion Principle closes it: every Introduction rule determines an Elimination rule via adjunction.

---

## Open Research Questions

1. Theoretical derivation of d = 2 via adjoint functor argument.
2. Tangent Topos hypothesis (why the sequence terminates).
3. Specification Selection formalization (variational principle).
4. Update Kolmogorov.hs with SU(2) and native HIT specifications.
5. Deeper uniform verification (tighter schema deduplication).
6. Compute ν estimates for new rejected candidates in the Haskell engine (currently paper-only estimates).
7. Investigate whether the d² scaling of ν_H is robust across cubical frameworks (CCHM vs Cartesian).
