# Step 2: Theoretical Derivation of $d = 2$

## Current State

**What's established:**
- **Upper bound ($d \leq 2$):** Proved in §3.3 via ∞-groupoid coherence (Lurie/Lumsdaine/van den Berg–Garner): once dims 0, 1, 2 are fixed, all higher coherence cells are uniquely determined. Combined with sealing encapsulation, machine-checked at steps 8–9 in Cubical Agda.
- **Lower bound ($d \geq 2$):** The Hopf fibration's clutching function is an irreducible depth-2 obligation carrying genuine data (`HopfTrace.agda`).
- **Five depth-3 counterexample attempts all failed** (`DepthThreeAttempt.agda`), all reducing to depth 2 via sealing.

**What's missing:**
The current proof says *what* d equals but not *why*. The upper bound appeals to ∞-groupoid coherence (a deep external theorem), and the lower bound is a single example. We need a **categorical reason** — ideally one sentence: "d = 2 because ___."

---

## Three Converging Proof Strategies

### Strategy A: The Adjoint Functor Argument (WHY $d \geq 2$)

**Core Thesis:** Every type former in HoTT is an adjunction (Intro $\dashv$ Elim). Verifying an adjunction requires data spanning exactly two composition levels (Unit + Counit + Triangle Identities). A $d = 1$ system cannot close the triangle identities.

**Phase A1 — Formalize the Adjunction Structure of Type Formers**

For each Genesis type former, exhibit the adjunction explicitly:

| Type Former | Left Adjoint (Intro) | Right Adjoint (Elim) | Unit $\eta$ | Counit $\varepsilon$ |
|---|---|---|---|---|
| $\Pi$-types | $\lambda$-abstraction | Application | $\eta$-expansion: $f \mapsto \lambda x. f(x)$ | $\beta$-reduction: $(\lambda x.t)(a) \mapsto t[a/x]$ |
| $\Sigma$-types | Pairing $(a, b)$ | Projection $\pi_1, \pi_2$ | $p \mapsto (\pi_1 p, \pi_2 p)$ | $\pi_i(a, b) \mapsto a$ or $b$ |
| Identity types | $\mathsf{refl}$ | $J$-eliminator | $\mathsf{refl}$-expansion | $J$ computation rule |
| HITs ($S^1$, etc.) | Constructors (base, loop) | $\mathsf{rec}_{S^1}$ | $\eta$-rule for $S^1$ | $\beta$-loop computation |

**Deliverable:** A table in the paper + Agda formalizations of each adjunction.

**Phase A2 — Prove Triangle Identities Require Depth 2**

The triangle identities for an adjunction $L \dashv R$ are:
$$(\varepsilon L) \circ (L\eta) = \mathrm{id}_L \qquad (R\varepsilon) \circ (\eta R) = \mathrm{id}_R$$

These have a specific **composition depth structure**:
- $\eta$ is a map $\mathrm{Id} \to RL$ — it references the *interaction* of $R$ and $L$ (depth 1: current layer ↔ previous layer)
- $\varepsilon$ is a map $LR \to \mathrm{Id}$ — same depth
- The triangle identity composes $\varepsilon$ and $\eta$ — this is a *coherence between two interactions*, referencing $L_{n}$ (the new type's Intro), $L_{n-1}$ (the library's Elim for older types), and their interaction. This is depth 2.

**Key lemma to prove:** In a system with window $d = 1$, where the current step can only see $L_n$, the triangle identity $(\varepsilon L) \circ (L\eta) = \mathrm{id}_L$ involves the composition $L \to LRL \to L$. The middle term $LRL$ requires seeing $R$ (from $L_{n-1}$) sandwiched between two applications of $L$ (from $L_n$). A $d = 1$ system sees only $L_n$'s interface and cannot verify the middle step.

**Formal statement:**
> **Theorem (Adjunction Depth).** Let $\mathcal{T}$ be a dependent type theory with type formers defined by adjunctions. Then verifying the triangle identities for any type former introduced at step $n$ requires data from both $L_n$ and $L_{n-1}$. In particular, $d \geq 2$.

**Phase A3 — Connect to Stagnation**

Show that $d = 1$ stagnation is *precisely* the failure of triangle identities:
- With $d = 1$, the system can define Intro and Elim rules (each requires only one layer)
- But it cannot verify their *mutual coherence* (the triangle identities)
- Without coherence, the type former is not well-defined, and integration fails
- This is exactly what the engine observes: with `--window 1`, the bar cannot be cleared because candidates lack the coherence data needed for genuine integration

**Deliverable:** Proof that $d = 1$ implies no new adjunctions can be verified beyond the first step, explaining the constant $\Delta$ sequence.

---

### Strategy B: The 2-Coskeletal Nerve Argument (WHY $d \leq 2$)

**Core Thesis:** The obligation category for HoTT has a nerve that is 2-coskeletal — its simplicial structure is entirely determined by its 0-, 1-, and 2-simplices. This is the precise categorical reformulation of the upper bound.

**Phase B1 — Define the Obligation Category**

For a library $\mathcal{B}$ with layers $L_1, \ldots, L_n$:
- **Objects:** Layers $L_k$
- **Morphisms $L_j \to L_k$ ($j < k$):** The set of interface obligations that $L_k$'s sealing must discharge against $L_j$'s exports
- **2-morphisms:** Coherence conditions between obligation chains (e.g., the obligation $L_k \to L_j$ composed with $L_j \to L_i$ vs. the direct obligation $L_k \to L_i$)

**Phase B2 — Prove 2-Coskeletality**

The claim: for $n \geq 3$, every $n$-simplex in the nerve is uniquely determined by its $(n-1)$-faces.

This follows from two facts:
1. **∞-Groupoid coherence** (Lurie): In an ∞-groupoid, $n$-cells for $n \geq 3$ are uniquely determined by their boundary. Applied to the path spaces of types in HoTT: once the 0-cells (points), 1-cells (paths), and 2-cells (homotopies) of the obligation structure are fixed, all higher cells are contractible.

2. **Sealing encapsulation**: The abstraction barrier means that a 3-simplex $L_k \to L_j \to L_i \to L_h$ factors through the sealed interfaces, making the composite obligation reducible.

**Formal statement:**
> **Theorem (2-Coskeletal Obligations).** The nerve of the obligation category $\mathcal{O}(\mathcal{B})$ is 2-coskeletal. Equivalently, the simplicial set $N(\mathcal{O})$ satisfies: for all $n \geq 3$, the canonical map $N_n \to (\mathrm{cosk}_2 N)_n$ is an equivalence.

**Deliverable:** This theorem directly implies $d \leq 2$ and connects the PEN framework to standard simplicial homotopy theory.

---

### Strategy C: The Spectral Sequence Argument (SYNTHESIS)

**Core Thesis:** The obligation complex admits a filtration whose associated spectral sequence degenerates at $E_2$. This is the precise content of "$d = 2$" in the language algebraic topologists recognize.

**Phase C1 — Define the Obligation Filtration**

Define a filtered chain complex:
- $C_n$ = the free abelian group on the set of $n$-dimensional obligation cells
- The filtration $F_p C_n$ = obligations referencing only layers $L_k$ with $k \leq p$
- This is a bounded, exhaustive, increasing filtration

**Phase C2 — Identify the Spectral Sequence**

The spectral sequence $\{E_r^{p,q}, d_r\}$ has:
- $E_0^{p,q} = F_p C_{p+q} / F_{p-1} C_{p+q}$ (obligations at filtration level exactly $p$ in dimension $p+q$)
- $d_0: E_0^{p,q} \to E_0^{p,q-1}$ is the internal differential
- $E_1^{p,q} = H_q(L_p)$ (the obligation homology of layer $p$)
- $d_1: E_1^{p,q} \to E_1^{p-1,q}$ captures cross-layer interactions

**Phase C3 — Prove Degeneration at $E_2$**

The key: $d_r = 0$ for all $r \geq 3$. This follows because:
- $d_2$ captures the depth-2 interactions (the highest non-trivial cross-layer coherence)
- $d_3$ would represent depth-3 interactions, but by the 2-coskeletal nerve (Strategy B), these are all trivial
- Therefore $E_2^{p,q} = E_\infty^{p,q}$ for all $p, q$

**Connection to known mathematics:**
- This mirrors the Atiyah-Hirzebruch spectral sequence for fiber bundles, which often degenerates at $E_2$ for dimensional reasons
- For Postnikov towers of ∞-groupoids, the $k$-invariants live in degree 2, matching the $d_2$ differential
- The Serre spectral sequence for $S^1 \to S^3 \to S^2$ (the Hopf fibration!) has its only non-trivial differential at $d_2$

**Deliverable:** A clean statement connecting PEN's $d = 2$ to spectral sequence degeneration, with the Hopf fibration's Serre spectral sequence as the canonical example.

---

## Implementation Roadmap

### Phase 1: Foundations (Paper + Math)

**Goal:** Write the adjunction structure table and the formal theorem statements.

1. **Paper §3.4 (new subsection): "Why $d = 2$: The Adjunction Depth Theorem"**
   - Present the adjunction table for all Genesis type formers
   - State the Adjunction Depth Theorem (Strategy A)
   - Give the proof that triangle identities require depth 2
   - Show that $d = 1$ stagnation follows as a corollary

2. **Paper §3.5 (new subsection): "The Obligation Nerve and Spectral Sequence"**
   - Define the obligation category and its nerve
   - State the 2-Coskeletality Theorem (Strategy B)
   - Outline the spectral sequence argument (Strategy C)
   - Connect to the Serre spectral sequence of the Hopf fibration

### Phase 2: Agda Formalization

**Goal:** Machine-check the core lemmas.

3. **`agda/Adjunction/AdjunctionDepth.agda`** — New module:
   - Formalize the adjunction structure for $\Pi$, $\Sigma$, Identity, $S^1$
   - Define what "verifying a triangle identity" means in the obligation framework
   - Prove: triangle identity verification requires data from 2 layers
   - This gives a machine-checked lower bound $d \geq 2$

4. **`agda/Adjunction/TriangleIdentity.agda`** — New module:
   - For each concrete adjunction ($\Pi$, $\Sigma$, Id, $S^1$), exhibit the unit, counit, and triangle identities
   - Show that each triangle identity references exactly 2 layers of the library
   - Verify the obligation decomposition: triangle = (depth-1 piece) + (depth-2 piece)

5. **`agda/Nerve/TwoCoskeletal.agda`** — New module:
   - Define the obligation category formally
   - Define its nerve as a simplicial set
   - Prove 2-coskeletality for the concrete Genesis cases (steps 1–9)
   - This gives a machine-checked upper bound $d \leq 2$ for those cases

### Phase 3: Engine Validation

**Goal:** Computational experiments supporting the theoretical claims.

6. **Engine: Adjunction detector**
   - Add a function that, given a Genesis step, identifies the adjunction pair (Intro $\dashv$ Elim)
   - For each type former, extract the unit ($\eta$-rule), counit ($\beta$-rule), and verify the triangle identity
   - Report the obligation depth of each triangle identity
   - Expected: all triangle identities have obligation depth exactly 2

7. **Engine: Obligation nerve computation**
   - For each step $k = 1, \ldots, 15$, compute the nerve of the obligation category up to dimension 3
   - Verify that all 3-simplices are degenerate (determined by their 2-faces)
   - This gives computational evidence for 2-coskeletality across all 15 steps

8. **Engine: Spectral sequence page computation**
   - Implement the $E_0, E_1, E_2$ pages of the obligation spectral sequence
   - Verify $d_3 = 0$ computationally for the Genesis Sequence
   - Print a formatted table of the spectral sequence pages

### Phase 4: Falsifiability and Stress Testing

**Goal:** Identify exactly what would break if $d \neq 2$.

9. **Fiber bundle family test** (from the improvement plan):
   - Construct a family of fiber bundles over $S^n$ with non-trivial clutching functions
   - For each member, compute the obligation depth
   - If all members require exactly depth 2: strong evidence
   - If some require depth 3: the framework is falsified

10. **Non-HoTT foundation test:**
    - Run the engine with a hypothetical $d = 3$ foundation (e.g., a type theory with non-trivial 3-truncation)
    - Predict: the d-bonacci sequence changes to tribonacci, and the Genesis sequence diverges
    - If the sequence still terminates at 15: evidence that $d$ is not a free parameter
    - If it changes: evidence that $d$ genuinely controls the theory

### Phase 5: Paper Integration

**Goal:** Write the final sections.

11. **Revise §3 to incorporate the three strategies:**
    - §3.2: $d = 1$ (extensional) — existing, keep
    - §3.3: $d = 2$ upper bound — strengthen with 2-coskeletal nerve argument
    - §3.4: $d = 2$ lower bound — strengthen with Adjunction Depth Theorem
    - §3.5: Synthesis — spectral sequence degeneration at $E_2$

12. **Update §8 (Discussion):**
    - Move $d = 2$ from "partly empirical" to "derived from adjunction structure"
    - Add the spectral sequence connection to the "connections to existing mathematics" section
    - Update the falsifiability section with the fiber bundle family test

---

## Key Theorems to Prove

| # | Name | Statement | Status |
|---|------|-----------|--------|
| 1 | **Adjunction Depth** | Verifying an adjunction's triangle identities at step $n$ requires data from $L_n$ and $L_{n-1}$ | To prove |
| 2 | **$d=1$ Stagnation (strengthened)** | A $d=1$ system cannot verify any adjunction beyond the initial, hence $\Delta_n = \mathrm{const}$ | To prove (strengthens existing Thm A) |
| 3 | **2-Coskeletal Obligations** | $N(\mathcal{O})$ is 2-coskeletal | To prove |
| 4 | **Spectral Degeneration** | The obligation spectral sequence satisfies $E_2 = E_\infty$ | To prove (follows from #3) |
| 5 | **Characterization** | $d = 2$ iff the foundation supports adjunctions and ∞-groupoid coherence collapses at dimension 2 | To prove (combines #1 + #3) |

---

## Dependencies

```
A1 (Adjunction table)
  └─→ A2 (Triangle depth proof)  ─→ A3 (d=1 stagnation)
                                    ↘
                                     → Phase 5 (Paper)
B1 (Obligation category)            ↗
  └─→ B2 (2-Coskeletal nerve) ─→ C3 (Spectral degeneration)
                                    ↗
C1 (Filtration)                    /
  └─→ C2 (Spectral sequence) ─→ ─┘
```

Phases A and B/C are largely independent and can be developed in parallel. The Agda formalization (Phase 2) depends on Phase 1 being settled. The engine experiments (Phase 3) are independent of the Agda work and can proceed in parallel.

---

## Risks and Mitigations

1. **Risk: The adjunction argument might not be tight enough.** Some type formers (e.g., Universe, W-types) don't have clean adjunctions in the classical sense.
   - **Mitigation:** Focus on the well-understood cases ($\Pi$, $\Sigma$, Id, HITs) first. For Universe/W-types, the argument may need to be about *local* adjunctions or *fibered* adjunctions.

2. **Risk: 2-coskeletality might fail for modal types (steps 10–14).** Modal operators have a different obligation structure (they're axioms, not definitions).
   - **Mitigation:** Prove 2-coskeletality for steps 1–9 first (all constructive). For steps 10–14, the argument may need an additional axiom about modal obligation structure. This is an honest gap to acknowledge.

3. **Risk: The spectral sequence approach requires heavy algebraic topology machinery.**
   - **Mitigation:** Present it as a remark/connection, not as the primary proof. The adjunction argument (Strategy A) is self-contained and more accessible. The spectral sequence gives the framework credibility with algebraic topologists.

4. **Risk: The Agda formalization of adjunctions in HoTT is substantial.**
   - **Mitigation:** Use the existing cubical library's adjunction infrastructure. Focus on the obligation depth aspect, not reproving the full adjunction theory.

---

## The One-Sentence Answer

If the plan succeeds, the answer to "why $d = 2$?" is:

> **$d = 2$ because type formers are adjunctions, adjunctions require two levels of data (unit + counit) to verify coherence (triangle identities), and ∞-groupoid coherence ensures no further levels are needed.**
