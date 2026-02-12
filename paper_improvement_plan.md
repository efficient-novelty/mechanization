# PEN Paper Improvement Plan

## Completed

### Priority 1: The Saturation Assumption — COMPLETED

**Status:** Resolved. The Integration Trace Principle has been upgraded from a lemma (proof sketch) to a theorem with a full proof in `pen_unified.tex`, supported by:

- **Linearity of Elimination:** rec_X distributes over type formers, so behavior is determined by an atomic basis (one clause per cell).
- **Context Extension Principle:** the active interface forms a context telescope; sealing requires exactly one clause per entry (constructive completeness + confluence).
- **Verification scope:** steps 3–9, uniform across HITs (steps 3–8) and maps (step 9, the Hopf fibration). The Hopf fibration crosses the HIT/map phase boundary, confirming the mechanism is not specific to HITs.
- **Machine-checked abstraction barrier** at steps 8–9 in Cubical Agda (`AbstractionBarrier.agda`, `AbstractionBarrier9.agda`).
- **Honest scope acknowledged:** steps 10–15 (modal operators, axiomatic extensions) are extrapolated; obligation structure for axioms differs from HITs and maps, and verification remains open.

**Outcome:** Exact match (first outcome). |S(L_k)| = Delta_k verified through step 9. The assumption is now a consequence of Linearity of Elimination + Context Extension Principle, not an independent postulate.

---

### Priority 2: Eliminate Circularity in nu — COMPLETED

**Status:** Resolved via the **Generative Capacity** reframing. The novelty metric is no longer defined as a sum of three independently fitted components ($\nu_G + \nu_H + \nu_C$). Instead:

- **Single intrinsic metric:** Novelty is defined as the marginal volume of the library's inference-rule algebra: $\nu(X \mid \mathcal{B}) := |\mathcal{L}(\mathcal{B} \cup \{X\})| - |\mathcal{L}(\mathcal{B})|$.
- **Spectral Decomposition Theorem:** The decomposition into Grammar (Introduction rules), Capability (Elimination rules), and Homotopy (Computation rules) is an emergent property — a structural partition of inference rules — not a parameter choice.
- **Corrected attribution:** Witness ($\nu_G = 1, \nu_C = 1$) and $\Pi/\Sigma$ ($\nu_G = 2, \nu_C = 3$) now naturally split across axes, explaining why the uniform algorithm's type-inhabitation approach missed their Elimination-rule novelty.
- **Equal-weight property reframed:** The 12.7% island centered at $(1,1,1)$ is now a discovery ("the Genesis Sequence selects structures where syntax, topology, and logic contribute equally to derivation power"), not a parameter-tuning problem.

**Outcome:** The paper no longer presents novelty as a "sum of parts" requiring weight justification. The Generative Capacity is a single canonical metric; the three-way decomposition is a theorem about its spectral structure.

**Remaining work from Priority 2:** ✅ ALL COMPLETE (2026-02-12)
- ✅ Inference-rule counter (`InferenceNu.hs`) directly counts atomic rules: 15/15 exact match.
- ✅ Witness fixed: ν = 1 → 2 (Elimination rule counted).
- ✅ Π/Σ fixed: ν = 2 → 5 (three Elimination rules counted).
- ✅ Steps 10–14 overcounting resolved (atomic rules only).
- ✅ DCT resolved: Lattice Tensor Product (14 × 11 − 4 = 150) replaced with Combinatorial Schema Synthesis (103 schemas + 2 formers = 105). The Kuratowski application was incorrect (cohesive modalities are all idempotent, not closure-complement) and the temporal monoid is infinite (○ is non-idempotent). The uniform algorithm's ν = 105 is the correct, computed value.
- ✅ Paper updated: new §7.4 "Inference-Rule Verification", §5.4 rewritten as "Combinatorial Schema Synthesis", genesis table updated, §8 Discussion updated.

---

## Next Research Steps

The paper is now a coherent mathematical framework with an intrinsic metric (Generative Capacity), a structural decomposition (Spectral Theorem), verified cost dynamics (Fibonacci recurrence), and a computed DCT novelty ($\nu = 105$). To transform it from a generative simulation into a foundational theory, the remaining work is to harden $\kappa$ and $d = 2$.

### Step 1: DCT Singularity — RESOLVED

**Status:** Resolved. The Lattice Tensor Product theorem ($14 \times 11 - 4 = 150$) was based on two mathematical errors:
1. **Spatial lattice:** Kuratowski's theorem requires an involution (closure) and an idempotent (complement), but cohesive modalities ($\flat, \sharp, \Pi_{\text{coh}}, \text{Disc}$) are all idempotent with full-faithfulness collapse rules, yielding ~4–6 elements, not 14.
2. **Temporal lattice:** $\bigcirc$ (next) is non-idempotent ($\bigcirc^n X \not\simeq \bigcirc X$ for $n > 1$), making the monoid $\langle \bigcirc, \Diamond \rangle$ infinite, not 11.

**Resolution:** The uniform algorithm's exhaustive type-inhabitation enumeration at depth 2 gives $\nu(\text{DCT}) = 105$ (103 non-trivial schemas + 2 new type formers). This clears the bar: $\rho = 105/8 = 13.12 \gg 7.25$. The singularity survives with a clearance factor of 1.8×.

**Paper updated:** §5.4 rewritten as "Combinatorial Schema Synthesis", genesis table updated ($\nu = 105$, $\rho = 13.12$), §8 Discussion updated, Appendix reframed.

---

### Step 2: Theoretical Derivation of $d = 2$

**Priority: HIGH.** We have an empirical proof that $d = 2$ works (Fibonacci) and $d = 1$ stagnates. We need a categorical *reason* why.

**Hypothesis:** $d = 2$ is the minimum window required for Adjoint Functors.

**Reasoning:** Type Theory is built on Adjunctions (Intro $\dashv$ Elim). Defining an adjunction requires data spanning two levels: the Unit $\eta : 1 \to R \circ L$ and Counit $\varepsilon : L \circ R \to 1$.

**Argument:** A system with $d = 1$ (seeing only the previous layer) cannot verify the triangle identities required for an adjunction. Thus, $d = 2$ is the *Event Horizon of Logic* — nothing simpler can support self-consistent operators.

**Concrete approach:**
- Define a filtration on the obligation complex indexed by construction step, and prove that the associated spectral sequence degenerates at $E_2$. This is the precise content of "$d = 2$" in the language algebraic topologists would recognize.
- If provable in Cubical Agda for the test cases ($S^1$, $S^2$, $T^2$, Hopf), that's a rigorous upper bound for those cases and strong evidence for the general claim.
- For the lower bound: exhibit a *family* of structures requiring depth-2 obligations (fiber bundles over $S^n$ with non-trivial clutching functions). If every member requires exactly depth-2, that's compelling. If some require depth-3, the framework is falsified.

---

### Step 3: Formalize Effort ($\kappa$) via Kolmogorov Complexity — COMPLETED

**Status:** Resolved. Construction Effort $\kappa(X \mid \mathcal{B})$ is now defined as the Conditional Kolmogorov Complexity upper bound via Minimum Description Length in a Minimal Binary Type Theory (MBTT).

**Implementation:**
- **Haskell module** `Kolmogorov.hs`: Prefix-free MBTT AST with 20 constructors (App, Lam, Pi, Sigma, Univ, Var, Lib, Id, Refl, Susp, Trunc, PathCon, Flat, Sharp, Disc, Shape, Next, Eventually). Library references use Elias Gamma coding ($O(\log i)$ bits), implementing the conditional aspect $K(X \mid \mathcal{B})$.
- **All 15 Genesis steps** have MBTT specifications. Steps 7 (S²) and 8 (S³) have multiple candidate specifications; the MDL principle selects the suspension definition automatically.
- **S³ ambiguity resolved**: Native HIT = 23 bits, $\Sigma S^2$ = 13 bits. The framework intrinsically selects the shorter program.
- **Scale factor**: MBTT κ averages ~16.8× larger than paper κ (generator counts). PEN dynamics are scale-invariant ($\rho = \nu/\kappa$, $\Omega = \Sigma\nu / \Sigma\kappa$), so the Genesis Sequence is preserved.
- **Divergence theorem strengthened**: Bounded Effort lemma replaced with Logarithmic Effort Growth ($\kappa_n \sim O(\log n)$); efficiency divergence now follows from $\rho_n \sim \Omega(n^c / \log n) \to \infty$.

**Paper updated:** Definition 2.3 rewritten as Algorithmic Construction Effort, Remark on representational ambiguity added, Lemma (Logarithmic Effort Growth) replaces Bounded Effort, Theorem (Divergence of Efficiency) proof strengthened, Discussion open problem resolved.

---

### Step 4: The "End of History" — Tangent Topos Hypothesis

**Priority: MEDIUM.** Why does the sequence stop at Step 15?

**Hypothesis:** The DCT is a Fixed Point.

**Insight:** In Synthetic Differential Geometry, the "Tangent Bundle" $T(X) = X^{\mathbb{D}}$ encodes the internal evolution of a space.

**Research Question:** Is the DCT effectively the Tangent Topos of Cohesion ($T(\text{Cohesion})$)? If so, the "Singularity" represents the moment the universe gains the capacity to simulate itself (internalizing its own "next" step). Adding external structure becomes less efficient than defining it internally.

**Concrete approach:**
- Show that the DCT's temporal modality ($\bigcirc$) is equivalent to the internal hom $X^{\mathbb{D}}$ (infinitesimal step = temporal step).
- If true, the termination is not contingent ("no more candidates clear the bar") but structural ("the system has internalized its own evolution operator, so external extension is redundant").

---

### Step 5: Stress-Test the Coherence Window (ongoing)

**Priority: MEDIUM.** The $d = 2$ claim mixes rigorous argument (upper bound via $\infty$-groupoid coherence) with heuristic reasoning (dimensional-to-temporal correspondence). This is partially addressed by Step 2 above.

**Remaining work:**
- The engine's `--window d` stress test is implemented. $d = 1$ stagnates, $d = 2$ produces 15 structures, $d = 3$ stalls earlier. This is computational evidence but not a proof.
- The Cubical Agda experiments (obligation tracing for $S^1$, $S^2$, $T^2$, Hopf) confirm $d = 2$ for those cases.
- A general proof that the Coherence Window of HoTT is exactly 2 (for all cell presentations, not just tested examples) remains open.
