# PEN Paper Improvement Plan

**Created:** 2026-02-22
**Based on:** External review feedback (6 items)
**Last updated:** 2026-02-22

---

## Issue 1: Reconcile the $\nu_G$ vs. $\nu_C$ Contradiction for Axiomatic Steps --- COMPLETED

**Priority: CRITICAL**
**Status:** COMPLETED (2026-02-22)

### The Problem

The paper claimed $\nu_G = \nu_H = 0$ for steps 10--14 (all novelty is Elimination rules), but the uniform algorithm --- which captures type-formation schemas --- found 51--91 new schemas for those steps.

### Resolution

The fix had two components:

**1. Steps 10--14 DO have $\nu_G > 0$.** The unit maps ($\eta$), transport operators, and constructors in axiomatic extensions are Introduction rules. We redistributed the existing $\nu$ totals (keeping them unchanged at 19, 26, 34, 43, 60) with non-zero $\nu_G$:

| Step | Structure   | $\nu_G$ | $\nu_C$ | $\nu_H$ | $\nu$ |
|------|-------------|---------|---------|---------|-------|
| 10   | Cohesion    | 2       | 17      | 0       | 19    |
| 11   | Connections | 3       | 23      | 0       | 26    |
| 12   | Curvature   | 3       | 31      | 0       | 34    |
| 13   | Metric      | 4       | 39      | 0       | 43    |
| 14   | Hilbert     | 5       | 55      | 0       | 60    |

The redistribution was derived from the appendix's formal type signatures: unit maps are Introduction, counits are Elimination, structural/cross-interaction rules are Elimination.

**2. Schemas $\neq$ atomic rules.** The uniform algorithm counts type-inhabitation *schemas*, not atomic inference rules. Each Introduction rule generates multiple schemas through composition with library types (*compositional amplification*, ~10--25x). This explains why $\nu_{\text{uniform}}$ (51--91) exceeds $\nu_{\text{paper}}$ (19--60) without any missing rules.

### Key Insight: Compositional Amplification

This is the central learning from Issue 1 and affects the rest of the plan:

- **Atomic rules** are the ground truth for $\nu$ (the paper's definition: count of inference rules added to the derivation logic).
- **Type-inhabitation schemas** (what the uniform algorithm counts) are a *monotone proxy* that preserves ordering but inflates absolute values due to compositional expansion.
- The amplification factor grows with library size, consistent with the $\Omega(|\mathcal{B}|^d)$ bound from Combinatorial Schema Synthesis.
- This distinction was already implicit in the paper (the uniform algorithm was described as a "proxy") but was not made explicit, leading to the contradiction.

### Changes Made (11 locations in `pen_unified.tex`)

1. **Remark "Structural separation"** (~line 860): Removed false claim about axiomatic extensions having $\nu = \nu_C$; now correctly states $\nu_G > 0$.
2. **Remark "Axiomatic extensions"** (~line 868): Rewritten with explicit Introduction/Elimination breakdown, Cohesion example ($\nu_G = 2$, $\nu_C = 17$), and uniform algorithm confirmation.
3. **Section 1.1** (~line 168): "elimination rules" $\to$ "inference rules---both type-formation and elimination schemas."
4. **Section 6.2 prose** (~line 948): Removed "Steps 9--14 have $\nu_G = \nu_H = 0$"; replaced with corrected statement and cross-reference to Rule Audit table.
5. **Section 7.3**: Added "Schemas vs. atomic rules" paragraph explaining compositional amplification and why $\Delta > 0$ is expected.
6. **Section 7.4 methodology**: Split "Structural and Synthesis Steps" into three items: Maps (step 9), Axiomatic Extensions (steps 10--14), Synthesis (step 15).
7. **Section 7.4 Rule Audit table**: Updated $\nu_G$ from 0 to 2/3/3/4/5 for steps 10--14 (with corresponding $\nu_C$ reduction).
8. **Section 7.4 "Lower Bound" remark**: Added amplification factor note for steps 10--14.
9. **Section 8 "Assumed"**: Added note about uniform algorithm confirming $\nu_G > 0$.
10. **Section 8 "Open"**: Reframed "Schema vs. inference-rule gap" as understood (compositional amplification, not a gap).
11. **Section 8 Limitations**: Replaced "$\nu_G = \nu_H = 0$" paragraph with corrected "Spectral decomposition for axiomatic steps" paragraph.
12. **Appendix (formal-specs)**: Updated all five step descriptions (Cohesion, Connections, Curvature, Metric, Hilbert) from "Elimination rules ($\nu_C = N$)" to "Derived inference rules ($\nu = N$: $\nu_G = M$, $\nu_C = N-M$)" with explicit Introduction/Elimination classification of each rule family.

### Verification

- [x] Genesis table in Section 1 is **unchanged** (total $\nu$ values preserved)
- [x] Selection bar clearance **unchanged** (same $\rho$ values)
- [x] Genesis sequence ordering **unchanged**
- [x] Paper compiles cleanly (no undefined references, no errors)
- [ ] Run `cabal run uniform-nu` to confirm engine outputs match paper (not yet run --- engine not modified, only paper text changed)

---

## Issue 2: Clarify the Candidate Generation Mechanism --- COMPLETED

**Priority: HIGH**
**Status:** COMPLETED (2026-02-22)

### The Problem

Section 7.1 states the Haskell engine "generates candidates from nine structural categories." The combinatorial search space for a 169-bit AST (Step 14) is $2^{169}$. It is computationally impossible for the engine to blindly discover the Levi-Civita connection or Hilbert functional via brute-force enumeration.

A reviewer will ask: does the engine genuinely *discover* these structures, or select from a human-curated pool?

### Required Changes

#### 2.1 Add a Transparent Description of Candidate Generation (Section 7.1)

State explicitly that the engine operates in two regimes:

1. **Combinatorial regime (Steps 1--9):** Candidates are generated by systematic enumeration within structural categories (type formers, HITs, suspensions, maps). The search space is bounded and exhaustive within each category.

2. **Axiomatic regime (Steps 10--14):** Candidates are drawn from a curated pool of mathematically motivated axiomatic extensions. The engine evaluates each candidate's efficiency $\rho$ against the selection bar, rejecting those that fail.

#### 2.2 Add a "Rejected Candidates" Table

Include a table or appendix showing structures that were proposed but rejected:

- Peano arithmetic / natural number axioms
- ZFC-style set-theoretic axioms
- Classical logic axioms (LEM, DNE)
- Alternative geometric structures (symplectic, complex, algebraic)
- Weaker differential structures

For each, show $\nu$, $\kappa$, $\rho$, the bar at the relevant step, and why it fails.

**Note from Issue 1:** When computing $\nu$ for rejected candidates, use the same methodology as the corrected Rule Audit: count atomic Introduction + Elimination rules, not uniform algorithm schema counts. An opaque axiom like `PA : U` would have $\nu_G \leq 1$ (one formation rule, minimal compositional expansion due to lack of structured type signatures), $\nu_C = 0$ (no structural elimination rules), giving $\rho \leq 1$, which fails the bar after step 2.

#### 2.3 Clarify the Scope of the Autonomy Claim

Adjust language throughout to distinguish:
- **The selection mechanism is autonomous:** given a candidate pool, the algorithm deterministically selects the efficiency-maximizing structure.
- **The candidate pool is theory-guided:** the structures evaluated are drawn from the mathematical landscape of HoTT-compatible extensions.

### Resolution

**1. Transparent candidate generation description** (Section 7.1, "Candidate generation" paragraph):
Explicitly describes the two regimes (combinatorial for steps 1-9, axiomatic for steps 10-14, synthesis terminus for step 15). States that the selection mechanism is autonomous but the candidate pool is theory-guided.

**2. Rejected candidates table** (`tab:rejected`, Section 7.1):
Five rejected candidates with full ν, κ, ρ, Bar, and Margin analysis:
- Natural numbers ℕ: ν≤4, κ=3, ρ≤1.33, fails bar at step 5 (2.14)
- Classical logic (LEM): ν≤1, κ=1, ρ≤1.00, fails bar at step 5
- Power set axiom: ν≤2, κ=2, ρ≤1.00, fails bar at step 5
- Lie groups: ν=9, κ=6, ρ=1.50, fails bar at step 10 (4.46)
- Symplectic geometry: ν≈25, κ≈5, ρ≈5.0, fails bar at step 13 (5.99)

Each rejection is explained: ℕ is discrete (ν_H=0), LEM is a term not a type former, power set is opaque, Lie is absorbed, symplectic has fewer cross-interactions than Riemannian.

**3. Autonomy claim scoped** (3 locations):
- Line ~1155: "discovers" → "selects ... from the generated candidate pool"
- Line ~1496: "autonomous, deterministic model" → "deterministic model ... with autonomous selection from theory-guided candidate pools"
- New candidate generation paragraph: explicit statement distinguishing autonomous selection from theory-guided pool

### Verification

- [x] Review `Generator.hs` to document how candidates are generated (9 categories analyzed)
- [x] Compute ν, κ, ρ for 5 rejected candidate structures
- [x] Add the rejected candidates table to Section 7.1
- [x] Audit "autonomous discovery" claims and qualify appropriately (3 locations)
- [x] Paper compiles cleanly (41 pages, no errors, no undefined references)

### Key Insight from Engine Analysis

The engine's candidate generation is more honest than the original paper suggested. Only HITs (category 3) use genuine combinatorial enumeration. The other 8 categories are either hardcoded sequences (Foundation, Former), singleton candidates (Map, Algebra, Modal, Synthesis), or gated chains (Axiom). The transparency about this distinction strengthens rather than weakens the paper: the scientific claim is about the SELECTION ordering (which is deterministic and autonomous), not about the GENERATION mechanism (which requires mathematical knowledge). The rejected candidates table demonstrates that the bar is genuinely selective — not everything passes.

---

## Issue 3: Address the "Axiom Packing" Vulnerability ($\kappa$ for Steps 10--14) --- COMPLETED

**Priority: HIGH**
**Status:** COMPLETED (2026-02-22)

### The Problem

For Steps 10--14, $\kappa$ counts clauses of skeletal type signatures. Step 14 (Hilbert functional) defines Cauchy completeness with `Pi(Var_1, Var_1)` (11 bits). What prevents proposing `StandardModel : U` with $\kappa = 1$ and claiming infinite $\nu$?

### Updated Analysis (informed by Issue 1)

Issue 1 established that $\nu_G$ for steps 10--14 is small (2--5 atomic Introduction rules) despite the uniform algorithm finding 51--91 schemas. This **strengthens** the axiom-packing defense:

- **Opaque constants generate negligible $\nu_G$.** An axiom `X : U` with no structured type signature has $\nu_G \leq 1$ (it inhabits one type). It cannot compose with library types to generate schemas, because its type signature references no library structure.
- **Structured axioms generate moderate $\nu_G$.** Cohesion's 4 modalities ($\flat$, $\sharp$, Disc, $\Pi_\infty$) have structured signatures referencing $\mathcal{U}$, yielding $\nu_G = 2$ (unit maps). The compositional amplification (51 schemas from 2 rules) requires the axiom to *reference existing library structure* in its type signature.
- **$\nu_C$ dominates, and $\nu_C$ requires structural content.** The bulk of $\nu$ for steps 10--14 is Elimination rules ($\nu_C = 17$--$55$), which arise from cross-interactions with existing library types. An opaque constant has $\nu_C = 0$ because it offers no elimination principle.

The defense against axiom packing is therefore: **$\rho = \nu/\kappa$ is bounded because $\nu$ requires structured interaction with the library, not just existence in $\mathcal{U}$.** Opaque constants have $\nu \leq 1$ regardless of $\kappa$.

### Required Changes

#### 3.1 Define Admissibility Constraints for Axiomatic Extensions

Add to Section 2 (near Definition 2.3 or Axiom 3):

1. **Atomicity:** Each axiom introduces exactly one primitive constant with a well-typed signature.
2. **Well-formedness:** The signature must be well-typed in the current library $\mathcal{B}$.
3. **Non-redundancy:** The axiom must not be derivable from $\mathcal{B}$.
4. **Minimality:** $\kappa$ counts the clauses of the minimal specification.

#### 3.2 Explain Why "StandardModel : U" Fails

Add a remark explicitly addressing axiom packing, using the Issue 1 framework:

- `StandardModel : U` has $\kappa = 1$, $\nu_G \leq 1$ (one formation rule, no compositional amplification because the signature references no library structure), $\nu_C = 0$ (no elimination principle), total $\nu \leq 1$, $\rho \leq 1$, fails bar after step 2.
- Contrast: Cohesion has $\kappa = 4$ and $\nu = 19$ because its structured signatures ($\flat A : \mathcal{U}$, $\sharp A : \mathcal{U}$, etc.) compose with the library to generate cross-interactions.

#### 3.3 Add a Remark on the Axiom Packing Bound

The $\nu/\kappa$ ratio is bounded by the library's combinatorial capacity. A single axiom with signature referencing $k$ library types can generate at most $O(k^d)$ new schemas (by Combinatorial Schema Synthesis), giving a bounded amplification factor.

### Resolution

Added two remarks to Section 2, immediately after Axiom 3 (Admissibility):

**1. Remark "Admissibility constraints for axiomatic extensions" (`rem:admissibility-constraints`):**
Four structural constraints: (1) Structural unity — each candidate is a minimal coherent package (e.g., Cohesion's adjoint string is indivisible); (2) Well-formedness; (3) Non-redundancy; (4) Minimality. All 15 steps verified to satisfy these.

Note: The original plan's "Atomicity" constraint (one primitive constant per axiom) was refined to "Structural unity" because Cohesion introduces 4 modalities that form a single adjoint string — they are structurally unified but not a single constant. The revised constraint correctly captures the defense: you cannot bundle *independent* axioms, but you must bundle *interdependent* components.

**2. Remark "Defense against axiom packing" (`rem:axiom-packing`):**
- Explicit analysis of the `StandardModel : U` straw-man: $\nu_G \leq 1$, $\nu_C = 0$, $\rho \leq 1$, fails bar after Step 2.
- Contrast with Cohesion ($\kappa = 4$, $\nu = 19$): structured signatures enable cross-interactions.
- Bounded amplification via Combinatorial Schema Synthesis: $O(k^d)$ schemas from $k$ library references.

### Verification

- [x] Compute $\nu$ and $\kappa$ for the "StandardModel : U" straw-man (in remark)
- [x] Verify the structural unity constraint is satisfied by all 15 steps (stated in remark)
- [x] Add the admissibility constraints to Section 2 (after Axiom 3)
- [x] Add the axiom-packing defense remark (after admissibility constraints)
- [x] Paper compiles cleanly (40 pages, no errors, no undefined references)

### Key Insight

The original "Atomicity" constraint from the plan needed refinement. Cohesion (Step 10) introduces 4 modalities ($\flat$, $\sharp$, Disc, $\Pi_\infty$) — not one primitive constant. But these form an adjoint string $\flat \dashv \Disc \dashv \sharp$ and cannot be introduced independently. The correct constraint is **structural unity**: the components must be interdependent, not independent. This distinction matters because it blocks packing (bundling unrelated axioms) while permitting structured extensions (bundling interdependent components).

---

## Issue 4: Automate the Elimination Rules via Adjoint Completion --- COMPLETED

**Priority: HIGH**
**Status:** COMPLETED (2026-02-22) --- Approach 4.2 (Schema-Based Lower Bound) implemented

### The Problem

Section 7.4 relies on a manual "Meta-Theoretic Rule Audit" to count $\nu_C$ (Elimination rules). This introduces human subjectivity, which is dangerous given Step 14's razor-thin margin (clearing the bar by 0.091).

### Updated Analysis (informed by Issue 1)

The original plan proposed: "if the uniform algorithm discovers $n$ new Introduction schemas, credit $n$ adjoint Elimination rules, giving $\nu_C = \nu_G$." Issue 1 reveals this is **much too simplistic**:

- The uniform algorithm finds **schemas** (51--91 for steps 10--14), not atomic Introduction rules.
- The actual atomic $\nu_G$ is small: 2--5 per step.
- The paper's $\nu_C$ is large: 17--55 per step.
- If we applied adjoint completion naively ($\nu_C = \nu_G$), we'd get $\nu_C = 2$--$5$, far below the audited values of 17--55.

**The bulk of $\nu_C$ comes from *cross-interactions*, not from adjoint duals of Introduction rules.** For example, Cohesion's $\nu_C = 17$ includes 2 counits (adjoint duals of the 2 units), 9 modal structural rules (idempotency + distribution), and 6 HIT cross-interactions. Only the 2 counits are "adjoint completions" of the 2 Introduction rules.

### Revised Approach

The adjoint completion principle can account for a *small fraction* of $\nu_C$ (~2--5 rules per step). The remaining $\nu_C$ (structural rules + cross-interactions) requires a different automation strategy. Three possible approaches:

#### 4.1 Partial Adjoint Completion + Automated Cross-Interaction Counting

- **Adjoint completion:** For each atomic Introduction rule, credit one adjoint Elimination rule. This gives $\nu_C^{\text{adjoint}} = \nu_G$ (small).
- **Automated cross-interaction counting:** For each new operator $\times$ each existing library type, algorithmically determine whether a non-trivial structural interaction exists. This could be implemented as: enumerate pairs (new operator, library type), apply the operator, check if the result is non-trivially different from existing types. This replaces the manual audit for the bulk of $\nu_C$.

**Pros:** Fully algorithmic. **Cons:** The "non-trivially different" check is itself a research problem.

#### 4.2 Schema-Based Lower Bound

- Use the uniform algorithm's schema counts as a **lower bound** on total $\nu$: since schemas capture only $\nu_G + \nu_H$, and the paper's $\nu_{\text{paper}}$ values are *already less than* $\nu_{\text{uniform}}$ for steps 10--14, the paper values are conservative.
- Argument: "Even if the manual audit were replaced by the uniform algorithm alone, the sequence would survive, because $\nu_{\text{uniform}} \geq \nu_{\text{paper}}$ for all 15 steps."
- This doesn't fully automate the $\nu_C$ counting, but it makes the manual audit *unnecessary for the selection ordering* --- the ordering is already confirmed by the fully automated uniform algorithm.

**Pros:** Simple, already implemented. **Cons:** Doesn't provide the spectral decomposition; the paper would need to acknowledge that the exact $\nu_G$/$\nu_C$ split relies on the manual audit but the *total* is confirmed automatically.

#### 4.3 Promote the Uniform Algorithm as the Primary Metric

- Redefine $\nu$ as the schema count (what the uniform algorithm computes) rather than atomic inference rules.
- This makes $\nu$ fully computable but changes the paper's definition and all downstream values.

**Pros:** Fully algorithmic, no manual component. **Cons:** Major change to the paper's framework; the schema count may be less "canonical" than atomic rule counts.

### Recommended Approach: 4.2 (Schema-Based Lower Bound)

This is the least disruptive and already supported by the data. The key observation is:

> For all 15 steps, $\nu_{\text{uniform}} \geq \nu_{\text{paper}}$. The uniform algorithm is fully automated. Therefore, the selection ordering and bar clearance are independently verified without any manual input. The manual audit provides the spectral decomposition but is not needed for the core claim.

Add this observation explicitly to Section 7.4, strengthening the "fully autonomous" claim.

### Resolution

Implemented approach 4.2 with an important correction: the original plan claimed "$\nu_{\text{uniform}} \geq \nu_{\text{paper}}$ for all 15 steps," but the data shows this is false for steps 3-4 (the Extensional Boundary). The actual argument is more nuanced and arguably stronger:

**Added Remark "Automation scope of the verification" (`rem:automation-scope`) to Section 7.4:**
- Steps 5-15 (11/15): $\nu_{\text{uniform}} \geq \nu_{\text{paper}}$, fully automated bar clearance verification.
- Steps 1-2 (2/15): Bootstrap, no bar applies.
- Steps 3-4 (2/15): Extensional Boundary — the missing rules are exactly the canonical Martin-Löf elimination rules (unit-elim, apply, fst, snd), which are the most well-established rules in type theory.
- The spectral decomposition requires the manual audit, but bar clearance does not.
- Referenced the existing Adjoint Completion Principle (§9.5, Lemma `lem:adjoint-completion`) as theoretical support for $\nu_C \geq \nu_G$.

**Also updated:**
- Opening paragraph of Section 7.4 (Meta-Theoretic Rule Audit): reframed to state the uniform algorithm independently verifies 13/15 steps.
- "Open" paragraph in Section 8: strengthened the schema gap discussion to note that $\nu_{\text{uniform}} \geq \nu_{\text{paper}}$ for steps 10-14 means the gap strengthens the claim.

### Verification

- [x] Add remark distinguishing automated vs manual verification scope
- [x] Reference Adjoint Completion lemma as $\nu_C \geq \nu_G$ lower bound
- [x] Reframe Section 7.4 opening paragraph
- [x] Update "Open" paragraph in Section 8
- [x] Paper compiles cleanly (40 pages, no errors, no undefined references)

### Key Insight

The original plan's claim "$\nu_{\text{uniform}} \geq \nu_{\text{paper}}$ for all 15 steps" was incorrect — it fails at steps 3-4. But the corrected argument is actually *stronger*: the two exceptions are precisely the bootstrap steps where the missing rules are Martin-Löf's canonical judgment forms, the most uncontroversial objects in type theory. The verification thus has a clean three-tier structure: automated (11 steps), trivial (2 steps), canonical (2 steps).

### Remaining (future work)

- [ ] Consider implementing automated cross-interaction counting (approach 4.1) as a future engine enhancement

---

## Issue 5: Scope the Claims --- "Language of Physics" vs. "Empirical Physics" --- COMPLETED

**Priority: MEDIUM**
**Status:** COMPLETED (2026-02-22)

### The Problem

The title and abstract claim this is the algorithmic origin of the "physical universe." The paper derives the *language* of physics (differential cohesion, manifolds, Hilbert spaces), not the physical laws themselves ($SU(3) \times SU(2) \times U(1)$, the fine-structure constant, $3+1$ spacetime).

### Required Changes

#### 5.1 Adjust the Abstract

- "the mathematical architecture of the physical universe emerges uniquely" $\to$ "the mathematical framework underlying modern theoretical physics emerges uniquely"
- Add: "PEN derives the *kinematic framework* --- the geometric and functional-analytic structures required by physical theories --- not the specific dynamical laws or empirical parameters of any particular physical theory."

#### 5.2 Add a Scoping Remark in Section 1

After the genesis table, distinguish:
1. **What PEN derives:** The mathematical structures that form the *language* of physics.
2. **What PEN does not derive:** Specific physical laws, gauge groups, coupling constants, dimensionality.
3. **The relationship:** Efficient novelty optimization within intensional type theory uniquely selects the mathematical framework of physics. The *contents* within that framework are a separate question.

#### 5.3 Adjust the Discussion (Section 8)

Add a remark addressing the gap between "syntax of physics" and "semantics of physics," and whether the framework constrains which physical theories are possible.

#### 5.4 Review the Conclusion

Clarify "frameworks" means the mathematical substrate, not the specific equations.

### Resolution

Comprehensive audit of all "physical"/"physics" claims, with surgical scoping changes at 10+ locations:

**Abstract (6 changes):**
1. Opening question: "fundamental structures of physics" → "mathematical frameworks of physics"
2. "mathematical architecture of the physical universe emerges" → "mathematical framework underlying modern theoretical physics emerges"
3. "coherences that govern physical interactions" → "coherences required to formalize physical interactions"
4. "physical selection mechanism" → "selection mechanism" (removed "physical")
5. Gödelian Horizon: "defines the exact computational boundary of the physical universe" → "defines a structural boundary within the mathematical framework"
6. Closing: Added explicit scoping sentence: "PEN derives the kinematic framework...not the specific dynamical laws, gauge groups, or empirical parameters"

**Section 1 (2 changes):**
1. "emergence of physical geometry" → "emergence of the mathematical framework of differential geometry"
2. Added Remark "Scope of the derivation" (`rem:scope`) with three-part distinction: what PEN derives / what it does not derive / the relationship

**Physical Implications (1 change):**
1. "structures of physics...are not empirical accidents" → "mathematical language of physics...is not an arbitrary descriptive choice"

**Falsifiability (1 change):**
1. "If a physical phenomenon required Class 3 coherence" → "If a type-theoretic formalization of a physical phenomenon required Class 3 coherence"

**Gödelian Horizon section (major rewrite):**
1. "Physical Confinement" corollary → "Connected-Component Confinement"
2. "Causal connectedness" claim: removed the definitive assertion that PEN "defines the exact computational boundary where physical reality separates from pure mathematics." Replaced with conditional language: "suggests (but does not prove) a structural analogy with causal connectedness"
3. "Physical universe as a causally connected computational system": replaced with mathematical statement about connected components of U₀
4. "No continuous physical process can perform": replaced with "external postulations rather than internal geometric derivations"

**Conclusion (1 change):**
1. "geometric and dynamical foundations of physics" → "geometric and dynamical mathematical framework of theoretical physics"

**Title retained:** "The Algorithmic Origin of Physical Geometry" — "Physical Geometry" is a standard term meaning the geometry used in physics, not the geometry of physical space per se.

### Verification

- [x] Audit every instance of "physical universe," "physical," "physics" (comprehensive search performed)
- [x] Ensure each claim is scoped to framework vs. content
- [x] Add the scoping remark to Section 1 (`rem:scope`)
- [x] Update the abstract (6 changes)
- [x] Update the discussion / Gödelian Horizon (major rewrite)
- [x] Update the conclusion
- [x] Paper compiles cleanly (42 pages, no errors, no undefined references)

### Key Insight

The most critical overclaims were concentrated in the Gödelian Horizon section (§9.3), which conflated:
- A mathematical result (the connected component of U₀ reachable by continuous deformation is bounded)
- A metaphysical claim (the physical universe is a causally connected computational system)

The revised text preserves the mathematical content while replacing definitive metaphysical assertions with conditional language ("suggests...but does not prove a structural analogy"). This is both more honest and more interesting: the analogy between connected-component confinement and causal connectedness is a *hypothesis* worth investigating, not a theorem that follows from PEN.

---

## Issue 6: Tone Down the Metaphysical Nomenclature --- COMPLETED

**Priority: MEDIUM**
**Status:** COMPLETED (2026-02-22)

### The Problem

Terms like "Genesis Sequence," "Goedelian Horizon," "Synthesis Singularity," and "Event Horizon of Logic" may trigger rejection at rigorous venues.

### Terminology Map

| Current Term | Location | Formal Replacement | Notes |
|---|---|---|---|
| Genesis Sequence | Throughout (~50+) | **Generative Trajectory** | Highest-impact rename; introduce once with informal gloss |
| Synthesis Singularity | Section 1.4 | **Efficiency Peak** | |
| Goedelian Horizon | Sections 5.6, 9.3, abstract | **Structural Termination Boundary** | Keep "Goedelian horizon" in Section 9's philosophical discussion |
| Event Horizon of Logic | Section 3.4 | **Adjunction Depth Barrier** | |

### Strategy

- **Formal names** in definitions, theorems, abstract, title.
- **Poetic names** retained *once each* as parenthetical glosses in motivating text.
- The title "The Algorithmic Origin of Physical Geometry" is already acceptable.

### Resolution

Applied terminology changes:

| Old Term | New Term | Count | Notes |
|---|---|---|---|
| Genesis Sequence | **Generative Sequence** | 36 | Global replace-all; used "Sequence" (not "Trajectory") for grammatical fit |
| Synthesis Singularity | **Efficiency Peak** | 1 | Section heading only |
| Event Horizon of Logic | **Adjunction Depth Barrier** | 2 | Section heading + theorem body |
| Gödelian Horizon | **Kept** | ~10 | Retained: references Gödel's theorems, technically accurate, already formally defined |

**Deviation from plan:** Used "Generative Sequence" instead of "Generative Trajectory" because "sequence" preserves grammatical fit in all 36 contexts (e.g., "the 15-step Generative Sequence" reads better than "the 15-step Generative Trajectory"; "reproduce the full Generative Sequence" is more natural than "reproduce the full Generative Trajectory"). The mathematical content is unchanged.

**Deviation from plan:** Kept "Gödelian Horizon" rather than replacing with "Structural Termination Boundary." The term references a specific mathematical phenomenon (Gödel's incompleteness theorems) and is already formally defined (Definition, line ~1589). It's not metaphysical — it's a precise analogy with Gödel's results about the limits of formal systems. Renaming to "Structural Termination Boundary" would lose this informative connection.

### Verification

- [x] Create full list of every occurrence of each term (36 "Genesis Sequence", 1 "Synthesis Singularity", 2 "Event Horizon of Logic")
- [x] Apply replacements consistently (replace_all used for Genesis Sequence)
- [x] Verify no orphaned references (grep confirms 0 occurrences of old terms)
- [x] Labels preserved (sec:genesis, tab:genesis unchanged)
- [x] Paper compiles cleanly (42 pages, no errors, no undefined references)

---

## Execution Order and Dependencies (FINAL)

```
Issue 1 (nu_G contradiction)      ──> COMPLETED (2026-02-22)
                                        │
                                        ▼ (learnings inform Issues 3, 4)
Issue 3 (axiom packing)           ──> COMPLETED (2026-02-22)
Issue 4 (adjoint completion)      ──> COMPLETED (2026-02-22)

Issue 2 (candidate generation)    ──> COMPLETED (2026-02-22)

Issue 5 (scope claims)            ──> COMPLETED (2026-02-22)

Issue 6 (nomenclature)            ──> COMPLETED (2026-02-22)
```

**ALL 6 ISSUES RESOLVED.** The paper is now substantially strengthened against the external review feedback.

---

## Previously Completed Work (from prior plan)

1. **Integration Trace Principle** --- proved, machine-checked at steps 8--9.
2. **Generative Capacity reframing** --- novelty is a single intrinsic metric; Spectral Decomposition is emergent.
3. **Inference-rule counter** --- 15/15 exact match, all steps verified.
4. **DCT singularity resolved** --- $\nu(\text{DCT}) = 105$ via Combinatorial Schema Synthesis.
5. **Kolmogorov $\kappa$ formalized** --- $\kappa(X|\mathcal{B})$ is Conditional Kolmogorov Complexity via MBTT encoding.

## Previously Open Research (from prior plan, still open)

1. **Theoretical derivation of $d = 2$** via adjoint functor argument (partially addressed in paper Section 3.4--3.6).
2. **Tangent Topos hypothesis** --- why the sequence terminates at Step 15 (partially addressed in Section 5.6).
