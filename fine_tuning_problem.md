# Research Plan: Is $\nu_H = m + (\max d_i)^2$ Reverse-Engineered?

## The Criticism

> The metric for topological novelty ($\nu_H$) is defined as
> $\nu_H = m + (\max d_i)^2$. This formula appears highly reverse-engineered
> specifically to ensure that $S^2$ and $S^3$ yield exact values (5 and 10)
> required to clear the razor-thin selection bar and perfectly satisfy the
> "Spectral Decomposition" equal-weight claim. This slightly contradicts the
> abstract's claim of having "no free parameters."

**Severity: High.** This strikes at the credibility of the central empirical
claim (equal-weight Spectral Decomposition). If the $d^2$ scaling is ad hoc,
then the "12.7% island" result is a consequence of formula tuning, not an
emergent property. The "no free parameters" claim (line 108 of
`pen_unified.tex`) would be misleading.

## Current State of the Theory

The spectral decomposition table (Table 2, lines 816-835) shows:

| Step | Structure | $\nu_G$ | $\nu_H$ | $\nu_C$ | $\nu$ |
|------|-----------|---------|---------|---------|-------|
| 5    | $S^1$     | 5       | 2       | 0       | 7     |
| 7    | $S^2$     | 5       | 5       | 0       | 10    |
| 8    | $S^3$     | 5       | 10      | 3       | 18    |

The $\nu_H$ formula gives:
- $S^1$: $m=1, d=1 \Rightarrow 1 + 1^2 = 2$
- $S^2$: $m=1, d=2 \Rightarrow 1 + 2^2 = 5$
- $S^3$: $m=1, d=3 \Rightarrow 1 + 3^2 = 10$

The paper justifies $d^2$ via a loop-space argument (Cluster.hs, lines 85-90):
"$d$ loop spaces $\Omega^j(S^d) \times d$ homotopy levels $= d^2$ pairs."
This is informal and the critic is right to question whether this is post-hoc.

The paper already acknowledges the thin sample (Section 8.2, line 1196-1200):
only $S^1, S^2, S^3$ have nonzero $\nu_H$, and the claim is limited to the
Genesis Sequence. But it does not address whether the formula itself is a
hidden free parameter.

## What Needs to Be Investigated

### Phase 1: Sensitivity to Alternative $\nu_H$ Formulas

**Goal:** Determine which alternative formulas for $\nu_H$ also reproduce the
Genesis Sequence, and quantify how constrained the formula choice actually is.

**Candidate formulas to test** (for a HIT with $m$ path constructors,
max dimension $d$):

| Label | Formula | $S^1$ | $S^2$ | $S^3$ | Rationale |
|-------|---------|-------|-------|-------|-----------|
| A (current) | $m + d^2$ | 2 | 5 | 10 | "Loop space pairs" |
| B | $m + d$ | 2 | 3 | 4 | Linear, simplest |
| C | $m + d(d+1)/2$ | 2 | 4 | 7 | Triangular numbers |
| D | $m + 2^d$ | 3 | 5 | 9 | Exponential (cells in CW-structure) |
| E | $m + \binom{d+1}{2}$ | 2 | 4 | 7 | Same as C |
| F | $m \cdot d^2$ | 1 | 4 | 9 | Multiplicative |
| G | $d^2$ (no $m$) | 1 | 4 | 9 | Pure dimension |
| H | $m + d(d-1) + 1$ | 2 | 4 | 8 | Homotopy groups $\pi_k(S^d)$ for $k \le d$ |
| I | $m(d+1)$ | 2 | 3 | 4 | Linear in both |
| J | $\sum_{k=1}^{d} k$ | 1 | 3 | 6 | Sum of integers |
| K | $m + d^2 + d$ | 3 | 7 | 13 | Oblong numbers |

**Method:**
1. For each formula, substitute the resulting $\nu_H$ values into the total
   $\nu = \nu_G + \nu_H + \nu_C$ for steps 5, 7, 8.
2. Recompute $\rho = \nu / \kappa$ for these steps.
3. Check whether each step still clears its selection bar (from Table 1).
4. Check whether the ordering of candidates is preserved (i.e., the right
   structure is selected at each step).
5. Run the full 2D sweep ($\alpha, \beta$ over $[0.5, 1.5]^2$) for each
   formula variant to measure the island size.

**Key question:** Is $d^2$ the *unique* scaling that works, or does a family
of formulas succeed? If $d^2$ is the only one, that's either evidence of
fine-tuning or evidence of deep structure.

**Deliverable:** A table showing, for each candidate formula, (a) whether the
Genesis Sequence is reproduced, (b) how tight the margins are, (c) the island
size in the 2D sweep.

### Phase 2: Principled Derivation of $\nu_H$

**Goal:** Either derive $d^2$ from first principles or identify it honestly as
a modeling choice.

**Approach 2a: Inference-rule counting.**
The Generative Capacity $\nu$ is defined as the count of *atomic inference
rules* (Intro, Elim, Comp). For $\nu_H$ (Computation rules), we should be
able to *directly count* the independent computation rules contributed by
$S^d$.

For $S^d$ in HoTT, the computation rules are:
- The $\beta$-reduction rule for the point constructor (1 rule)
- The $\beta$-reduction rule for the $d$-cell constructor (1 rule)
- The dependent eliminator's computation on paths (1 rule)

But this gives $\nu_H = 3$ for all spheres, which is too low and
dimension-independent. The issue is what counts as "atomic." The paper
needs to reconcile the formula with the operational semantics of HoTT.

**Specific questions:**
- In cubical type theory, how many independent computation rules does $S^d$
  contribute? Does the cubical structure introduce $d$-dependent terms?
- Does the Kan filling operation for a $d$-cell involve $O(d^2)$ face maps?
- In the HoTT book's presentation, how many clauses does the eliminator for
  $S^d$ require?

**Approach 2b: Homotopy-theoretic argument.**
The current justification is "$d$ loop spaces $\times$ $d$ homotopy levels."
This needs to be made precise:
- $\Omega^k(S^d)$ is non-trivial for $k \le d$ (by Freudenthal).
- For each $k$, the iterated loop space has distinct computational content
  (transport along $k$-fold loops).
- The number of *independent transport operations* might scale as $d^2$.

**Specific investigation:** For $S^d$ in HoTT, enumerate the independent
transport/ap operations at each loop level. Count them rigorously. Does the
count scale as $d^2$?

**Approach 2c: CW-complex face counting.**
$S^d$ as a CW-complex has one 0-cell and one $d$-cell. But the *boundary
operator* $\partial$ on the $d$-cell attaches it to the 0-cell with
information about all $(d-1)$-faces. In cubical type theory, a $d$-cube has
$2d$ faces, $4\binom{d}{2}$ edges, etc. The total face information might
scale quadratically.

**Specific investigation:** Count the face-map information in the cubical
encoding of $S^d$. Does it scale as $d^2$?

**Deliverable:** Either a rigorous derivation of $d^2$ from one of these
approaches, or an honest assessment that $d^2$ is a modeling choice with
heuristic support.

### Phase 3: Reframing the "No Free Parameters" Claim

**Goal:** Make the paper's claims honestly match the actual epistemic status.

**The core tension:** The paper says "no free parameters have been fitted"
(line 108). But the $\nu_H$ formula is itself a *definitional choice*. The
question is whether definitional choices count as "parameters."

**Key distinction to draw:**
- **Fitted parameter:** A continuous knob tuned to match data (e.g.,
  $\nu_H = m + d^\alpha$ with $\alpha$ chosen to fit).
- **Definitional choice:** A discrete formula chosen from a class of
  candidates (e.g., "$d^2$ vs. $d$ vs. $2^d$").
- **Derived quantity:** A formula that follows from a prior principle with
  no degrees of freedom.

If $d^2$ can be *derived* (Phase 2), then the claim "no free parameters" is
legitimate. If $d^2$ is a definitional choice, then the claim needs to be
qualified. If $d^2$ is one of several formulas that work, the claim is
misleading.

**Possible reframings:**
1. **Best case (derivation succeeds):** "The $d^2$ scaling follows from
   [counting argument]. No continuous parameters have been fitted."
2. **Medium case (multiple formulas work):** "The Genesis Sequence is
   reproduced by any $\nu_H$ in the family $m + f(d)$ where $f$ is
   superlinear and $f(3) \in [8, 12]$. The specific choice $f = d^2$ has
   the cleanest homotopy-theoretic interpretation."
3. **Worst case (only $d^2$ works):** Acknowledge the formula as a
   definitional choice that is constrained but not derived. Remove or qualify
   the "no free parameters" claim.

### Phase 4: The Deeper Structural Question

**Goal:** Understand whether the equal-weight property is robust to the
$\nu_H$ formula or dependent on it.

**Key insight:** The spectral decomposition's equal-weight property means
$\nu_G \approx \nu_H \approx \nu_C$ in some averaged sense. But for the
HIT steps:
- $\nu_G = 5$ is *computed* (schema counting, verified by engine)
- $\nu_C$ is small (0 or 3, from elimination rules)
- $\nu_H$ is *defined by formula*

So the equal-weight property at the HIT steps is effectively a claim that
$\nu_H \approx \nu_G$, which means the formula must give values near 5
for $S^2$. The question is whether this is a coincidence, a constraint,
or a tautology.

**Investigation:** If we hold $\nu_G = 5$ fixed (which is independently
computed) and ask "what $\nu_H$ values reproduce the sequence?", we can
determine the range of acceptable $\nu_H(S^2)$ and $\nu_H(S^3)$.

**Deliverable:** The feasible region for $(\nu_H(S^1), \nu_H(S^2),
\nu_H(S^3))$ that reproduces the Genesis Sequence. This tells us how much
freedom the formula has.

### Phase 5: Alternative Architectures

**Goal:** Consider whether the spectral decomposition itself is the right
framework, or whether the criticism reveals a deeper issue.

**Alternative 1: Drop the spectral decomposition entirely.**
If $\nu$ is defined as the total count of inference rules (which it is),
then the decomposition into $\nu_G + \nu_H + \nu_C$ is just a diagnostic
tool. The theory doesn't *need* the decomposition to make predictions—it
needs only the total $\nu$. The decomposition is interesting but optional.

- Investigate: Can the paper be restructured so the spectral decomposition
  is presented as an *observation* about the sequence, not a *component* of
  the model?
- This would sidestep the criticism entirely: the model has no $\nu_H$
  formula; it has total $\nu$ values that happen to decompose cleanly.

**Alternative 2: Define $\nu_H$ operationally.**
Instead of a formula, define $\nu_H$ as the *actual count* of computation
rules in the formal type theory. This requires:
- A formal definition of "computation rule" in HoTT/cubical type theory
- An explicit enumeration for $S^1$, $S^2$, $S^3$
- Machine verification (in Agda or similar)

This is the gold standard: no formula, just counting. If the count happens
to be $m + d^2$, that's a theorem, not a definition.

**Alternative 3: Use the uniform algorithm.**
The uniform $\nu$ algorithm (Section 7.2, `UniformNu.hs`) already computes
$\nu$ with "zero domain knowledge—no spectral decomposition, no capability
rules, no homotopy bonuses, no hand-tuned parameters" (line 1008). It
matches 13/15 steps. If the uniform algorithm can be extended to cover all
15 steps, it would provide a completely formula-free computation of $\nu$,
making the $\nu_H$ formula unnecessary.

## Execution Plan

| # | Task | Priority | Effort | Depends On |
|---|------|----------|--------|------------|
| 1 | Compute $\nu$ under all 11 alternative formulas; check sequence reproduction | **Critical** | Medium | — |
| 2 | Run 2D sweep for each formula that reproduces the sequence | **Critical** | Medium | 1 |
| 3 | Determine feasible region for $(\nu_H(S^1), \nu_H(S^2), \nu_H(S^3))$ | **Critical** | Low | — |
| 4 | Enumerate computation rules for $S^1, S^2, S^3$ in cubical type theory | High | High | — |
| 5 | Count cubical face maps / Kan filling operations for $d$-cells | High | Medium | — |
| 6 | Review HoTT book elimination rules for $S^d$ | High | Low | — |
| 7 | Assess whether spectral decomposition can be demoted to observation | Medium | Low | 1-3 |
| 8 | Extend uniform algorithm to cover Witness and $\Pi/\Sigma$ steps | Medium | High | — |
| 9 | Write honest assessment for paper: derived vs. modeling choice | **Critical** | Low | 1-6 |
| 10 | Revise "no free parameters" language if needed | **Critical** | Low | 9 |

## Phase 1 Results (Completed)

**Script:** `formula_sensitivity.py` — tested 16 alternative formulas plus
a full feasible-region scan of all 9,261 integer triplets in $[0,20]^3$.

### Result: d² is the unique simple formula

Of 16 formulas tested, **only $m + d^2$ reproduces all 15 steps**.
The next best is $d(d+1)$ at 13 steps. Seven formulas fail by step 6
or earlier.

| Formula | $\nu_H(S^1, S^2, S^3)$ | Steps correct | Failure point |
|---------|------------------------|---------------|---------------|
| $m + d^2$ (current) | (2, 5, 10) | **15** | — |
| $d(d+1)$ | (2, 6, 12) | 13 | Hilbert skipped for DCT |
| $m + d(d+1)/2$ | (2, 4, 7) | 7 | Hopf before S³ |
| $m + d(d-1)+1$ | (2, 4, 8) | 7 | Hopf before S³ |
| $2d$ | (2, 4, 6) | 7 | Hopf before S³ |
| $m + d$ (linear) | (2, 3, 4) | 6 | Hopf before S² |
| $m + d^3$ (cubic) | (2, 9, 28) | 6 | Hopf before S³ |
| $m + 2^d$ | (3, 5, 9) | 5 | S² before PropTrunc |
| $m + d^2 + 1$ | (3, 6, 11) | 5 | S² before PropTrunc |

### The feasible region is tiny

The full integer scan found only **9 triplets out of 9,261** (0.10%) that
reproduce all 15 steps:

| $\nu_H(S^1)$ | $\nu_H(S^2)$ | $\nu_H(S^3)$ | $\nu(S^1)$ | $\nu(S^2)$ | $\nu(S^3)$ |
|:---:|:---:|:---:|:---:|:---:|:---:|
| 2 | 4 | 9–13 | 7 | 9 | 17–21 |
| 2 | 5 | 10–12 | 7 | 10 | 18–20 |
| 2 | 6 | 11 | 7 | 11 | 19 |

**Critical constraints:**
- $\nu_H(S^1) = 2$ is **completely locked** — the only feasible value.
  This is because $\nu(S^1) = 7$ is the minimum integer clearing Bar₅ = 2.143
  (needs $\nu/3 \geq 2.143$, so $\nu \geq 7$), and $\nu(S^1) = 8$ would
  push PropTrunc below its bar (margin goes from +0.107 to −0.053).
- $\nu_H(S^2) \in \{4, 5, 6\}$ — only 3 values work.
- $\nu_H(S^3) \in \{9, ..., 13\}$ — 5 values work (bounded below by
  S³'s bar, above by not inflating Ω too much for subsequent steps).

### The cascade mechanism

The tightness is caused by a **cascading constraint**:
1. PropTrunc at step 6 has margin 0.107 (second-tightest in the sequence).
2. $\nu(S^1)$ feeds into $\Omega_5$, which sets Bar₆ for PropTrunc.
3. $\nu(S^1) = 8$ (i.e., $\nu_H(S^1) = 3$) makes PropTrunc margin = −0.053,
   killing the sequence at step 6.
4. Therefore $\nu_H(S^1) = 2$ exactly, with zero freedom.

This means **any formula must satisfy $f(1,1) = 2$ exactly**. For formulas
of the form $m + g(d)$, this forces $g(1) = 1$.

### Assessment

**The criticism is substantially correct.** The $d^2$ formula is very
tightly constrained by the sequence dynamics, and is the unique simple
formula from a large candidate set. The feasible region contains only 0.10%
of triplets, and $\nu_H(S^1)$ has zero degrees of freedom.

However, there is an important nuance: **the tightness comes from the
selection dynamics, not from the spectral decomposition claim.** The
equal-weight property is a consequence of the formula, but the formula is
constrained by the *bar-clearing requirements*, not by a desire for equal
weights. The bars are determined by the Fibonacci integration costs and
cumulative efficiency — both of which are independently motivated.

The key question is now: **can $d^2$ be derived from the operational
semantics of HITs in cubical type theory?** If yes, the formula is a
theorem and the criticism dissolves. If no, the paper must acknowledge
$\nu_H$ as a modeling choice.

### Implications for the paper

1. **"No free parameters" must be qualified.** The formula $\nu_H = m + d^2$
   is a discrete definitional choice. While not a continuously-fitted
   parameter, it is a formula selected from a class. The paper should say:
   "No continuous parameters have been fitted; the topological projection
   formula is a discrete modeling choice tightly constrained by the selection
   dynamics."

2. **The spectral decomposition section should present the sensitivity
   analysis.** Showing that only 9/9261 triplets work (and that d² is the
   unique simple formula) actually *strengthens* the paper if paired with
   a derivation — it shows the theory is highly predictive, not loose.

3. **Phase 2 (principled derivation) is now critical priority.** Without
   a derivation, the tightness of the constraint is a liability. With one,
   it becomes evidence of deep structure.

## Phase 2 Results: Attempted Derivation of $d^2$ (Completed)

### Summary: $d^2$ cannot be cleanly derived from first principles

Three independent derivation approaches were investigated. None produced
a rigorous derivation of $d^2$ as the unique computation-rule count for
a $d$-cell in cubical type theory. However, one approach yields a strong
heuristic argument.

### Approach 2a: Direct computation-rule counting (NEGATIVE)

**Sources:** Coquand-Huber-Mörtberg (2018), Vezzosi-Mörtberg-Abel (2019),
Cavallo-Harper (2019), Dore-Cavallo-Mörtberg (2024).

**Result:** In cubical type theory, each HIT constructor — regardless of
dimension $d$ — generates exactly **3 formal computation clauses**:
1. Introduction rule (the constructor itself)
2. Transport clause (`transp`)
3. Eliminator clause (dependent elimination β-rule)

Plus 1 shared `hcomp` clause for the entire HIT.

The number of formal clauses is **constant** (~3), not $d^2$.

**Implication:** $\nu_H$ does NOT count formal computation clauses in the
sense of the cubical type theory implementation.

### Approach 2b: Kan filling conditions (NEGATIVE)

**Result:** For a $d$-cube in a cubical Kan complex, the number of
independent open-box filling conditions is **$2d$** (one for each face
that can be the missing face). This is linear in $d$, not $d^2$.

The face-face compatibility conditions scale as $2d(d-1)$ or
$4 \binom{d}{2}$, which is quadratic but gives $2d(d-1)$, not $d^2$.

No published theorem states "a $d$-cell contributes $d^2$ independent
Kan conditions" or any similar result.

### Approach 2c: Cubical interaction matrix (PARTIAL POSITIVE)

**Result:** The strongest route to $d^2$ is through the combinatorics of
**ordered pairs of coordinate directions**.

A $d$-cell in cubical type theory has $d$ coordinate directions
$i_1, \ldots, i_d \in I$ (the interval). Each ordered pair $(i_a, i_b)$
with $a, b \in \{1, \ldots, d\}$ defines a 2-dimensional interaction:

- **Off-diagonal ($a \neq b$):** A genuine 2-face (square) of the
  $d$-cube, governing how direction $i_a$ interacts with direction $i_b$
  during composition. There are $d(d-1)$ such interactions.
- **Diagonal ($a = b$):** A degenerate square (created by connection maps),
  encoding the self-interaction / groupoid structure of direction $i_a$.
  There are $d$ such interactions.

Total: $d(d-1) + d = d^2$.

**The argument:** Each ordered pair $(i_a, i_b)$ corresponds to a
potential transport-over-transport operation: first transport along
direction $i_a$, then along direction $i_b$. Since:
- Path composition is non-commutative in dimension $\geq 2$
  (left whiskering $\neq$ right whiskering for general paths)
- The operation "transport along $i_a$ then $i_b$" is a priori
  independent of "transport along $i_b$ then $i_a$"

we get $d^2$ *ordered* pairs, not $d(d+1)/2$ unordered pairs.

Including the diagonal is justified because:
- Connection maps (degenerate squares) are non-trivial operations
  in cubical type theory
- They encode the inverse and composition operations needed for
  coherent transport
- Without connections, Kan filling is underdetermined (this is a
  known technical issue in cubical sets without connections)

**The formula interpretation:**
- $m$ counts the path constructors (each contributing a primary β-rule)
- $d^2$ counts the independent 2-dimensional interactions of the
  highest-dimensional cell
- $\nu_H = m + d^2$ counts: "primary β-rules + interaction terms"

**Verification for specific cases:**

| Sphere | $d$ | $d^2$ | $m$ | $\nu_H = m + d^2$ | Interpretation |
|--------|-----|-------|-----|-------------------|----------------|
| $S^1$  | 1   | 1     | 1   | 2                 | 1 β-rule + 1 self-interaction |
| $S^2$  | 2   | 4     | 1   | 5                 | 1 β-rule + 4 direction pairs |
| $S^3$  | 3   | 9     | 1   | 10                | 1 β-rule + 9 direction pairs |

### Strengths and Weaknesses of the Interaction Matrix Argument

**Strengths:**
1. Gives exactly $d^2$, no adjustment needed.
2. Has clear combinatorial meaning (ordered pairs of directions).
3. Non-commutativity of path algebra justifies ordered (not unordered) pairs.
4. Diagonal entries have concrete meaning (connection maps / groupoid structure).
5. The argument is purely structural — it depends only on the cell dimension,
   not on any specific HIT.

**Weaknesses:**
1. **Not a formal theorem.** No paper in the cubical type theory literature
   states this result. It is our interpretation, not an established fact.
2. **Why 2-dimensional interactions specifically?** The argument counts
   ordered *pairs* of directions (2-dimensional interactions). Why not:
   - Individual directions (gives $d$, the boundary face count per dimension)
   - Ordered triples (gives $d^3$, too many)
   - The sub-cubes of all dimensions ($3^d - 1$, exponential)?

   Answer: The PEN model counts *atomic* computation rules, and the
   fundamental unit of "interaction" in cubical type theory is the *square*
   (2-cube). All higher-dimensional cubes decompose into compositions of
   squares via the cubical identity laws. But this answer, while reasonable,
   is an interpretation, not a proof.
3. **The formal clause count is 3, not $d^2$.** In Cubical Agda, each
   constructor generates exactly 3 clauses regardless of dimension. The
   $d^2$ "interaction terms" are implicit in the *internal structure* of the
   eliminator clause, not explicit as separate rules. This means $\nu_H$
   counts "sub-structure" within a single formal clause, which is a valid
   but debatable counting convention.
4. **No prediction beyond $d = 3$.** The Genesis Sequence only has $d \leq 3$
   (the $S^3$ step). The $d^2$ formula cannot be tested beyond this range
   within the theory. Any formula $f(d)$ with $f(1) = 1$, $f(2) = 4$,
   $f(3) = 9$ would fit equally well (including $d^2$, $d^2 - \epsilon$
   for small $\epsilon$, etc.).

### Assessment: $d^2$ Is a Well-Motivated Modeling Choice, Not a Theorem

The interaction-matrix argument provides a genuine structural explanation
for the $d^2$ scaling, but it falls short of a rigorous derivation. The
gap is:

1. The cubical type theory literature defines formal computation rules
   per constructor (constant, ~3), not per direction-pair ($d^2$).
2. The "counting convention" that treats direction-pair interactions as
   independent computation rules is reasonable but not standard.
3. The formula cannot be tested beyond $d = 3$.

**Bottom line:** $d^2$ is the unique simple polynomial consistent with
the feasible region, and it has a clear combinatorial interpretation in
terms of the cubical cell structure. But it is a *modeling choice informed
by structural reasoning*, not a *derived consequence of the type theory*.

## Recommended Response to the Criticism

Given the combined findings of Phases 1 and 2, the paper should:

### 1. Restructure the $\nu_H$ presentation

**Current:** $\nu_H = m + (\max d_i)^2$ is stated as a definition with
an informal justification (the loop-space argument in a remark).

**Proposed:** Present $\nu_H$ in three layers:

**(a) The primary quantity is total $\nu$, not $\nu_H$.**
The main PEN model uses total $\nu$ (Generative Capacity), computed by
the inference-rule audit (Section 7.1) and verified by the uniform algorithm
(Section 7.2). The spectral decomposition is an *analysis* of $\nu$, not
a *component* of the model. The synthesis loop never uses $\nu_H$ directly.

**(b) The decomposition is structural, the formula is a modeling choice.**
When we partition inference rules into Introduction ($\nu_G$), Computation
($\nu_H$), and Elimination ($\nu_C$), the topological projection counts
the Computation rules contributed by the cell structure of HITs. The
formula $\nu_H = m + d^2$ is motivated by the interaction-matrix argument
(ordered pairs of coordinate directions in the $d$-cell), but this is an
interpretive convention, not a theorem of cubical type theory.

**(c) The constraint is structural, not parametric.**
The formula is tightly constrained by the selection dynamics: $\nu_H(S^1) = 2$
is the unique value compatible with the Genesis Sequence (Phase 1 result).
The remaining freedom ($\nu_H(S^2) \in \{4,5,6\}$,
$\nu_H(S^3) \in \{9,...,13\}$) accommodates $d^2$ but also 8 other
integer triplets. The formula $d^2$ is chosen as the simplest polynomial
in the feasible region.

### 2. Qualify "no free parameters"

**Current (line 108):** "Every quantity is computable from the five axioms
of §X; no free parameters have been fitted."

**Proposed:** "Every quantity is computable from the five axioms of §X.
The total Generative Capacity $\nu$ is verified by direct inference-rule
enumeration (§7.1). The spectral decomposition (§6) uses the formula
$\nu_H = m + d^2$ for the topological projection; this is the unique
simple polynomial consistent with the selection dynamics (§6.X), motivated
by the cubical interaction structure (Remark X), but not derivable from
the axioms alone."

### 3. Add a sensitivity analysis subsection

Add a new subsection to §6 (or §8) presenting the Phase 1 results:
- The feasible region: only 9/9261 integer triplets reproduce the sequence.
- $\nu_H(S^1) = 2$ is completely locked (zero freedom).
- $d^2$ is the unique simple formula from a set of 16 candidates.
- The constraint comes from the selection dynamics (cascading bar constraints),
  not from tuning for equal weights.

This *strengthens* the paper: it shows the theory is highly constrained
and makes precise predictions, while being honest that the formula is a
modeling choice rather than a derived result.

### 4. Present the interaction-matrix argument as a remark

Replace the informal "loop space × homotopy levels" justification with
the cleaner interaction-matrix argument:

> *Remark (Cubical interaction structure).* A $d$-cell in cubical type
> theory has $d$ coordinate directions. Each ordered pair $(i_a, i_b)$
> defines a 2-dimensional interaction: the off-diagonal pairs ($a \neq b$)
> are non-degenerate squares governing direction-direction composition;
> the diagonal pairs ($a = a$) are degenerate squares encoding the
> groupoid self-interaction via connection maps. The total count $d^2$
> reflects the non-commutativity of the path algebra in dimensions $\geq 2$.
> This provides structural motivation for the formula $\nu_H = m + d^2$,
> though a formal derivation from the operational semantics remains open.

## Remaining Phases

### Phase 3: Reframing "No Free Parameters"

The reframing should follow option (c) above: the total $\nu$ is
parameter-free (verified by inference-rule counting); the spectral
decomposition uses a modeling choice (the $\nu_H$ formula) that is
tightly constrained but not derived.

### Phase 5: Alternative Architectures

The strongest response to the criticism is **Alternative 1: clarify that
the spectral decomposition is an observation, not a model component.**
The total $\nu$ values are the primary quantities (verified by 15/15
inference-rule audit). The decomposition is an analysis of those totals,
not a component of the synthesis loop. The $\nu_H$ formula is used only
*within* the spectral decomposition section — the main PEN model uses
total $\nu$ directly.

This reframing addresses the criticism head-on: the model has no $\nu_H$
formula as a "parameter." It has total $\nu$ values that happen to
decompose into three approximately equal components under the partition
(Introduction, Computation, Elimination). The formula $m + d^2$ is
a *characterization* of this decomposition, not an *input* to the model.
