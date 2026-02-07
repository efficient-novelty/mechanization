# PEN Information-Theoretic Research Plan
# κ as Kolmogorov Complexity, ν as Proof-Rank, ρ as Amplitude

## Why This Matters

The Phase 3b results revealed a puzzle: the OpSchema grammar matches ν for R1-R10
by hand but the machine implementation undercounts for complex types because it lacks
the conceptual vocabulary. We proposed "vocabulary co-evolution" as the fix — but that
just names the problem. It doesn't solve it.

Halvor's original intuition — that novelty is "Shannon surprise" and effort is
"Kolmogorov complexity" — pointed in the right direction. But a pencil calculation
(testing S¹ at step 5) revealed that raw counting of newly inhabited types gives
either ~2 (strict) or ~30+ (loose), never 7. The Genesis ν values count something
more specific: **the number of independent proof techniques** that a structure
enables. This is a rank, not a cardinality.

Combined with the insight that ρ = ν/κ functions as a **probability amplitude**
(with ρ² giving the "physical probability" of a structure manifesting), the
information-theoretic reformulation becomes:

- **κ(X | L)** = Kolmogorov complexity: shortest description of X using library L
- **ν(X | L)** = proof-rank: number of independent clusters of newly provable
  theorems at depth ≤ 2 (fixed by the Complexity Scaling Theorem's d=2)
- **ρ = ν/κ** = efficiency amplitude (not a probability)
- **ρ²** = realization probability (Born rule)

The research question: **Can we implement proof-rank ν computably, confirm that
it reproduces the Genesis values, verify that ρ² gives viable selection dynamics,
and connect the Born rule at the foundational level to the Born rule in the
emergent physics (Paper 5)?**

---

## Part 1: The Information-Theoretic Reformulation

### 1.1 κ as Kolmogorov Complexity

**The idea:** κ(X | L) = the length of the shortest definition of X using the library L
as a "language."

**Why this is better than constructor counting:**

Constructor counting treats all constructors as equal cost. But a constructor that
reuses library infrastructure should be "cheaper" than one that introduces genuinely
new structure. Kolmogorov complexity captures this automatically:

- Defining S¹ from scratch: "A type with one point constructor and one path
  constructor forming a loop" → several bits of information
- Defining S² given S¹: "Suspension of S¹" → very few bits (one operation applied
  to one argument)
- Defining S² from scratch: "A type with one point constructor and one 2-cell
  filling a constant path" → more bits than the suspension definition

The Kolmogorov framing says: **κ measures how much genuinely new information
the definition of X contains, beyond what the library already provides.**

**Formal definition:**

Let Prog(L) be a simple programming language whose primitives are:
- All types and terms in L
- Type formers available (Π, Σ, Id, if present in L)
- Standard combinators: composition, application, pairing, projection

Then:
```
κ(X | L) = min { |p| : p ∈ Prog(L), p defines X }
```

where |p| is the length of program p in bits.

**Crucial property:** κ(X | L) decreases as L grows. The Circle is expensive to
define from nothing but cheap to define given Π, Σ, and the concept of a HIT.
This is exactly the behavior we want — later realizations can "inherit" from earlier
ones, reducing their marginal cost.

**Approximation strategy for Agda:**

We can't compute true Kolmogorov complexity (it's uncomputable). But we can
compute a good upper bound by defining a concrete description language:

```agda
-- A "program" that defines a type using library ingredients
data TypeProgram (L : Library) : Set where
  -- Use a library type directly
  Ref     : (i : Fin |L|) → TypeProgram L
  -- Apply a type former
  Arrow   : TypeProgram L → TypeProgram L → TypeProgram L
  Prod    : TypeProgram L → TypeProgram L → TypeProgram L
  -- HIT specification
  MakeHIT : (points : ℕ) → (paths : List PathSpec) → TypeProgram L
  -- Suspension (if S¹ is in library)
  Susp    : TypeProgram L → TypeProgram L
  -- Loop space
  Omega   : TypeProgram L → TypeProgram L
  -- Truncation
  Trunc   : ℕ → TypeProgram L → TypeProgram L
  -- ... other operations available from L

-- The "cost" is the AST size
programCost : TypeProgram L → ℕ
programCost (Ref _) = 1
programCost (Arrow a b) = 1 + programCost a + programCost b
programCost (MakeHIT pts paths) = 1 + pts + length paths
programCost (Susp a) = 1 + programCost a
-- etc.

-- κ is the minimum cost over all programs that produce X
κ(X, L) = min { programCost p | p ∈ TypeProgram L, p ≡type X }
```

**Key design decision:** The set of operations in TypeProgram should grow with L.
When S¹ enters the library, "Susp" becomes available as an operation. When Hopf
enters, "Fiber" becomes available. This is the vocabulary co-evolution, but now it's
principled: the vocabulary is exactly the library itself.


### 1.2 ν as Proof-Rank

**The idea:** ν(X | L) = the number of **independent proof clusters** among the
newly provable theorems that X enables, at depth ≤ 2.

**Why not raw theorem counting (Shannon surprise):**

The pencil calculation for S¹ (step 5) showed that counting newly inhabited types
gives the wrong answer at every threshold. Counting all of them gives ~30+.
Counting only those whose inhabitation essentially requires S¹'s path constructor
gives ~2. The Genesis value ν = 7 counts neither theorems nor essential theorems
but **independent proof techniques** — qualitatively distinct capabilities.

The 7 items for S¹ are: (1) existence, (2) non-trivial loop, (3) rich map space,
(4) dependent elimination with transport, (5) loop space algebra, (6) fundamental
group computation, (7) suspension template. Each is a *generator* of a family of
related theorems. Raw counting sees the individual theorems; proof-rank sees only
the generators.

**Formal definition:**

Let NewThm(X, L, d) = { T : T is inhabited in L∪{X}, not in L, complexity(T) ≤ d }
be the set of newly provable theorems at bounded depth.

Define a **derivability relation** ≻ on NewThm: T₁ ≻ T₂ if T₂ can be proved
from T₁ using only library operations (without additional appeal to X).

Two theorems are in the same **cluster** if they are connected by chains of ≻.
Formally, cluster equivalence is the symmetric transitive closure of ≻.

Then:
```
ν(X | L) = |NewThm(X, L, d) / ~|    (number of equivalence classes)
```

**In words:** ν counts the number of independent directions in which X extends the
library's proof capabilities. Each cluster represents one qualitatively new thing
you can do.

**Why d = 2 (not a free parameter):**

The Complexity Scaling Theorem proves d = 2 for the coherence window: the
Fibonacci recurrence Δ_{n+1} = Δ_n + Δ_{n-1} decomposes the interface into
interactions with layers n and n-1. Everything older is absorbed into Ω.

If d = 2 governs cost, it should also govern novelty. The relevant new theorems
at step n are those involving X interacting with the 2-step window {R_{n-1}, R_{n-2}}.
Deeper chains involve layers outside the coherence window.

This was confirmed by the pencil calculation: for R4, R5, and R6, all substantive
novelty appears at expression depth ≤ 2. No depth-3 type adds a genuinely new
capability. There is no free parameter to tune.

**Connection to OpSchema:**

The OpSchema categories (EXIST, PATH, MAP, DEP-ELIM, LOOP, HOMOTOPY-GROUP,
SUSPENSION) are exactly the cluster representatives for S¹. The grammar was
manually discovering what proof-rank computes automatically: the number of
independent proof generators.

The advantage of the proof-rank formulation: no grammar needed. Enumerate theorems,
cluster by derivability, count clusters. The "vocabulary" emerges from the
clustering, not from a pre-designed grammar.


### 1.3 The Clustering Algorithm

Since ν is the number of independent proof clusters at depth ≤ 2, the computation
has three steps:

**Step 1: Enumerate.** Generate all types T of expression depth ≤ 2 formable from
L ∪ {X}, using the type formers available in L.

For the 2-step window, the atoms are X, R_{n-1}, and R_{n-2}. The type formers
are those in L (→, ×, Σ, Id after step 4; plus ‖-‖ after step 6; etc.)

At depth ≤ 2 with ~5 atoms and ~5 type formers, this produces on the order of
50-200 candidate types. Very tractable.

**Step 2: Filter.** For each candidate type T, check (heuristically):
- Is T inhabited in L ∪ {X}? (inhabitation oracle)
- Was T inhabited in L alone? (pre-addition oracle)
Keep only the newly inhabited types: NewThm(X, L, 2).

**Step 3: Cluster.** Among the newly inhabited types, determine which are
derivably related:

```
T₁ ≻ T₂  iff  there exists f : T₁ → T₂ constructible from L alone
```

Two theorems T₁, T₂ are in the same cluster if connected by ≻ chains. In
practice, the clustering rules are:

- T and (1 → T) are in the same cluster (trivial currying)
- T and (T × 1) are in the same cluster (trivial product)
- (A → T) and T are in the same cluster if A is inhabited in L (const eliminates A)
- (T → 1) is in a trivial cluster (always derivable from existence of T)
- If T₁ ≃ T₂ up to known library equivalences, same cluster
- Ω(X) and (base =_X base) are the same cluster (definitional equality)

After clustering, ν = number of non-trivial clusters.

**Example: S¹ at step 5**

NewThm(S¹, L₄, 2) includes: S¹, 1→S¹, S¹→1, S¹→S¹, S¹×S¹, S¹×1,
base=base, Ω(S¹), Ω(S¹)→Ω(S¹), (x:S¹)→P(x), S¹→U₀, ...

Clustering:
- {S¹, 1→S¹, S¹×1, Σ(x:S¹).1} — all derived from S¹'s existence
- {S¹→1} — trivial (terminal map), don't count
- {S¹→S¹} — contains id, const base, but also degree maps. Independent of existence.
- {S¹×S¹} — torus. Derived from existence? Yes: if A and B are inhabited, A×B is
  inhabited. So this clusters with existence. BUT topologically it's new.
  Decision: for inhabitation purposes, it's derived. For structure, it's not.
  Under our definition (derivability of inhabitation), it clusters with existence.
- {base =_{S¹} base, Ω(S¹)} — the loop. Independent (requires loop constructor).
- {Ω(S¹)→Ω(S¹)} — path algebra. Derivable from the loop? The TYPE is inhabited
  because Ω(S¹) is inhabited (via const loop). So it clusters with the loop cluster.
  BUT it has additional structure (composition, inversion) not present in just Ω(S¹).
- {(x:S¹)→P(x)} — dependent elimination. Independent (new proof technique).
- {S¹→U₀} — type families over S¹. Independent (fibrations).

This gives 5-6 clusters depending on boundary decisions. Still short of 7.

**The gap:** Items 6 (π₁ ≅ ℤ) and 7 (suspension) from the OpSchema analysis are
not captured because they involve structures (ℤ, pushouts) not in L₄. They're
"potential" novelty — capabilities that become actual when later structures arrive.

**Resolution options:**
(a) Accept ν ≈ 5-6 for S¹ and check if selection dynamics are robust to this
(b) Expand the window to include "latent" capabilities (provable in L ∪ {X} ∪ {future})
(c) Count type families X → U as producing one cluster per non-trivial fiber shape

Option (a) is the most honest. Option (c) has merit: S¹ → U produces genuinely
independent fiber families (constant, universal cover, ...) and each is a new
proof cluster. This may close the gap.

**For the Haskell engine:** Implement (a) first. If the selection loop works
with ν ≈ 5-6 for geometric types, the exact count doesn't matter for the dynamics.
If it fails, try (c).


### 1.4 The Reformulated Efficiency and the Born Rule

Putting it together:

```
ρ(X | L) = ν(X | L) / κ(X | L)
          = (independent proof clusters at depth ≤ 2)
            / (Kolmogorov complexity of X given L)
```

**The critical reinterpretation: ρ is an amplitude, not a probability.**

In quantum mechanics, the probability of an event is proportional to the
square of the number of independent paths (amplitudes) leading to it. PEN's
ρ has exactly this structure:

- ν counts the number of independent "paths" (proof clusters) through which
  X impacts the library
- κ is the "cost" of constructing X
- ρ = ν/κ is the **amplitude** of the realization
- ρ² = (ν/κ)² is the **realization probability**

**The selection rule becomes:**

```
Select X if: ρ(X)² ≥ Bar(n)²     (equivalently, ρ ≥ Bar)
Among viable candidates: select argmax ρ(X)²  (= argmax ρ(X))
```

Since squaring is monotone on non-negatives, the selection dynamics are identical
whether we maximize ρ or ρ². The ordering of candidates doesn't change. What
changes is the *interpretation*: the Genesis Sequence is not a sequence of
"efficient structures" but a sequence of **high-amplitude structures** — those
with the most independent proof paths per unit of descriptive cost.

**Why this matters:**

The Born rule appears at two levels in the PEN framework:

1. **Foundational level:** The realization probability of a mathematical structure
   is ρ² = (ν/κ)². Structures with many independent proof paths (high ν) and
   short descriptions (low κ) have high amplitude and are "more likely to exist."

2. **Emergent level (Paper 5):** The PEN → Linear Logic → No-Cloning derivation
   produces quantum mechanics, in which the probability of a physical event is
   |ψ|² — the Born rule applied to quantum amplitudes.

**The self-consistency claim:** The same rule (probability ∝ amplitude²) governs
both which mathematical structures get realized and which physical events occur
within those structures. The Born rule is not derived from something deeper — it
IS the foundational selection principle, appearing at every level.

This is a strong, falsifiable claim. It predicts that any attempt to derive the
Born rule "from below" will eventually bottom out at a selection principle that
looks like PEN's efficiency maximization. Conversely, PEN's efficiency dynamics
must produce exactly the |ψ|² rule when restricted to the quantum regime.


### 1.4a The Delta-Ratio Equivalence (Amplitude Thresholding)

An important structural observation: the selection rule ρ ≥ Bar(n) rearranges to:

```
ν - Bar(n) · κ ≥ 0
```

This IS a delta form: ν - λ(n)·κ ≥ 0, where λ(n) = Bar(n) rises on the Fibonacci
schedule. Among candidates clearing this threshold, PEN selects the ρ-maximizer.

In amplitude terms: the delta threshold ensures a minimum absolute amplitude
(filtering out structures whose total proof-path count is too low relative to their
cost), while the ratio selects the highest amplitude-per-cost within the viable set.

The Bar prevents noise (many paths but expensive — low amplitude) and the ratio
prevents triviality (cheap but few paths — low amplitude). The "sweet spot" — many
independent proof paths at low descriptive cost — is the high-amplitude regime.


### 1.5 Why Proof-Rank Resolves the Vocabulary Problem

The OpSchema approach required pre-designing categories (EXIST, PATH, MAP, DEP-ELIM,
LOOP, HOMOTOPY-GROUP, FIBRATION, TRUNCATION) and manually checking which were
"newly realized." The vocabulary problem was that complex types (S³, Connections,
DCT) needed domain-specific categories the grammar didn't have.

Proof-rank eliminates the grammar entirely. The "categories" emerge automatically
as clusters:

1. Enumerate newly inhabited types at depth ≤ 2 (mechanical)
2. Cluster by derivability (mechanical)
3. Count clusters (trivial)

No hand-designed vocabulary needed. The clusters ARE the vocabulary — they're
discovered, not assumed.

**The cost:** The clustering step requires checking derivability between pairs of
types, which means checking inhabitation of function types. This is harder than
just checking inhabitation of individual types. But at depth ≤ 2 with ~50-200
candidates, the pairwise checks are O(n²) ≈ O(40,000) — entirely feasible.

**What depth ≤ 2 buys us:** The Complexity Scaling Theorem's d=2 means no free
parameter. We don't search over k values. We don't need to find a "natural
complexity horizon." The horizon IS 2, dictated by the same principle that
produces Fibonacci timing. The entire model has one structural constant (d=2)
and everything flows from it: Fibonacci costs, depth-2 novelty, and now
proof-rank clustering.

---

## Part 2: Connecting to Known Information Theory

### 2.0 Notes from External Review (Gemini Deepthink)

An external review raised several points. One was incorporated, one was
superseded by later analysis:

1. **Delta vs. Ratio:** The concern that ρ = ν/κ can be gamed by trivial
   structures is valid, but the existing Bar mechanism already provides the
   delta correction. See Section 1.4a above. No change to the formalism needed.

2. **Bennett's Logical Depth:** Originally motivated a "depth-weighted"
   novelty measure (Weight D). However, the pencil calculation showed that
   all substantive novelty lives at depth ≤ 2 (as predicted by the Complexity
   Scaling Theorem). Depth-weighting is unnecessary — there's nothing deep
   enough to weight differently. The d=2 constraint from Fibonacci timing
   simultaneously determines the novelty horizon, which is more elegant than
   a separate depth parameter. Bennett's insight survives in a different form:
   the *clustering* step captures "proof technique independence," which is the
   qualitative aspect of logical depth that matters.

Two other suggestions were evaluated and set aside:

3. **Compression proxies (gzip/LZW):** Not applicable at our scale.
4. **Cellular automata experiment:** Orthogonal to our type-theoretic project.

### 2.1 The Rate-Distortion Analogy

The PEN selection dynamics have a natural interpretation in rate-distortion theory:

- The "source" is the space of potential mathematical structures
- The "channel capacity" is the Fibonacci-bounded integration budget
- The "distortion" is the failure to realize a structure that would increase novelty
- The system operates at the rate-distortion limit: maximum novelty per unit effort

The Selection Bar = Φ · Ω is analogous to the rate-distortion function R(D):
it specifies the minimum "rate" (effort) required to achieve a given "fidelity"
(novelty level). The Fibonacci schedule describes how R(D) changes over time
as the source distribution (available structures) evolves.

This analogy may be more than an analogy. It might be formalizable.

### 2.2 The Minimum Description Length Connection

MDL principle (Rissanen): The best model is the one that minimizes the total
description length = (model description) + (data encoded using model).

In PEN terms:
- "Model description" = κ (the cost of defining X)
- "Data encoded using model" = the compression X provides for future structures
- "Best model" = the X that maximizes ν/κ

The Genesis Sequence can be seen as a sequence of "model selections" in the MDL
sense: each new type is chosen because it compresses future mathematics more than
it costs to define.

This connection is worth making explicit in the paper because MDL is a well-known,
well-respected framework. Grounding PEN in MDL gives it credibility with
information theorists.

### 2.3 The Logical Depth Connection (Bennett) — Revisited

Charles Bennett's "logical depth" measures the computational effort required to
derive the interesting consequences of a string, given its shortest description.

The d=2 finding from the pencil calculation changes how logical depth enters PEN.
Originally, we thought deeper consequences should be weighted more heavily. Instead,
the Complexity Scaling Theorem says the depth horizon IS 2 — there are no "deep"
consequences to weight, because the coherence window only sees 2 steps back.

What remains of Bennett's insight is the **clustering** aspect: within the depth-2
window, some newly provable theorems are derivably related (shallow) and some are
independent (deep in a qualitative sense). The proof-rank ν counts the independent
ones. This is a categorical version of logical depth — not "how long is the
derivation?" but "how many independent derivation KINDS are there?"

**Prediction:** If we plot κ vs. ν for the Genesis types, the sequence should
trace the Pareto frontier of the (low κ, high ν) region. Types below the frontier
are "low-amplitude" (few independent paths per bit). The Genesis Sequence is the
high-amplitude trajectory.

### 2.4 Algorithmic Mutual Information

The novelty of X given L can also be framed as:

```
ν(X | L) ≈ I(X ; Future | L)
```

where I is the algorithmic mutual information between X and the "future structures"
that X enables. This measures how much knowing X tells you about what comes next.

A type with high mutual information with the future is one that "unlocks" many
subsequent realizations. This is exactly the enabling power interpretation.

### 2.5 The Born Rule as Foundational Principle

The reinterpretation of ρ as amplitude (Section 1.4) has a deep structural
consequence that connects the PEN foundation to its physical predictions.

**The claim:** The Born rule (probability ∝ amplitude²) is not derived within PEN.
It IS PEN. The selection dynamics are:

```
P(X realized at step n) ∝ ρ(X | L_n)²  =  (ν/κ)²
```

This says: the probability of a mathematical structure manifesting is proportional
to the square of the number of independent proof paths per unit of description.

**Why squared and not linear?**

Consider the analogy with quantum amplitudes. A quantum event with n independent
paths to it has amplitude ψ = Σᵢ aᵢ, and probability |ψ|². The squaring
implements constructive interference — independent paths reinforce each other
superlinearly. Two paths don't just double the probability; they quadruple it
(if equal amplitude).

For PEN: a structure with 7 independent proof clusters (S¹) is not 7× more
likely than one with 1 cluster — it's 49/1 = 49× more likely (at equal κ).
The independent proof techniques "interfere constructively."

**Why this is self-consistent with Paper 5:**

Paper 5 derives: PEN → Linear Logic → No-Cloning → Hilbert Space → Born Rule.

The Born rule at the emergent level (quantum mechanics) says P(event) = |⟨ψ|φ⟩|².
The Born rule at the foundational level (PEN selection) says P(structure) ∝ (ν/κ)².

If the derivation in Paper 5 is correct, these must be the same rule appearing at
different levels of description. The emergent Born rule is a "projection" of the
foundational one into the quantum regime.

**Testable prediction:** The ρ² dynamics should produce the same Genesis Sequence
as the ρ dynamics (since squaring preserves ordering). BUT ρ² matters for the
*physical* predictions — specifically, for computing the coupling constants and
cosmological parameters that Paper 5 claims to derive. If those derivations
implicitly assume ρ (linear) rather than ρ² (Born), they need correction.

**Research task:** Audit Paper 5's derivations. Every place where "efficiency"
appears in a physical formula, check whether it should be ρ or ρ². The Born
rule interpretation predicts ρ². If the existing formulas already use ρ², the
self-consistency is confirmed. If they use ρ, the physical predictions change.

---

## Part 3: The Experimental Plan

### Experiment 1: Computable κ via Description Length

**Goal:** Compute κ(X | L) for all 16 Genesis types using a concrete description
language, and compare to the paper's hand-assigned κ values.

**Method:**

Step 1: Define the description language.

```agda
data TypeProgram : Library → Set where
  -- Atoms (cost 1 each)
  LitUnit    : TypeProgram L                     -- the type 1
  LitBool    : TypeProgram L                     -- the type 2
  Ref        : Fin |L| → TypeProgram L           -- reference a library type

  -- Unary operations (cost 1 + cost of argument)
  Susp       : TypeProgram L → TypeProgram L     -- suspension ΣX
  Omega      : TypeProgram L → TypeProgram L     -- loop space ΩX
  Trunc      : ℕ → TypeProgram L → TypeProgram L -- ‖X‖_n
  Deloop     : TypeProgram L → TypeProgram L     -- BG (delooping)
  Free       : TypeProgram L → TypeProgram L     -- free group on X
  
  -- Binary operations (cost 1 + cost of arguments)
  Arrow      : TypeProgram L → TypeProgram L → TypeProgram L
  Prod       : TypeProgram L → TypeProgram L → TypeProgram L
  Coprod     : TypeProgram L → TypeProgram L → TypeProgram L
  Fiber      : TypeProgram L → TypeProgram L → TypeProgram L
  
  -- HIT specification (cost = 1 + points + Σ path_dims)
  MakeHIT    : (points : ℕ) → (paths : List ℕ) → TypeProgram L
  -- Each path has a dimension: 1-path, 2-path, etc.
  -- S¹ = MakeHIT 1 [1]          cost = 1 + 1 + 1 = 3
  -- S² = MakeHIT 1 [2]          cost = 1 + 1 + 2 = 4
  -- T² = MakeHIT 1 [1,1,2]      cost = 1 + 1 + 4 = 6
  -- But S² = Susp(S¹)           cost = 1 + 3 = 4  (same!)
  -- Or S² = Susp(Susp(1))       cost = 1 + 1 + 1 = 3 (cheaper given Susp!)

  -- Type formers as types (special)
  TypeFormerΠ : TypeProgram L                    -- "Π-types exist"
  TypeFormerΣ : TypeProgram L                    -- "Σ-types exist"
  TypeFormerId : TypeProgram L                   -- "identity types exist"
```

Step 2: For each Genesis type, find the shortest program.

| n | Structure | Shortest program (given L at that point) | |p| | Paper κ |
|---|-----------|------------------------------------------|------|---------|
| 1 | Universe  | LitU                                     | 1    | 2       |
| 2 | Unit      | LitUnit                                  | 1    | 1       |
| 3 | Witness   | (term, not type — special)               | 1    | 1       |
| 4 | Π/Σ       | TypeFormerΠ + TypeFormerΣ                 | 2    | 3       |
| 5 | S¹        | MakeHIT 1 [1]                            | 3    | 3       |
| 6 | PropTrunc | Trunc 0 (Ref _)                          | 2    | 3       |
| 7 | S²        | Susp(Ref S¹)                             | 2    | 3       |
| 8 | S³        | Susp(Ref S²)                             | 2    | 5       |
| ...| ...      | ...                                      | ...  | ...     |

**Key observation already visible:** For S³, the paper says κ=5 (counting the group
structure constructors), but the Kolmogorov definition gives κ=2 (it's just a
suspension of S²). These will diverge. The question is which gives better
selection dynamics.

**This disagreement is informative.** The paper's κ=5 for S³ counts the SU(2)
group structure (multiplication, inversion, associativity proofs). The Kolmogorov κ=2
says "the underlying type is cheap; the group structure is a separate theorem about
S³, not part of its definition."

This raises a real question: **Does κ measure the cost of defining the type, or the
cost of defining the type together with its key properties?**

If κ = definition only → Kolmogorov gives κ ≈ 2 for S³
If κ = definition + key structure → paper gives κ = 5 for S³

Both are defensible. We should compute both and see which produces better dynamics.

Step 3: Compare the two κ measures and check which one, combined with
computable ν, produces a viable selection sequence.


### Experiment 2: Computable ν via Proof-Rank

**Goal:** Compute ν(X | L) as the number of independent proof clusters among
newly inhabited types at depth ≤ 2, and compare to Genesis ν values.

**Method:**

Step 1: Define the type enumeration at depth ≤ 2 (as in Section 1.3).
Use the 2-step window {R_{n-1}, R_{n-2}} as the atom set.

Step 2: For each enumerated type, check approximate inhabitation using the
heuristic oracle. Filter for newly inhabited types.

Step 3: Cluster the newly inhabited types by derivability. Two types T₁, T₂ are
in the same cluster if a function T₁ → T₂ (or T₂ → T₁) is constructible
from library operations alone.

Step 4: Count clusters. This is ν.

Step 5: Compare to Genesis ν. Compute for R1-R10.

**Critical test cases:**

- R4 (Π/Σ): Pencil calculation gives ν = 5. Engine should match.
- R5 (S¹): Pencil calculation gives ν ≈ 5-6. Genesis says 7.
  Gap of 1-2 is expected (latent capabilities not yet constructible).
- R6 (PropTrunc): Pencil calculation gives ν = 8. Engine should match.

**The gap test:** If the engine consistently undercounts by ~15-25% for geometric
types, that's the "latent capability" effect. Check whether selection dynamics
are robust to this systematic undercount.

**Prediction:** There should NOT be a "natural k" to tune — the depth is fixed
at 2. Either the proof-rank at d=2 preserves the Genesis ordering or it doesn't.
No parameter search.


### Experiment 3: The Cross-Validation (Including Born Rule Check)

**Goal:** Determine whether (Kolmogorov κ, proof-rank ν) produces viable
selection dynamics, and whether ρ or ρ² gives better physical predictions.

**Method:**

For each of the following (κ, ν) pairs:
- (paper κ, paper ν) — the original hand-tuned values
- (Kolmogorov κ, paper ν) — new κ, old ν
- (paper κ, proof-rank ν) — old κ, new ν
- (Kolmogorov κ, proof-rank ν) — both new

Compute ρ = ν/κ, Bar = Φ · Ω, and check whether ρ ≥ Bar for all 16 steps.

**Additional Born rule check:** For each combination, also compute ρ² and
check whether any physical predictions from the companion papers (coupling
constants, cosmological parameters) are sensitive to using ρ vs. ρ². If they
are, recompute with ρ² and check against observations.

**This is the decisive test.** If (Kolmogorov κ, proof-rank ν) produces a viable
sequence, we've found a fully computable foundation for PEN. If it doesn't, we know
exactly which measure fails and where.


### Experiment 4: The Selection Loop (Phase 4)

**Goal:** Run the selection loop with computable measures and see what sequence
the machine produces.

**Method:**

Step 1: Define the candidate space.

At each step n, the candidates are all types definable by programs of bounded
length in TypeProgram(L_n):

```
Candidates(L, budget) = { X : TypeProgram L, programCost(X) ≤ budget }
```

The budget should be generous — say, 10 or 15. We want the candidate space to
include many options so the selection pressure does real work.

Step 2: For each candidate, compute (κ, ν, ρ).

```
κ(X) = programCost(X)              -- Kolmogorov κ
ν(X) = proofRank(X | L, d=2)       -- proof-rank at depth 2
ρ(X) = ν(X) / κ(X)                 -- amplitude
```

Note: Since ρ² is monotone in ρ, maximizing ρ and maximizing ρ² give the same
winner. The Born rule interpretation doesn't change the selection loop's behavior,
only the physical interpretation of the output.

Step 3: Apply the selection rule.

```
Bar(n) = Φ(n) · Ω(n-1)   -- from the Fibonacci dynamics
Winner(n) = argmax { ρ(X) : X ∈ Candidates, ρ(X) ≥ Bar(n) }
```

If no candidate clears the Bar, the sequence halts.

Step 4: Record the trace and analyze.

```
Step 1:  Selected: ???   κ=?  ν=?  ρ=?  Bar=---
Step 2:  Selected: ???   κ=?  ν=?  ρ=?  Bar=?
Step 3:  Selected: ???   κ=?  ν=?  ρ=?  Bar=?
...
```

**The key question:** Does the machine-generated sequence resemble the Genesis
Sequence? Specifically:

a) Does it select types in a recognizable order?
b) Does it produce something like S¹ before S²?
c) Does the sequence halt (Bar becomes unclearable)?
d) If it halts, at which step?

**Expected outcomes:**

**If the sequence matches (approximately):** PEN is a genuine computational theory
of mathematical evolution. The information-theoretic foundation works.

**If the sequence diverges early:** The divergence point tells us exactly where
the computable measures fail. For example, if the machine selects Bool instead of
S¹ at step 5, it means the enumeration doesn't capture S¹'s enabling power.

**If the sequence halts prematurely:** The Bar becomes too high for any computable
candidate to clear. This means either (a) the computable ν underestimates true
novelty, or (b) the Fibonacci schedule is too aggressive.

**If the sequence doesn't halt but goes weird:** The machine finds types that
clear the Bar but don't correspond to known mathematics. This would be the
most interesting outcome — it would mean there are "efficient" mathematical
structures that mathematicians haven't discovered because we don't think in
terms of efficiency optimization.


### Experiment 5: The κ-ν Pareto Frontier

**Goal:** Map the space of all types (up to bounded complexity) in the (κ, ν)
plane and check whether the Genesis types lie on the Pareto frontier.

**Method:**

At step n, enumerate ALL candidates, not just the winner. Plot each candidate
as a point (κ, ν). Mark the Genesis type. Check:

1. Is the Genesis type on the Pareto frontier (no other type has both lower κ AND
   higher ν)?
2. Is the Genesis type the ρ-maximizer (highest ν/κ)?
3. Are there types with higher ρ that the Genesis Sequence didn't select?

If (3), study these types. Are they:
- Trivial variations (S¹ with a different base point)?
- Genuinely different structures the sequence "missed"?
- Artifacts of the computable approximation?

This experiment directly tests whether the Genesis Sequence is optimal (as claimed)
or merely viable (as currently proven).

---

## Part 4: Implementation Architecture

### 4.1 Language Choice

The bounded theorem enumeration is computationally intensive. Agda is slow for
brute-force enumeration. I recommend a **two-language architecture:**

- **Haskell (or Python):** The enumeration engine. Generates all types up to
  complexity k, checks inhabitation heuristically, computes κ and ν.
  This needs to be fast — we'll enumerate thousands of types per step.

- **Agda:** The verification layer. For any (κ, ν) pair the Haskell engine produces,
  Agda checks that the type is well-formed and that the claimed inhabitation
  witnesses are correct. Agda also houses the Phase 1 proofs (Fibonacci, Bar dynamics).

The Haskell engine is where the science happens. Agda is the notary.

### 4.2 The Type AST (shared between Haskell and Agda)

```haskell
-- Haskell version
data TypeExpr
  = TUnit
  | TBool
  | TRef String           -- reference to library type by name
  | TArrow TypeExpr TypeExpr
  | TProd TypeExpr TypeExpr
  | TCoprod TypeExpr TypeExpr
  | TId TypeExpr           -- identity type (self-loop)
  | TOmega TypeExpr        -- loop space
  | TSusp TypeExpr         -- suspension
  | TTrunc Int TypeExpr    -- n-truncation
  | THIT Int [Int]         -- HIT with k points and paths of given dimensions
  | TFiber TypeExpr TypeExpr  -- fiber of a map
  deriving (Eq, Ord, Show)

complexity :: TypeExpr -> Int
complexity TUnit = 1
complexity TBool = 1
complexity (TRef _) = 1
complexity (TArrow a b) = 1 + complexity a + complexity b
complexity (TProd a b) = 1 + complexity a + complexity b
complexity (TId a) = 1 + complexity a
complexity (TOmega a) = 1 + complexity a
complexity (TSusp a) = 1 + complexity a
complexity (TTrunc _ a) = 2 + complexity a
complexity (THIT pts paths) = 1 + pts + sum paths
complexity (TFiber a b) = 1 + complexity a + complexity b
```

### 4.3 The Inhabitation Heuristic

```haskell
data InhabResult = Yes Witness | No Reason | Unknown

-- The key heuristic rules:
checkInhab :: TypeExpr -> Library -> InhabResult

-- Rule 1: Library types with constructors are inhabited
checkInhab (TRef name) lib
  | hasConstructor name lib = Yes (Constructor name)

-- Rule 2: Unit is always inhabited
checkInhab TUnit _ = Yes TT

-- Rule 3: A → B is inhabited if B is inhabited (const function)
checkInhab (TArrow _ b) lib = case checkInhab b lib of
  Yes w -> Yes (Const w)
  _     -> Unknown  -- might still be inhabited via other means

-- Rule 4: A × B is inhabited if both A and B are inhabited
checkInhab (TProd a b) lib = case (checkInhab a lib, checkInhab b lib) of
  (Yes wa, Yes wb) -> Yes (Pair wa wb)
  _                -> Unknown

-- Rule 5: Id_X is inhabited if X has a constructor (refl at that point)
checkInhab (TId x) lib = case checkInhab x lib of
  Yes w -> Yes (Refl w)
  _     -> Unknown

-- Rule 6: Ω(X) is inhabited if X has a non-trivial loop
checkInhab (TOmega x) lib
  | hasLoop x lib = Yes (Loop x)
  | otherwise     = Unknown

-- Rule 7: Susp(X) is inhabited if X is inhabited (north pole)
checkInhab (TSusp x) lib = case checkInhab x lib of
  Yes _ -> Yes North
  _     -> Unknown

-- Rule 8: Fiber(f, b) — hard in general
checkInhab (TFiber _ _) _ = Unknown
```

**Important:** This is deliberately conservative. Returning Unknown is always safe;
it just means we undercount ν. As we understand more, we can add rules.

### 4.4 The Derivability Checker (for Clustering)

```haskell
-- Check if T₂ is derivable from T₁ using library operations
-- i.e., is (T₁ → T₂) inhabited using only L?
derivable :: TypeExpr -> TypeExpr -> Library -> Bool
derivable t1 t2 lib = case checkInhab (TArrow t1 t2) lib of
  Yes _ -> True
  _     -> False

-- Cluster newly inhabited types by derivability
cluster :: [TypeExpr] -> Library -> [[TypeExpr]]
cluster newTypes lib = connectedComponents graph
  where
    -- Edge if either direction is derivable
    graph = [ (t1, t2)
            | t1 <- newTypes, t2 <- newTypes, t1 /= t2
            , derivable t1 t2 lib || derivable t2 t1 lib
            ]

-- Filter out trivial clusters (types derivable from any inhabited type)
-- E.g., X → 1 is always derivable from X's existence
nonTrivialClusters :: [[TypeExpr]] -> Library -> [[TypeExpr]]
nonTrivialClusters clusters lib =
  filter (not . isTrivialCluster lib) clusters

-- ν = number of non-trivial clusters
proofRank :: TypeExpr -> Library -> Int
proofRank newType lib =
  let newThms = enumerateNewInhabitants newType lib 2  -- depth ≤ 2
      clusters = cluster newThms lib
  in length (nonTrivialClusters clusters lib)
```

**The key design decision:** What counts as "trivial"? A cluster containing only
types of the form (X → 1), (1 → X), (X × 1) etc. is trivial — these exist for
any new type. A cluster containing Ω(X) or (x : X) → P(x) is non-trivial — these
require X's specific structure.

### 4.4 The Kolmogorov κ Computation

```haskell
-- Generate all programs up to cost c that produce a given type
allPrograms :: Library -> Int -> TypeExpr -> [TypeProgram]

-- κ is the minimum cost
kolmogorovKappa :: Library -> TypeExpr -> Int
kolmogorovKappa lib target = minimum
  [ programCost p
  | c <- [1..maxCost]
  , p <- allPrograms lib c target
  ]
```

For small libraries and bounded cost, this is feasible.

### 4.5 The Selection Loop

```haskell
selectionLoop :: Int -> IO [SelectionResult]
selectionLoop maxSteps = go emptyLibrary 1 where
  go lib n
    | n > maxSteps = return []
    | otherwise = do
        -- Generate candidates
        let candidates = allTypes lib maxProgramCost
        -- Evaluate each
        let scored = [(x, kappa x lib, nu x lib) | x <- candidates]
        -- Compute bar
        let bar = selectionBar n lib
        -- Find winner
        let viable = filter (\(_, k, v) -> v % k >= bar) scored
        case viable of
          [] -> do
            putStrLn $ "HALT at step " ++ show n ++ " (bar = " ++ show bar ++ ")"
            return []
          xs -> do
            let winner = maximumBy (comparing (\(_, k, v) -> v % k)) xs
            let (typ, k, v) = winner
            putStrLn $ showResult n typ k v bar
            rest <- go (addToLibrary typ lib) (n + 1)
            return (winner : rest)
```

### 4.6 Integration with the Current Agda Work

The existing Agda implementation already has reflection-based κ and ν macros, but
these are constructor-counting surrogates. The proof-rank and Kolmogorov programs
should be implemented **outside Agda** and then checked by Agda. A practical
interface plan:

- **Library export:** Maintain a simple JSON/YAML manifest of the current library
  in `agda/` listing types, constructors, and a small set of algebraic operations
  (→, ×, Σ, Id, truncation, suspension). The Haskell engine reads this manifest
  to build `Library`.
- **Witness logging:** When the Haskell engine claims inhabitation of a candidate
  type, it should emit a proof sketch (e.g., "const constructor", "pair of
  witnesses") that can be translated into an Agda term for verification.
- **Result ingestion:** Store `(κ, ν, ρ)` results for each candidate in a table
  and import into Agda as a data module. Agda checks the claims but does not
  perform enumeration.

This keeps enumeration and clustering fast while preserving Agda as the trusted
checker for all claims that are used in proofs.

### 4.7 Immediate Next Steps (for the Engine)

1. Implement a small Haskell prototype that enumerates depth-2 types for a toy
   library (Unit, Bool, Π/Σ) and computes proof-rank ν by clustering.
2. Validate that the prototype reproduces the pencil-calculated ν for Π/Σ and
   S¹ (expecting the 5–6 cluster result).
3. Add a JSON manifest format and a tiny translator that can read the Agda
   library inventory into the Haskell engine.
4. Define a minimal witness format so that the Haskell engine can explain
   its inhabitation decisions to the Agda checker.

---

## Part 5: Specific Research Questions

### Q1: Does Kolmogorov κ match paper κ?

**Prediction:** They'll diverge for types that come with "extra structure" beyond
their bare definition. S³ is the sharpest test case (paper κ=5 vs. likely Kolmogorov
κ=2).

**If they diverge:** Compute selection dynamics with both. See which produces a
viable sequence. The one that works is the "right" definition.

**Deeper question:** Maybe κ should measure the *joint* complexity of the type and
its key properties. K(S³, group structure | L) > K(S³ | L). This would mean κ is
not just "type definition cost" but "useful type definition cost" — defining the type
together with enough structure to make it usable.

This connects to the "logical depth" idea: S³ is cheap to *define* but expensive
to *use* (you need the group structure, which takes work to establish). Maybe:

```
κ_effective(X | L) = K(X | L) + K(key_properties(X) | L, X)
```

This is computable if "key_properties" is bounded.

### Q2: Does proof-rank ν match paper ν?

**Prediction:** It will match within ±20% for R1-R6 and undercount by ~25-35% for
R7+ due to "latent capabilities" (proof clusters that become actual only when
later structures arrive). The critical test: does it preserve the *ordering*?
If proof-rank says ν(Π/Σ) < ν(S¹) < ν(PropTrunc), matching the Genesis ordering
(5 < 7 < 8), the measure is correct even if absolute values differ.

**The S¹ gap:** The pencil calculation shows ν ≈ 5-6 for S¹ vs. Genesis ν = 7.
If this gap is consistent across all geometric types (always ~15% under), it's a
systematic effect of the depth-2 window and can be characterized precisely.

### Q3: Is the d=2 bound sufficient?

The Complexity Scaling Theorem predicts d=2. The pencil calculation confirms it
for R4-R6: all substantive novelty appears at depth ≤ 2.

**Test:** Compute proof-rank at d=2 AND d=3 for R1-R10. If d=3 captures
significantly more clusters than d=2, the Complexity Scaling Theorem's prediction
is wrong (or its applicability to novelty is looser than to cost). If d=2 and d=3
give essentially the same clusters, the theorem governs both cost and novelty.

### Q4: Does the machine sequence match the Genesis Sequence?

Run the full selection loop (Experiment 4). The answer is probably "partially."
The interesting part is characterizing the divergences:

- **Substitutions:** Machine picks Bool where Genesis picks S¹ → the machine
  doesn't see S¹'s homotopical value
- **Reorderings:** Machine picks S² before PropTrunc → the machine sees more
  geometric novelty than logical novelty at that point
- **Premature halt:** Machine can't clear Bar at step 8 → the complexity horizon
  is too shallow to see S³'s value

Each divergence type has a different implication for the theory.

### Q5: Does ρ² give better physical predictions than ρ?

The Born rule interpretation predicts that physical observables should depend on
ρ² = (ν/κ)², not ρ = ν/κ. Paper 5 derives coupling constants and cosmological
parameters from the Genesis Sequence. Audit these derivations:

- Everywhere "efficiency" appears in a formula, does it enter as ρ or ρ²?
- If the existing formulas use ρ linearly but should use ρ², the predicted
  values change. Recompute and check against observations.
- If ρ² gives better agreement with measured constants, that's direct evidence
  for the Born rule interpretation at the foundational level.

**The deeper test:** The Genesis Sequence's ordering is the same under ρ and ρ²
(squaring preserves order). But the *relative magnitudes* change. Under ρ², the
gap between a ν=7 structure and a ν=2 structure is 49:4, not 7:2. This changes
the Bar dynamics quantitatively (though not qualitatively). Run the selection loop
with both and check if ρ² produces a tighter, more constrained sequence.

### Q6: Does proof-rank connect to the number of "independent amplitudes"?

The Born rule analogy becomes precise if each proof cluster corresponds to an
independent amplitude in the quantum-mechanical sense. In QM:

```
P(event) = |Σᵢ aᵢ|² = Σᵢⱼ aᵢ aⱼ*
```

For PEN, if each cluster contributes amplitude 1/√κ:

```
ρ² = (ν/κ)² = (ν · (1/κ))² = |Σᵢ₌₁ⁿ (1/κ)|²  where n = ν
```

The independent proof clusters are the "Feynman paths" of mathematical structure.
Each cluster is an independent way the structure connects to the library, just as
each Feynman path is an independent way a particle connects initial to final state.

**Test:** Can we assign phases to proof clusters (not just amplitudes)? If some
clusters "destructively interfere" (represent mutually exclusive proof strategies),
ν should count the NET rank after interference, not the gross count. Check whether
any Genesis ν values show evidence of cancellation.

---

## Part 6: Practical Schedule

### Week 1-2: The Haskell Engine

- Define TypeExpr and TypeProgram ASTs
- Implement complexity measure (depth bound = 2, fixed)
- Implement inhabitation heuristic (start with rules 1-8)
- Implement type enumeration at depth ≤ 2
- Implement Kolmogorov κ computation (shortest program search)
- Test on R1-R4 (Universe, Unit, Witness, Π/Σ)

### Week 3-4: Proof-Rank Computation

- Implement derivability checker (is T₁ → T₂ inhabited from L alone?)
- Implement clustering (connected components of derivability graph)
- Implement trivial-cluster filter
- Compute proof-rank ν for R1-R10
- Compare to Genesis ν values (ordering preservation, absolute errors)
- Key test: does ordering ν(Π/Σ) < ν(S¹) < ν(PropTrunc) hold?
- Sanity check: also compute at d=3 to confirm d=2 sufficiency

### Week 5-6: Cross-Validation and Born Rule Audit

- Compute (Kolmogorov κ, proof-rank ν) for all 16 types
- Check selection dynamics: does ρ clear Bar?
- Compare four (κ, ν) combinations (paper/paper, paper/rank, kolm/paper, kolm/rank)
- Audit Paper 5's physical predictions: identify where ρ vs ρ² matters
- If predictions differ: recompute with ρ², compare to observed constants
- Identify which combination produces viable dynamics
- Plot the Pareto frontier (Experiment 5)

### Week 7-8: The Selection Loop

- Implement the full selection loop (Experiment 4)
- Run with maxProgramCost = 10, k = 4
- Record the trace
- Compare to Genesis Sequence
- Characterize divergences
- Run sensitivity analysis (vary maxProgramCost and k)

### Week 9-10: Analysis and Write-Up

- Synthesize findings across all experiments
- Determine whether proof-rank ν is the right definition
- Assess Born rule interpretation: does ρ² improve physical predictions?
- If successful: draft revised Paper 1 with computable (κ, ν) and amplitude ρ
- If partially successful: document precise boundary of computability
- If failed: document why and what it implies about mathematical novelty
- In all cases: document the d=2 sufficiency result and Born rule connection

---

## Part 7: What Success Looks Like

### Minimum viable outcome

The Haskell engine produces proof-rank ν values that preserve the **ordering** of
the Genesis Sequence for R1-R10, even if absolute values differ by ~20-30%.
Combined with Kolmogorov κ, the selection dynamics ρ ≥ Bar hold for at least
10 steps. The d=2 bound is confirmed as sufficient.

This would justify: "PEN with information-theoretic measures and fixed depth d=2
produces a viable mathematical evolution sequence consistent with the known hierarchy."

### Good outcome

Proof-rank ν matches Genesis ν within ±15% for R1-R10 and within ±30% for R11-R16.
The selection loop produces a recognizable mathematical sequence. The Born rule
interpretation (ρ²) doesn't break anything and may improve physical predictions.

This would justify: "We have found a computable definition of mathematical novelty
as proof-rank, grounded in the same d=2 parameter that produces Fibonacci timing."

### Great outcome

The selection loop, running blind, produces S¹ before S², group structures before
bundles. The machine sequence is recognizably "the curriculum of HoTT." The Born
rule audit of Paper 5 reveals that ρ² gives better agreement with observed physical
constants than ρ.

This would be: a computational model of mathematical discovery whose foundational
selection principle (Born rule) is self-consistently reflected in its physical
predictions (quantum mechanics).

### Extraordinary outcome

The proof-rank computation reveals that the Genesis ν values are not hand-tuned
but are the UNIQUE output of the clustering algorithm at d=2. The Born rule
interpretation produces a specific numerical prediction (e.g., a corrected coupling
constant) that can be checked against experiment. The machine generates an
unexpected type at some step that turns out to be mathematically interesting.

This would be: the framework teaching us both mathematics and physics.

### The outcome that unifies the papers

The proof-rank ν exactly matches Genesis for R1-R10. The Born rule ρ² gives
the right physical constants in Paper 5. The d=2 parameter simultaneously
determines: Fibonacci timing (Paper 1), depth-2 novelty horizon (this work),
the Born rule (Paper 5), and the gauge group structure (Paper 4).

One parameter. Four consequences. That would be PEN.

> **The universe instantiates the mathematical structures that maximize the
> amplitude ν/κ of independent proof paths per bit of description, subject to
> Fibonacci-timed integration at depth d=2. The realization probability is the
> Born rule: P ∝ (ν/κ)².**
