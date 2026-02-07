# PEN Information-Theoretic Research Plan
# κ as Kolmogorov Complexity, ν as Shannon Surprise, and the Selection Loop

## Why This Matters

The Phase 3b results revealed a puzzle: the OpSchema grammar matches ν for R1-R10
by hand but the machine implementation undercounts for complex types because it lacks
the conceptual vocabulary. We proposed "vocabulary co-evolution" as the fix — but that
just names the problem. It doesn't solve it.

Halvor's original intuition offers something better: a **principled, vocabulary-free**
definition of both κ and ν grounded in information theory. If κ ≈ Kolmogorov complexity
and ν ≈ Shannon surprise, then neither measure requires a hand-designed grammar.
Both are defined relative to the library and a universal computation model.

The research question: **Can we reformulate PEN in information-theoretic terms,
approximate the resulting measures computably in Agda, and does the reformulated
model produce viable selection dynamics?**

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


### 1.2 ν as Shannon Surprise

**The idea:** ν(X | L) measures how much X changes the distribution of provable
statements. The more unexpected the consequences of X, the more novel it is.

**Why this resolves the vocabulary problem:**

The OpSchema grammar required us to design categories of operations and argue
about which are "qualitatively distinct." Shannon surprise replaces all of that
judgment with a single quantity: **how unlikely were the new consequences of X,
given what was known from L?**

A type whose consequences were all "almost provable" from L has low surprise.
A type that enables wildly unexpected new theorems has high surprise. No grammar
needed — just a probability model and a way to enumerate consequences.

**Formal definition (ideal):**

Let Thm(L) be the set of provable type-judgments (Γ ⊢ t : T) using library L.
Define a "prior" distribution P_L over potential theorems, based on their syntactic
complexity:

```
P_L(t : T) ∝ 2^{-|T|}    (shorter types are more likely a priori)
```

Then:
```
ν(X | L) = Σ_{(t:T) ∈ Thm(L∪{X}) \ Thm(L)} surprise(T)
         = Σ_{new theorems} -log P_L(T)
         = Σ_{new theorems} |T|    (up to constant, with the 2^{-|T|} prior)
```

**Translation:** Novelty is the total description complexity of the new theorems
that X enables.

This is elegant: a type that enables many short, unexpected theorems is more novel
than one that enables one very long theorem. And a type that enables theorems that
were "almost provable" anyway (short descriptions, high prior probability) contributes
less than one that opens genuinely new territory.

**But wait — this is still infinite.** Thm(L∪{X}) \ Thm(L) is generally infinite.
We need a finite approximation.


### 1.3 The Bounded Surprise Approximation

**Practical definition:**

Fix a complexity bound k. Enumerate all types T of syntactic complexity ≤ k
formable from L ∪ {X}. For each, check (approximately) whether:
1. T is inhabited in L ∪ {X} (a new theorem exists)
2. T was NOT inhabited in L (the theorem is genuinely new)

Then:
```
ν_k(X | L) = Σ_{T : new, |T| ≤ k} w(T)
```

where the weight w(T) captures the "surprise" of T.

**Three candidate weight functions:**

**Weight A — Uniform (count new inhabited types):**
```
w(T) = 1 for all T
```
This is just counting new inhabited types up to complexity k. Simple but doesn't
distinguish trivial from deep theorems.

**Weight B — Complexity-weighted (Shannon-style):**
```
w(T) = |T|    (the syntactic size of the type)
```
Longer types (more complex theorems) contribute more surprise.
Rationale: under the 2^{-|T|} prior, surprise = |T| · log 2.

**Weight C — Rarity-weighted:**
```
w(T) = 1 / (number of types at the same complexity level that are also newly inhabited)
```
If adding X makes many types at complexity level k inhabitable, each one is less
surprising. If it makes only one type at that level inhabitable, it's very surprising.

**Weight D — Depth-weighted (Bennett-inspired):**
```
w(T) = depth(T)
```
where depth(T) is the minimum derivation length needed to establish T's
inhabitation from L ∪ {X}. A consequence provable in one step (e.g., "S¹ is
inhabited" follows directly from its constructor) contributes less than a
consequence requiring a chain of reasoning (e.g., "π₁(S¹) ≅ ℤ" requires
the loop space, winding number construction, and group isomorphism).

This captures Bennett's logical depth insight: the *value* of a mathematical
structure lies not in its shallow consequences but in its deep ones. A type
whose interesting properties require elaborate proofs is "deeper" than one
whose properties are immediate.

Operationally, depth(T) ≈ the number of intermediate types needed in the
derivation. In the Haskell engine, this is the length of the witness term
produced by the inhabitation checker. Short witnesses → shallow consequences.
Long witnesses → deep consequences.

Note that depth-weighting naturally resolves a puzzle from Phase 3b: type
formers (Π, Σ) enable many types but most are *shallow* (just combining
existing types). S¹ enables fewer types but they're *deep* (loop spaces,
fundamental groups). Depth-weighting suppresses the former and amplifies
the latter, which is exactly the behavior we need.

**My recommendation: start with Weight A (counting) and Weight B (complexity-weighted).
If those don't match, try Weight D (depth-weighted) before giving up.
Weight C is interesting but harder to compute.**


### 1.4 The Reformulated Efficiency

Putting it together:

```
ρ(X | L) = ν_k(X | L) / κ(X | L)
          = (total surprise of new theorems up to complexity k)
            / (Kolmogorov complexity of X given L)
```

**In words:** Efficiency is surprise-per-bit-of-description. The system selects
structures that deliver the most unexpected consequences for the least definitional
investment.

**The Selection Bar remains Fibonacci-timed** (proven in Phase 1). The
information-theoretic reformulation changes the *measures* but not the *dynamics*.


### 1.5a The Delta-Ratio Equivalence

An important structural observation: one might ask whether PEN should maximize
the ratio ν/κ or the delta ν - λ·κ. The ratio can be "hacked" by trivial
structures with tiny κ; the delta directly rewards absolute impact.

The existing PEN dynamics already resolve this. The selection rule is:

```
ρ(X) = ν/κ ≥ Bar(n) = Φ(n) · Ω(n-1)
```

Rearranging: ν - Bar(n) · κ ≥ 0. This IS a delta form with a time-dependent
penalty λ(n) = Bar(n) that rises on the Fibonacci schedule. Among candidates
clearing this threshold, PEN selects the ratio-maximizer.

So PEN naturally combines both: the delta ensures absolute novelty scales with
the rising bar (filtering out trivially cheap structures), while the ratio
selects the most efficient within the viable set. The Bar prevents noise
(high ν, high κ — wasteful) and the ratio prevents triviality (low ν, low κ —
boring). The "sweet spot" — high ν, low κ — is exactly what survives.

In information-theoretic terms: PEN selects for maximum **surprise per bit
of description**, subject to a minimum **absolute surprise** that grows over
time. This is a well-posed optimization that doesn't need additional correction.


### 1.5 Why This Might Resolve the Vocabulary Problem

The key insight: **the bounded theorem enumeration automatically discovers the
vocabulary.**

When we enumerate types of complexity ≤ k after adding S¹, we will automatically
find:
- base =_{S¹} base (loop — which IS the "loop space" concept)
- (x : S¹) → P(x) (dependent elimination — which IS "transport")
- ‖Ω(S¹)‖₀ (which IS "π₁" once truncation exists)

We don't need to pre-specify LOOP-SPACE, DEP-ELIM, or HOMOTOPY-GROUP as
schema categories. They emerge from the enumeration. The "vocabulary" is just
the set of short types that become inhabited.

**This is the crucial advantage over OpSchema:** the grammar writes itself.

**The cost:** Enumeration up to complexity k is expensive. For k=5 and a library
of 10 types, the number of formable types might be in the thousands. But this is
a computational cost, not a conceptual one — and it's exactly the kind of thing
Agda (or a Haskell/Python helper) can do.

---

## Part 2: Connecting to Known Information Theory

### 2.0 Notes from External Review (Gemini Deepthink)

An external review raised several points. Two were incorporated:

1. **Delta vs. Ratio:** The concern that ρ = ν/κ can be gamed by trivial
   structures is valid, but the existing Bar mechanism already provides the
   delta correction. See Section 1.5a above. No change to the formalism needed,
   but worth making explicit in any paper.

2. **Bennett's Logical Depth:** The observation that Kolmogorov complexity
   measures description length but not derivation effort. This motivated
   Weight D (depth-weighted novelty) in Section 1.3, where we weight newly
   inhabited types by the length of their inhabitation witness. This is our
   operationalization of logical depth within the bounded enumeration framework.

Two other suggestions were evaluated and set aside:

3. **Compression proxies (gzip/LZW):** Not applicable. Our type definitions
   are tiny ASTs (3-10 nodes). The TypeProgram cost in the Haskell engine IS
   the description length directly. Compression ratios are meaningless at this
   scale.

4. **Cellular automata experiment:** Interesting in its own right but orthogonal
   to our project. We're measuring novelty of mathematical structures in type
   theory, not searching for Life-like patterns in binary grids. The connection
   to PEN would require a separate theoretical bridge that doesn't currently exist.

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

### 2.3 The Logical Depth Connection (Bennett)

Charles Bennett's "logical depth" measures the computational effort required to
derive the interesting consequences of a string, given its shortest description.
A string is "deep" if its shortest program runs for a long time.

In PEN terms:
- A type X has high "depth" if its short description (low κ) produces consequences
  that require extensive computation to derive (high ν)
- The Genesis Sequence should select for "deep" types — ones with low Kolmogorov
  complexity but high logical depth

**Prediction:** If we plot κ vs. ν for the Genesis types, the sequence should
trace the Pareto frontier of the (low κ, high ν) region. Types below the frontier
are "shallow" (easy to define, few consequences). Types above are impossible
(can't get that much novelty from that little definition).

This is testable once we have computable κ and ν!

### 2.4 Algorithmic Mutual Information

The novelty of X given L can also be framed as:

```
ν(X | L) ≈ I(X ; Future | L)
```

where I is the algorithmic mutual information between X and the "future structures"
that X enables. This measures how much knowing X tells you about what comes next.

A type with high mutual information with the future is one that "unlocks" many
subsequent realizations. This is exactly the enabling power interpretation.

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


### Experiment 2: Computable ν via Bounded Theorem Enumeration

**Goal:** Compute ν(X | L) as the count (or weighted sum) of newly inhabited types
up to complexity k, and compare to Genesis ν values.

**Method:**

Step 1: Define the type enumeration (as in Part 1.3).

Step 2: For each enumerated type, check approximate inhabitation.

Step 3: Count newly inhabited types under four weighting schemes:

```
ν_A(X | L, k) = |new types|                          -- raw count
ν_B(X | L, k) = Σ complexity(T) for new types        -- complexity-weighted
ν_C(X | L, k) = Σ 1/cohort_size(T) for new types    -- rarity-weighted
ν_D(X | L, k) = Σ depth(T) for new types             -- depth-weighted
```

Step 4: Compare all four to Genesis ν. Compute correlations, orderings, and
absolute errors. The winner is the weight that best preserves the Genesis ordering
across R1-R10.

**Key prediction for Weight D:** If depth-weighting is correct, then Π/Σ (which
enables many shallow consequences) should have lower ν_D than S¹ (which enables
fewer but deeper consequences). Check whether ν_D(Π/Σ) < ν_D(S¹) matches
the Genesis ordering (5 < 7). If Weight A gives the wrong ordering here but
Weight D gets it right, that's strong evidence for depth-weighting.

**Prediction:** There should be a "natural" value of k where the enumerated ν
approximately matches the Genesis values. If k=3 works for R1-R6 but k=5 is
needed for R7-R10, that tells us something about the effective complexity horizon
of each structure.


### Experiment 3: The Cross-Validation

**Goal:** Determine whether the information-theoretic κ and ν produce viable
selection dynamics (ρ clears Bar for all 16 steps).

**Method:**

For each of the following (κ, ν) pairs:
- (paper κ, paper ν) — the original hand-tuned values
- (Kolmogorov κ, paper ν) — new κ, old ν
- (paper κ, enumerated ν) — old κ, new ν
- (Kolmogorov κ, enumerated ν) — both new

Compute ρ = ν/κ, Bar = Φ · Ω, and check whether ρ ≥ Bar for all 16 steps.

**This is the decisive test.** If (Kolmogorov κ, enumerated ν) produces a viable
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
κ(X) = programCost(X)
ν(X) = ν_enum(X | L, k)   -- bounded theorem enumeration
ρ(X) = ν(X) / κ(X)
```

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

### Q2: Does bounded enumeration ν match paper ν?

**Prediction:** It will match for small k (R1-R6) and undercount for large k (R7+),
but the gap should be smaller than the OpSchema gap because the enumeration
automatically discovers operation categories that the grammar missed.

**Critical test:** S¹ at step 5. If the enumeration at k=4 finds approximately 7
newly inhabited types (loop space, dependent elimination, free loop space...), we're
on track. If it finds significantly fewer, the enumeration depth is too shallow or
the inhabitation heuristic is too conservative.

### Q3: What is the "natural" complexity horizon?

Plot ν(X | L, k) as a function of k for each Genesis type. There should be a
"knee" in the curve where most of the novelty has been captured. The position of
the knee tells us the effective complexity horizon for that type.

**Prediction:** The knee moves rightward (higher k) as types get more complex.
Simple types (Unit, Bool) have their novelty captured at k=2. S¹ needs k=3-4.
Hopf needs k=5-6. DCT might need k=10+.

**If this is true:** The PEN model has a natural notion of "mathematical depth" —
the complexity horizon at which a type's novelty is fully captured. Deep types are
those whose consequences require long chains of reasoning. This connects directly
to Bennett's logical depth.

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

### Q5: Is novelty ≈ entropy of the type's "future light cone"?

An alternative information-theoretic interpretation: ν(X | L) measures the
**entropy** of the distribution of types that X enables.

If X enables many diverse types (high entropy of Thm(L∪{X}) \ Thm(L)),
it's more novel. If X enables many types that are all "similar" (low entropy),
it's less novel even if the count is high.

This could explain why type formers (Π, Σ) have moderate ν despite enabling
infinitely many types: most of those types are "similar" (just different
combinations of existing types). Whereas S¹ enables a smaller number but
more diverse set of types (loop spaces, winding numbers, covering spaces —
all qualitatively different).

```
ν_entropy(X | L, k) = H({ T ∈ Thm(L∪{X}, k) \ Thm(L, k) })
```

where H is the entropy of the distribution (uniform over newly provable types).

If all new types are at the same complexity level: H = log(count). So for
uniform-complexity new types, ν_entropy ≈ log(ν_count). But for types that
enable consequences at many different complexity levels, H > log(ν_count).

**This might explain the "Candidate 5 matches by hand" result:** The OpSchema
categories (EXIST, PATH, MAP, DEP-ELIM, HIGHER, FIBRATION...) are
essentially the *diversity dimensions* of the new theorem space. Counting
categories ≈ counting independent dimensions ≈ measuring entropy.

---

## Part 6: Practical Schedule

### Week 1-2: The Haskell Engine

- Define TypeExpr and TypeProgram ASTs
- Implement complexity measure
- Implement inhabitation heuristic (start with rules 1-7)
- Implement type enumeration up to depth k
- Implement Kolmogorov κ computation (shortest program search)
- Test on R1-R4 (Universe, Unit, Witness, Π/Σ)

### Week 3-4: ν Computation

- Implement bounded theorem enumeration
- Implement "new inhabitant" detection (compare Thm(L∪X) vs Thm(L))
- Implement all four weight functions (A: count, B: complexity, C: rarity, D: depth)
- For depth-weighting: track witness length in inhabitation checker
- Compute ν_A, ν_B, ν_D for R1-R10 with k = 2, 3, 4, 5
- Compare all three to Genesis ν values (correlations, ordering preservation)
- Identify the "natural" k and best weight function for each type
- Key test: does ν_D preserve the Π/Σ < S¹ ordering (5 < 7)?

### Week 5-6: Cross-Validation

- Compute (Kolmogorov κ, enumerated ν) for all 16 types
- Test with best weight function from Week 3-4
- Check selection dynamics: does ρ clear Bar?
- Compare four (κ, ν) combinations (paper/paper, paper/enum, kolm/paper, kolm/enum)
- If no single weight works: test ν_D (depth) which should better capture
  the difference between shallow type-former consequences and deep geometric ones
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

- Synthesize findings
- Determine which information-theoretic formulation works best
- If successful: draft the revised Paper 1 with computable (κ, ν)
- If partially successful: document the precise boundary of computability
- If failed: document why and what it implies about mathematical novelty

---

## Part 7: What Success Looks Like

### Minimum viable outcome

The Haskell engine produces ν values that preserve the **ordering** of the Genesis
Sequence for R1-R10, even if absolute values differ. Combined with Kolmogorov κ,
the selection dynamics ρ ≥ Bar holds for at least 10 steps.

This would justify: "PEN with information-theoretic measures produces a viable
mathematical evolution sequence consistent with the known hierarchy."

### Good outcome

The bounded enumeration ν approximately matches Genesis ν (within ±20%) for
R1-R10 at a natural k. The selection loop produces a sequence recognizable as
"mathematics" (types appear in a logical developmental order).

This would justify: "We have found a computable approximation to mathematical
novelty that explains most of the Genesis Sequence."

### Great outcome

The selection loop, running blind, produces S¹ before S², and group structures
before bundles, and the sequence clearly traces the development of topology and
geometry. The machine "rediscovers" the curriculum of a HoTT textbook.

This would be: a genuine computational model of mathematical discovery.

### Extraordinary outcome

The machine produces a type at some step that is NOT in the Genesis Sequence
but is mathematically interesting — a structure that "should" appear at that
efficiency level but was overlooked. Verifying it by hand reveals a genuine
mathematical insight produced by the optimization.

This would be: the model teaching us mathematics we didn't know.

### The outcome that changes everything about PEN's philosophical claims

The bounded enumeration ν, at the right k, exactly reproduces the Genesis ν
values — revealing that the "semantic" novelty that seemed to require mathematical
judgment is actually just "number of newly inhabited types at complexity horizon k."
The deep-sounding "enabling power" was counting theorems all along.

This would mean: mathematical novelty IS Shannon surprise. The vocabulary
problem dissolves because the vocabulary was always just "short provable types."
The Genesis Sequence is the sequence that maximizes surprise-per-bit on a
Fibonacci schedule. And PEN reduces to a clean information-theoretic principle:

> **The universe instantiates the mathematical structures that maximize
> surprise per bit of description, subject to Fibonacci-timed integration.**

That would be worth a paper.
