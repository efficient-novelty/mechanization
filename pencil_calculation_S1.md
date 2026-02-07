# Pencil Calculation: Does Bounded Enumeration Give ν = 7 for S¹?

## Setup

**Library at step 4:** L₄ = {U₀, 1, ★ : 1, Π, Σ, Id}

**Adding:** X = S¹ with constructors:
- base : S¹ (point constructor)
- loop : base =_{S¹} base (path constructor)
- elim : for any P : S¹ → U, given b : P(base) and l : transport(loop, b) = b,
  we get (x : S¹) → P(x)

**2-step window:** R₄ = Π/Σ, R₃ = ★

**Method:** Enumerate all types of bounded expression depth, check which are
newly inhabited (inhabited in L₄ ∪ {S¹} but not in L₄). Try to recover ν = 7.

---

## The Enumeration

I'll classify by what *witness* inhabits the type, since that determines whether
the inhabitation is trivial (would work for any new type) or essential (uses
S¹'s specific structure).

### Class T: Trivially new types

These are types that become inhabited for ANY new type A with a point constructor.
They tell us nothing specific about S¹.

| # | Type | Witness | Why trivial |
|---|------|---------|-------------|
| T1 | S¹ | base | Any type with a constructor is inhabited |
| T2 | 1 → S¹ | λ _ → base | 1 → A exists for any inhabited A |
| T3 | S¹ → 1 | λ _ → ★ | A → 1 exists for any A |
| T4 | S¹ → U₀ | λ _ → 1 | Constant family; works for any A |
| T5 | S¹ × 1 | (base, ★) | A × 1 for any inhabited A |
| T6 | S¹ × S¹ | (base, base) | A × A for any inhabited A |
| T7 | Σ(x:S¹).1 | (base, ★) | Isomorphic to S¹ |
| T8 | (x:S¹) → 1 | λ _ → ★ | Same as S¹ → 1 |
| T9 | S¹ → S¹ | id | A → A exists for any A |

**Count: 9 trivially new types** (and there are more at higher depth).

Every one of these uses only `base` as a witness. None uses `loop`.

### Class E: Essentially new types

These are types whose inhabitation **depends on S¹ having a path constructor**.
Their witness requires `loop`, not just `base`.

**E1: base =_{S¹} base**

Witness: `loop` (the path constructor itself).

This is the single most important new type. It's the first non-trivial identity
type in the whole library. In L₄, every identity type a =_A a was inhabited
only by `refl`. Now we have a *non-reflexivity* path.

Status: **ESSENTIALLY NEW** ✓

**E2: (x : S¹) → P(x) for non-constant P**

Example: Let P(x) = (base =_{S¹} x). Then:
- P(base) = (base =_{S¹} base), which is inhabited by loop.
- But transport_loop : P(base) → P(base) must equal the identity.
- This fails for the universal cover — (x : S¹) → (base = x) is NOT inhabited.

Better example: Let P(base) = A (some library type) with transport_loop = id_A.
Then (x : S¹) → P(x) IS inhabited by (λ x → const), i.e., constant sections.

Wait — that's just the constant function, which is trivial (Class T).

The ESSENTIALLY new case: P such that transport_loop ≠ id. For this, we need
A to have a non-trivial automorphism, and P encodes it. With library L₄, the
only types are 1 and U₀:
- 1 has only trivial automorphisms (id)
- U₀ has type equivalences as automorphisms

So the essentially new dependent types over S¹ exist in principle (e.g., the
universal cover, Z-indexed families) but we can't fully construct them until
we have Bool, ℤ, etc.

However, the SCHEMA of "dependent elimination that requires a transport datum"
is new. In L₄, every dependent function (x : 1) → P(x) reduces to P(★) with
no transport obligation. Over S¹, the elimination principle requires BOTH a
base case AND a loop coherence. This is a qualitatively new proof obligation.

Status: **ESSENTIALLY NEW** ✓ (the proof *pattern* is new, even if the
interesting instances require more library types)

**E3: Ω(S¹) → Ω(S¹) (endomorphisms of the loop space)**

Type: (base =_{S¹} base) → (base =_{S¹} base)

Witness: id, (λ p → p⁻¹), (λ p → p · p), etc.

These exist because we have a non-trivial loop space. In L₄, the only identity
types were (★ =_1 ★), whose endomorphisms are all trivial (just id).

But wait — is this type "newly inhabited"? In L₄, (★ =_1 ★) → (★ =_1 ★) IS
inhabited by id. So "endomorphisms of a path space" already existed (trivially).
What's new is that the endomorphism space is RICH (it's essentially ℤ → ℤ via
winding numbers).

If we're just checking "inhabited or not," this is NOT essentially new (it was
already inhabited by id for the trivial loop space).

Status: **NOT essentially new** (inhabited trivially) — but the RICHNESS is new.

**E4: loop =_{base=base} loop (self-identity of loop)**

Type: loop =_{(base =_{S¹} base)} loop

Witness: refl

This IS inhabited (refl : loop = loop). But it would be inhabited for ANY path
in any identity type. Not essentially new.

Status: **TRIVIALLY NEW** (refl at a new path)

**E5: loop =_{base=base} refl (comparing loop to refl)**

Type: loop =_{(base =_{S¹} base)} refl

NOT inhabited — this is the statement that loop = refl, which is FALSE.
(This is essentially the content of π₁(S¹) ≅ ℤ.)

Status: **NOT inhabited** — but this non-inhabitation IS informative!

**E6: ΣS¹ (suspension of S¹)**

If suspension is available as an operation, ΣS¹ gives us S².

But suspension is not a primitive in L₄. It requires pushouts (higher inductive
types). We're asking: can you DEFINE the suspension using L₄ ∪ {S¹}?

Answer: Not without a new HIT constructor. Suspension is:
ΣX = pushout(X ← 1 → 1)
which requires the pushout type former, not available in L₄.

Status: **NOT constructible** at this step. Would be a future realization.

**E7: Ω(S¹) has concatenation structure**

Type: (base = base) → (base = base) → (base = base)

Witness: path concatenation (p · q)

Is this newly inhabited? In L₄, the type
(★ =_1 ★) → (★ =_1 ★) → (★ =_1 ★)
IS inhabited (by λ p q → refl, since all paths in 1 are refl).

But path concatenation in S¹ is *non-trivial* — it gives a group structure.

Again, the TYPE is not essentially new (it was trivially inhabited before).
The RICHNESS is new.

Status: **NOT essentially new** as a type — but new as a structure.

---

## The Problem

Counting "newly inhabited types" directly gives:
- ~9 trivially new (would be new for any new type)
- ~1-2 essentially new (E1, E2)
- Many more trivially new at depth 2 (easily 20+)

**The raw count does NOT give 7.** It gives either ~2 (essentially new only)
or ~30+ (all newly inhabited types).

---

## What the Genesis ν = 7 Actually Counts

Going back to the OpSchema analysis, the 7 items were:

1. **S¹ exists** — a new type (trivially new, but still counts)
2. **Non-trivial loop** — base =_{S¹} base inhabited by loop (E1)
3. **S¹ → S¹ non-trivially** — the map space is rich (new structure)
4. **Dependent elimination** — transport along loop (E2)
5. **Loop space as type** — Ω(S¹) as an algebraic object
6. **π₁(S¹) ≅ ℤ** — the fundamental group computation
7. **Suspension template** — ΣS¹ as a pattern for S²

Items 1, 2, 4 are newly inhabited types.
Items 3, 5 are existing types with NEW STRUCTURE (the type was inhabited before,
but now it's inhabited in a richer way).
Items 6, 7 are THEOREMS or CONSTRUCTIONS, not types.

**The Genesis ν values count a mixture of:**
- New inhabited types (~3)
- Existing types with enriched structure (~2)
- New theorems and constructions (~2)

This is NOT the same as "count of newly inhabited types at bounded depth."
The OpSchema grammar captures this heterogeneous mix; raw enumeration doesn't.

---

## The Window Hypothesis

Halvor's intuition: "we never need more than depth 1 and 2 due to the
Complexity Scaling Theorem."

Let me check whether the 7 items fall within a 2-step window:

| # | Item | Uses R₄ (Π/Σ)? | Uses R₃ (★)? | Uses older? |
|---|------|-----------------|---------------|-------------|
| 1 | S¹ exists | No | No | Just S¹ |
| 2 | Non-trivial loop | No | No | Just S¹ |
| 3 | S¹ → S¹ rich | Yes (→) | No | Just S¹ |
| 4 | Dep elimination | Yes (Π) | Yes (★ as base case) | No |
| 5 | Ω(S¹) as algebra | Yes (→) | No | Uses Id |
| 6 | π₁(S¹) ≅ ℤ | Yes (Π, Σ) | Yes (terms) | Needs ℤ! |
| 7 | Suspension | Yes (Σ, pushout) | Yes (points) | Needs pushouts! |

**Observation:** Items 1-5 live within the 2-step window {R₄, R₃}. Items 6-7
reach forward or sideways (they need structures not yet in the library).

This partially supports the window hypothesis: the CORE novelty (items 1-5)
lives within 2 steps, but the FULL novelty (items 6-7) anticipates future
structures. The Genesis table may be counting the "potential" novelty —
capabilities that become actual once later realizations arrive.

---

## Conclusions

### 1. Raw bounded enumeration doesn't give 7

Counting newly inhabited types at depth ≤ 2 gives either ~2 (strict essential)
or ~30+ (everything). The number 7 requires a quotient that distinguishes
qualitative capabilities, which is what OpSchema does but which isn't a
natural consequence of pure enumeration.

### 2. The 2-step window partially works

5 of the 7 items live within {S¹, Π/Σ, ★}. The remaining 2 reference future
structures. So the window captures ~70% of the novelty.

### 3. The Genesis ν counts heterogeneous things

New types, enriched structures, and anticipated constructions are mixed.
No single homogeneous counting method (just types, just theorems, just
operations) reproduces the values.

### 4. What this means for the information-theoretic approach

**The counting target is wrong.** We shouldn't count "newly inhabited types."
We should count something like "independent dimensions of the space of new
proofs." The OpSchema categories (EXIST, PATH, MAP, DEP-ELIM, LOOP, π₁,
SUSP) are independent dimensions — each represents a qualitatively different
proof technique that S¹ enables.

This is closer to counting **generators** of the new proof space, not
counting all proofs. In linear algebra terms: ν counts the RANK of the
new theorem space, not its CARDINALITY.

### 5. A refined information-theoretic proposal

Perhaps:

```
ν(X | L) = rank of the "novelty module" generated by X over L
```

where the "novelty module" is the set of new proof schemas, and "rank" is
the number of independent generators.

Operationally: two new theorems T₁ and T₂ are "dependent" if T₂ can be
derived from T₁ using only library operations. They're "independent" if
neither can be derived from the other.

```
ν = |maximal independent set of newly provable theorems|
```

This is the number of NEW PROOF TECHNIQUES, not new proofs. It's the
dimension of the advance, not the volume.

### 6. Connection to Shannon entropy

If the new theorems cluster into k independent groups, and within each group
there are multiple derivatively-related theorems, then:

- Raw count = total theorems (too many)
- Entropy ≈ log(k independent groups) (too few)
- Rank = k independent generators (might be right)

For S¹: k = 7 independent proof techniques. Within each, there are many
derived consequences (e.g., "loop space" generates all theorems about
Ω(S¹)). The rank captures what the OpSchema grammar captures: one
count per independent capability.

---

## The Honest Summary

The calculation shows that ν = 7 does NOT fall out of simple bounded
enumeration at any depth. The Genesis ν values encode a more nuanced
quantity — roughly, the rank of the space of new proof techniques.

**For the Haskell engine, this means:** Don't count newly inhabited types.
Count CLUSTERS of newly inhabited types, where types in the same cluster
are derivably related. Each cluster = one unit of novelty.

Or equivalently: enumerate newly inhabited types, then quotient by
derivability, and count equivalence classes. The number of classes is ν.

This is harder to implement but it's the honest answer to what ν
measures. And it might still be computable — the derivability relation
between types at bounded depth should be decidable.
