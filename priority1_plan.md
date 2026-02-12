# Priority 1: Proving or Refuting the Saturation Assumption

## The Claim

**Saturation Assumption** (Lemma 3.2 in pen_unified.tex, line 222):
> Each integration layer exports exactly as many schemas as its integration cost: |S(L_k)| = Delta_k.

This assumption is the load-bearing foundation for the Complexity Scaling Theorem. It converts the interface basis definition (Def 2.6, line 214):

    I^(d)_n := disjoint union of S(L_{n-j}) for j = 0..d-1

into the Fibonacci recurrence:

    Delta_{n+1} = Delta_n + Delta_{n-1}

If saturation fails, the Fibonacci timing collapses, and the entire quantitative structure of the Genesis table is unfounded.

---

## Current Status

### What exists in Agda

The Cubical Agda mechanization (`agda/ObligationGraph/Recurrence.agda`) proves:

    fibonacci-recurrence : (n : N) -> Delta (3 + n) === Delta (2 + n) + Delta (1 + n)
    golden-schedule : (n : N) -> tau n + 1 === fib (suc n)

But this proof **assumes** saturation as a postulate. The recurrence is proved *conditional on* |S(L_k)| = Delta_k.

### What exists in Haskell

The engine (`engine/src/`) computes Delta_k values for each step using the Fibonacci sequence directly. It does not independently enumerate exported schemas to verify saturation.

### What the paper says

Line 906-907: "The saturation assumption... is justified by the universal property of eliminators in HoTT but not formally proved."

This is the gap. The paper's honesty about it is good, but closing it would transform the framework from "conditional on an assumption" to "proved."

---

## Detailed Plan

### Phase 1A: Define "Exported Schema" Formally (Week 1)

**Goal:** Create a precise, computable definition of S(L_k) -- the set of schemas exported by integration layer L_k.

**What S(L_k) should capture:**
When a structure X_k is sealed against the library B_{k-1}, it creates an integration layer L_k. This layer "exports" schemas -- these are the interface points that subsequent structures can interact with. Concretely:

1. **Elimination schemas**: For each type family Y over X_k, the eliminator produces obligations. Each distinct obligation pattern is a schema.
2. **Transport schemas**: For each path constructor in X_k, transport along that path produces an interface point.
3. **Composition schemas**: For each way X_k composes with existing library types (function types, products, dependent types), there is a schema.

**Deliverable:** An Agda module `agda/Saturation/Schema.agda` with:
```agda
record Schema : Type where
  field
    source : Structure    -- which structure generated this
    target : Structure    -- what it interfaces with
    depth  : Nat          -- obligation depth (0, 1, or 2)

exportedSchemas : (k : Nat) -> Library -> List Schema
```

**Key decision:** The schema definition must be *intrinsic* (derivable from the type-theoretic structure) not *extrinsic* (listed by the researcher). This is what makes it a proof rather than a fit.

### Phase 1B: Enumerate Schemas for Steps 1-4 (Week 1-2)

**Goal:** Compute |S(L_k)| for the bootstrap phase and compare to Delta_k.

These are the simplest cases and will validate the methodology before tackling harder structures.

| Step | Structure      | Delta_k | Expected |S(L_k)| | What to count |
|------|---------------|---------|----------------------|---------------|
| 1    | Universe U_0   | 1       | 1                    | The universe elimination (type case analysis) |
| 2    | Unit 1         | 1       | 1                    | The unit eliminator (trivial) |
| 3    | Witness *      | 2       | 2                    | Function application + pattern match |
| 4    | Pi/Sigma       | 3       | 3                    | Lambda, apply, pair/fst/snd |

**Deliverable:** Agda proofs for each:
```agda
-- In agda/Saturation/Bootstrap.agda
step1-saturation : length (exportedSchemas 1 emptyLib) === 1
step2-saturation : length (exportedSchemas 2 lib1) === 1
step3-saturation : length (exportedSchemas 3 lib2) === 2
step4-saturation : length (exportedSchemas 4 lib3) === 3
```

**Critical check:** If these don't work out, STOP and diagnose before proceeding. The bootstrap phase is where the assumption is most constrained (small numbers, concrete structures). If saturation fails here, it fails everywhere.

### Phase 1C: Enumerate Schemas for Steps 5-8 (Week 2-3)

**Goal:** Compute |S(L_k)| for the geometric ascent phase (HITs).

This is where saturation becomes non-trivial. HITs have path constructors, and the exported schemas include transport obligations.

| Step | Structure  | Delta_k | Key complexity |
|------|-----------|---------|----------------|
| 5    | S^1       | 5       | 1 point + 1 loop -> elimination needs point case + path case + 3 transport schemas |
| 6    | PropTrunc | 8       | Universal property generates multiple interface points |
| 7    | S^2       | 13      | 2-cell introduces surface obligations |
| 8    | S^3       | 21      | SU(2) structure adds group-theoretic schemas |

**Deliverable:** Agda proofs:
```agda
-- In agda/Saturation/Geometric.agda
step5-saturation : length (exportedSchemas 5 lib4) === 5
step6-saturation : length (exportedSchemas 6 lib5) === 8
step7-saturation : length (exportedSchemas 7 lib6) === 13
step8-saturation : length (exportedSchemas 8 lib7) === 21
```

**The hard case:** Step 7 (S^2, Delta = 13) and Step 8 (S^3, Delta = 21) are the real tests. The paper claims these match Fibonacci numbers, which is a strong constraint. If the actual count is, say, 11 or 15 instead of 13, we learn something important.

### Phase 1D: Cross-Validate with the Haskell Engine (Week 2, parallel)

**Goal:** Add an independent schema-counting mode to the Haskell engine.

The engine already has the infrastructure for type enumeration (`Enumerate.hs`), schema computation (`Cluster.hs`), and inhabitation checking (`Inhabitation.hs`). Add a new module:

```haskell
-- engine/src/SaturationCheck.hs
module SaturationCheck where

-- For each step k, enumerate the schemas that L_k exports
-- and compare to Delta_k (= F_k)
checkSaturation :: Int -> Library -> (Int, Int, Bool)
-- Returns (delta_k, |S(L_k)|, match?)
```

This provides an independent check against the Agda computation. If the Haskell engine and Agda agree, confidence is high. If they disagree, the disagreement itself is informative.

**Implementation approach:**
1. After each structure is sealed in the synthesis loop, enumerate all depth-1 type expressions involving the new structure and existing library
2. Abstract to schemas (replace specific library types with generic L)
3. Filter to those that are newly inhabited (not inhabited before this step)
4. Count distinct schemas -- this is |S(L_k)|
5. Compare to Delta_k = F_k

This reuses existing infrastructure from `Cluster.hs` (schema abstraction) and `ExactNu.hs` (exhaustive enumeration).

### Phase 1E: Analyze Results and Determine Next Steps (Week 3)

**Three possible outcomes:**

#### Outcome A: Exact Match (|S(L_k)| = Delta_k for all k)

The saturation assumption becomes a theorem. Actions:
- Write the proof cleanly in Agda
- Update the paper: change "assumed" to "proved" in Section 8.1
- The Fibonacci structure is on solid ground
- Proceed to Priority 2

#### Outcome B: Proportional (|S(L_k)| = c * Delta_k for constant c)

The recurrence still holds (Delta_{n+1} = c*Delta_n + c*Delta_{n-1} = c*(Delta_n + Delta_{n-1})). Actions:
- Determine c and whether it's exact or approximate
- Redefine the "unit" of schema counting to absorb c
- Update the table: the numbers change but the Fibonacci *ratios* survive
- Update the paper's saturation statement to reflect the proportionality
- Proceed to Priority 2 (the framework is fine)

#### Outcome C: No Consistent Relationship

The Fibonacci timing is an artifact. Actions:
- Document the actual |S(L_k)| values
- Determine what recurrence (if any) the actual values satisfy
- Assess whether the Genesis Sequence still emerges under the true costs
- This may require a fundamental revision of the framework
- Priorities 2-4 are suspended until this is resolved

---

## Technical Details

### What "Schema" Means Precisely

A schema exported by L_k is an interface point through which future structures can create obligations. Concretely, when structure X_{k+1} is sealed against the library, it must satisfy coherence conditions. Each such condition involves some subset of the interface. The schemas are the *atomic* interface elements.

For a HIT with cell presentation P = (C_0, C_1, C_2, ...):

- **Point schemas:** Each point constructor c in C_0 contributes one schema (the requirement to specify what the eliminator does at c)
- **Path schemas:** Each path constructor p in C_1 contributes one transport schema (the requirement that elimination is consistent along p)
- **Composition schemas:** Each way the new type interacts with existing library types through dependent types contributes a schema

The claim is that the total count equals Delta_k = F_k.

### Connection to Existing Code

| Existing module | How it relates to saturation |
|----------------|------------------------------|
| `Cluster.hs` | Already computes schema sets; needs to be adapted to count *exported* rather than *imported* schemas |
| `ExactNu.hs` | Exhaustive enumeration infrastructure; reusable for schema counting |
| `GenuineNu.hs` | Currently computes nu (novelty); saturation is about the *cost* side, but the enumeration machinery is similar |
| `Enumerate.hs` | Generates well-formed types; directly usable for schema enumeration |
| `Independence.hs` | Filters trivial schemas; relevant for determining which schemas are "genuine" exports |
| `agda/ObligationGraph/Recurrence.agda` | Where the saturation assumption is currently postulated; this is where the proof should land |
| `agda/Experiments/` | HIT elimination experiments (Circle, Sphere, Torus, Hopf) already trace obligations; these are the empirical data points for saturation |

### What the Agda Experiments Already Show

The experiments in `agda/Experiments/` trace obligations for S^1, S^2, T^2, and Hopf. They confirm:
- All obligations reference at most 2 layers (confirming d=2)
- At least one obligation genuinely references 2 layers (confirming d >= 2)
- None references 3 layers

These experiments count *how deep* obligations go but don't count *how many* schemas each layer exports. Extending them to count schemas is the core of this work.

---

## Milestones and Success Criteria

| Milestone | Deliverable | Success criterion |
|-----------|-------------|-------------------|
| 1A: Schema definition | `agda/Saturation/Schema.agda` | Definition type-checks, is intrinsic |
| 1B: Bootstrap verification | `agda/Saturation/Bootstrap.agda` | Steps 1-4 all satisfy |S(L_k)| = Delta_k (or identify proportionality constant) |
| 1C: Geometric verification | `agda/Saturation/Geometric.agda` | Steps 5-8 satisfy the same relationship |
| 1D: Haskell cross-check | `engine/src/SaturationCheck.hs` | Haskell and Agda counts agree |
| 1E: Paper update | Updated pen_unified.tex | Saturation status upgraded from "assumed" to "proved" or "refuted with replacement" |

---

## Risk Assessment

**Highest risk:** The schema definition (Phase 1A) may be underdetermined. There could be multiple reasonable definitions of "exported schema" that give different counts. If so, the question becomes: is there a *canonical* definition that yields saturation?

**Mitigation:** Start with the most natural definition (elimination schemas + transport schemas) and see if it works for steps 1-4. If it doesn't, try alternatives systematically. The bootstrap phase (steps 1-4) has small enough numbers that any definition yielding 1, 1, 2, 3 is highly constrained.

**Medium risk:** The Agda formalization may be technically difficult for HITs (steps 5-8). Cubical Agda's handling of higher inductive types can be subtle.

**Mitigation:** The experiments in `agda/Experiments/` already handle S^1, S^2, and Hopf. The infrastructure exists; the question is whether it can be extended to count schemas rather than just tracing obligations.

**Low risk:** The Haskell cross-check (Phase 1D) is straightforward given existing infrastructure.

---

## Dependencies

- **Blocks:** Priority 2 (nu circularity) -- if saturation fails, the recurrence is wrong and nu values need recalibration
- **Blocks:** Priority 3 (coherence window) -- the d=2 proof uses the layer structure that saturation defines
- **Blocks:** Priority 4 (DCT lattice) -- if the framework needs revision, the DCT analysis changes
- **Blocked by:** Nothing -- this can start immediately
- **Parallel with:** Phase 1D (Haskell) can run in parallel with Phases 1B-1C (Agda)
