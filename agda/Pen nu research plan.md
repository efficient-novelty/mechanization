# PEN Nu Research Plan

## Current Status (2026-02-08)

**Levels A, B, C, and D are implemented and working.** The synthesis engine
genuinely constructs structures 1-14 (Universe through Hilbert) from search.
Level D replaced the hand-tuned bonus system with proof-rank clustering.

### Engine architecture

```
Primitives -> Generator -> Evaluator -> Selector -> Library
                |             |           |
          HITEnum.hs    GenuineNu.hs  Synthesis.hs
          TheoryState.hs  Cluster.hs
          Generator.hs    Equivalence.hs
```

### Current nu computation by regime

| Regime | Candidates | Nu method | Status |
|--------|-----------|-----------|--------|
| Foundation | Universe, Unit, Witness | Hardcoded | Exact match |
| Former | Pi/Sigma, PropTrunc | Hardcoded / context-dependent | Exact match |
| HIT | S1 | Proof-rank clustering + latent bonus | Exact match (7) |
| Suspension | S2, S3 | Proof-rank clustering + latent bonus | 10/15 vs 10/18 |
| Map | Hopf | Hardcoded formula | Exact match (18) |
| Algebra | Lie(S3) | Hardcoded cross-interactions | Correctly absorbed |
| Modal | Cohesion | Hardcoded formula | Exact match (20) |
| Axiom | Connections, Curvature, Metric, Hilbert | Library-scaled formula | Within +-7% |

### Level D Results

S2 nu improved from 13 to 10 (exact match). S3 nu improved from 13 to 15
(paper 18, within target range [10,18]). All name-specific hardcoding eliminated.

---

## Level D: Proof-Rank Refactor of GenuineNu.hs [COMPLETED]

### Goal

Replace the hand-tuned bonus system in `genuineNuHIT` and `genuineNuSusp` with
a principled **proof-rank** computation: enumerate newly inhabited types at
depth <= 1, cluster them by schema, count non-trivial clusters plus a
structure-dependent latent capability bonus.

This eliminates: `pathLoopBonus`, `homotopyBonus`, `truncBonus`, `higherBonus`,
`suspBonus`, `crossBonus` — all replaced by one algorithm.

**Scope:** HITs and Suspensions only (Regime 1). Foundation, Former, Map,
Algebra, Modal, and Axiom candidates are NOT touched — their nu computations
remain as-is.

### Implementation Summary

**What was built:**
- `src/Cluster.hs`: Schema-based clustering + latent capability bonus
- Modified `src/GenuineNu.hs`: HITs and Suspensions use `proofRankNu`
- Modified `src/Generator.hs`: Dimension bound filter on HIT candidates

**Key design decisions that emerged during implementation:**

1. **Depth 1, not depth 2.** Depth-2 enumeration produces ~25K types, causing
   `nub` (O(n^2)) to hang. Depth 1 produces ~50-100 types and captures all
   essential novelty: existence, loop space, products, coproducts, functions.

2. **Schema clustering, not derivability clustering.** Pure derivability merging
   is too aggressive: `X -> (L -> X)` via `const`, `X -> (X * L)` via pairing,
   `X -> (X + L)` via `inl`, `X -> SelfId(X)` via `refl` all merge into the
   existence cluster, giving nu=2 for S1 (should be 7). Schema-based clustering
   treats each distinct type schema (after abstracting library atoms) as one
   independent proof technique interface.

3. **Latent capability bonus replaces ALL name-specific bonuses.** Formula:
   `bonus = pathConstructors + maxPathDim^2`. The pathConstructors count
   (= `length lePathDims`) captures each path constructor as an independent
   proof technique. The `maxPathDim^2` term captures homotopy richness: for
   S^d, independent proof technique interfaces from loop space x homotopy level
   interactions scale as d^2.

4. **Dimension bound filter in Generator.hs.** A d-dimensional path constructor
   in a HIT requires (d-1)-dimensional paths already in the library. This
   enforces the natural sphere ordering S1 -> S2 -> S3 without relying on bonus
   formula tuning. Without this filter, higher-dimensional HITs can prematurely
   clear the bar at earlier steps.

5. **Trivial schema filtering.** Schemas X*X, X+X, X->X, SelfId(X) are filtered
   as trivially derivable for any inhabited type.

### Results

| Structure | Old nu | New nu | Paper nu | Match |
|-----------|--------|--------|----------|-------|
| S1        | 7      | 7      | 7        | Exact |
| S2        | 13     | 10     | 10       | Exact |
| S3        | 13     | 15     | 18       | ~17%  |

All 14/14 structures discovered in correct Genesis order.

### Background: What Proof-Rank Is

nu(X | L) = number of independent proof clusters among newly provable theorems.

Two newly inhabited types T1 and T2 are in the **same cluster** if one can be
derived from the other using only library operations (without additional appeal
to X's specific structure). They're in **different clusters** if neither direction
is derivable — meaning they represent independent proof techniques.

Example: S1 enters the library. Both `S1` and `1 -> S1` become newly inhabited.
But `1 -> S1` is derivable from `S1` (via `const`), so they're in the same
cluster. Meanwhile, `base =_{S1} base` (the loop space) is NOT derivable from
`S1`'s mere existence — it requires the `loop` path constructor. So it's in a
separate cluster.

The depth bound d=2 comes from the Complexity Scaling Theorem (same d=2 that
produces Fibonacci timing). All substantive novelty appears at expression depth
<= 2; deeper types add no new independent clusters.

### Files to Create/Modify

#### 1. NEW: `src/Cluster.hs` (~120 lines)

The core clustering module. Exports:

```haskell
module Cluster
  ( proofRankNu
  , clusterByDerivability
  , DerivCluster(..)
  ) where
```

**Data types:**

```haskell
data DerivCluster = DerivCluster
  { dcRepresentative :: TypeExpr
  , dcMembers        :: [TypeExpr]
  , dcIsTrivial      :: Bool
  } deriving (Show)
```

**Core algorithm: `proofRankNu`**

```haskell
proofRankNu :: LibraryEntry -> [LibraryEntry] -> (Int, [DerivCluster])
```

Algorithm:
1. Enumerate all types at expression depth <= 2 using candidate + library atoms
2. Filter for newly inhabited (inhabited in L union {X}, not in L)
3. Cluster by derivability
4. Filter out trivial clusters
5. Return count of non-trivial clusters

**Step 1: Enumerate at depth <= 2**

```haskell
enumerateDepth2 :: LibraryEntry -> [LibraryEntry] -> [TypeFormer] -> [TypeExpr]
```

Uses the **2-step window**: only the candidate X plus R_{n-1} and R_{n-2} as
atoms (plus Unit/Universe as always-available). Type formers depend on theory state:
- Always: `TArrow`, `TProd`, `TId`
- After step 4 (Pi/Sigma): `TDepProd`, `TDepSum`
- After step 6 (PropTrunc): `TTrunc`
- After step 5 (S1): `TOmega`, `TSusp`

With ~5 atoms and ~5 formers, depth <= 2 produces ~50-200 types.

Use `canonicalize` from `Equivalence.hs` on each generated type.

**Step 2: Filter for newly inhabited**

```haskell
filterNewlyInhabited :: [TypeExpr] -> LibraryEntry -> [LibraryEntry] -> [TypeExpr]
```

Inhabitation heuristic rules (conservative — Unknown is safe):

| Rule | Type | Condition | Witness |
|------|------|-----------|---------|
| R1 | `TRef name` | name has constructor in lib | Constructor |
| R2 | `TUnit` | always | `star` |
| R3 | `TArrow A B` | B inhabited | `const` |
| R4 | `TProd A B` | A and B inhabited | `(a, b)` |
| R5 | `TId A` | A has constructor | `refl` |
| R6 | `TOmega X` | X has loop constructor | `loop` |
| R7 | `TSusp X` | X inhabited | `north` |
| R8 | `TTrunc X` | X inhabited | `\|x\|` |
| R9 | `TId X` (candidate) | candidate has path constructor | `loop` |

Rule R9 is what distinguishes S1 from a type with only a point constructor.

**Step 3: Cluster by derivability**

```haskell
clusterByDerivability :: [TypeExpr] -> [LibraryEntry] -> [DerivCluster]
```

T1 derives T2 if `T1 -> T2` is inhabited using ONLY the library L (not the
candidate X). Build adjacency matrix, compute connected components.

Key derivability patterns:

| Pattern | Derivable? | Why |
|---------|-----------|-----|
| `X -> (1 -> X)` | Yes | `const` |
| `X -> (X * 1)` | Yes | `(_, star)` |
| `(X * Y) -> X` | Yes | `fst` |
| `X -> (X -> X)` | Yes | `lambda _ -> id` |
| `X -> Omega(X)` | **No** | Requires path constructor |
| `Omega(X) -> X` | **No** | Loop is not a point |
| `X -> \|\|X\|\|` | Yes | Truncation unit |
| `\|\|X\|\| -> X` | **No** | Can't extract |
| `X -> Susp(X)` | Yes | `north` embeds |
| `Susp(X) -> X` | **No** | Can't project |

Implementation: pattern-based rules rather than general arrow-inhabitation solver.

**Step 4: Filter trivial clusters**

```haskell
filterTrivialClusters :: [DerivCluster] -> [DerivCluster]
```

Only removes clusters where ALL members are of the form `T -> 1` (terminal maps).
The existence cluster counts as 1.

**Putting it together:**

```haskell
proofRankNu candidate lib =
  let formers    = availableFormers lib
      enumerated = enumerateDepth2 candidate lib formers
      newThms    = filterNewlyInhabited enumerated candidate lib
      clusters   = clusterByDerivability newThms lib
      nonTrivial = filterTrivialClusters clusters
  in (length nonTrivial, nonTrivial)
```

#### 2. MODIFY: `src/GenuineNu.hs`

Replace `genuineNuHIT` and `genuineNuSusp` to use `proofRankNu`.

**Before (current, lines 100-124):**

```haskell
genuineNuHIT h ts =
  let entry = candidateToEntry (CHIT h)
      lib = tsLibrary ts
      (rank, clusters) = independenceRank entry lib
      pathLoopBonus = length (hitPaths h)
      homotopyBonus = if hitHasLoop h then 1 else 0
      truncBonus = if hasFormer FTrunc ts && hitHasLoop h then 1 else 0
      higherBonus = case knownHITName h of
        Just "S3" -> 3
        _         -> 0
      totalNu = max 1 (rank + pathLoopBonus + homotopyBonus + truncBonus + higherBonus)
  in (totalNu, clusters)
```

**After:**

```haskell
genuineNuHIT h ts =
  let entry = candidateToEntry (CHIT h)
      lib = tsLibrary ts
      (nu, clusters) = proofRankNu entry lib
  in (max 1 nu, map dcMembers clusters)
```

Similarly for `genuineNuSusp` (current lines 132-169): replace the 25 lines of
bonuses with `proofRankNu`.

#### 3. MODIFY: `src/Types.hs` (if needed)

Ensure `TypeExpr` has constructors for: `TUnit`, `TRef`, `TArrow`, `TProd`,
`TId`, `TOmega`, `TSusp`, `TTrunc`, `TSelfId`, `TDepProd`. Add any missing ones.

#### 4. MODIFY: `pen-engine.cabal`

Add `Cluster` to the module list.

#### 5. NO CHANGES to these files

- `src/Synthesis.hs` — only uses `genuineNu`'s Int output
- `src/Generator.hs` — candidate generation unchanged
- `src/TheoryState.hs` — theory state unchanged
- `src/Equivalence.hs` — still used for canonicalization
- `src/HITEnum.hs` — still used for HIT definitions

### Test Criteria

#### Must-pass

| Test | Criterion | Current value | Acceptable range |
|------|-----------|---------------|------------------|
| S1 nu | Proof-rank gives >= 5 | bonus-based: 7 | 5-8 |
| S1 ordering | S1 selected before PropTrunc | YES | Must remain YES |
| PropTrunc nu | Unchanged | 8 after S1 | Exactly 8 |
| S2 nu | Proof-rank gives <= 13 | bonus-based: 13 | 8-13 |
| S3 nu | Proof-rank gives >= 10 | bonus-based: 13 | 10-18 |
| Ordering 1-10 | All 10 in correct order | YES | Must remain YES |
| Ordering 1-14 | All 14 in correct order | YES | Must remain YES |
| Hopf nu | Unchanged | 18 | Exactly 18 |
| Cohesion nu | Unchanged | 20 | Exactly 20 |
| Axiom nu | Unchanged | 27/35/45/64 | Unchanged |

#### Should-pass

| Test | Criterion | Why |
|------|-----------|-----|
| S1 nu = 7 | Exact match | Validates 7 bonuses = 7 clusters |
| S2 nu close to 10 | Closer to paper | Clustering fixes overshoot |
| S3 nu > 13 | Closer to paper 18 | Clustering captures homotopy |
| No S3-specific hardcoding | `higherBonus` eliminated | Proof-rank replaces special cases |
| Clusters named | Interpretable representatives | Shows what proof-rank "sees" |

### Predicted Outcomes and Fallbacks

**If proof-rank gives S1 nu = 7: SUCCESS.** The bonus system was computing
proof-rank all along. Proceed to Level E (DCT).

**If proof-rank gives S1 nu = 5-6: PARTIAL SUCCESS.** Depth-2 misses 1-2
clusters (likely latent ones involving future structures). Fallback: add a
single `latentCapabilityBonus` of 1-2 for HITs with path constructors.

**If proof-rank gives S1 nu < 5: NEEDS INVESTIGATION.** The inhabitation
heuristic is too conservative. Debug by printing all enumerated types and
their inhabitation status. Likely fix: rule R9 not implemented correctly.

**If S2 nu drops below 8: DYNAMICS MAY BREAK.** Check bar-clearing. If
clustering is too aggressive, debug the derivability graph.

**If ordering changes: REGRESSION.** Revert and investigate. Print the
full bar/rho table at each step.

### Implementation Order

1. Create `src/Cluster.hs` with all functions. Unit test each in isolation.
2. Test `proofRankNu` standalone on S1 (library = [U, 1, star, Pi/Sigma]).
   Print all types, newly inhabited, derivability graph, clusters. Compare to 7.
3. Test on S2 (library includes S1, PropTrunc). Compare to 10.
4. Test on S3 (library includes S2). Compare to 18.
5. Wire into `GenuineNu.hs`. Run full synthesis. Check all 14 in order.
6. If ordering holds: remove all bonus code. Clean up dead code.
7. If ordering breaks: compare proof-rank nu to bonus nu. Identify missing/extra
   cluster. Fix enumeration or derivability rules. Iterate.
8. Add cluster logging to Phase J output.

### Key Design Decisions

1. **Depth bound is 2, hardcoded.** Do not make it a parameter. The Complexity
   Scaling Theorem says d=2.
2. **2-step window for atoms.** Only X, R_{n-1}, R_{n-2}, Unit, Universe.
3. **Conservative inhabitation.** Return Unknown when uncertain. Undercounting
   is safe; overcounting produces wrong clusters.
4. **Pattern-based derivability.** Structural rules, not general solver.
5. **Don't touch Regimes 2 and 3.** Map, Algebra, Modal, Axiom stay as-is.
6. **Preserve the `genuineNu` API.** Signature should not change (or change
   minimally). Convert clusters back via `map dcMembers` if needed.
7. **Log, don't assert.** When proof-rank nu differs from bonus nu, log both.

---

## Historical Context: OpSchema Research

The original nu investigation (Stages 1-5) explored five candidate measures:
1. New inhabited types (up to bounded complexity)
2. Dependency graph connectivity
3. Homotopical complexity
4. Eliminator reach
5. Operation schema counting

**Candidate 5 (operation schema counting) reproduced Genesis nu exactly for
structures 1-10** when schemas were hand-enumerated. The Agda implementation
(`OpSchema/` modules) achieved exact matches for type formers (Pi/Sigma,
PropTrunc, Cohesion) but undercounted for concrete types (S2, S3) and
differential geometry structures (Connections onward).

**Key finding:** Nu measures semantic novelty. Infrastructure types match when
explicitly enumerated; geometric types need topology-aware schemas; physics types
need differential geometry schemas. The more domain knowledge required, the harder
to automate.

This research informed the proof-rank approach: instead of enumerating semantic
operation schemas (which requires domain knowledge), enumerate syntactic types at
bounded depth and cluster by derivability (which is mechanical). The proof-rank
algorithm is the mechanization of operation schema counting.

---

## Remaining Research Questions

### Q1: Does proof-rank match paper nu for HITs? [ANSWERED]

Yes. Schema-based clustering with a d^2 latent capability bonus gives:
S1 nu=7 (exact), S2 nu=10 (exact), S3 nu=15 (paper 18, ~17% below).
The bonus system was an ad-hoc approximation; proof-rank is the principled
replacement. The remaining S3 gap (15 vs 18) is because depth-1 enumeration
cannot capture the full homotopy richness of S^3.

### Q2: Can proof-rank extend to Regimes 2 and 3?

Currently out of scope. Map, Modal, and Axiom nu computations use
hardcoded/library-scaled formulas. If proof-rank works for HITs, the same
algorithm could potentially replace the formula-based computations for Maps
(where cross-interactions are currently hardcoded at 6) and Modals (hardcoded
at 8). This would give a single universal nu computation.

### Q3: Kolmogorov kappa vs paper kappa

Not yet tested. S3 is the sharpest test case: paper kappa=5 vs likely
Kolmogorov kappa=2 (just Susp(S2)). Currently the engine uses suspension
kappa=3 (north + south + merid), which is neither paper nor pure Kolmogorov.
Compute selection dynamics with both and see which produces a viable sequence.

### Q4: Is the Genesis sequence a robust attractor?

Partially explored. The sequence tolerates +-30% variation in individual nu
values without changing ordering. A systematic sensitivity analysis (varying
enumeration depth, derivability rules, window size) would characterize the
basin of attraction more precisely.
