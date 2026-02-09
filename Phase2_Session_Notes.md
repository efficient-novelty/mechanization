# Phase 2 Session Notes — ν Convergence Work

## Date: 2026-02-09

---

## Current State (where we left off after restart)

### Completed Work:

1. **ExactNu.hs** (D2.1 from WP 2.1) — COMPLETE
   - Module at `engine/src/ExactNu.hs`
   - Enumerates all library atoms (not just 2-step window) at depths 1, 2, 3
   - Uses ProofRank's `enumWindowExact` for enumeration, then schemaizes and counts
   - Compares exact schema counts against proof-rank schema counts

2. **Phase M in Main.hs** — COMPLETE
   - Runs exact oracle for steps 1–7
   - Compares ν_paper vs ν_PR vs exact_d1 vs exact_d2 vs exact_d3
   - Includes sanity check: d1 exact schemas == PR schemas (minus latent bonus)
   - Includes detailed S¹ depth-2 schema dump

3. **ProofRank.hs** — Minor change: exported `enumWindowExact`

4. **pen-engine.cabal** — ExactNu added to both executable and library

5. **Engine builds** and runs (Phases A–L complete, Phase M pending)

### Not Yet Started:

- WP 2.2: Extending proof-rank to axioms/modals/synthesis
- WP 2.3: The convergence test (4 measures × 10 steps)
- WP 2.4: Fixing the Exponentiality Theorem
- WP 2.5: Sensitivity analysis
- WP 2.6: Integration phases N/O/P

---

## Key Document: Nu_Convergence_Test_Design.md

This design document sharpens the Phase 2 plan considerably:

### Adds to the plan:
1. **Four measures, not three** — Proof-Rank (A), Compression Drop (B), Categorical (C), Homotopy (D)
2. **Grammar sensitivity testing** for Measure B (G1/G2/G3 variants)
3. **Pre-registered decision thresholds** (Spearman ρ > 0.9, R² > 0.9)
4. **Worked examples** for steps 1–4
5. **NuConvergence.hs** module architecture with common interface

### Key insight: Measure B (compression drop) is the most important comparison
- It's the formal definition from the paper (Definition 3.5)
- Grammar sensitivity within B is as important as cross-measure comparison
- If B isn't stable across grammars, the whole framework is fragile

### Measure D (homotopy-theoretic): LLM-assisted, not purely manual
- An LLM like Opus 4.6 can systematically enumerate:
  - Homotopy groups π_k(X_n) for each Genesis structure
  - New fibration sequences
  - New cohomology operations
  - New equivalences
- More systematic and reproducible than manual human tabulation
- Still requires mathematical knowledge, but LLM has this

---

## Implementation Plan (Updated Priority Order)

### Step 1: Analyze Phase M Results (exact ν oracle)
- Wait for engine run to complete
- Compare exact ν vs proof-rank ν for steps 1–7
- Key question: does depth-2 exact ν explain what the latent bonus approximates?
- Save results to file

### Step 2: Implement Measure B (compression drop)
- Create `NuConvergence.hs` with the common interface from §7.1 of design doc
- Define grammar G2 first (types, Π, Σ, =, application, transport)
- Implement expression enumerator up to token count H
- Implement minimum token count computation
- Run for steps 1–10, horizons H = 3, 4, 5
- Then add G1 (minimal) and G3 (with eliminators) for sensitivity testing

### Step 3: Grammar Sensitivity Test
- Run Measure B under G1, G2, G3 × H = 3, 4, 5
- 9 configurations × 10 steps = 90 data points
- Check: is ν_B stable across grammars?

### Step 4: Implement Measure C (categorical — map counting)
- Count new maps T → U between library types when X is added
- Quadratic in |B| but feasible for |B| ≤ 10

### Step 5: LLM-Assisted Measure D (homotopy-theoretic)
- For each of 10 Genesis structures, use LLM to enumerate:
  - π_k for k = 0,1,2,3
  - New long exact sequences
  - New equivalences
  - New cohomology operations
- Produce ν_D table with mathematical justification

### Step 6: Run Convergence Analysis
- Assemble all four columns
- Compute Spearman correlations, R² values
- Test Genesis Sequence stability under each measure
- Determine outcome (1–4 from design doc)

---

## Engine Run Status

- Full engine run started at ~18:13 on 2026-02-09
- Output being saved to `/home/halvor/PEN/engine_run_output.txt`
- As of last check: Phases A–L complete, running Phase L synthesis (d=1,2,3)
- Phase M (exact oracle) is the last phase — should follow after L completes
- IMPORTANT: Phase L runs 3 synthesis passes (d=1,2,3), each expensive
- Depth-3 exact computation in Phase M is also expensive (only runs for n≤5)

---

## Observations from Phase K Output (Saturation Test)

From the engine output so far, the saturation test (Phase K) shows significant
mismatches between Δ_n (Fibonacci deltas) and schema counts for most steps.
This is expected — the saturation assumption (that schema count exactly equals
Fibonacci delta) was always an approximation. The important thing is whether
the *ordering* is preserved, not the exact counts.

---

## Observations from Updated pen_paper.tex

The paper has been significantly updated. Key observations relevant to Phase 2:

### Section 6 (Combinatorial Novelty Theorem) — Already Partially Fixed
The exponentiality section (WP 2.4 target) has already been rewritten. The new version:
1. **OIT Exponentiality** (Thm 6.1): Clean and correct. Maps X → **2** give 2^Δ₀. No changes needed.
2. **HIT Constraints** (§6.3): Explicitly acknowledges that path constructors collapse Boolean predicates.
   Gives concrete examples (S¹ → only 2 maps to **2**, not 4). This is honest and correct.
3. **Three recovery mechanisms** for HITs:
   - Dependent elimination (Prop 6.6): type families P : X → U give |B|^Δ₀ · Π|Aut| choices
   - Library cross-interaction (Prop 6.8): at least Ω(n) new constructions per step
   - Composite constructions (Prop 6.9): superadditivity under products
4. **Divergence theorem** (Thm 6.10): Uses only the weakest mechanism (linear library scaling)
   to prove ρ_n → ∞. Conservative but rigorous.

### Key insight for Phase 2: The paper's ν definition (Def 3.5) IS the compression-drop measure
Definition 3.5 in the updated paper defines ν as:
  ν(X | B, H) = |{Y : K_B(Y) − K_{B∪{X}}(Y) ≥ 1}|

This IS Measure B from the convergence test design doc. So the convergence test is really:
"Does the paper's formal definition (compression drop) agree with what the engine computes
(proof-rank clustering)?" If they agree, the engine is computing what the paper defines.
If they disagree, either the engine or the definition needs revision.

### What the paper claims about ν sensitivity (§7.1):
- ν matches exactly for 12/15 structures
- Within ±15% for remaining 3 (S³, Connections, Hilbert)
- "These variations arise from the depth-1 enumeration window"
- In all cases ordering is preserved

This gives us a concrete benchmark: the convergence test should verify these ±15% claims.

### Paper's ν is formally Kolmogorov-style, engine's is proof-rank
There's a gap between:
- **Paper's Definition 3.5**: counts structures whose Kolmogorov complexity drops by ≥1
- **Engine's computation**: counts independent type schemas at depth ≤1

These should be equivalent for well-behaved cases, but the convergence test will verify this.
The existing `kNovelty` function in ProofRank.hs already implements something close to
Definition 3.5 (it's the K-Based Novelty from Phase F). Phase F results should be checked
against Phase M (exact oracle) results.

---

## Performance Note

Phase L (coherence window comparison, d=1,2,3) takes 30+ minutes at 100% CPU.
The d=3 synthesis is the bottleneck — tribonacci sequence grows much faster.
Phase M (exact oracle) will also be expensive for depth-3 computations.

Consider: for future runs, add a `--skip-phases` flag to skip expensive phases
when only testing new code.

---

## To Resume Next Session

1. Check if engine run completed: `tail -20 /home/halvor/PEN/engine_run_output.txt`
2. If completed, the file contains ALL output from Phases A–M
3. If not completed (or corrupted by restart), rerun:
   `cd engine && cabal run pen-engine > ../engine_run_output.txt 2>&1`
   NOTE: This takes 30-40+ minutes due to Phase L and Phase M
4. Read and analyze Phase M results (exact ν oracle) — compare against proof-rank
5. Also compare Phase F (K-based novelty) against Phase M — these are related measures
6. Proceed with implementing NuConvergence.hs (Measure B from convergence test design)
7. The design doc at `Nu_Convergence_Test_Design.md` has the full architecture
8. The existing `kNovelty` in ProofRank.hs and ExactNu.hs already have partial
   implementations that can be adapted for the convergence test
9. The paper's Definition 3.5 is the formal target — Measure B should implement it exactly

### Key files:
- `engine/src/ExactNu.hs` — exact oracle (complete)
- `engine/src/ProofRank.hs` — proof-rank + kNovelty (complete)
- `engine/src/Main.hs` — Phases A–M (complete)
- `Nu_Convergence_Test_Design.md` — convergence test architecture
- `PEN_Maximum_Impact_Plan.md` — full Phase 2 plan (WP 2.1–2.6)
- `pen_paper.tex` — the paper (Def 3.5 is the canonical ν definition)
