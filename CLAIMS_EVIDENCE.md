# PEN Claims Evidence Matrix

Maps each major claim in the paper to executable evidence in the codebase.

**Status key**: Demonstrated = reproducible by command; Assumed = modeling choice; Open = unresolved.

---

## Claim A: Sequence Ordering — 15 structures in deterministic order

**Paper reference**: Table 1 (§1), Theorem 4.1 (§4)

**Claim**: Starting from an empty library, PEN's efficiency criterion (minimal
overshoot of ρ = ν/κ above a rising bar) uniquely selects 15 structures in
the order: Universe, Unit, Witness, Pi/Sigma, S¹, Trunc, S², S³, Hopf,
Cohesion, Connections, Curvature, Metric, Hilbert, DCT.

### Evidence

| Mode | Command | What it demonstrates | Caveats |
|------|---------|---------------------|---------|
| Paper replay | `cd engine && cabal run pen-engine` (Phase G) | 15/15 structures clear bar in correct order using paper ν/κ | Uses hardcoded values — validates bar dynamics, not ν computation |
| Genuine synthesis | `cd engine && cabal run pen-engine` (Phase J) | 15/15 discovered from scratch via GenuineNu + Generator | ν for axioms (steps 10-14) uses component formulas, not proof-rank |
| Paper-calibrated ab initio | `cd engine && cabal run ab-initio` | 15/15 discovered from MBTT telescopes + MCTS | Uses paper ν/κ for canonical names via EvalPaperCalibrated |
| Strict ab initio | `cd engine && cabal run ab-initio -- --strict` | 13/15 names in correct order (steps 1-13) | Steps 14-15 fail: F1 (ν overestimate), F2 (Pi ν=0), F3 (Trunc explosion) |

### Key modules

- `Synthesis.hs` — genuine synthesis loop (Phase J)
- `Simulation.hs` — paper-mode replay (Phase G)
- `RunAbInitio.hs` — ab initio engine (both modes)
- `Generator.hs` — candidate generation (9 categories, prerequisite-gated)
- `TelescopeEval.hs` — telescope classification + prerequisite chain

### Known caveats

1. **Strict mode partial failure**: Genuine paper-free ν computation (via
   `computeUniformNu`) diverges from paper at steps 14-15. Root causes
   identified (F1-F3 in physics_creation.md) but not yet resolved.
2. **Candidate pool**: Generator.hs uses 9 categories with prerequisite
   gating. The pool is broad but not exhaustive — exotic structures outside
   these categories are not considered.
3. **Minimal overshoot**: Selection criterion (Axiom 5) is a design choice.
   Maximum-ρ selection would pick suspensions at every step (L2, L6).

---

## Claim B: Numerical ν/κ Agreement

**Paper reference**: Table 1 (§1), §7 computational verification

**Claim**: The paper's ν and κ values (Σν = 356, Σκ = 64) are reproducible
by independent computation, not hand-tuned.

### Evidence

| Method | Command | Result | Status |
|--------|---------|--------|--------|
| Paper values (reference) | `cd engine && cabal run pen-engine` (Phase G) | Σν = 356, Σκ = 64 | Baseline |
| Capability rules (18 rules) | `cd engine && cabal run pen-engine` (Phase H) | 16/16 ν match paper | Demonstrated |
| Uniform inhabitation (before/after) | `cd engine && cabal run uniform-nu` | 15/15 non-zero ν | Demonstrated |
| Strict ab initio (paper-free) | `cd engine && cabal run ab-initio -- --strict` | Σν = 759, Σκ = 48 | **Diverges** |

### Key modules

- `KappaNu.hs` — paper reference values (`paperNu`, `paperKappa`)
- `Capability.hs` — 18 hand-tuned capability rules
- `UniformNu.hs` — before/after inhabitation comparison
- `GenuineNu.hs` — dispatch by structure category
- `TelescopeEval.hs` — `EvalMode` controls which ν/κ path is used

### Known caveats

1. **ν divergence in strict mode**: `computeUniformNu` produces ν ~2x higher
   than paper for most steps (depth-2 enumeration counts compositions).
   See L16 in physics_creation.md.
2. **κ metric mismatch**: Paper κ (clause count) ≠ telescope κ (entry count)
   ≠ bit-cost κ (MBTT encoding). See L3 in physics_creation.md.
3. **DCT ν discrepancy**: Paper uses ν=105 (independent schemas).
   GenuineNu computes ν=150 (lattice tensor product). Both are valid
   measures; 105 is publication-canonical.
4. **Capability rules are hand-tuned**: The 18 rules in Capability.hs
   encode domain knowledge. Their agreement with paper ν validates
   self-consistency, not independence.

---

## Claim C: d=2 Coherence Window Forces Fibonacci Scaling

**Paper reference**: Theorem 3.1 (§3), Theorem 4.1 (§4)

**Claim**: Intensional type theory (HoTT) has coherence window d=2, which
forces integration latency Δ_n = F_n (Fibonacci). Extensional type theory
(d=1) stagnates. The golden ratio φ is the dominant eigenvalue.

### Evidence

| Test | Command | Expected result | Status |
|------|---------|----------------|--------|
| d=2 Fibonacci | `cd engine && cabal run pen-engine` (default) | Δ = 1,1,2,3,5,8,...,610. Φ_n → φ ≈ 1.618 | Demonstrated |
| d=1 stagnation | `cd engine && cabal run pen-engine -- --window 1` | Δ = 1,1,1,1,... Constant bar, early saturation | Demonstrated |
| d=3 tribonacci | `cd engine && cabal run pen-engine -- --window 3` | Δ = 1,1,2,4,7,13,... Different sequence | Demonstrated |
| Formal proof (d=2 recurrence) | `cd agda && agda --cubical --safe PEN.agda` | Δ_{n+1} = Δ_n + Δ_{n-1} type-checks | Demonstrated (Agda) |
| Cumulative sum | Agda: `Core/Sequence.agda` | τ_n = F_{n+2} - 1 | Demonstrated (Agda) |
| Abstraction barrier (step 8) | Agda: `Saturation/AbstractionBarrier.agda` | Group B obligations from opaque L₇ | Demonstrated (Agda) |
| Abstraction barrier (step 9) | Agda: `Saturation/AbstractionBarrier9.agda` | Hopf obligations from opaque L₈ | Demonstrated (Agda) |

### Key modules

- `CoherenceWindow.hs` — d-Bonacci sequence generator
- `agda/ObligationGraph/Recurrence.agda` — Fibonacci recurrence proof
- `agda/Core/Sequence.agda` — cumulative sum proof
- `agda/Saturation/` — abstraction barrier proofs

### Known caveats (Assumed)

1. **d=2 for HoTT is assumed, not derived**: The paper assumes intensional
   type theory has d=2. Theoretical derivation via adjoint functor argument
   is a research goal (physics_creation.md, next steps #1).
2. **Maximal interface density**: PEN assumes each candidate seals against
   the ENTIRE exported interface of the past d layers. This is a modeling
   choice (analogous to universal coupling in physics), not a native
   requirement of HoTT. See Remark 3.2 (rem:maximal-coupling) in paper.
3. **CCHM dependence**: The abstraction barrier proofs use cubical Agda
   (CCHM model). Framework-dependence caveat noted in paper.

---

## Verification Quick Reference

```bash
# Full 10-phase analysis (recommended first run)
cd engine && cabal run pen-engine 2>&1 | less

# Ab initio discovery
cd engine && cabal run ab-initio                  # Paper-calibrated
cd engine && cabal run ab-initio -- --strict      # Strict (paper-free)

# Stress tests
cd engine && cabal run pen-engine -- --window 1   # d=1 stagnation
cd engine && cabal run pen-engine -- --window 3   # d=3 tribonacci

# Uniform novelty (standalone)
cd engine && cabal run uniform-nu

# Agda proofs
cd agda && agda --cubical --safe PEN.agda
```

---

## Unresolved Assumptions (across all claims)

| Assumption | Where used | Status |
|------------|-----------|--------|
| d=2 for HoTT | Claim C, all bar dynamics | Assumed (adjoint derivation is open) |
| Maximal interface density | Integration latency Δ_n | Modeling choice (rem:maximal-coupling) |
| Candidate pool completeness | Claim A ordering uniqueness | Broad but not exhaustive |
| κ = clause count | Claim B numerics, bar computation | Convention (tri-metric comparison pending) |
| ν = independent derivation schemas | Claim B numerics | Definition (multiple valid measures exist) |
| Sequence terminates at DCT | All claims | Conjectured (Tangent Topos hypothesis is open) |
