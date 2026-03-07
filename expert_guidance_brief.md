# Expert Briefing Packet: Making PEN Discovery Feel Like Intelligent Mechanized Mathematics

Date: 2026-03-07  
Repository: `C:\DEV\pen`  
Intended audience: senior expert in mathematical reasoning systems, proof search, theorem proving architecture, symbolic AI, and combinatorial optimization.

## 1. Purpose of This Brief

This document is designed to let an external world-class expert quickly understand:

1. The mathematical problem PEN claims to solve.
2. The precise formal framing (state, candidate, costs, novelty, selection dynamics, coherence window).
3. The full target 15-step generative sequence we are trying to rediscover autonomously.
4. The current implementation architecture in Agda and Haskell.
5. Why the current system still feels too brute-force and not enough like mathematical thought.
6. The concrete redesign problem: how to convert a mostly enumerate-and-filter system into obligation-driven, high-value, low-branching intelligent symbolic discovery without target leakage.

The goal is to ask for concrete guidance, not broad commentary.

## 2. Short Executive Summary

We are building an autonomous symbolic discovery engine under the Principle of Efficient Novelty (PEN). The claim is strong: starting from an empty intensional type-theoretic library and fixed structural axioms, the system should discover a specific 15-step sequence of mathematical structures via a single efficiency law:

- novelty gain `nu` divided by effort cost `kappa`,
- under a rising selection bar determined by coherence integration debt.

The intended sequence is:

1. Universe
2. Unit
3. Witness
4. Pi/Sigma
5. Circle S1
6. PropTrunc
7. Sphere S2
8. H-space S3
9. Hopf
10. Cohesion
11. Connections
12. Curvature
13. Metric
14. Hilbert
15. DCT

The engineering challenge is not merely scoring candidates. The real challenge is producing candidates intelligently. Today, despite substantial progress, the system still has brute-force DNA in key places:

1. It still often generates many telescopes first, then scores and prunes.
2. It still depends on bounded rescue lanes and curated seed patterns for hard steps.
3. It has many heuristics that improve outcomes but do not yet compose into a true proof-state reasoning loop.
4. Intelligence often arrives late (in ranking) rather than early (in move generation).

We have already implemented major upgrades:

1. Agenda-based proof-state search exists (`SearchState`, obligations, certificates).
2. Safe-vs-unsafe action discipline exists in the agenda lane.
3. Premise top-k retrieval and gating exist.
4. `rhoUB` pruning and dominance caching exist.
5. Macro learning and macro reuse exist.
6. MBTT global enumeration has been demoted in parts of the pipeline.
7. Strict/structural evaluation has begun decoupling from name-mediated capability unlocking.
8. Runtime memory and pagefile budgets are adaptive.

But it is still not yet fully "mathematical thought." It still feels like a sophisticated search engine with better filters, not a theorem constructor that explicitly discharges obligations by cheapest valid high-value proof moves.

The core redesign question for expert guidance is:

How should we architect a purely symbolic, leakage-safe, high-performance proof-search engine that behaves like a mathematician constrained by logical necessity, not like a syntax farm, while still rediscovering the first 9 steps in less than 60 seconds?

## 3. Formal Problem: PEN Model and Axioms

This section summarizes the mathematical model from `pen_paper.tex` (sections "The Model", "The Five Axioms", "Coherence Windows", "Combinatorial Schema Synthesis", etc.), translated into implementation-facing language.

### 3.1 State and Candidate

PEN models discovery as a discrete-time process over a growing library `B`.

1. State:
   - `B_n` is a monotone context closed under derivability.
   - Transition: `B_n -> B_{n+1}` by adding one sealed structure.

2. Candidate:
   - A candidate `X` is a pair `(X_core, G_obl)`.
   - `X_core` is definitive type-theoretic data (formers, constructors, operators, equations).
   - `G_obl` is an obligation graph: atomic coherence obligations required to seal `X` against current history.

### 3.2 Dual Cost Model

Two costs are distinguished:

1. Integration Latency:
   - `Delta(X | B) = |G_obl|`.
   - Counts coherence witnesses required to integrate X with recent library interface.

2. Construction Effort:
   - Candidate specification `S` has clause count `kappa(S) = |S|`.
   - MBTT bit-length is an audit-level complexity tie-breaker, while clause count is the primary denominator in the paper's dynamics.

PEN emphasizes that different specifications of "the same mathematical object" can have different `nu` and `kappa`, and selection evaluates admissible specifications directly, not abstract names.

### 3.3 Novelty and Efficiency

Novelty is marginal expansion of derivation capacity:

- `nu(X | B) = |L(B U {X})| - |L(B)|`

where `L(B)` is the set of derivation schemas available in the library.

Efficiency is:

- `rho(X) = nu(X) / kappa(X)`

### 3.4 Selection Bar

At step `n`, the required threshold is:

- `Bar_n = Phi_n * Omega_{n-1}`

where:

1. `Phi_n = Delta_n / Delta_{n-1}` (marginal inflation).
2. `Omega_{n-1} = (sum_{i=1..n-1} nu_i) / (sum_{i=1..n-1} kappa_i)` (cumulative baseline efficiency).

Selection rule:

1. Consider admissible candidates.
2. Keep candidates with `rho >= Bar_n`.
3. Select minimal positive overshoot `(rho - Bar_n)`.
4. Tie-break by smaller `kappa`.
5. If nothing clears bar, system idles.

### 3.5 The Five Axioms (paper section "The Five Axioms")

From the paper:

1. Cumulative Growth:
   - `R(tau - 1) subseteq R(tau)`.
   - Library only grows; no deletion.

2. Horizon Policy:
   - After realization: `H <- 2`.
   - After idle tick: `H <- H + 1`.

3. Admissibility:
   - Candidate X admissible iff derivable from `B` and `kappa(X) <= H`.

4. Selection:
   - `Bar_n = Phi_n * Omega_{n-1}`.
   - Choose minimal overshoot among bar-clearing candidates.

5. Coherent Integration:
   - Realizing `X_{n+1}` yields layer `L_{n+1}` with gap `Delta_{n+1} := kappa(L_{n+1})`.

### 3.6 Additional Structural Commitments That Matter in Practice

Beyond the five axioms, implementation and analysis depend on stronger structural commitments.

1. Constructive Irreducibility Boundary:
   - If operation is derivable in existing eliminator/recursor fragment, it must not be charged as new clause.
   - If it requires non-constructive analytic existence machinery, it is structurally irreducible and must be explicit in API clauses.

2. Fully Coupled Type Theory (FCTT) modeling postulate:
   - New structure must define interactions with full active interface, not only local internals.
   - This maximal coupling drives obligation recurrence and selection pressure.

3. Sealing encapsulation:
   - Resolved obligations become opaque exports.
   - Future steps interact with sealed interface, not raw historical internals.

These assumptions are central to deriving recurrence and bar dynamics.

### 3.7 Coherence Window and Recurrence

Coherence window `d` is minimal history depth after which induced obligations stabilize.

1. Extensional systems:
   - `d = 1`.

2. Intensional HoTT/Cubical systems:
   - `d = 2`.

Given Integration Trace Principle and interface depth `d`:

- `Delta_{n+1} = sum_{j=0..d-1} Delta_{n-j}`

For `d = 2`:

- `Delta_{n+1} = Delta_n + Delta_{n-1}` (Fibonacci recurrence).

This implies rising coherence debt and drives nontrivial bar escalation.

### 3.8 Spectral Decomposition of Novelty

Novelty is decomposed into three axes:

1. `nu_G` (Grammar / Introduction)
2. `nu_H` (Homotopy / Computation)
3. `nu_C` (Capability / Elimination)

with:

- `nu = nu_G + nu_H + nu_C`.

This decomposition is important because a system can look productive in one axis while stalling in others; intelligent search should reason about deficits across axes.

## 4. The Full 15-Step Target Sequence

Paper table (from `pen_paper.tex`) gives:

| n | tau | Structure | Delta_n | nu | kappa | rho | Phi_n | Omega_(n-1) | Bar |
|---|-----|-----------|---------|----|-------|-----|-------|-------------|-----|
| 1 | 1 | Universe U0 | 1 | 1 | 2 | 0.50 | - | - | - |
| 2 | 2 | Unit | 1 | 1 | 1 | 1.00 | 1.00 | 0.50 | 0.50 |
| 3 | 4 | Witness | 2 | 2 | 1 | 2.00 | 2.00 | 0.67 | 1.33 |
| 4 | 7 | Pi/Sigma | 3 | 5 | 3 | 1.67 | 1.50 | 1.00 | 1.50 |
| 5 | 12 | Circle S1 | 5 | 7 | 3 | 2.33 | 1.67 | 1.29 | 2.14 |
| 6 | 20 | PropTrunc | 8 | 8 | 3 | 2.67 | 1.60 | 1.60 | 2.56 |
| 7 | 33 | Sphere S2 | 13 | 10 | 3 | 3.33 | 1.62 | 1.85 | 3.00 |
| 8 | 54 | H-space S3 | 21 | 18 | 5 | 3.60 | 1.62 | 2.12 | 3.43 |
| 9 | 88 | Hopf fibration | 34 | 17 | 4 | 4.25 | 1.62 | 2.48 | 4.01 |
| 10 | 143 | Cohesion | 55 | 19 | 4 | 4.75 | 1.62 | 2.76 | 4.46 |
| 11 | 232 | Connections | 89 | 26 | 5 | 5.20 | 1.62 | 3.03 | 4.91 |
| 12 | 376 | Curvature | 144 | 34 | 6 | 5.67 | 1.62 | 3.35 | 5.42 |
| 13 | 609 | Metric + frame | 233 | 46 | 7 | 6.57 | 1.62 | 3.70 | 5.99 |
| 14 | 986 | Hilbert functional | 377 | 62 | 9 | 6.89 | 1.62 | 4.13 | 6.68 |
| 15 | 1596 | DCT | 610 | 103 | 8 | 12.88 | 1.62 | 4.57 | 7.40 |

Key dynamic landmarks:

1. Step 6 is very tight (smallest margin over bar in the paper narrative).
2. Steps 10-14 are API-like axiomatic packages, not mere primitive constructor steps.
3. Step 15 is a large synthesis jump, then termination.

## 5. PEN Axioms and Model as They Exist in the Codebase

The repository has two mechanized pillars:

1. Agda formal side (`agda/`), with recurrence and saturation modules.
2. Haskell executable side (`engine/`), where the discovery process is run.

Important practical note for expert reviewers:

1. `pen_paper.tex` is the primary narrative/formula source for current target sequence.
2. Some legacy constants in older modules may still reflect earlier calibration snapshots.
3. The strict claim-grade runtime path is primarily represented by `RunAbInitio`, `TelescopeEval`, `MBTTNu`, `StructuralNu`, `AgendaSearch`, and supporting modules.

## 6. Current Discovery Engine Architecture (High Level)

### 6.1 Top-level loop (`engine/src/RunAbInitio.hs`)

Per step:

1. Compute bar from discovered history and d-bonacci recurrence.
2. Compute adaptive search budget from memory pressure.
3. Generate candidate telescopes from phase-A lanes:
   - agenda-first lane (`AgendaSearch`) when active;
   - MBTT fallback lane (`MBTTEnum`) when needed.
4. Type-check / structural validity filtering (`TelescopeCheck`).
5. Semantic prefilter (`semanticDeltaScore`).
6. Evaluate `(nu, kappa, rho)` via mode-specific evaluator.
7. Optionally run MCTS portfolio/fallback lane.
8. Keep bar-clearing candidates.
9. Canonical quotient dedupe.
10. If empty, optionally run bounded rescue expansion lane.
11. Select best candidate by rank tuple:
   - higher structural utility first,
   - then minimal overshoot,
   - then lower kappa,
   - then canonical key and source preference.
12. Insert selected entry into discovered library.
13. Repeat.

### 6.2 Evaluation modes and paper-independence

`TelescopeEval` exposes:

1. `EvalPaperCalibrated`
2. `EvalStrictComputed`
3. `EvalStructural`

Claim-grade modes are strict/structural, not paper mode.

Recent phase-6 first cut:

1. strict/structural insertion now uses structural conversion path;
2. paper-calibrated path remains explicit and isolated.

### 6.3 Candidate generation lanes

1. MBTT enumeration lane (`MBTTEnum`)
   - Budget/depth bounded expression generation.
   - Telescope streaming over context depths.
   - Heuristic retention frontier.
   - Macro seed lane.

2. Agenda lane (`AgendaSearch`)
   - Explicit `SearchState` with obligations and capabilities.
   - Safe closure then unsafe branching.
   - Premise top-k gating.
   - Dominance cache + `rhoUB` prune.
   - Macro learning/reuse.
   - Local MBTT expression leaf solver.

3. MCTS lane (`MCTS`)
   - Progressive widening tree over partial telescopes.
   - Rollout completion and UCT updates.
   - Portfolio role, not sole default.

### 6.4 Memory/pagefile policy

`RunAbInitio` includes adaptive memory budget logic:

1. Reads RTS memory stats when enabled.
2. Classifies pressure into low/moderate/high/critical.
3. Scales bit budget, AST depth, candidate cap, MCTS budget by step and pressure.
4. Can force MCTS off under critical pressure.

This stabilizes runtime but does not solve combinatorial search quality by itself.

## 7. Current Algorithm in Pseudocode

### 7.1 Outer synthesis loop (current)

```text
function AB_INITIO(cfg):
  lib = []
  history = []
  step = 1

  while step <= cfg.maxSteps:
    bar = computeBarD(cfg.window, cfg.mode, step, history)
    budget = computeSearchBudget(cfg, step)
    goalProfile = deriveGoalProfile(lib)
    emode = toEvalMode(cfg.mode)

    agendaCandidates = []
    if agendaActive(cfg, lib, step):
      agendaCandidates = agendaGenerateCandidates(lib, goalProfile, agendaCfg(budget, bar, step))

    fallbackCandidates = []
    if needsFallback(agendaCandidates, cfg):
      fallbackCandidates = enumerateMBTTTelescopes(lib, mbttCfg(budget, goalProfile))

    rawTelescopes = choosePrimaryLane(agendaCandidates, fallbackCandidates, cfg)
    telescopes1 = canonicalDedupe(rawTelescopes)
    validTelescopes = checkAndFilter(lib, telescopes1)
    telescopes2 = canonicalDedupe(validTelescopes)

    semScored = parallelMap(semanticDeltaScore(tele, lib, goalProfile), telescopes2)
    selectedForEval = topSemantic(semScored, budget)
    enumEvaluated = parallelMap(evalTelescope(emode, tele, lib, nuDepth, history), selectedForEval)
    enumEvaluated = filter(nu > 0, enumEvaluated)

    mctsCandidates = []
    if shouldRunMCTS(cfg, budget, enumEvaluated, bar, step):
      mctsCandidates = mctsSearchStep(emode, mctsCfg(budget), lib, bar)

    rawCandidates = enumEvaluated + mctsCandidates
    assertClaimGradeSources(rawCandidates, cfg.mode)

    viable = filter(rho >= bar, rawCandidates)
    viable = quotientCandidates(viable)

    if empty(viable) and shouldRescue(cfg, step):
      rescue = boundedRescueExpansion(lib, goalProfile, emode, history, bar)
      viable = quotientCandidates(filter(rho >= bar, rescue))

    if empty(viable):
      stop

    best = argmin_rank(viable, rank = (
      -semanticDeltaScore,
      rho - bar,
      kappa,
      canonicalKey,
      sourceRank
    ))

    bestName = detectCanonicalName(best.tele, lib)
    discoveredEntry = mkLibraryEntryByMode(cfg.mode, lib, best.tele, bestName)
    lib = lib + [discoveredEntry]
    history = history + [(best.nu, best.kappa)]
    step += 1

  return lib, history
```

### 7.2 Agenda lane (current)

```text
function AGENDA_GENERATE(lib, goalProfile, cfg):
  startState = deriveSearchState(lib, goalProfile, entries=[])
  agenda = [Node(startState, certs=[])]
  finals = []
  seen = {}
  domCache = {}
  macroBank = {}
  iter = 0

  while iter < cfg.maxAgendaStates and not empty(agenda):
    node = popBestByNodePriority(agenda)
    node = safeClosure(node, macroBank, cfg.safeClosureSteps)
    st = node.state

    if stateKey(st) in seen:
      iter += 1
      continue
    seen.add(stateKey(st))

    if not passesRhoUB(st, cfg.barFloor):
      iter += 1
      continue

    if dominated(st, domCache):
      iter += 1
      continue
    insertFrontier(st, domCache)

    if isTerminal(st, cfg.maxEntries):
      finals.append(node)
      iter += 1
      continue

    actions = validActionsWithProfile(goalProfile, hole(st.entries), lib)
    actions = filterPremiseAllowed(actions, st.premisesTopK(cfg.premiseTopK))
    actions = preferUnsafeUnlessNone(actions)
    actions = rankBy(
      safeBonus,
      macroActionBonus,
      actionGoalGain,
      actionPriority,
      formRank
    )
    actions = take(cfg.branchPerState, actions)

    for act in actions:
      child = applyTransition(node, act, macroBank, cfg.leafSolver)
      if child exists:
        if passesRhoUB(child.state) and not dominated(child.state):
          updateDominance(child.state)
          updateMacroBankFromTransition(child.certificate)
          agenda.push(child)

    iter += 1

  return topCandidatesFrom(finals + seedCandidates)
```

### 7.3 Structural novelty evaluation (current)

```text
function STRUCTURAL_NU(tele, lib, nuHistory):
  if triviallyDerivable(tele, lib):
    return 0

  cls = classifyTelescope(tele, lib)
  nuG = computeNuG(cls, tele, lib)
  nuH_base = computeNuH(tele)            # m + d^2 for path constructors
  nuC_base = computeNuC(cls, tele, lib, nuHistory)

  distBonus = 0
  univPolyBonus = 0
  infShiftBonus = 0

  if cls == Synthesis:
    distBonus = detectDistributiveLaws(tele, lib, nuHistory)
    univPolyBonus = detectUniversePolymorphism(tele, lib)
    infShiftBonus = detectInfinitesimalShift(tele, lib)

  nuH = nuH_base + infShiftBonus
  nuC = nuC_base + distBonus + univPolyBonus
  return nuG + nuH + nuC
```

## 8. Why the System Still Feels Too Brute-Force

Despite strong improvements, the current behavior still has brute-force signatures.

### 8.1 Intelligence still arrives late

The most consequential issue is sequencing:

1. Candidate generation still often creates many syntactically legal objects first.
2. High-level structure is often inferred later via ranking, instead of enforced during construction.

Even with agenda search, MBTT expression enumeration remains a major substrate and still drives significant branching. This makes intelligence "post-hoc filtering" rather than "first-principles derivation."

### 8.2 Rescue lane is still structurally hand-shaped

Current fallback rescue includes bounded structural former/HIT rescue templates.

This is pragmatic and has helped step 4/5 quality under strict time caps, but to an external theorist it still looks like:

1. if primary search fails, inject mini-template banks,
2. then evaluate and select.

That behavior is closer to engineered search scaffolding than to autonomous proof planning.

### 8.3 Goal model is present but not fully causal

`SearchState` includes obligations and capability signatures, which is a big step.
However:

1. Obligations are still coarse and mostly hand-crafted categories.
2. Transition admissibility is not yet strongly tied to explicit proof debt decomposition.
3. There is limited explicit notion of "proof object under construction with open holes and local theorem obligations."

In human mathematical work, "what remains to prove" is first-class. In the current engine it is present but not yet the hard governor of every move.

### 8.4 Local leaf solving still acts like bounded enumeration

Agenda transitions call a local leaf expression solver using MBTT enumeration bounds. This is useful, but the behavior remains:

1. generate a small pool of matching expressions,
2. rank by heuristic.

This is better than global brute force, but still not full obligation-specific synthesis by theorem constraints.

### 8.5 Action model still broad and mostly syntax-shaped

Actions are constructor-level (`APi`, `ASigma`, `ALam`, etc.) and not yet richer proof actions like:

1. "discharge NeedTypeFormer by introducing dependent abstraction with witness compatibility,"
2. "lift curvature compatibility through existing modal bridge."

Those richer actions require typed sub-goals and certificates at a finer grain.

### 8.6 MCTS remains secondary and rollout-heavy

MCTS now has improved transition integrity and full-path backprop, but:

1. it remains rollout-based completion,
2. rollout generation is still random-weighted over constructors with heuristics.

That is useful for coverage, not for mathematically elegant derivation planning.

### 8.7 Residual target-aware pressure exists in architecture even when no paper values are used

Even in strict structural mode, some influences are still "target-shaped" at architecture level:

1. Canonical name detection remains pervasive for reporting and path labeling.
2. Macro seeds and readiness heuristics are tuned around expected structural deficits.
3. Reference telescope infrastructure is deeply integrated in acceptance checks and many module assumptions.

This does not imply cheating, but it does increase perceived target awareness.

### 8.8 Calibration drift between paper and some constants

Paper table gives step-13/14/15 novelty as 46/62/103.
Some legacy mappings in parts of code still include 43/60/105 snapshots.

Even if strict structural mode bypasses those paths for claim-grade runs, this drift creates conceptual and audit friction, and expert reviewers will notice.

### 8.9 Runtime quality remains fragile on key transitions

Current empirical status:

1. early prefix is strong (steps 1-5 under tight cap);
2. downstream quality and speed still require rescue and heavy tuning.

This is exactly where "intelligent mathematical process" should dominate brute-force breadth.

## 9. What We Mean by "Intelligent Mechanized Mathematical Thought"

This must be operational, not rhetorical.

The desired behavior has the following characteristics:

1. Explicit objective state:
   - open obligations are first-class.
   - candidate state has a proof debt profile.

2. Forced moves before speculative moves:
   - safe closure to fixpoint.
   - only then branch.

3. Premise retrieval as deliberate relevance search:
   - not broad `ALib` fanout.
   - typed premise ranking conditioned on obligation.

4. Reuse of discovered structure:
   - learned macros as typed abstractions with retention tests.
   - no unbounded macro swamp.

5. Dominance pruning with theorem-like bounds:
   - if no possible continuation can clear bar, prune immediately.

6. Explainable derivation:
   - each transition has certificate, premises used, obligations discharged, capabilities gained.

7. Invariance:
   - name scrambling and irrelevant reorderings do not alter strict behavior.
   - no paper lookup in strict/structural discovery.

8. Efficiency target:
   - first 9 steps correct in <60 seconds on target machine profile.

## 10. Constraints We Must Preserve During Redesign

These are non-negotiable for claim-grade evidence.

1. No paper-value substitution in strict/structural discovery path.
2. No step-index injection of reference telescopes.
3. No hidden canonical-name unlocking in strict scoring path.
4. No explicit or subtle target-sequence injection in generation policy.
5. Deterministic or tightly controlled reproducibility settings for evidence.
6. CI guards against forbidden sources or leakage patterns.
7. Runtime invariance tests under name scrambling and equivalent rewrites.

## 11. Current Progress Snapshot (Phased Roadmap)

As of this branch state:

1. Phase 1 first execution slice is in place:
   - explicit `SearchState`, obligations, capability tracking, certificates.
2. Phase 2 first pass:
   - safe-closure plus unsafe branching discipline.
3. Phase 3 first pass:
   - obligation-conditioned premise scoring and top-k premise gating.
4. Phase 4 first pass:
   - `rhoUB` pruning and dominance cache.
5. Phase 5 first pass:
   - macro learning/retention and agenda-time macro application.
6. Phase 7 first pass:
   - agenda-primary outer lane, MBTT as local leaf solver/fallback.
7. Phase 9 first-pass guard:
   - claim-grade forbidden source assertion for candidate sources.
8. Phase 6 first cut:
   - strict/structural conversion path decoupled from name-mediated capability hints on insertion/evaluation conversion path.

Remaining high-priority work:

1. finish phase-6 internals end-to-end (all residual name unlock paths),
2. phase-8 portfolio scheduler formalization,
3. stronger runtime invariance and leakage gates,
4. quality/runtime hardening through step 9.

## 12. The Core Technical Problem to Give the Expert

If we reduce this to one sentence:

How do we convert a still enumeration-heavy symbolic discovery engine into a proof-state-first obligation-discharge system that autonomously reconstructs the PEN sequence with high confidence, strong invariance, and sub-minute prefix performance?

A longer precise framing:

1. We already have a strong formal objective (`rho = nu/kappa` under bar dynamics).
2. We already have typed syntax, structure checks, and novelty decomposition.
3. We already have partial proof-state search.
4. But we do not yet have a mathematically principled, low-branching move system that:
   - generates only meaningfully promising transitions,
   - composes lemmas/premises intelligently,
   - avoids rescue/template dependence,
   - remains leakage-safe and auditable.

The request is not for ML black-box priors. It is for symbolic algorithmic design that behaves like rigorous theorem construction.

## 13. Concrete Questions for the Expert

Please ask for concrete algorithmic prescriptions on these points.

1. Search object design:
   - What is the right minimal proof-state representation to make obligations, capability deficits, and admissibility debt first-class?

2. Rule taxonomy:
   - How should we partition deterministic safe rules versus speculative unsafe rules in a way that is both mathematically sound and computationally effective?

3. Obligation calculus:
   - How fine-grained should obligations be?
   - Should obligations be typed by expected novelty-axis impact (`nu_G/nu_H/nu_C`)?

4. Premise retrieval:
   - What symbolic retrieval strategy best approximates human premise selection under this type-theoretic setting?

5. Move valuation:
   - What upper bounds and lower bounds should be used to prune provably hopeless states early?
   - How to avoid bounds that over-prune subtle constructive paths?

6. Macro learning:
   - Which abstraction-learning criterion is principled enough to avoid overfitting while still gaining speed?

7. Equivalence compression:
   - Is e-graph style equality saturation worthwhile at subexpression level for this domain?
   - What is the lowest-risk insertion point?

8. Portfolio scheduling:
   - What principled scheduler should allocate budget across agenda, local leaf solving, and MCTS-like long-horizon search?

9. Correctness versus speed:
   - How to formally reason about when aggressive pruning preserves discovery guarantees under bar dynamics?

10. Invariance architecture:
    - How to prove (or strongly certify) that strict mode is invariant under name permutation and non-semantic syntax rewrites?

11. Anti-leakage verification:
    - What static and dynamic checks should be mandatory for publication-grade autonomy claims?

12. Human-like process criterion:
    - Which measurable behavioral signatures best demonstrate "mathematical thought" over "search plus reranking"?

## 14. Proposed "Current vs Desired" Algorithm Contrast

### 14.1 Current pattern (simplified)

```text
Generate many candidate telescopes
-> filter invalid
-> semantic score
-> evaluate nu/kappa/rho
-> choose best bar-clearing candidate
```

### 14.2 Desired pattern

```text
Maintain explicit proof/search state with obligations
-> run safe rules to closure
-> select highest-value obligation
-> retrieve top premises/macros relevant to that obligation
-> apply cheapest valid move that reduces proof debt and preserves future bar potential
-> continue until complete candidate with certificate
-> score completed candidate
```

The desired flow makes candidate generation itself intelligent rather than relying on downstream scoring to rescue quality.

## 15. Important Subtleties and Known Tensions

### 15.1 Name-independence versus practical engineering

We need names for reporting and comparability, but not for strict semantics.  
Current state is improved but not yet perfectly clean across all internals.

### 15.2 Structural heuristics versus hidden sequence pressure

Heuristics like class readiness and representation bonuses are structurally motivated, but because they are tuned in a known target landscape, expert review may still classify them as implicit guidance unless formalized as theorem-level admissibility constraints.

### 15.3 Enumeration is not evil, but role matters

A tiny local expression solver can be useful as a leaf tactic.  
Global telescope enumeration as primary strategy undermines the "intelligent mechanized reasoning" claim.

### 15.4 Paper constants versus code constants

Any mismatch between narrative reference values and code lookup tables must be resolved or explicitly isolated to non-claim-grade paths to avoid audit ambiguity.

## 16. Requested Deliverable From the Expert

We want a concrete design memo, not a conceptual essay.

Ideal output from expert:

1. A formal search-state schema and transition algebra.
2. A safe/unsafe rule framework with prioritization law.
3. A pruning and dominance theorem sketch suitable for implementation.
4. A premise retrieval policy with explicit scoring terms.
5. A macro abstraction learning criterion with retention guarantees.
6. A minimal benchmark protocol proving first-9-in-<60s without leakage.
7. A validation checklist for publication-grade autonomy claims.

## 17. Reproducibility Notes for the Expert

Repository root: `C:\DEV\pen`

Key files to inspect first:

1. `pen_paper.tex` (model, axioms, sequence)
2. `engine/src/RunAbInitio.hs` (orchestration)
3. `engine/src/AgendaSearch.hs` (proof-state lane)
4. `engine/src/ProofState.hs` (state/obligations)
5. `engine/src/MBTTEnum.hs` (enumeration and local solver substrate)
6. `engine/src/MCTS.hs` (portfolio long-horizon lane)
7. `engine/src/TelescopeEval.hs` (mode behavior and strict/paper boundary)
8. `engine/src/StructuralNu.hs` and `engine/src/MBTTNu.hs` (native novelty)
9. `engine/src/AcceptanceSuite.hs` (invariance and contract tests)
10. `intelligent_discovery_plan.md` (current phase status and open work)

Representative run command used during tuning:

```powershell
cd C:\DEV\pen\engine
cabal run ab-initio -- --phase1-shadow --max-steps 5 --skip-validation +RTS -N -RTS
```

Structural mode shadow run:

```powershell
cd C:\DEV\pen\engine
cabal run ab-initio -- --structural --phase1-shadow --max-steps 5 --skip-validation +RTS -N -RTS
```

Acceptance harness:

```powershell
cd C:\DEV\pen\engine
cabal run acceptance-core
cabal run acceptance-mbtt -- --mbtt-fast --mbtt-max-candidates 80
```

## 18. Closing Statement

The system is no longer naive brute force, but it is not yet at the level where the generation process itself unmistakably resembles theorem construction by constrained logical necessity.

We have crossed from "pure enumeration" into "hybrid guided search."  
The remaining step is to cross from "guided search" into "obligation-native mathematical reasoning."

That final step is architectural, not cosmetic:

1. represent mathematical debt explicitly,
2. prioritize forced moves,
3. retrieve only relevant premises,
4. prune impossibilities early,
5. learn reusable abstractions safely,
6. keep strict invariance and no-leakage guarantees intact.

This is the exact problem we want guidance on.

## 19. Step-by-Step Deep Dive: What Should Be Discovered, Why, and How Intelligence Should Look

This section gives a detailed interpretation of each step in the intended sequence, with emphasis on:

1. the mathematical role,
2. the selection pressure logic,
3. what an intelligent search policy should infer,
4. how current code approximates that behavior,
5. where brute-force behavior still appears.

### 19.1 Step 1: Universe

Mathematical role:

1. Introduce a universe `U0` so types become first-class entities in the system.
2. This is the minimal foundational move that makes typed construction possible.

Selection pressure:

1. At startup, only very low-complexity candidates are admissible.
2. Universe is minimal but not vacuous; it opens a type formation regime.

Intelligent move interpretation:

1. A mathematician-like policy sees that no nontrivial structured candidate can be meaningful before a type universe exists.
2. Therefore this step is forced by foundational necessity rather than opportunistic score-chasing.

Current engine behavior:

1. Usually recovered quickly in strict and structural runs.
2. Appears from MBTT/agenda with low complexity and high foundational relevance.

Residual brute-force smell:

1. Even here, the system still may enumerate many tiny syntactic alternatives before selecting.
2. Ideally this should be a near-deterministic forced move from empty state.

### 19.2 Step 2: Unit

Mathematical role:

1. Add canonical simple inhabitant-bearing type.
2. Enables witness-level construction and elimination structure.

Selection pressure:

1. Bar is still low but rising.
2. Candidate must create usable constructor semantics without excessive specification burden.

Intelligent move interpretation:

1. This is the cheapest move that converts universe-level type existence into concrete inhabited object-level data.
2. It unlocks witness and elimination forms in the minimal way.

Current engine behavior:

1. Usually stable and fast.
2. Quality is strong across strict/structural modes.

Residual brute-force smell:

1. Search still evaluates many variations with similar triviality.
2. A proof-state engine should model this as a direct obligation discharge: NeedWitness-preconditioned primitive constructor.

### 19.3 Step 3: Witness

Mathematical role:

1. Add an inhabitant witness (`star : 1`) and associated elimination impact.
2. This converts type-level infrastructure into term-level constructive power.

Selection pressure:

1. Must increase derivational capability enough to keep pace with rising bar.
2. Must remain cheap in `kappa`.

Intelligent move interpretation:

1. Once unit exists, producing a witness is logically immediate and high leverage.
2. This move should be recognized as low-risk, high-value, and structurally inevitable.

Current engine behavior:

1. Stable in early shadow runs.
2. Usually appears from foundational lane quickly.

Residual brute-force smell:

1. The system still relies on broad action pools and syntactic checks instead of direct "witness obligation solved" transitions.

### 19.4 Step 4: Pi/Sigma

Mathematical role:

1. Introduce dependent function and sum infrastructure.
2. This is the first significant representational lift.

Selection pressure:

1. Bar now requires nontrivial novelty.
2. Candidate must avoid too-low-cost trivial forms and too-expensive overengineered constructs.

Intelligent move interpretation:

1. Once witness + base typing exists, dependent formers are the natural next expressive lift.
2. Search should detect this via obligation deficits (`NeedTypeFormer`) and capability gaps, not by target sequence memory.

Current engine behavior:

1. Recent optimization has made step 4 often emerge directly from agenda primary lane under 30-second capped runs.
2. This is an important improvement over rescue dependence.

Residual brute-force smell:

1. To stabilize this step, system still uses quality boosts, structural heuristics, and bounded rescue options when needed.
2. This suggests transition valuation is not yet principled enough to make step-4-like moves uniformly robust.

### 19.5 Step 5: Circle S1

Mathematical role:

1. First nontrivial homotopy object with loop structure.
2. Opens path-based novelty axis (`nu_H`) in a meaningful way.

Selection pressure:

1. Candidate must now produce enough topological/computational novelty to beat a larger bar.
2. Overly simple map-like candidates are usually insufficient.

Intelligent move interpretation:

1. The system should infer that after dependent formers, a loop-bearing HIT provides maximal novelty-per-effort progression.
2. This is where mathematical intuition starts to matter: not any new type, but one with high coherent path consequence.

Current engine behavior:

1. Currently recovered in strict and structural capped runs.
2. Agenda quality improved, but novelty calibration has needed tuning in strict mode.

Residual brute-force smell:

1. Step-5 quality may still require additional ranking calibration to avoid overcount or mis-preferred variants.
2. The system can still look like "find any bar-clearing candidate then patch representation quality."

### 19.6 Step 6: PropTrunc

Mathematical role:

1. Introduces propositional truncation, controlling information level and proposition collapse behavior.
2. Important bridge object for subsequent homotopy development.

Selection pressure:

1. In paper narrative this is one of the tightest margin steps.
2. Slight extra specification overhead could cause bar failure.

Intelligent move interpretation:

1. Search should identify truncation as a high-necessity bridge despite tight margin.
2. This is a hallmark of mathematical intelligence: selecting structurally necessary but narrowly viable moves.

Current engine behavior:

1. This step can be sensitive to scoring depth and candidate quality filters.
2. Structural decomposition helps, but robust low-time discovery remains nontrivial.

Residual brute-force smell:

1. Tight-margin success can still depend heavily on bounded search profile settings.
2. Intelligent engine should prioritize this by obligation-chain necessity, not by broad candidate luck.

### 19.7 Step 7: Sphere S2

Mathematical role:

1. Move from 1-dimensional loop object to higher-dimensional sphere structure.
2. Expands topological complexity and prepares for higher coherence interactions.

Selection pressure:

1. Must sustain efficiency against rising bar.
2. Candidate should deepen homotopy richness while staying compact.

Intelligent move interpretation:

1. With S1 and truncation in place, a higher sphere is an expected topological lift.
2. Search should reason over path-dimension deficits and choose this as controlled complexity increase.

Current engine behavior:

1. Step-7 discovery quality is currently downstream of early runtime and representation quality work.
2. More tuning is still needed beyond first five steps.

Residual brute-force smell:

1. Harder later steps still rely on combined heuristics and expanded pools.
2. Not yet a clean obligation-first derivation path.

### 19.8 Step 8: H-space S3

Mathematical role:

1. Not just bare S3 shape, but H-space structure with additional algebraic elimination consequences.
2. Provides richer interaction profile for later mapping/fibration work.

Selection pressure:

1. Competing S3 specifications may all clear bar.
2. Minimal overshoot should select the most structurally economical but sufficient package.

Intelligent move interpretation:

1. System should evaluate specification variants of similar object and choose the one with best continuity to baseline.
2. This is subtle design-space reasoning, not simple constructor counting.

Current engine behavior:

1. Candidate quality at this stage remains challenging under hard runtime caps.
2. Macro and obligation tooling helps but is not yet fully decisive.

Residual brute-force smell:

1. Specification-level comparison still can look like precomputed alternatives plus ranking.
2. Need more explicit proof-debt-aware comparison between competing specs.

### 19.9 Step 9: Hopf

Mathematical role:

1. Introduces map/fibration structure linking previous high-dimensional objects.
2. A key bridge from pure type/HIT growth into structural interaction patterns.

Selection pressure:

1. Candidate needs strong capability novelty, often more in logical interaction axis than formation axis.
2. Must remain integrated with previous sphere and loop content.

Intelligent move interpretation:

1. Search should infer that map-level bridging is now cheaper and more valuable than introducing yet another isolated primitive.
2. This is a move from object introduction to relation introduction.

Current engine behavior:

1. Recovery beyond step 5 is current performance frontier under runtime constraints.
2. This step is a key milestone for proving system is not prefix-specialized.

Residual brute-force smell:

1. If this step only appears with broad fallback expansion, intelligence claim weakens.
2. Need direct obligation-driven map synthesis behavior.

### 19.10 Step 10: Cohesion

Mathematical role:

1. Introduce modal adjoint package (flat, sharp, disc, shape/cohesive variants).
2. This begins the axiomatic API regime.

Selection pressure:

1. Small opaque axiom would fail due low interaction novelty.
2. Unified cohesive package clears because cross-interaction yields sufficient capability growth.

Intelligent move interpretation:

1. Search should prefer coherent API bundle when isolated fragments cannot survive bar.
2. This is where mathematical design intelligence is crucial: package selection by inferential coupling.

Current engine behavior:

1. Structural capability flags and modal detection exist.
2. However, full autonomous path to this step under tight runtime remains a hard target.

Residual brute-force smell:

1. Modal bundles can still be approached through constructor enumeration.
2. Desired mode is theorem-like construction of required compatibility package.

### 19.11 Step 11: Connections

Mathematical role:

1. Differential-geometric connection structure and transport/covariant machinery.
2. Depends on cohesive modalities.

Selection pressure:

1. Candidate must demonstrate meaningful compatibility interactions with existing modal exports.
2. Pure local clauses insufficient unless coupled to prior structures.

Intelligent move interpretation:

1. Once cohesion exists, differential bridge obligations should become explicit and dominant.
2. Search should retrieve cohesive premises and construct minimum compatible connection package.

Current engine behavior:

1. Capability gating for differential layers exists.
2. Still not fully demonstrated in fast autonomous prefix benchmarks.

Residual brute-force smell:

1. Without stronger obligation decomposition, system may over-search irrelevant Pi/Sigma-like constructions.

### 19.12 Step 12: Curvature

Mathematical role:

1. Elevates connection into curvature-level elimination and compatibility structure.
2. Adds identities and operators that materially increase derivation capacity.

Selection pressure:

1. Requires referencing connection-related capability and higher interaction depth.
2. Efficiency survives only if package is coherent and not overburdened.

Intelligent move interpretation:

1. Curvature should emerge as direct continuation of unresolved differential obligations.
2. Premise retrieval should heavily prioritize connection and modal-compatible entries.

Current engine behavior:

1. StructuralNu has explicit class behavior and history-aware components.
2. End-to-end rapid autonomous discovery of this step is still open performance work.

Residual brute-force smell:

1. May still depend on broad expression search with after-the-fact filtering.

### 19.13 Step 13: Metric

Mathematical role:

1. Introduces metric and dependent geometric operator chain.
2. Strongly coupled API package in paper framing.

Selection pressure:

1. Bare metric alone is insufficient in paper argument.
2. Requires enough integrated operators to clear bar.

Intelligent move interpretation:

1. The search should identify dependency chain (`g -> Levi-Civita -> curvature links -> derived operators`) as coherent indivisible package.
2. This is a high-level mathematical abstraction task.

Current engine behavior:

1. Structural classification and axiomatic capability scoring exist.
2. Legacy paper-value tables in some modules differ from current paper totals, creating audit friction.

Residual brute-force smell:

1. Without explicit dependency-chain obligation objects, package emergence can look heuristic.

### 19.14 Step 14: Hilbert

Mathematical role:

1. Functional-analytic operator framework with spectral and variational structure.
2. Bridges geometric side to analysis-style operator reasoning.

Selection pressure:

1. Clearance margin can be narrow in discussion.
2. Clause count growth must be compensated by strong capability gain.

Intelligent move interpretation:

1. Engine should recognize this as minimal high-payoff analytic layer after metric-curvature chain, not arbitrary large package.

Current engine behavior:

1. Capability flags include `leHasHilbert` for readiness.
2. Full autonomous performance in this region remains unproven in short budget profile.

Residual brute-force smell:

1. Search may still try many irrelevant temporal/modal forms before converging to analytic package.

### 19.15 Step 15: DCT

Mathematical role:

1. Synthesis of spatial modalities, temporal operators, and infinitesimal compatibility.
2. In paper narrative this is a high novelty synthesis jump and structural termination trigger.

Selection pressure:

1. Candidate must produce strong synthesis-level novelty components, including structural multipliers in the implementation model.
2. Must remain relatively compact in kappa.

Intelligent move interpretation:

1. System should infer this as closure-level synthesis from unresolved temporal and cross-modal obligations.
2. It should not be a hard-coded target; it should be selected because it uniquely discharges late-stage cross-domain obligations efficiently.

Current engine behavior:

1. StructuralNu includes synthesis detectors:
   - distributive law patterns,
   - universe polymorphism patterns,
   - infinitesimal shift patterns.
2. Strict-mode decoupling from name hints is improved but not fully complete across every internal ranking edge.

Residual brute-force smell:

1. Meta-theorem detectors are structural but may appear pattern-engineered unless generalized and formally justified.
2. Need stronger proof-state evidence that DCT-like synthesis is reached by obligation resolution, not large candidate lottery.

## 20. Detailed Current Architecture Analysis (Module-by-Module)

### 20.1 `RunAbInitio`: orchestration and mode boundary

Strengths:

1. Clear mode separation:
   - paper calibration,
   - strict computed,
   - structural.
2. Adaptive memory/pagefile aware budgeting.
3. Integrated portfolio across agenda, MBTT, and MCTS.
4. Claim-grade source guard to reject paper/reference source labels in strict-like modes.

Weaknesses:

1. Orchestration still couples many tactical heuristics directly into top-level step loop.
2. Rescue lane insertion logic is complex and hand-shaped.
3. Selection rank currently mixes utility and overshoot in fixed tuple; theoretical grounding of this composite objective is weaker than pure PEN selection axiom.
4. Large top-level function can hide conceptual separation between theorem-level logic and engineering fallback controls.

### 20.2 `TelescopeEval`: conversion, classification, evaluation

Strengths:

1. Has explicit conversion policy split:
   - name-mediated caps,
   - structural-only caps.
2. Supports detailed traces for audit.
3. Provides strict `desugaredKappa`.

Weaknesses:

1. Canonical naming and prerequisite machinery still deeply integrated for reporting and some flows.
2. Legacy paper mappings for some late-step values in code differ from paper table, which can create confusion.
3. Structural gating heuristics are useful but still partly ad hoc from expert perspective unless linked to formal obligation calculus.

### 20.3 `ProofState`: first-class state scaffold

Strengths:

1. Good start on capability signatures, obligation types, and state scoring bounds.
2. Explicit premise ranking and action-goal gain mapping.

Weaknesses:

1. `capsFromLibrary` still checks universe via literal name equality for one field.
2. Obligation set is coarse relative to late-step API dependency chains.
3. `nuLower/nuUpper` approximations are heuristic and not yet tied to stronger guarantees.

### 20.4 `AgendaSearch`: most promising intelligence lane

Strengths:

1. Safe closure before unsafe branching.
2. Dominance frontiers and `rhoUB` pruning.
3. Premise top-k gating.
4. Derivation certificates with discharged obligations and capabilities gained.
5. Macro retention criteria and template instantiation.
6. Local leaf solver integration for bounded expression completion.

Weaknesses:

1. Still depends on MBTT expression candidate generation inside transitions.
2. Seeds and deterministic fallback expressions remain handcrafted.
3. Some macro abstraction choices are simple atom templates and may miss higher-order reusable structure.
4. No explicit higher-level proof object with typed hole obligations that map to theorem statements.

### 20.5 `MBTTEnum`: bounded but still combinatorial substrate

Strengths:

1. Typed grammar constraints.
2. Bit-budget and depth controls.
3. Goal-aware pruning and semantic heuristic frontier.
4. Candidate validation and connectivity checks.

Weaknesses:

1. Still fundamentally expression/telescope generation heavy.
2. Even with capped splits, search can grow quickly.
3. Heuristic budget split strategy may miss useful structure or still generate large irrelevant sets depending on context.

### 20.6 `MCTS`: portfolio exploratory lane

Strengths:

1. Progressive widening and UCT path logic.
2. Full-path backprop updates.
3. Validity-aware rollout acceptance statistics.

Weaknesses:

1. Rollout is still random-expression-heavy.
2. Hard to audit as mathematically intentional compared to obligation-driven lane.
3. Partial-function warning and random policy artifacts reduce confidence for claim-grade primary lane usage.

### 20.7 `StructuralNu` and `MBTTNu`: explainable novelty layer

Strengths:

1. Native structural decomposition for `nu_G/nu_H/nu_C`.
2. Explicit trace output for explainability and evidence.
3. Meta-theorem detectors for synthesis structures.

Weaknesses:

1. Detector logic is pattern-based and may appear tailored unless generalized and justified as theorem-family detectors.
2. Some formulas rely on history and class assumptions that need clearer formal derivation alignment.
3. Potential mismatch perception with paper calibrations needs stronger harmonization narrative.

### 20.8 `AcceptanceSuite`: invariance and contract testing

Strengths:

1. Includes name-scramble and classification invariance checks.
2. Includes non-interference and bar-computation checks.
3. Includes MBTT grammar and deterministic trace checks.

Weaknesses:

1. Some tests still use name-mediated conversion helper for library replay.
2. CI leak guards can be strengthened with stricter forbidden-symbol checks in claim-grade entrypoints and runtime assertions.

## 21. Failure Taxonomy: Why Combinatorics Still Dominates at Hard Steps

This section is intentionally diagnostic and concrete.

### 21.1 Failure mode A: combinatorial candidate saturation before semantic closure

Pattern:

1. many candidates are generated,
2. semantic scoring narrows later,
3. expensive evaluation budget consumed before high-value proof moves are isolated.

Effect:

1. runtime blow-up,
2. unstable behavior around tight-margin steps.

### 21.2 Failure mode B: obligation granularity too coarse

Pattern:

1. obligations like `NeedDifferentialBridge` are broad buckets.
2. multiple candidate forms can satisfy superficial signals without advancing the true dependency chain.

Effect:

1. search explores semantically adjacent but unproductive branches,
2. quality depends on heuristics rather than obligation progress theorem.

### 21.3 Failure mode C: action abstraction too syntax-level

Pattern:

1. action space defined in terms of constructors.
2. not enough domain-level derivation actions with explicit preconditions/postconditions.

Effect:

1. many actions have weak guidance signal,
2. branching remains large.

### 21.4 Failure mode D: representation equivalence not compressed enough

Pattern:

1. canonical key dedupe removes exact structural duplicates.
2. many near-equivalent states still survive because equivalence classing is shallow.

Effect:

1. repeated evaluation of logically similar branches,
2. wasted budget and memory churn.

### 21.5 Failure mode E: rescue dependence at transition boundaries

Pattern:

1. primary lane fails to clear bar at key steps,
2. rescue lane injects bounded structured templates.

Effect:

1. short-term performance gains,
2. weakens external perception of pure autonomous reasoning.

### 21.6 Failure mode F: mixed objective semantics

Pattern:

1. PEN axiom is minimal overshoot among admissible bar-clearers.
2. runtime ranking prioritizes semantic utility first and overshoot second.

Effect:

1. practical improvements,
2. but introduces distance between formal principle and implemented selector.

### 21.7 Failure mode G: historical calibration artifacts

Pattern:

1. multiple novelty sources and legacy mappings coexist.
2. strict/structural paths improved, but codebase still contains old calibration tables in some routes.

Effect:

1. audit complexity increases,
2. expert confidence decreases unless path boundaries are formally documented and enforced.

### 21.8 Failure mode H: insufficient theorem-backed pruning

Pattern:

1. current bounds are heuristic but useful.
2. no strong proof that they preserve discovery completeness under bounded budgets.

Effect:

1. conservative settings keep runtime high,
2. aggressive settings risk missing correct path.

### 21.9 Failure mode I: portfolio adaptation not yet principled enough

Pattern:

1. lane activation is mostly rule-based (fallback/portfolio flags).
2. no online regret-minimizing budget allocator with formal reward-per-cost accounting.

Effect:

1. compute may be spent on weaker lanes at wrong times.

### 21.10 Failure mode J: proof certificates not yet central in selection

Pattern:

1. certificates exist but are mainly reporting artifacts.
2. selection still primarily driven by score tuple and bar crossing.

Effect:

1. misses opportunity to prioritize candidates with superior derivational proof narratives.

## 22. What a Strong Expert Redesign Could Look Like

This is not implementation instruction; it is the target for expert critique.

### 22.1 Replace "candidate-first" by "debt-first"

State should include:

1. open obligations,
2. resolved obligations,
3. required interfaces still unsatisfied,
4. minimal completion cost estimates,
5. upper novelty potential bounds.

All transitions should explicitly reduce this debt or increase certified capability.

### 22.2 Typed transition contracts

Every move should carry:

1. precondition on capabilities/premises,
2. postcondition on obligations/capabilities,
3. cost increment,
4. estimated novelty impact interval.

This allows stronger planning and dominance checks.

### 22.3 Force-safe closure as default semantics

The system should aggressively apply deterministic closure rules until fixed point before considering speculative expansion.

This mirrors mathematical practice:

1. finish all forced obligations first,
2. then choose the next non-forced design move.

### 22.4 Premise retrieval as theorem-level service

Implement a dedicated symbolic retriever:

1. query = current obligation + local context signature,
2. retrieve top-k premises by structural relevance and interface compatibility,
3. explain retrieval via human-readable rationale.

### 22.5 Learn abstractions from successful derivation motifs

Macro learning should move beyond atom templates to typed mini-derivation skeletons that:

1. recur across successful traces,
2. have clear obligation-to-capability mapping,
3. pass retention thresholds on support, debt reduction, and branch reduction.

### 22.6 Tight equivalence compression

Use stronger equivalence classes at subexpression or proof-state level:

1. canonical normalization beyond syntactic keys,
2. optionally e-graph-inspired local saturation where profitable.

### 22.7 Principled lane scheduler

Use adaptive budget allocation:

1. track reward-per-cost over recent window per lane,
2. allocate proportionally with exploration floor,
3. demote chronically low-yield lanes.

### 22.8 Certify strict invariance and no-leakage in CI

Add mandatory gates:

1. forbidden symbol scans for claim-grade entrypoint,
2. no-paper lookup assertions in strict runtime path,
3. name-scramble invariance smoke tests,
4. no step-index injection assertions.

## 23. Benchmark and Evaluation Protocol to Hand the Expert

### 23.1 Primary target metric

1. First 9 steps correct under strict or structural claim-grade mode.
2. Total wall time < 60 seconds on target machine profile.

### 23.2 Secondary metrics

1. Nodes expanded per step.
2. Branch factor before and after safe closure.
3. Premise fanout and hit rate.
4. Dominance prune ratio.
5. Macro reuse rate and branch savings.
6. Viable-candidate density.
7. Rescue activation frequency.
8. Peak memory and memory pressure class distribution.

### 23.3 Fairness constraints for experiments

1. No paper mode for claim-grade metrics.
2. No hand-injected step-index templates.
3. Keep same grammar and admissibility checks across ablations unless explicitly testing those.
4. Record seeds and run configs for reproducibility.

### 23.4 Runtime policy during tuning

Given current iterative workflow:

1. use 30-second capped runs for rapid feedback loops,
2. prioritize step-prefix stabilization (1-4, then 1-5, then 1-6...) before full-depth scaling,
3. exploit full CPU with `+RTS -N`,
4. treat rescue lane usage as temporary debt, not success criterion.

## 24. Additional Clarifications for External Reviewer

### 24.1 This is not a request for a neural black box

The project requirement is symbolic and auditable.

Desired improvements should remain:

1. interpretable,
2. structurally justified,
3. leakage-safe,
4. reproducible.

### 24.2 This is not a request for target memorization tricks

The challenge explicitly forbids:

1. explicit sequence injection,
2. hidden name-based guidance in strict path,
3. paper-value substitution.

### 24.3 This is not a request to remove all enumeration

Enumeration is acceptable as:

1. bounded local tactic,
2. fallback micro-solver,
3. ablation baseline.

It is not acceptable as primary outer discovery mechanism for the intelligence claim.

### 24.4 What "autonomous" should mean here

Autonomous should mean:

1. selection and construction arise from fixed formal laws and current discovered state,
2. no external oracle of intended step identity,
3. reproducible behavior under controlled random seeds and invariance transformations.

## 25. Suggested Request Text You Can Send to the Expert

You can give the following directly to the expert:

"We have an executable symbolic system intended to rediscover a 15-step type-theoretic generative sequence under the Principle of Efficient Novelty (PEN). We have a formal objective and strong filtering/evaluation machinery, but candidate generation still behaves too much like bounded brute-force with heuristics. We need guidance on redesigning the engine so it behaves like intelligent mechanized mathematics: explicit obligations, safe-first closure, premise retrieval, abstraction reuse, dominance pruning with valid bounds, and strict no-leakage invariance. Please propose a concrete architecture and algorithmic plan that can plausibly recover the first 9 steps under 60 seconds on a commodity multi-core machine, without target injection or paper-value lookup in strict mode."

## 26. Final Framing

The project is currently at an inflection point:

1. The formal model is strong and explicit.
2. The executable system has crossed into meaningful guided search.
3. The remaining gap is architectural intelligence, not raw compute.

The expert's contribution can be decisive if focused on:

1. obligation-native search object design,
2. mathematically grounded pruning and retrieval,
3. symbolic abstraction learning strategy,
4. publication-grade invariance and leakage certification.

If those are solved, the claim shifts from "we can tune search to recover a sequence" to "the machine genuinely reasons through constrained mathematical necessity."

## 27. Reference Pseudocode for the Desired Intelligent Engine

This section gives a concrete target design sketch. It is intentionally symbolic, deterministic-by-default, and auditable.

### 27.1 Core state and transition contracts

```text
State S:
  entries           # partial telescope
  obligations       # open proof debt items
  capabilities      # structural capability signature
  premisePool       # ranked premises for current obligations
  certTrace         # derivation certificates
  kappaSoFar
  nuLower, nuUpper  # bounded novelty estimate
  admissibilityInfo # window/depth/typing metadata

Transition T:
  name
  precondition(S) -> Bool
  apply(S) -> S'
  debtDelta(S,S')           # obligations discharged / added
  capDelta(S,S')            # capability gain
  costDelta(S,S')           # kappa increment
  noveltyDeltaInterval      # conservative estimate
  proofCertificate          # deterministic audit object
```

### 27.2 Main discovery loop (desired)

```text
function DISCOVER_STEP(lib, history, config):
  bar = computeBar(history, config.window)
  root = initState(lib, deriveGlobalGoals(lib))
  frontier = PriorityQueue()
  push(frontier, root, priority(root))
  visited = {}
  pareto = {}

  while budgetRemaining(config):
    S = popBest(frontier)

    if seenEquivalent(S, visited):
      continue
    markSeen(S, visited)

    if not typeAdmissible(S):
      continue
    if not canStillBeatBar(S, bar):       # rhoUB pruning
      continue
    if dominated(S, pareto):
      continue
    updatePareto(S, pareto)

    # deterministic closure phase
    S_closed = safeClosureFixpoint(S)
    if S_closed != S:
      push(frontier, S_closed, priority(S_closed))
      continue

    if isCompleteCandidate(S_closed):
      candidate = finalize(S_closed)
      if candidate.rho >= bar:
        return candidate
      else:
        continue

    # choose next obligation with highest debt pressure
    g = selectCriticalObligation(S_closed)

    # symbolic premise retrieval
    premises = retrievePremises(lib, S_closed, g, topK=config.topKPremises)

    # generate transitions directly for obligation g
    moves = deriveMovesForObligation(S_closed, g, premises)

    # rank moves by forcedness, expected debt reduction, cost, and invariance safety
    ranked = rankMoves(moves, S_closed, bar)

    for move in take(config.maxMovesPerState, ranked):
      S_next = applyMove(S_closed, move)
      if validatesTransition(S_closed, S_next, move):
        learnAbstractionIfUseful(S_closed, move, S_next)
        push(frontier, S_next, priority(S_next))

  return FAIL_NO_BARCLEARING
```

### 27.3 Safe closure discipline (desired)

```text
function safeClosureFixpoint(S):
  repeat:
    changed = False
    for rule in SAFE_RULES:
      if rule.precondition(S):
        S2 = rule.apply(S)
        if improvesDeterministically(S, S2):
          S = S2
          changed = True
  until not changed
  return S
```

### 27.4 Premise retrieval (desired)

```text
function retrievePremises(lib, S, obligation g, topK):
  scored = []
  for each premise p in lib:
    score =
      w1 * classCompatibility(g, p) +
      w2 * capabilityCompatibility(g, p) +
      w3 * pathDimensionFit(g, p) +
      w4 * modalityTemporalFit(g, p) +
      w5 * recencySignal(p) +
      w6 * traceAffinity(S.certTrace, p)
    scored.append((p, score))
  return topKByScore(scored)
```

### 27.5 Abstraction learning (desired)

```text
function learnAbstractionIfUseful(S, move, S_next):
  pattern = extractTypedPattern(move, localContext(S))
  if support(pattern) >= thresholdSupport and
     debtReduction(pattern) > 0 and
     branchSavings(pattern) > 0 and
     invarianceSafe(pattern):
       retain(pattern)
```

### 27.6 Strict invariance guard (desired)

```text
assert noPaperLookupInStrictPath()
assert noNameBasedCapabilityUnlockInStrictPath()
assert noStepIndexReferenceInjection()
assert nameScrambleInvariantOnBenchmarkPrefix()
```

The benefit of this design is that generation and reasoning become identical: a candidate is not "assembled then judged," but "proved into existence under explicit debt constraints."

## 28. Transition Plan From Current Code to Desired Engine

This is a practical module-level migration path that minimizes risk.

### 28.1 Stage A: isolate strict semantic core

Goal:

1. lock strict/structural path to a single semantics package,
2. eliminate residual ambiguous paths.

Actions:

1. create a dedicated strict-evaluation facade that all claim-grade code paths must call.
2. route all strict conversion through structural-only capability policy.
3. remove or hard-fence legacy paper calibration maps from strict execution graph.
4. add CI static check to fail if strict entrypoint imports paper calibration module.

Success metric:

1. all strict-mode runs and tests pass with zero paper-table symbol references.

### 28.2 Stage B: strengthen proof-state as canonical search object

Goal:

1. make `SearchState` the mandatory outer search type.

Actions:

1. convert any remaining outer telescope enumeration in primary lane into `SearchState` expansion.
2. relegate `MBTTEnum` to leaf tactical role only.
3. require each state transition to produce derivation certificate.
4. store open/closed obligation lists explicitly in state for each transition.

Success metric:

1. no primary-lane run should require global telescope enumeration to produce a candidate.

### 28.3 Stage C: obligation refinement

Goal:

1. replace coarse obligation buckets with more causally useful debt objects.

Actions:

1. split broad obligations into typed sub-obligations with dependencies.
2. encode obligation graph edges explicitly.
3. require move applicability to cite obligation targets.

Success metric:

1. branch factor drops and safe-closure ratio rises in telemetry.

### 28.4 Stage D: theorem-backed pruning

Goal:

1. keep pruning aggressive but defensible.

Actions:

1. refine `rhoUB` and `minExtraKappa` estimates with tighter obligation-aware bounds.
2. introduce proof notes in code documenting why each bound is sound or conservative.
3. log prune reasons with stable categories.

Success metric:

1. node expansion decreases materially without reducing correctness prefix.

### 28.5 Stage E: abstraction and equivalence compression

Goal:

1. reduce repeated near-equivalent branching.

Actions:

1. expand macro templates to typed mini-derivation skeletons.
2. add stronger equivalence cache at expression/state level.
3. keep strict retention policy to avoid abstraction sprawl.

Success metric:

1. macro reuse rate and branch-savings increase while peak memory remains bounded.

### 28.6 Stage F: scheduler formalization

Goal:

1. portfolio allocation by measured yield, not static rules.

Actions:

1. implement lane credits based on reward-per-cost moving average.
2. preserve minimal exploration floor per lane.
3. enforce hard memory-pressure caps and scheduler-aware throttling.

Success metric:

1. reduced timeout variance and better runtime predictability for step-prefix benchmarks.

### 28.7 Stage G: benchmark hardening and publication gate

Goal:

1. convert improvements into claim-grade reproducible evidence.

Actions:

1. establish fixed benchmark suite for first 9 steps.
2. run invariance matrix:
   - name scramble,
   - key permutation,
   - premise reorder where semantics preserved.
3. enforce CI gate requiring:
   - correctness threshold,
   - runtime threshold,
   - invariance pass,
   - leakage pass.

Success metric:

1. green CI gate across repeated runs on target machine class.

## 29. Operational Glossary for Expert Discussion

This glossary is provided so external discussion can be unambiguous.

### 29.1 Library

Monotone sequence of discovered entries.  
In strict mode, each entry must come only from discovered candidate conversion, no paper fallback.

### 29.2 Telescope

Ordered list of typed entries representing candidate specification clauses.

### 29.3 Clause count (`kappa`)

Primary effort metric as desugared clause count in claim-grade runs.  
Bit-length is secondary tie-break/audit metric.

### 29.4 Novelty (`nu`)

Marginal derivational capability increase.  
In structural mode, computed via structural AST decomposition and synthesis detectors.

### 29.5 Efficiency (`rho`)

`nu / kappa`; compared against bar each step.

### 29.6 Bar

`Bar_n = Phi_n * Omega_(n-1)`, with `Phi` from d-bonacci inflation and `Omega` from discovered history in strict claim-grade mode.

### 29.7 Coherence window (`d`)

Historical depth of relevant obligations:

1. `d=1` extensional stagnation regime,
2. `d=2` intensional Fibonacci regime.

### 29.8 Agenda state

Proof-search state carrying entries, obligations, capability signature, premise shortlist, and novelty bounds.

### 29.9 Safe versus unsafe rules

1. Safe: deterministic progress, low branching, should run to closure.
2. Unsafe: speculative branching, should be bounded and prioritized.

### 29.10 Dominance pruning

If another state has at least as strong capability, at most equal debt, and no greater cost under same key bucket, weaker state is pruned.

### 29.11 `rhoUB`

Upper bound estimate of best possible efficiency reachable from partial state; used for bar-aware early prune.

### 29.12 Macro

Learned reusable typed pattern of successful transitions retained only when support and branch savings justify storage.

### 29.13 Leaf solver

Small bounded expression generator used as local tactic under agenda transitions.  
It should not be the outer search policy.

### 29.14 Rescue lane

Bounded emergency expansion when no candidate clears bar in current profile.  
Useful during tuning, but should trend toward non-use in mature intelligent lane.

### 29.15 Claim-grade mode

Strict evidence mode requiring:

1. no paper lookup,
2. no hidden target injection,
3. invariance checks,
4. reproducible benchmark success.

## 30. Final Expert Ask (Condensed)

If you share only one paragraph with the expert, use this:

"We have a formally specified novelty-efficiency discovery law with a strong mechanized codebase. We need to redesign candidate generation so discovery behaves like obligation-driven theorem construction rather than bounded syntax farming. Please provide a concrete symbolic architecture and algorithmic plan for proof-state-first search with safe-closure discipline, premise retrieval, abstraction reuse, theorem-backed pruning, and strict invariance/leakage guarantees, targeting first-9-step correctness under 60 seconds without sequence injection."

## 31. Benchmark and Evidence Protocol for External Review

To get useful guidance from a world-class expert, we should provide not only code and theory, but a clean, reproducible evidence packet. The most valuable expert feedback depends on seeing exactly where the engine loses mathematical focus. The protocol below is intended to make that visible in a way that is rigorous and hard to game.

### 31.1 Fixed benchmark contract

Each benchmark run should be executed under a fixed contract:

1. strict no-paper mode enabled,
2. no canonical-name assistance in ranking or critical selection paths,
3. fixed random seed set,
4. fixed time budgets per step and per run,
5. fixed CPU parallelism configuration reported in logs,
6. same initial empty library state.

This ensures that run-to-run differences are attributable to algorithm design, not hidden configuration drift.

### 31.2 Core reported metrics

For each step index `n`, report at least:

1. wall-clock time to first accepted candidate,
2. number of expanded proof states,
3. number of pruned proof states (and prune reasons),
4. safe-rule applications,
5. unsafe-rule applications,
6. premise retrieval hits used in accepted derivation,
7. macro invocations used in accepted derivation,
8. rescue-lane usage count,
9. final `nu`, `kappa`, `rho`, `bar`, and margin.

These metrics should be produced for steps 1 through 9 under the 60-second run requirement.

### 31.3 Progress quality metrics

To evaluate "mathematical intelligence" rather than raw throughput, add process-quality metrics:

1. obligation closure ratio over time,
2. average branching factor before and after safe closure,
3. percentage of accepted derivations that are rescue-generated,
4. proportion of actions justified by explicit local obligations,
5. dominance-prune effectiveness by state bucket,
6. duplicate-equivalence suppression rate.

If these move in the right direction while runtime improves, the redesign is genuinely structural, not just tuned scoring.

### 31.4 Required ablation set

Provide an ablation matrix with at least these toggles:

1. safe-closure on/off,
2. premise retrieval on/off,
3. dominance pruning on/off,
4. rho upper-bound pruning on/off,
5. macro learning on/off,
6. rescue lane enabled/disabled.

The expert should be able to inspect which components reduce combinatorics and which merely reweight outcomes after broad expansion.

### 31.5 Failure transcript format

For every failed step within budget, emit a compact failure transcript:

1. unresolved obligations at timeout,
2. top 10 near-miss states with highest `rhoUB`,
3. most common prune reason counts,
4. largest branching action families,
5. whether best path required rescue.

This is critical. Experts usually diagnose search pathology quickly when near-miss structure is visible.

### 31.6 Fairness and leakage guarantees

Each benchmark report should include a leakage certificate section:

1. forbidden symbol scan status,
2. no-paper lookup assertion status,
3. name-scramble invariance status,
4. deterministic seed reproducibility status.

Without this, fast results can still be dismissed as target-contaminated.

## 32. Failure Case Taxonomy and Diagnostic Questions

The current system exhibits several recurring failure modes. Framing these precisely will let the expert map each failure to known theorem-search design remedies.

### 32.1 Candidate abundance with obligation stagnation

Symptom:

1. many generated candidates,
2. low acceptance rate,
3. repeated unresolved debt classes.

Interpretation: search is expanding syntax faster than it is closing proof debt.  
Likely fix class: stronger local-goal prioritization and safe closure saturation.

### 32.2 Good novelty but poor representational adequacy

Symptom:

1. bar-clearing candidates exist,
2. selected candidates are semantically weak representatives,
3. later steps require rescue to compensate.

Interpretation: objective mismatch between immediate `rho` and long-horizon capability utility.  
Likely fix class: include forward capability value and debt-reduction potential in state score.

### 32.3 Rescue dependence pattern

Symptom:

1. primary agenda lane times out or misses,
2. rescue lane repeatedly provides accepted candidate,
3. quality is unstable between seeds.

Interpretation: primary transition policy is not expressive enough or is mis-prioritized for that phase.  
Likely fix class: enrich high-value rule set and improve obligation-conditioned action ordering.

### 32.4 Premise retrieval drift

Symptom:

1. reuse often selects syntactically nearby but semantically weak premises,
2. deeper composition fails even when relevant premises exist.

Interpretation: retriever lacks strong structural relevance features.  
Likely fix class: improve capability overlap scoring and debt-specific retrieval heads.

### 32.5 Local minima due to weak upper bounds

Symptom:

1. search spends budget in many non-competitive branches,
2. pruning happens late,
3. near-miss states are numerous but shallow.

Interpretation: `rhoUB` or debt lower bounds are too loose.  
Likely fix class: tighter admissible bounds and stronger dominance key design.

### 32.6 Diagnostic questions to ask the expert directly

Use these exact questions:

1. What admissible upper bound family would you use to prune partial theorem-construction states under a ratio objective?
2. How would you formalize safe-rule closure for this MBTT-like setting so that closure is strong but cheap?
3. What state abstraction best predicts future debt closure without leaking target knowledge?
4. How should premise retrieval be structured when no external corpora are allowed and all knowledge is self-generated?
5. What macro retention criterion best balances reuse power against macro-library combinatorics?
6. Which process metrics most convincingly show a transition from heuristic search to mechanized reasoning?

## 33. Concrete Deliverables Requested From the Expert

To maximize value and avoid vague feedback, request the following concrete outputs.

### 33.1 Architecture deliverable

A detailed proposed architecture (state schema, rule classes, scheduler design, pruning guarantees), with explicit mapping to modules we already have (`RunAbInitio`, `AgendaSearch`, `ProofState`, `TelescopeGen`, `MBTTEnum`, `MCTS`, `TelescopeEval`).

### 33.2 Algorithm deliverable

Pseudocode for:

1. obligation-first expansion,
2. safe closure loop,
3. premise retrieval and cutoff,
4. macro induction and retention,
5. bar-aware bound pruning,
6. fallback policy with bounded rescue dependence.

### 33.3 Theoretical deliverable

A concise argument sketch for why the proposed search policy is expected to reduce branching while preserving completeness over relevant candidate classes (or a clearly defined near-completeness guarantee under budgets).

### 33.4 Experimental deliverable

An experiment design that can validate:

1. first 9 steps discovered under 60 seconds,
2. improved primary-lane quality at step 4 and step 5,
3. reduced rescue usage,
4. stronger invariance under name scrambling,
5. stable behavior across seeds and machine loads.

### 33.5 Engineering deliverable

A priority-ordered implementation plan of approximately 4 to 8 weeks:

1. week-by-week milestones,
2. risk register,
3. rollback criteria,
4. acceptance criteria per phase.

### 33.6 Reviewer note

The target is not to hand-design the sequence and force the engine to replay it.  
The target is to make the engine discover high-value structure by principled, local, logically valid moves under strict anti-leakage controls.  
Any recommendation that silently injects target-specific constraints should be explicitly rejected.

