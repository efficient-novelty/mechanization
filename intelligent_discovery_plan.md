# Intelligent Discovery Plan

## Objective

Make PEN discovery operate as obligation-driven mechanized mathematical reasoning, not enumerate-then-rerank syntax farming, while preserving strict non-target-aware behavior.

Success criterion:

1. The engine discovers the first 9 structures correctly in under 60 seconds on the target machine profile.
2. Discovery mode uses no step-indexed reference injection, no name-based unlocking, and no paper-value substitution.
3. Search traces read like proof construction: explicit obligations, explicit moves, explicit capability gain.

## Current State (What Is Already In Place)

1. Goal profile inference exists (`deriveGoalProfile`).
2. Action generation already uses structural pruning (`validActionsWithProfile`).
3. A proof-state scaffold exists (`ProofState.hs`) with:
   1. `CapabilitySig`
   2. `Obligation`
   3. `SearchState`
4. MCTS core correctness has been improved:
   1. Children now materialize actual action applications.
   2. Backprop updates along the full path.
   3. Rollout entry generation no longer duplicates/reorders partial state.
5. MCTS is no longer only hard fallback; a bounded portfolio lane can run alongside enumeration.

## Remaining Work

## Phase 1: Make Search State First-Class

Goal:
Replace "generate many full telescopes, then score" with "maintain partial proof/search states and apply best next move."

Tasks:

1. Introduce an explicit agenda search engine:
   1. Priority queue keyed by proof value estimate.
   2. State object includes entries, obligations, capability signature, premise shortlist, and upper/lower novelty bounds.
2. Add transition rules over `SearchState`:
   1. `IntroducePi`, `IntroduceSigma`, `IntroduceWitness`, `OpenLoop`, `ApplyModal`, `ApplyTemporal`, `ReusePremise`, `ApplyMacro`, `Normalize`.
3. Represent each transition with a derivation certificate:
   1. Rule id
   2. Inputs
   3. Obligations discharged
   4. Capabilities gained

Acceptance criteria:

1. New search path can produce candidates without calling full MBTT global enumeration.
2. Every generated candidate has a replayable state-transition trace.

## Phase 2: Safe/Unsafe Rule Discipline

Goal:
Force deterministic logical progress before speculative branching.

Tasks:

1. Tag rules as `safe` or `unsafe`.
2. Apply safe rules to closure at each state before branching unsafe moves.
3. Add hard limits on unsafe branching per state, conditioned by obligation deficit.

Acceptance criteria:

1. Branching factor drops significantly in traces.
2. Safe-closure step appears before unsafe expansion in every state transition block.

## Phase 3: Premise Retrieval Instead of Broad Reuse

Goal:
Use only structurally relevant library premises per obligation.

Tasks:

1. Add obligation-conditioned premise scorer:
   1. Class compatibility
   2. Path-dimension compatibility
   3. Modal/temporal compatibility
   4. Recency and window proximity
2. Restrict `ALib i`/reuse transitions to top-k premises.
3. Add retriever metrics to logs.

Acceptance criteria:

1. Premise fanout is bounded and explainable.
2. Retrieval quality is visible in run logs and improves candidate viability.

## Phase 4: Bound-Based Pruning and Dominance

Goal:
Prune states that cannot clear the bar before expensive expansion.

Tasks:

1. Implement upper bound:
   1. `rhoUB = (nuSoFar + bestFutureNu(goals,caps)) / (kappaSoFar + minExtraKappa(goals))`
2. Prune if `rhoUB < bar`.
3. Add dominance cache keyed by:
   1. capability signature
   2. obligation multiset
   3. premise bucket
   4. kappa bucket
4. Keep only Pareto-undominated states.

Acceptance criteria:

1. Search nodes expanded drops substantially.
2. No regression in discovered sequence quality at matched budgets.

## Phase 5: Learned Macro System

Goal:
Replace static macro seeds with reusable typed abstractions learned from successful derivations.

Tasks:

1. Extract macro candidates from successful traces.
2. Keep macro only if:
   1. appears in multiple successful derivations
   2. reduces local branching/latency
   3. preserves typedness and obligation semantics
3. Store macro metadata:
   1. arity
   2. capability effect
   3. expected proof-debt reduction
4. Integrate macro selection into agenda rules, not post-hoc candidate reranking.

Acceptance criteria:

1. Macro inventory is learned and versioned.
2. Measured search-cost reduction with no increase in target leakage risk.

## Phase 6: Decouple Strict Evaluation From Names

Goal:
Make strict discovery semantics fully structural and certificate-based.

Tasks:

1. Remove any name-mediated capability unlocking from strict discovery path.
2. Feed `CapabilitySig`/derivation certificates into novelty and former-availability logic.
3. Keep names for reporting only.

Acceptance criteria:

1. Scrambling canonical names leaves strict discovery behavior invariant.
2. CI fails if strict discovery path uses forbidden symbolic labels.

## Phase 7: Demote MBTTEnum to Leaf Solver

Goal:
Use MBTT enumeration only for local hole completion and micro-search, not as the outer search algorithm.

Tasks:

1. Introduce local leaf-solver interface:
   1. bounded depth
   2. bounded budget
   3. obligation-targeted fill
2. Replace outer loop full-telescope enumeration with agenda transitions.
3. Keep MBTTEnum as fallback micro-solver and ablation baseline.

Acceptance criteria:

1. Outer search is state/obligation-driven by default.
2. Disabling MBTTEnum global mode does not break primary discovery lane.

## Phase 8: Portfolio Scheduler and Runtime Policy

Goal:
Allocate compute adaptively across deterministic agenda expansion, learned-macro moves, and repaired MCTS.

Tasks:

1. Add lane scheduler with reward-per-cost accounting.
2. Promote/demote lane budgets online by recent yield.
3. Keep memory/pagefile guardrails integrated with scheduler.

Acceptance criteria:

1. Scheduler logs lane budget decisions and outcomes.
2. Runtime is stable and predictable under memory pressure.

## Phase 9: Verification and CI Leakage Guards

Goal:
Make autonomy and invariance claims testable and enforceable.

Tasks:

1. Add claim-grade entrypoint tests:
   1. forbidden symbol checks
   2. no-paper lookup assertions
   3. no-step-index injection assertions
2. Add runtime invariance tests:
   1. name scrambling
   2. key permutation
   3. equivalent-premise reorder
3. Add benchmark gate:
   1. first 9 steps correct
   2. wall-clock < 60s

Acceptance criteria:

1. CI blocks merges when invariance or leakage tests fail.
2. Benchmark gate is green on target machine class.

## Metrics Dashboard (Must Track Continuously)

1. Search nodes expanded per step.
2. Average branching factor before and after safe closure.
3. Premise retrieval fanout and hit rate.
4. Dominance-prune ratio.
5. Macro reuse rate and benefit.
6. Viable-candidate density.
7. Sequence correctness prefix length.
8. Wall-clock runtime and peak memory.

## Execution Order (Recommended)

1. Complete Phase 1 and Phase 2 before adding new heuristics.
2. Complete Phase 3 and Phase 4 before scaling search budgets.
3. Complete Phase 6 before any claim-grade autonomy assertions.
4. Complete Phase 9 before publication-facing results.

## Risks and Controls

1. Risk: Hidden target leakage through helper utilities.
   1. Control: static forbidden-symbol scans + runtime assertions.
2. Risk: New abstractions increase complexity without speed gains.
   1. Control: per-phase performance gates and rollback flags.
3. Risk: Macro system overfits local traces.
   1. Control: MDL-style retention criteria and ablation checks.

## Immediate Next Sprint

1. Implement agenda queue and state transitions (Phase 1).
2. Implement safe-rule closure pass (Phase 2).
3. Wire premise retriever into action generation (Phase 3).
4. Add `rhoUB` pruning and dominance cache (Phase 4).
5. Run first benchmark iteration for 9-step/<60s target.
