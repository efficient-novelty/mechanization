## Current verified state



Verified commands:



```text

cabal run acceptance-core

cabal run ab-initio -- --strict --max-steps 5 --skip-validation --skip-mcts

```



Current results:



- `acceptance-core`: `55 passed, 0 failed`

- bounded strict run:

  - step 1: `Universe`, `nu = 1`, `kappa = 2`, `rho = 0.50`

  - step 2: `Unit`, `nu = 1`, `kappa = 1`, `rho = 1.00`

  - step 3: `Witness`, `nu = 2`, `kappa = 1`, `rho = 2.00`

  - step 4: `candidate`, `nu = 6`, `kappa = 3`, `rho = 2.00`, elapsed `21.65s`

  - step 5: no bar-clearing candidate, elapsed `20.02s`



Direct step-4 frontier probe on the current code:



```text

frontier_kept = 590

viable = 170

pi_count = 0

reference_count = 0

top viable names = Axiom_2, Axiom_2, ...

```



So the current issue is now:



- step 4: wrong structure, wrong `nu`, correct `kappa`

- step 5: no viable candidate

- even after target-bound prefix roles, the retained step-4 frontier still contains no `Pi`



## Strict lane: current control flow



The strict run still works like this:



```text

steps 1-3:

  bootstrap-gated exact-band search



steps 4+:

  debt-derived admissibility

  exact desugared-kappa bands

  MBTT-first enumeration

  strict structural evaluation

  SCC-based semantic minimality

  honest final selection

```



### Admissibility



```text

strictStepBudgetSeconds(step):

  return [10, 15, 20, 30, 45, 60, 75, 90, ...][step - 1]



strictAdmissibility(step, lib):

  if step == 1: return cap = 2, bands = [2]

  if step == 2: return cap = 1, bands = [1]

  if step == 3: return cap = 1, bands = [1]



  debt = strictInterfaceDebt(activeWindow = 2, lib)

  return cap = debt.suggestedCap, bands = debt.suggestedBands

```



So stage tables are still absent after step 3.



### Bootstrap gating



```text

bootstrapEligible(step, tele):

  if step == 1:

    require exact Univ

    require App Univ _



  if step == 2:

    require App Univ _



  if step == 3:

    require App (Lib _) (Var _)



  otherwise:

    allow tele

```



### Exact-band bounds



```text

honestEnumBounds(kappaCap):

  if kappaCap <= 2: return (bitBudget = 16, astDepth = 2, maxCandidates = 1024, exprCap = 96)

  if kappaCap == 3: return (bitBudget = 24, astDepth = 3, maxCandidates = 768,  exprCap = 128)

  if kappaCap == 4: return (bitBudget = 20, astDepth = 2, maxCandidates = 768,  exprCap = 96)

  if kappaCap == 5: return (bitBudget = 24, astDepth = 2, maxCandidates = 1024, exprCap = 128)

  if kappaCap == 6: return (bitBudget = 30, astDepth = 3, maxCandidates = 2048, exprCap = 192)

  if kappaCap == 7: return (bitBudget = 34, astDepth = 3, maxCandidates = 2560, exprCap = 224)

  if kappaCap == 8: return (bitBudget = 38, astDepth = 3, maxCandidates = 3072, exprCap = 256)

  else:             return (bitBudget = 42, astDepth = 3, maxCandidates = 4096, exprCap = 320)

```



The important step-4 configuration is still:



- `kappa = 3`

- `bitBudget = 24`

- `astDepth = 3`

- `maxCandidates = 768`



## MBTT enumeration: current implementation



The enumerator still:



1. builds expression banks by context depth

2. grows a bounded prefix frontier

3. keeps exact-band completed candidates

4. merges them with a tiny macro lane



Pseudo-code:



```text

enumerateMBTTTelescopesWithDiagnostics(lib, cfg, cache):

  exprsByCtx = buildExprsByCtx(...)

  prefixes = [empty telescope]



  for ctxDepth in [0 .. maxEntries - 1]:

    bucketed = {}



    for prefix in prefixes:

      for expr in exprsByCtx[ctxDepth]:

        tele' = append expr

        heuristic = frontierHeuristic(cfg, tele', lib, goalProfile)



        if checkTelescope(lib, tele') == CheckOK

           and heuristic > prefixHeuristicFloor(cfg):

          bucket = structuralSignature(tele')

          insert tele' into bucket



    prefixes = retainStratifiedPrefixes(prefixLimit, bucketed)



    for retained prefix:

      if prefix is in the exact kappa band

         and isValidCandidate(...):

        keep it in the completed frontier



  merge with small macro lane

  return retained frontier + diagnostics

```



The important difference in the current code is that the prefix representation is now target-bound, not just role-bound.



## Prefix state: current target-bound form



The code now has:



```text

TargetHead =

  TargetUniverse

  TargetPi

  TargetSigma

  TargetId

  TargetSusp

  TargetTrunc

  TargetFlat

  TargetSharp

  TargetDisc

  TargetShape

  TargetNext

  TargetEventually



RoleTarget =

  Ambient(TargetHead)

  Local(clauseIndex)



ClauseRole =

  RoleFormation(RoleTarget)

  RoleIntro(RoleTarget)

  RoleElim(RoleTarget)

  RolePath(RoleTarget)

  RoleMacro



PendingObligation =

  NeedIntroFor(RoleTarget)

  NeedElimOrPathFor(RoleTarget)



PrefixState =

  roleHistory

  openObligations

  fulfillmentBonus

```



And the current code reifies that state from the current prefix:



```text

reifyPrefixState(tele):

  state = empty

  earlier = []



  for entry in tele:

    role = classifyClauseRole(earlier, entry)

    state = updatePrefixState(state, role)

    earlier = earlier ++ [entry]



  return state

```



## Clause-role classification: current rules



The current classifier is now target-bound:



```text

classifyClauseRole(earlier, entry):

  if pathTarget(earlier, expr) exists:

    return RolePath(target)



  if elimTarget(entry) exists:

    return RoleElim(target)



  if formationTarget(earlier, expr) exists:

    return RoleFormation(target)



  if introTarget(earlier, expr) exists:

    return RoleIntro(target)



  otherwise:

    return RoleMacro

```



### Formation targets



Current rule:



```text

formationTarget(expr):

  Univ            -> Ambient(TargetUniverse)

  Pi _ _          -> Ambient(TargetPi)

  Sigma _ _       -> Ambient(TargetSigma)

  Id _ _ _        -> Ambient(TargetId)

  Susp _          -> Ambient(TargetSusp)

  Trunc _         -> Ambient(TargetTrunc)

  Flat _          -> Ambient(TargetFlat)

  Sharp _         -> Ambient(TargetSharp)

  Disc _          -> Ambient(TargetDisc)

  Shape _         -> Ambient(TargetShape)

  Next _          -> Ambient(TargetNext)

  Eventually _    -> Ambient(TargetEventually)



  App Univ inner  -> Local(currentClauseIndex)

                     only if inner is a simple carrier (Var or Lib)



  otherwise       -> no formation target

```



This is the current universe-shell demotion rule:



- simple local carriers like `App Univ (Var 1)` still count as local formation targets

- complex `App Univ (...)` shells no longer open foundational buckets



### Intro targets



Current rule:



```text

introTarget(earlier, expr):

  Lam body        -> Ambient(TargetPi)

                     only if the body actually contains Pi-structure



  Var i           -> target of the referenced earlier formation clause



  App(App h a) b  -> Ambient(TargetSigma)

                     if it has pair-like shape and simple arguments



  App head args   -> target of the head

                     if all args are simple constructor args



  otherwise       -> no intro target

```



Important consequence:



- `Lam (Var 0)` is no longer automatically a foundational intro

- generic lambda macros stopped stealing intro rewards by shape alone



### Elim targets



Current rule:



```text

elimTarget(entry):

  if trueEliminatorScore(singletonEntry) > 0:

    Ambient(TargetPi)



  else if entry is App (Lam _) _:

    Ambient(TargetPi)



  else:

    no elim target

```



### Path targets



Current rule:



```text

pathTarget(earlier, expr):

  PathCon _ ->

    latest pending NeedElimOrPath target

    else latest intro target

    else latest formation target



  Refl _ ->

    latest pending NeedElimOrPath target

    else latest intro target



  otherwise ->

    no path target

```



So local HIT path clauses can close the specific local target they belong to.



## Target-bound state machine



The state machine is now target-bound rather than generic:



```text

updatePrefixState(state, role):

  if role == RoleFormation(target):

    push NeedIntroFor(target)



  if role == RoleIntro(target):

    if NeedIntroFor(target) is present:

      remove it

      add +50

      push NeedElimOrPathFor(target)



    else if target is Ambient(...):

      add +30

      push NeedElimOrPathFor(target)



  if role == RoleElim(target) or role == RolePath(target):

    if NeedElimOrPathFor(target) is present:

      remove it

      add +50

```



And the prefix bonus is still:



```text

prefixStateAdjustment(state):

  fulfillmentBonus - 12 * numberOfOpenObligations

```



This means:



- local `App Univ` shells can no longer be closed by unrelated ambient `Lam` intros

- ambient former lifecycles can still begin directly without a prior local formation clause



## Connectedness: current target-aware rule



The current in-flight connectedness rule is:



```text

frontierConnectednessScore(tele):

  for each clause after the first:

    role = classifyClauseRole(earlier, clause)

    edgeCount = explicitRefs + typologicalSupportRefs



    if edgeCount == 0 and role is Formation/Intro/Elim:

      add 0



    else if edgeCount == 0:

      add -1000



    else:

      add +15 * edgeCount

```



So orthogonal foundational roots are still allowed, but disconnected path/macro junk is still heavily penalized.



## Typological support edges: current target-aware form



Typological support edges are now target-aware, not global:



```text

typologicalSupportRefs(earlier, entry):

  if RolePath(target):

    latest formation ref for that exact target

    + latest intro ref for that exact target



  if RoleElim(target):

    latest formation ref for that exact target

    + latest intro ref for that exact target



  if RoleIntro(target):

    latest formation ref for that exact target



  otherwise:

    []

```



And the support refs are found using the already target-classified earlier prefix, not just coarse judgments.



## Prefix beam stratification



The prefix beam is now stratified by exact targeted role history:



```text

structuralSignature(tele):

  state = reifyPrefixState(tele)

  return state.roleHistory

```



So current buckets are things like:



```text

[RoleIntro(AmbientPi), RoleIntro(AmbientSigma), RoleElim(AmbientPi)]

[RoleFormation(Local1), RoleIntro(Local1), RolePath(Local1)]

[RoleMacro, RoleMacro, RoleMacro]

```



not just coarse generic roles.



Retention is still:



```text

retainStratifiedPrefixes(limit, buckets):

  quota = max(1, limit / activeBucketCount)

  keep top quota from each bucket

  fill remaining slots from leftovers

```



## Strict critic: current role in the frontier



The strict critic still computes:



- interface density

- generic binder count

- dependent-motive density

- internal adjoint score

- true eliminator score

- former lifecycle score

- closure score

- floating clause count

- missing bridge debt

- universe-shell penalty



And the frontier heuristic is still:



```text

frontierHeuristic(cfg, tele, lib, profile):

  connectedness = frontierConnectednessScore(tele)

  prefixBonus = prefixStateAdjustment(reifyPrefixState(tele))



  if neutral:

    1

  else if goal-directed:

    goalDirectedHeuristic + connectedness + prefixBonus

  else:

    obligationScore + connectedness + prefixBonus

```



So the current prefix frontier is now driven by:



1. the strict critic

2. target-aware connectedness

3. target-aware obligation fulfillment

4. exact targeted-role bucketing



## Strict semantic minimality



Strict minimality is still the SCC amputation filter:



```text

applyStrictSemanticMinimalityFilter(candidates):

  for each candidate:

    test lower-kappa terminal-SCC amputations

    if any amputated remainder still clears the bar:

      reject the original candidate

```



Current bounded strict runs still show:



- `minimality_rejections = 0`



So minimality is still not the blocker.



## Final selection



The final strict selector is still:



```text

honestSelectionKey(lib, bar, candidate):

  overshoot = max(0, rho - bar)

  kappa = candidate.kappa

  eliminatorScore = trueEliminatorScore(tele)

  lifecycleScore = formerLifecycleScore(tele)

  dependentDensity = dependentMotiveDensity(tele)

  internalAdjoints = internalAdjointScore(tele)

  externalDensity = countDistinctLibraryRefs(tele)

  genericity = countPolymorphicBinders(tele)

  closure = closureScore(lib, tele)

  bitCost = teleBitCost(tele)

  stableHash = canonicalKey



  sort by:

    1. smaller overshoot

    2. smaller kappa

    3. larger eliminatorScore

    4. larger lifecycleScore

    5. larger dependentDensity

    6. larger internalAdjoints

    7. larger externalDensity

    8. larger genericity

    9. larger closure

    10. smaller bitCost

    11. stableHash

```



So if `Pi` were present in the viable set, it would still beat the current `nu = 6`, `kappa = 3` junk by overshoot.



## What improved in this iteration



What genuinely changed:



1. Prefix roles are now target-bound, not generic.

2. Debts are now exact-target debts, not generic intro/elim debts.

3. Complex universe shells no longer open foundational formation buckets.

4. Generic lambda macros no longer automatically satisfy foundational intro obligations.

5. Prefix buckets are now exact targeted role histories.



This was a real proof-theoretic tightening of the prefix state.



## What is still broken



The measured result is still:



```text

step 4:

  selected structure = wrong

  selected nu = 6

  selected kappa = 3

  retained frontier contains no Pi

  retained frontier contains no exact reference Pi



step 5:

  no bar-clearing candidate

```



The decisive fact is still:



```text

pi_count = 0

reference_count = 0

```



So the current failure is still not:



- final ranking

- SCC minimality

- raw search depth

- raw bit budget



It is still retained-prefix representation.



## Issue classification by step



### Step 4



```text

right structure?  no

right kappa?      yes

right nu?         no

finding Pi at all? no

finding correct Pi values? no

time-bound status? yes, still within the nominal 30s budget

current failure mode? targeted prefix state still does not preserve Pi into the retained frontier

```



### Step 5



```text

right structure?  no candidate found

right kappa?      unresolved

right nu?         unresolved

time-bound status? yes, still within the nominal 45s budget

current failure mode? step 5 is still downstream of the wrong step-4 library state, plus the same retained-prefix weakness for local HIT scaffolds

```



## Why work stopped here



I stopped here because the next move no longer looks like a small principled local patch.



The current code already has:



- exact-band search

- `astDepth = 3` at `kappa = 3`

- target-aware role classification

- target-aware debts

- target-aware support edges

- exact targeted-role bucketing

- strict critic pressure

- SCC minimality



and the direct frontier probe still shows:



- no retained `Pi`

- no retained exact reference step-4 structure



That means the next honest move is probably not another local scoring change.



It now looks like a deeper redesign such as:



1. former-specific prefix families rather than one shared generic frontier

2. stronger partial-state typing for constructor/motive/eliminator compatibility

3. target instances richer than just `Ambient(...)` / `Local(i)`

4. grammar-level preservation of dependent former packages before full telescope completion



So the current honest boundary is:



- speed problem: acceptable

- step-4 structure problem: not solved

- step-5 discovery problem: not solved

- next required work: deeper prefix-state redesign, not honest local tuning