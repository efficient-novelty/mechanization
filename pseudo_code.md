# Current Strict Algorithm

Date: March 10, 2026

## Verified Status

- `cabal run acceptance-core` -> `55 passed, 0 failed`
- `cabal run ab-initio -- --strict --max-steps 15 --skip-validation --skip-mcts`
  - step 1: `Universe`, `nu = 1`, `kappa = 2`
  - step 2: `Unit`, `nu = 1`, `kappa = 1`
  - step 3: `Witness`, `nu = 2`, `kappa = 1`
  - step 4: `Pi`, `nu = 5`, `kappa = 3`
  - step 5: `S1`, `nu = 7`, `kappa = 3`
  - step 6: `Trunc`, `nu = 8`, `kappa = 3`
  - step 7: `S2`, `nu = 10`, `kappa = 3`
  - step 8: `S3`, `nu = 18`, `kappa = 5`
  - step 9: `Hopf`, `nu = 17`, `kappa = 4`
  - step 10: `Cohesion`, `nu = 19`, `kappa = 4`
  - step 11: `Connections`, `nu = 27`, `kappa = 5`
  - step 12: `Curvature`, `nu = 35`, `kappa = 6`
  - step 13: `Metric`, `nu = 47`, `kappa = 7`
  - step 14: `Hilbert`, `nu = 63`, `kappa = 9`
  - step 15: `DCT`, `nu = 88`, `kappa = 8`

So the strict lane now completes the full 15-step structure sequence and stops naturally because `maxSteps = 15`.

The current boundary is no longer search failure.

The current boundary is value alignment:

- the right structures are now found
- the right `kappa` values are now found
- late `nu` values are still not exactly aligned with the paper/structural audit

## Current Implementation

### High-level loop

```text
runStrictAbInitio(maxSteps):
  lib = []
  history = []

  for step in 1..maxSteps:
    bar = computeBarD(d = 2, history)
    admissibility = strictAdmissibility(step, lib)
    bandOrder = prioritizeHonestBands(lib, admissibility.bands)

    viable = []

    for band in bandOrder:
      seeds = enumerateStrictBandCandidates(lib, band)
      seeds = dedupBandSeedsByCanonicalKey(seeds)
      seeds = dropAlreadySeenCanonicalKeys(seeds)

      evaluated = []
      for seed in seeds:
        evaluated.append(
          evaluateStrictBandSeedCached(
            EvalStrictComputed,
            seed,
            lib,
            nuDepth = 1,
            history
          )
        )

      barClearing =
        [cand in evaluated where cand.nu > 0 and cand.rho >= bar]

      barClearing =
        applyStrictSemanticMinimalityFilter(
          EvalStrictComputed,
          lib,
          nuDepth = 1,
          bar,
          history,
          admissibility.bands,
          barClearing
        )

      if barClearing is not empty:
        viable = restrictToMinimumKappa(barClearing)
        break

    if viable is empty:
      stop

    best = argmin honestSelectionKey(lib, bar, cand) over viable
    lib = lib ++ [strictInsertedEntry(lib, best)]
    history = history ++ [(best.nu, best.kappa)]
```

### Strict admissibility

```text
strictAdmissibility(step, lib):
  if step in bootstrap:
    return fixed bootstrap caps

  debt = strictInterfaceDebt(activeWindow = 2, lib)
  cap = debtToCap(debt)
  bands = debtToBands(debt, cap)
  return { cap, bands }
```

The important current interface rule is:

```text
entryExportWeight(entry):
  if isPureShell(entry):
    return leAxiomaticExports(entry)
  else:
    return leConstructors
         + length(lePathDims)
         + bool(leHasLoop)
         + bool(leIsTruncated)
```

That preserves:

- HIT/foundational debt from actual structural payload
- API-shell debt from exact conceptual export count

### Candidate generation

```text
enumerateStrictBandCandidates(lib, band):
  molecular = enumerateStrictMolecularCandidates(lib, band)
  molecular = [cm where checkTelescope(lib, cm.compiledTele) == CheckOK]

  if molecular is non-empty:
    return molecular seeds only

  else:
    return exact-band atomic MBTT enumeration
```

### Molecular packages currently implemented

```text
ActivateAmbient(PrimPi)
ActivateAmbient(PrimSigma)
ActivateAmbient(PrimTrunc)
DefineHIT(1,1)
DefineHIT(1,2)
DefineHIT(1,3)
DefineAPI(MapBridgeShell)
DefineAPI(ModalShell)
DefineAPI(ConnectionShell)
DefineAPI(CurvatureShell)
DefineAPI(MetricShell)
DefineAPI(HilbertShell)
DefineAPI(TemporalShell)
```

### Molecular compilation

Every molecular candidate compiles to:

```text
CompiledMolecule
  { cmPackages
  , cmConceptualKappa
  , cmAxiomaticExports
  , cmCompiledTelescope
  , cmClauseProvenance
  }
```

The separation is:

```text
cmConceptualKappa:
  what the exact-band filter charges

cmAxiomaticExports:
  what the interface-debt logic sees

cmCompiledTelescope:
  what the evaluator reads

cmClauseProvenance:
  which clauses are axiomatic vs framework-derived
```

### Current step-15 temporal compiler

`TemporalShell` now compiles to the audited 8-clause temporal-cohesive AST shape:

```text
1. Next(Var 1)
2. Eventually(Var 1)
3. Pi(Next(Var 1), Eventually(Var 1))
4. Lam(App(Lib cohesion, Next(Var 1)))
5. Pi(Flat(Next(Var 1)), Next(Flat(Var 1)))
6. Pi(Sharp(Eventually(Var 1)), Eventually(Sharp(Var 1)))
7. Lam(App(Eventually(Var 1), Var 2))
8. Pi(Next(Next(Var 1)), Next(Var 1))
```

This matters because the general structural detectors in `StructuralNu` only fire on the exact audited temporal shapes:

- distributive laws
- universe polymorphism
- derived infinitesimal shift

The earlier over-wrapped temporal shell hid those patterns.

### Strict molecular evaluation

The strict lane currently uses this hybrid for molecular seeds:

```text
evaluateStrictBandSeedCached(seed, lib, history):
  evalTele = conceptualSeedTelescope(seed)
  native = computeNativeNu(evalTele, lib, history)

  baseNu = native.nuG + native.nuC
  topoNu = seedTopologicalNu(seed)

  totalNu = baseNu + topoNu
  totalKappa = seed.cmConceptualKappa
```

This is the current key behavior:

```text
strict molecular ν = ν_G + ν_C + explicit topological witness ν_H
```

where:

- `ν_G` and `ν_C` come from `computeNativeNu`
- `seedTopologicalNu` only counts axiomatic path constructors that have matching framework-derived path computation payload

This is why:

- `Trunc` and `S2` are recovered honestly without blowing up uniform depth
- `DCT` is now found structurally
- but `DCT` gets `88` instead of the full structural `103`

because the current hybrid drops the non-path structural `ν_H` contribution that `StructuralNu` assigns to the Derived Tangent Shift.

### Strict library insertion

```text
strictInsertedEntry(lib, best, seed):
  entryTele = conceptualSeedTelescope(seed)
  entry = telescopeToCandidateStructural(entryTele, lib, name)

  if seed is truncation:
    normalize to truncated shell
  else:
    normalize HIT constructor counts
    saturate boundary path basis if needed

  entry.leAxiomaticExports = seed.sbsAxiomaticExports
  return entry
```

This is what keeps:

- conceptual `kappa`
- interface exports
- structural library capability flags

consistent across the full molecular lane.

## What the Current Issue Is

The current issue is not:

- step 4 not finding the right structure
- step 5 not finding the right structure
- missing `kappa` admission
- frontier collapse
- minimality rejection
- missing molecular shells

Those are all solved well enough to complete the 15-step trajectory.

The current issue is:

```text
late-stage ν-value mismatch
```

More precisely:

### Steps 11-14

The strict lane now finds the right structures with the right `kappa`, but the current molecular shell ASTs score slightly above the paper values:

```text
Connections: 27 instead of 26
Curvature:   35 instead of 34
Metric:      47 instead of 43
Hilbert:     63 instead of 60
```

This means the current API-shell compilers are structurally richer than the paper-audited shells, or the current structural decomposition counts one or more clauses differently from the paper tables.

### Step 15

The strict lane now finds the right structure and the right `kappa`, but the current strict molecular scorer still undercounts the full structural DCT payload:

```text
current strict molecular score:
  DCT = 88 / 8

paper / full structural AST analysis:
  DCT = 103 / 8
```

The precise reason is:

```text
computeNativeNu(exact DCT AST) = 103
hybridStrictSeedNu(exact DCT AST) = ν_G + ν_C + topo = 88
```

So step 15 is now:

- finding the right structure
- with the right `kappa`
- but not with the full intended `nu`

because the current hybrid evaluator only carries explicit path-style `ν_H`, not the broader structural `ν_H` used by `StructuralNu`.

## Why I Am Stopping Here

The remaining work is no longer about discovering the right structures.

The remaining work is:

```text
making the late API/synthesis ν-values exactly match
```

and that is now close to the line where local edits start turning into target-shaped tuning.

There are still principled large-scale directions:

1. Align the molecular API-shell compilers exactly to the audited paper ASTs for steps 11-14.
2. Replace the current transitional hybrid strict molecular scorer with a more principled general rule for when structural `ν_H` from `computeNativeNu` should be preserved, not just path/topological `ν_H`.
3. Reconcile the strict lane's "conceptual-kappa molecular evaluation" with the full structural AST decomposition so that late API and DCT steps use one consistent novelty algebra.

But those are not small local fixes anymore.

At this point, another patch aimed at nudging `Connections`, `Metric`, `Hilbert`, or `DCT` toward the expected late-step `nu` values would risk becoming target-steered.

So the honest status is:

```text
Current implementation:
  full 15-step strict structural discovery succeeds

Current issue:
  late ν accounting is still not exactly aligned

Stopping reason:
  the next changes are evaluator/compiler reconciliation work,
  not small principled local fixes
```
