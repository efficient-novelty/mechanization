# Open Questions

## 1. What Counts as "Atomic" for the New Repo?

The current repo proved that a molecular lane can recover the 15-step order. It did not prove that open-ended atomic discovery is solved.

The new repo needs an explicit honesty line:

- structural frontier shaping is allowed
- typed filters are allowed
- canonical dedupe is allowed
- obligation scoring is allowed
- hidden signature packages are not allowed if the claim is atomic-only discovery

## 2. Which Value Regime Is the Primary Goal?

There are at least three plausible milestones:

1. recover structure order and `kappa`
2. recover current strict `nu`
3. recover paper-level late `nu`

The repo should say explicitly which one it is pursuing at each milestone.

## 3. How Should Adjoint Completion Enter an Atomic Engine?

The old molecular plan argued that eliminators are categorically forced and should not be guessed atom by atom. An atomic-only rewrite must face that directly.

Possible positions:

- strict atomic means every clause is enumerated explicitly
- semi-atomic means elaboration or typing derives some completion structure after explicit primitive discovery
- hybrid means keep atomic hot path but allow proof-theoretic elaboration in evaluation only

This choice affects both `kappa` and honesty claims.

## 4. What Is the Right Anti-Junk Policy?

This is probably the central research question.

The new engine likely needs some combination of:

- prefix obligations
- diversity buckets
- stricter canonical quotienting
- better lower and upper `nu` bounds
- staged frontier compaction
- richer support-graph fingerprints

The open design challenge is to do this without smuggling in target semantics.

## 5. Should Conceptual and Compiled Cost Stay Separate?

The molecular lane needed a distinction between:

- conceptual `kappa`
- compiled clause count

If the new repo is truly atomic, maybe that distinction shrinks. But late proof-theoretic elaboration might still make the separation useful.

This should be decided early because it affects:

- selection math
- checkpoint schema
- telemetry
- proof narrative

## 6. What Is the Step-15 Truth Target?

The repo currently exposes a real tension:

- full structural DCT analysis gives the larger `nu`
- current strict molecular lane selects `DCT` with `nu = 88`

The new repo should decide whether step 15 success means:

- match current strict executable behavior,
- or match the larger structural target,
- or produce both numbers with a clear explanation.

## 7. How Minimal Should the Agda Sidecar Be?

There is a temptation to copy all of `agda/`. That is probably unnecessary.

The better question is:

- which proof obligations must the new repo re-check to support its claims?

Likely answer:

- bridge payload contract
- deterministic witness generation
- small number of recurrence and schema lemmas

Not every experimental Agda module needs to come along.

## 8. What Fixture Corpus Should Be Frozen Immediately?

The rewrite should freeze early:

- reference telescopes for all current steps
- expected canonical keys
- expected capability flags
- expected `nu` traces for key fixtures
- checkpoint round-trip fixtures

Without a frozen corpus, the rewrite will drift too easily.

## 9. When Should GPU or Learned Motifs Enter?

Probably late.

The current evidence says the real problem is semantic junk, not missing throughput alone. A faster bad frontier is still a bad frontier.

## 10. What Should Be Published if Atomic Search Still Fails?

The repo already contains the right philosophy:

publish the computation honestly.

If the atomic-only rewrite still cannot recover the full 15-step trajectory, that is still valuable if it produces:

- a reproducible frontier corpus,
- precise failure statistics,
- and a cleaner account of where atomic discovery breaks.

## Primary Source Files

- `strict_intelligence_plan.md`
- `pseudo_code.md`
- `runs/review_hardening/ablation_report.md`
- `engine/src/StrictMolecules.hs`
- `engine/src/RunAbInitio.hs`
