# Atomic Research Lessons

## The Central Warning

The most important engineering lesson in this repo is not "atomic search was too slow."

It is:

```text
atomic search was fast enough to reach the real failure mode,
and the real failure mode was representational junk.
```

This diagnosis is stated most clearly in `strict_intelligence_plan.md`.

## What the Old Atomic Attempt Actually Showed

The old atomic MBTT attempt established that:

- step 4 could run within budget,
- raw runtime was not the main blocker,
- but the retained frontier became dominated by generic well-typed macros,
- so sparse true eliminator structures died before they could be completed.

This is a frontier-shaping problem, not merely a bigger-computer problem.

## The Noise Wall

The strict plan describes the typical junk forms that dominated the atomic frontier:

- variable-swapping binders
- shallow self-application skeletons
- generic universe shells
- well-typed macros with little real eliminative payload

The true `Pi` or HIT eliminator packages are:

- deeper,
- sparser,
- structurally fragile,
- and hard to distinguish from junk until late in the telescope.

That means a naive beam or prefix frontier will delete the good branches first.

## Why the Current Repo Switched to Molecular Search

The repo's answer was to stop guessing eliminators node by node and instead compile:

- ambient packages like `Pi`, `Sigma`, `Trunc`
- HIT signatures like `(1 point, 1 path)`
- late API shells like cohesion, connections, metric, Hilbert, temporal

This is the logic in `engine/src/StrictMolecules.hs`.

It is honest as a search contract in one sense, but it is not the same as atomic-only discovery.

## Empirical Proof That the Current 15-Step Lane Is Mostly Molecular

`runs/review_hardening/ablation_report.md` shows:

- steps 1 to 3 come from `ENUM_MBTT`
- steps 4 to 15 come from `ENUM_MOLECULAR`

That single table is one of the most important context files for the rewrite.

## Lessons the New Atomic Repo Must Internalize

### 1. Anti-junk retention is the core problem

The new repo must invest heavily in:

- structural obligation tracking,
- prefix diversity,
- stronger canonicalization,
- better lower and upper `nu` bounds,
- and data structures that let promising but incomplete eliminator skeletons survive.

### 2. Atomic does not mean syntax-blind

The new repo should still be allowed to use:

- typed checking,
- structural obligations,
- shape hashes,
- support graphs,
- admissibility filters,
- connectivity filters,
- exact bands,
- and deterministic frontier shaping.

Those are not cheats. They are structural search discipline.

### 3. Do not hide templates inside "atomic" code

The current molecular lane is useful as audit material, but a true atomic rewrite should not sneak those same shells back in under different names.

### 4. Conceptual versus compiled cost still matters

Even in an atomic rewrite, the repo has already taught an important conceptual lesson:

- the selector may care about irreducible conceptual effort,
- while the evaluator may see a larger elaborated or derived structure.

The new repo must decide carefully how much of that distinction survives in a genuinely atomic lane.

### 5. The current late `nu` mismatch is partly a symptom of the molecular bridge

Because the current strict molecular path only preserves part of the full structural `nu_H`, late-step `nu` values drift away from the larger structural audit.

An atomic rewrite has a chance to unify:

- one search surface,
- one AST,
- one evaluator,
- and one novelty algebra.

## What Still Transfers Well from the Old Repo

Even though the main atomic search failed, these ideas still look worth keeping:

- exact-band search
- interface-debt admissibility
- obligation-guided frontier retention
- SCC semantic minimality
- canonical dedupe
- deterministic replay
- post-hoc-only naming

## Primary Source Files

- `strict_intelligence_plan.md`
- `runs/review_hardening/ablation_report.md`
- `engine/src/MBTTEnum.hs`
- `engine/src/StrictCritic.hs`
- `engine/src/StrictMinimality.hs`
- `engine/src/StrictMolecules.hs`
- `engine/src/RunAbInitio.hs`
