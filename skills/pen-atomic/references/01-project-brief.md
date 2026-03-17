# Project Brief

## Mission

Build a new Rust-first repository, `pen-atomic`, that tries to rediscover the entire 15-step PEN genesis trajectory from atomic MBTT primitives, while using Cubical Agda only as a sidecar for export and verification.

The new repo is not a straight port of the current Haskell engine. It is a selective rewrite with three goals:

1. Preserve the reusable semantic kernel.
2. Preserve the audit and verification story.
3. Replace the current search, storage, resume, and memory architecture with a cleaner deterministic Rust implementation.

## Canonical Scope

Treat these as frozen unless the user says otherwise:

- Rust owns all hot-path search, evaluation, storage, reporting, and resume logic.
- Agda is verification-only and never sits in the hot loop.
- Strict mode only.
- CPU-first and deterministic.
- No semantic names or target IDs in hot-path crates.
- No floating-point comparisons in the hot path.
- Step checkpoints are the stable resume unit.
- Frontier checkpoints are resumable only on full compatibility-hash match.

## What the New Repo Is Actually Trying to Solve

The old repo now has a working strict lane that reaches 15 steps, but it does that by switching from atomic MBTT generation to a molecular package compiler after the first few steps.

The new repo's harder ambition is different:

- keep the honest strict selection semantics,
- keep the kernel semantics and Agda export discipline,
- but get back to atomic discovery as the primary engine.

That means the most valuable export from this repo is not just code that works. It is:

- the exact semantic contracts,
- the evidence about what failed,
- the lessons about search noise and frontier collapse,
- and the places where the current implementation quietly stopped being atomic.

## Source-of-Truth Priority

When files disagree, use this priority order.

1. The user's frozen architecture brief in this thread.
2. `engine/src/*.hs` for executable semantics.
3. `strict_intelligence_plan.md` for the explicit engineering diagnosis of atomic failure.
4. `pseudo_code.md` for the current strict implementation status.
5. `../theory/genesis.md`, `../theory/pen-model.md`, `../theory/coherence-and-scaling.md`, and `../theory/terminal-dct.md` for the current proof narrative and quantitative canon.
6. `docs/adr/*.md`, `docs/reports/*.md`, and `runs/*` for invariants, ablations, and CI evidence.
7. `agda/Test/BridgePayloadContract.agda` and `agda/bridge/PEN/Genesis/*.agda` for verification/export contracts.
8. Older Agda overview files only as historical context.

## Important Conflicts and Stale Docs

The repo contains multiple PEN narratives. Do not flatten them together.

### Canonical current target

The current canonical target for the rewrite is the 15-step sequence described in:

- `strict_intelligence_plan.md`
- `pseudo_code.md`
- `../theory/genesis.md`
- `../theory/terminal-dct.md`

### Current executable strict lane

The current executable strict lane finds all 15 structures, but late `nu` values do not match the paper-level target exactly. It currently reports:

- exact structure order,
- exact `kappa`,
- exact early and middle `nu`,
- but late `nu` mismatches, especially step 15.

### Older or conflicting narratives

These are still useful, but not canonical for the rewrite target:

- `agda/PEN.agda` still lists an older 16-step sequence and older late-stage names.
- `agda/README.md` and `agda/instructions.md` describe earlier phase plans with incomplete or older targets.
- `scripts/benchmark.sh` still checks for an older total `nu` profile.
- `Haskell/MinimalAction.hs` and `Haskell/HodgeEndomorphism.hs` are downstream one-off artifact scripts, not core donor modules.
- `last_atomic_attempt.md` and `paper_architecture.md` are empty placeholders.

## Practical Repo Reading Order

If starting fresh, read in this order:

1. `strict_intelligence_plan.md`
2. `pseudo_code.md`
3. `engine/src/Kolmogorov.hs`
4. `engine/src/Telescope.hs`
5. `engine/src/TelescopeCheck.hs`
6. `engine/src/MBTTCanonical.hs`
7. `engine/src/MBTTNu.hs`
8. `engine/src/StructuralNu.hs`
9. `engine/src/AbInitioPolicy.hs`
10. `engine/src/MBTTEnum.hs`
11. `engine/src/StrictMinimality.hs`
12. `engine/src/RunAbInitio.hs`
13. `engine/src/AgdaExport.hs`
14. `agda/Test/BridgePayloadContract.agda`
15. `runs/review_hardening/ablation_report.md`

## What to Preserve No Matter What

- the MBTT atom family and encoding contract
- telescope as the neutral structural representation
- canonicalization and dedupe contracts
- native or structural `nu` extraction from anonymous ASTs
- structural capability flags derived from AST shape
- strict bar semantics
- semantic minimality checks
- post-hoc-only decoding and reporting
- deterministic Agda export and verification payloads

## Primary Source Files

- `strict_intelligence_plan.md`
- `pseudo_code.md`
- `../theory/README.md`
- `../theory/genesis.md`
- `../theory/pen-model.md`
- `../theory/coherence-and-scaling.md`
- `../theory/novelty-selection-and-rejection.md`
- `../theory/late-framework-abstraction.md`
- `../theory/terminal-dct.md`
- `engine/src/Kolmogorov.hs`
- `engine/src/Telescope.hs`
- `engine/src/TelescopeCheck.hs`
- `engine/src/MBTTCanonical.hs`
- `engine/src/MBTTNu.hs`
- `engine/src/StructuralNu.hs`
- `engine/src/MBTTEnum.hs`
- `engine/src/RunAbInitio.hs`
- `engine/src/AgdaExport.hs`
- `agda/Test/BridgePayloadContract.agda`
