# Repo Donor Map

## High-Value Directories

### `engine/src/`

This is the main donor surface. It contains the executable semantics that matter for the rewrite:

- MBTT AST and encoding
- telescope representation
- structural capability extraction
- conservative checking
- canonicalization
- native and structural `nu`
- admissibility and bar semantics
- exact-band search scaffolding
- SCC minimality
- Agda export

### `agda/`

This is not the hot path, but it matters for the verification split:

- `agda/Test/BridgePayloadContract.agda` is the clearest current verification contract.
- `agda/bridge/PEN/Genesis/*.agda` shows the current generated witness shape.
- `agda/Saturation/*` and related core modules capture the proof-side understanding of schema saturation and recurrence.

### `docs/adr/` and `docs/reports/`

These freeze important contracts:

- canonicalization scope
- native `nu` API and trace shape
- name independence
- reporting non-interference
- Agda bridge schema
- verification split
- pruning and evidence claims

### `runs/`

These provide empirical evidence about what the code actually does, not just what the papers say. The most important current artifact is:

- `runs/review_hardening/ablation_report.md`

## Donor Modules to Port or Reinterpret

| Current module | Why it matters | Rust home |
| --- | --- | --- |
| `Kolmogorov.hs` | frozen MBTT atom family and prefix-free bit encoding | `pen-core/atom.rs`, `pen-core/encode.rs` |
| `Telescope.hs` | neutral structural representation, reference telescopes, kappa utilities | `pen-core/expr.rs`, `pen-core/clause.rs`, `pen-core/telescope.rs`, `pen-core/library.rs` |
| `Types.hs` | capability flags and library-entry shape | `pen-core/library.rs` |
| `TelescopeCheck.hs` | conservative well-formedness checks | `pen-type/check.rs`, `pen-type/context.rs` |
| `MBTTCanonical.hs` | V1 canonicalization and stable keys | `pen-core/canonical.rs` |
| `MBTTNu.hs` | evaluator-facing native `nu` API and trace contract | `pen-eval/nu.rs`, `pen-eval/nu_trace.rs` |
| `StructuralNu.hs` | core structural `nu` decomposition and DCT detectors | `pen-eval/nu.rs`, `pen-eval/bounds.rs` |
| `CoherenceWindow.hs` | `d`-bonacci recurrence and bar support | `pen-eval/bar.rs` |
| `AbInitioPolicy.hs` | strict admissibility and interface debt | `pen-type/admissibility.rs`, `pen-search/branch_bound.rs` |
| `MBTTEnum.hs` | typed atomic enumeration and frontier shaping | `pen-search/enumerate.rs`, `pen-search/frontier.rs`, `pen-search/diversify.rs` |
| `StrictMinimality.hs` | terminal SCC minimality pruning | `pen-eval/minimality.rs` |
| `RunAbInitio.hs` | actual strict search loop, band logic, selection semantics | `pen-search/*`, `pen-cli/cmd_run.rs` |
| `AgdaExport.hs` | Agda step export and verification payload schema | `pen-agda/export.rs`, `pen-agda/render.rs`, `pen-agda/manifest.rs` |
| `MBTTDecode.hs` | post-hoc label decoding only | `pen-cli/report.rs` |
| `AcceptanceSuite.hs` | invariant catalogue for the port | Rust integration and property tests |

## Modules to Use as Lessons, Not as Hot-Path Donors

### `StrictMolecules.hs`

Important because it explains how the current repo reaches 15 steps. Not a hot-path donor for an atomic-only rewrite.

What to keep:

- the conceptual-versus-compiled distinction as a warning
- the exact late shell shapes as audit material
- the clause provenance split as a verification idea

What not to keep:

- molecular packages as the main generator
- hidden signature families in the atomic hot path

### `StrictCritic.hs`

This module is useful even for an atomic-only rewrite because it encodes structural obligation analysis and anti-junk frontier features. Port the ideas, not necessarily the exact code shape.

### `MCTS.hs`

Treat as optional or deferred. The user's frozen architecture already demotes this style of search, and the new repo is CPU-first deterministic exact-band search before any fancy fallback.

### `Capability.hs`

Historical and semantically hand-authored. Good for understanding old theory-language, but not appropriate as the core evaluator for the rewrite.

### `Generator.hs` and template-first lane

Do not port as the primary discovery engine. They belong to the older category-driven generator architecture that the rewrite is trying to leave behind.

## Files That Matter Mostly as Narrative or Audit

- `../theory/README.md`
- `../theory/source-map.md`
- `../theory/genesis.md`
- `../theory/pen-model.md`
- `../theory/coherence-and-scaling.md`
- `../theory/novelty-selection-and-rejection.md`
- `../theory/late-framework-abstraction.md`
- `../theory/terminal-dct.md`
- `strict_intelligence_plan.md`
- `pseudo_code.md`

These are important because they explain:

- why the engine exists,
- what the sequence means,
- what the strict lane currently claims,
- and where proof narrative and executable evidence diverge.

## Files That Are Useful but Non-Core

- `agda/Saturation/*`
- `agda/OpSchema/*`
- `agda/Oracle/*`
- `agda/Experiments/*`

These are research tracks and proof experiments. They are valuable as semantic background, but the new repo only needs a small Agda sidecar, not a full copy of every Agda exploration.

## Files to Ignore for the Rewrite Core

- empty placeholders such as `last_atomic_attempt.md`
- downstream one-off Haskell artifact scripts in `Haskell/`
- late RH-specific drafts unless you need the semantic downstream use of steps 14 and 15

## Primary Source Files

- `engine/src/Kolmogorov.hs`
- `engine/src/Telescope.hs`
- `engine/src/TelescopeCheck.hs`
- `engine/src/MBTTCanonical.hs`
- `engine/src/MBTTNu.hs`
- `engine/src/StructuralNu.hs`
- `engine/src/MBTTEnum.hs`
- `engine/src/RunAbInitio.hs`
- `engine/src/AgdaExport.hs`
- `engine/src/AcceptanceSuite.hs`
