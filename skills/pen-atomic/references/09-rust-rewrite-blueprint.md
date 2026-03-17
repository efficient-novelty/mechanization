# Rust Rewrite Blueprint

## Frozen Identity

- Repo name: `pen-atomic`
- Language split:
  - Rust for hot path
  - Cubical Agda for sidecar export and verification only
- Mode: strict only
- Output styles: standard and debug
- Deterministic CPU-first search
- Optional acceleration only after CPU truth path is stable

## Crate Layout

The user-provided architecture is the current blueprint:

```text
pen-core     AST, IDs, interners, canonical keys, encoding, library views
pen-type     contexts, checking, normalization, equality, obligations
pen-eval     bars, coherence, nu, bounds, SCC minimality, audit
pen-store    manifests, checkpoints, shards, blobs, telemetry, memory governor
pen-search   configs, states, enumerate, expand, branch/bound, dedupe, scheduler
pen-accel    optional GPU or batch acceleration, CPU fallback authoritative
pen-agda     export, render, manifest, verify orchestration
pen-cli      commands, reporting, post-hoc labels only
```

## Dependency Discipline

Keep the graph simple and acyclic:

```text
pen-core
  -> pen-type
  -> pen-store
pen-type -> pen-eval
pen-eval -> pen-search
pen-store -> pen-search
pen-search -> pen-cli
pen-agda -> pen-cli
pen-accel -> pen-search or pen-cli only as optional helper
```

Practical rule:

- no semantic reporting dependencies inside `pen-core`, `pen-type`, `pen-eval`, or `pen-search`

## On-Disk Contract

The rewrite should make run artifacts explicit and durable.

### Run manifest

Keep a human-readable `run.json` with:

- schema version
- run ID
- compatibility hashes
- host info
- config hash
- current step and frontier position
- artifact paths

### Step checkpoints

These are the stable resume unit. They should be:

- immutable
- compressed
- self-contained

### Frontier checkpoints

These are speed artifacts only. They can be discarded if compatibility hashes change.

## Compatibility Hashes

The frozen architecture wants these tracked explicitly:

- `ast_schema_hash`
- `type_rules_hash`
- `evaluator_hash`
- `search_semantics_hash`
- `store_schema_hash`

Resume policy:

- exact full match -> resume frontier
- same AST/type/eval but different search semantics -> resume from last step checkpoint
- AST schema change -> no automatic resume

## Frontier Record

The frozen plan defines a fixed-width frontier state record:

```text
64 bytes
state id
parent state id
last clause id
obligation set id
shape hash64
support hash64
nu lower and upper bounds
bit and clause kappa used
depth
step index
band index
flags
priority key
worker hint
reserved
```

Even if the final binary layout changes, the rewrite should preserve the design goal:

- frontier states carry IDs, counters, hashes, and bounds
- not mutable AST trees

## Exact Arithmetic

The frozen rewrite wants exact rational or integer arithmetic for:

- bar comparisons
- `rho` comparisons
- checkpointed objective data

This is one of the most valuable improvements over the old Haskell `Double` path.

## Memory Model

The new repo should treat memory as an explicit subsystem.

### Canonical target host

- 16 GB desktop

### Desired process behavior

- roughly 12 GB hard engine cap
- pressure and emergency checkpoints before crash
- fixed budgets for frontier, interner, dedupe, caches, worker scratch, and spill buffers

This is not just ops polish. An atomic search rewrite will need disciplined memory behavior to preserve frontier quality.

## Implementation Order

The frozen implementation order is sound:

1. `pen-core`
2. `pen-store`
3. schemas and round-trip tests
4. `pen-type`
5. `pen-eval`
6. `pen-search`
7. `pen-cli`
8. `pen-agda`
9. `pen-accel`

Reason:

- stable AST and checkpoint contracts need to exist before expensive search work

## Design Commitments Worth Preserving

1. No semantic names in hot-path crates.
2. No floating-point ranking in the hot path.
3. No mutable AST trees in frontier states.
4. Step checkpoints are self-contained.
5. Frontier checkpoints are speed features, not correctness anchors.
6. Accelerators are never authoritative.
7. Every prune is classified.
8. Molecular search is not the new repo's main hot path.

## Practical Porting Strategy

### Phase 1

Recreate the kernel and checkpoint contracts first:

- atom enum
- telescope and library storage
- canonical keys
- stable IDs and hashes
- manifests and blobs

### Phase 2

Port strict evaluation:

- structural and native `nu`
- bar math
- admissibility
- SCC minimality

### Phase 3

Port and then replace search:

- start by matching current strict selection behavior on frozen fixtures
- then attack the atomic-only frontier problem with better retention, bounds, and state storage

### Phase 4

Only after CPU parity:

- add optional acceleration
- add broader telemetry and dashboards

## Primary Source Files

- user-provided frozen architecture brief in this thread
- `engine/src/Kolmogorov.hs`
- `engine/src/Telescope.hs`
- `engine/src/MBTTEnum.hs`
- `engine/src/RunAbInitio.hs`
- `engine/src/AgdaExport.hs`
