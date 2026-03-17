# MBTT Kernel

## Frozen Atom Schema

For the rewrite, the atom family should match the current MBTT atom set exactly in v1:

```text
App
Lam
Pi
Sigma
Univ
Var
Lib
Id
Refl
Susp
Trunc
PathCon
Flat
Sharp
Disc
Shape
Next
Eventually
```

Any change to this family should be treated as an AST schema break that invalidates frontier resume and compatibility hashes.

## Prefix-Free Bit Encoding

`engine/src/Kolmogorov.hs` defines the current prefix-free encoding contract:

| Atom | Bit cost prefix |
| --- | --- |
| `App` | 2 |
| `Lam` | 2 |
| `Pi` | 3 |
| `Sigma` | 4 |
| `Univ` | 4 |
| `Var` | `3 + EliasGamma(i)` |
| `Lib` | `3 + EliasGamma(i)` |
| `Id` | 5 |
| `Refl` | 5 |
| `Susp` | 5 |
| `Trunc` | 6 |
| `PathCon d` | `6 + EliasGamma(d)` |
| `Flat` | 7 |
| `Sharp` | 7 |
| `Disc` | 7 |
| `Shape` | 8 |
| `Next` | 9 |
| `Eventually` | 9 |

This is the basis for bit-level `kappa` and stable AST cost accounting.

## Core Representation

### Expressions

The executable kernel uses `MBTTExpr` from `Kolmogorov.hs` as the language of anonymous typed structure.

### Telescope

`engine/src/Telescope.hs` lifts the AST into the real structural unit:

```text
TeleEntry { teName, teType }
Telescope [TeleEntry]
```

The important idea is that every candidate structure, from `Universe` to `DCT`, becomes an ordered telescope of dependent clauses. This is the representation the Rust rewrite should preserve.

### Library

The library is a sequence of structural entries with capability flags, not merely names:

```text
LibraryEntry
  leConstructors
  lePathDims
  leHasLoop
  leIsTruncated
  leAxiomaticExports
  leHasDependentFunctions
  leHasModalOps
  leHasDifferentialOps
  leHasCurvature
  leHasMetric
  leHasHilbert
  leHasTemporalOps
```

These capability flags are crucial for admissibility, generation, and late-shell gating. The rewrite should compute them structurally, not by names.

## Kappa Views

The current repo has three important `kappa` notions.

### Entry kappa

Raw telescope length.

### Desugared kappa

This is the main clause-count donor. `Telescope.hs` desugars certain compact expressions into core judgments:

- formation
- introduction
- elimination
- path attachment
- computation

The key special case is `Susp`, which expands to four conceptual judgments. HIT-like path bundles without an explicit formation clause may also get an additional formation charge.

### Bit kappa

Total prefix-free bit cost from the MBTT encoding.

The new Rust repo should preserve all three as separate metrics even if one becomes the primary selector metric.

## Conservative Well-Formedness Checks

`engine/src/TelescopeCheck.hs` is intentionally conservative:

- `Lib i` must point inside the current library.
- `Var i` must fit within binder depth plus a small ambient depth.
- binders extend scope by one.
- `App _ Univ` is rejected.
- empty telescopes are rejected.
- required ambient depth above `2` is rejected.

This checker is not a full dependent type checker. It is a lightweight filter for obviously malformed telescopes. That same division of labor is worth keeping in Rust:

- fast conservative checker in the search loop,
- richer type/equality logic behind evaluator or acceptance boundaries.

## Canonicalization Contract

The current V1 canonicalization from `MBTTCanonical.hs` is deliberately weak:

- recursively canonicalize children,
- preserve constructor order,
- preserve child order,
- do not beta reduce,
- do not eta reduce,
- do not alpha normalize,
- do not apply symmetry or permutation quotienting.

The core guarantees are:

- idempotence
- deterministic canonical key derivation
- stable dedupe ordering

The rewrite should start from the same conservative contract and only widen equivalence classes deliberately.

## Structural Queries Already Used Everywhere

`Telescope.hs` contains kernel-level structural predicates that are worth porting early:

- `teleBitCost`
- `teleLibRefs`
- `teleVarRefs`
- `teleMaxLibRef`
- `teleIsConnected`
- `teleReferencesWindow`
- `teleHasPointConstructor`
- `telePathDimensions`
- `teleHasLoop`
- `isTriviallyDerivable`
- `classifyTelescope`

These are not just helpers. They are the bridge between anonymous syntax and search/evaluation semantics.

## Reference Telescopes

`referenceTelescope` in `engine/src/Telescope.hs` is the current executable gold corpus for steps 1 through 15. It is the cleanest source of:

- exact AST skeletons,
- charged clause structure,
- expected library references,
- and current donor shapes for export and tests.

The new repo should preserve an equivalent frozen fixture corpus from day one.

## Primary Source Files

- `engine/src/Kolmogorov.hs`
- `engine/src/Telescope.hs`
- `engine/src/TelescopeCheck.hs`
- `engine/src/MBTTCanonical.hs`
- `engine/src/Types.hs`
