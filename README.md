# ✨ PEN Theory — Mechanized from First Principles

> **This repository is a mechanized, executable research artifact.**
> It contains both:
> 1. **Formal proofs in Cubical Agda** (the theory layer), and
> 2. **A reproducible synthesis/verification engine in Haskell** (the computational layer).

If you only read one thing first: **PEN is not just a narrative paper; the core claims are encoded as machine-checked proofs and deterministic runs.**

---

## What is PEN?

**PEN (Principle of Efficient Novelty)** is an information-theoretic selection rule for growing formal mathematical libraries:

- each candidate structure contributes novelty \(\nu\) (new derivational power),
- each candidate has effort cost \(\kappa\) (specification/integration burden),
- selection favors efficient novelty (high \(\nu/\kappa\)) under coherence constraints.

In this project, PEN is developed and audited through:

- a **Cubical Agda mechanization** of key obligations/theorems, and
- a **Haskell engine** that computes and stress-tests synthesis trajectories (including window-sweep experiments around the coherence parameter \(d\)).

---

## 🚀 New visitor quick start (10 minutes)

### 1) See the mechanized proofs first

```bash
cd agda
agda PEN.agda
```

Then run proof-oriented Agda tests:

```bash
agda Test/Fibonacci.agda
agda Test/OpSchemaTest.agda
agda Test/BridgePayloadContract.agda
```

### 2) Build and run the executable synthesis engine

```bash
cd engine
cabal build all
cabal run ab-initio -- --structural --csv ../runs/quickstart_structural_d2.csv
```

### 3) Run one-command benchmark verification

```bash
cd ..
./scripts/benchmark.sh quickstart_benchmark
```

Read the generated report:

- `runs/quickstart_benchmark/REPORT.txt`

---

## Repository map

| Path | Purpose | Start here when... |
|---|---|---|
| `agda/` | Formal mechanization in Cubical Agda | You want machine-checked proofs and contracts |
| `engine/` | Haskell synthesis + analysis engine | You want reproducible computational evidence |
| `scripts/` | Reproduction and comparison harnesses | You want one-command benchmark/regression runs |
| `docs/adr/` | Architecture decision records | You want historical design rationale |
| `docs/reports/` | Phase reports and signoffs | You want implementation evidence by milestone |
| `runs/` | Output artifacts from experimental runs | You want to inspect generated CSV/log evidence |
| `*.tex` / `*.pdf` | Theory manuscripts | You want the full narrative and theorem exposition |

---

## How to make this repository digestible for new visitors

A practical documentation strategy for this project:

1. **Keep the root README short, visual, and task-oriented** (what PEN is, where proofs live, how to run).
2. **Move deeper design detail into dedicated docs** (architecture, ADRs, phase reports).
3. **Document workflows by role**:
   - *Reader* (papers + high-level map),
   - *Verifier* (Agda + benchmark scripts),
   - *Contributor* (architecture + ADR trail).
4. **Prefer command-copyability over prose** for setup and run sections.

This README follows that pattern and links to deeper docs.

---

## Should architecture be separate from README?

**Recommendation: yes.**

- Keep an **architecture summary** in this README (for orientation).
- Keep a **separate detailed architecture doc** for maintainability and versioned design evolution.

➡ See [`docs/architecture.md`](docs/architecture.md) for the full system view.

---

## Installation and prerequisites

For complete prerequisites, see [`environment_requirements.md`](environment_requirements.md).

### Minimum toolchain

- **Agda** 2.6.4+ (project currently targets 2.8.0)
- **Cubical library** v0.9
- **GHC** (base 4.14–4.19 compatible)
- **Cabal** 2.4+
- **Git**
- Optional: **pdflatex** for paper builds

### Suggested installation flow

1. Install GHC + Cabal (e.g., via GHCup).
2. Install Agda.
3. Install Cubical library and register `cubical.agda-lib` in `~/.agda/libraries`.
4. Verify versions:

```bash
agda --version
ghc --version
cabal --version
```

---

## Running the codebase (by subsystem)

### A) Agda mechanization

```bash
cd agda
agda PEN.agda
```

Useful checks:

```bash
agda Test/Fibonacci.agda
agda Test/WarmUp.agda
agda Test/SaturationTest.agda
```

### B) Haskell engine

```bash
cd engine
cabal build all
```

Key executables:

```bash
cabal run pen-engine
cabal run ab-initio -- --structural --csv ../runs/structural_d2.csv
cabal run acceptance
```

### C) Reproducibility scripts

From repository root:

```bash
./scripts/repro_ab_initio.sh my_run
./scripts/compare_runs.sh runs/baseline runs/my_run
./scripts/benchmark.sh external_review
```

---

## Common newcomer paths

### “I want to verify the core claim quickly”

1. Type-check `agda/PEN.agda`.
2. Run `./scripts/benchmark.sh`.
3. Read `runs/<tag>/REPORT.txt`.

### “I want to understand the design before contributing”

1. Read this README.
2. Read [`docs/architecture.md`](docs/architecture.md).
3. Skim ADRs in `docs/adr/`.

### “I want to inspect empirical artifacts”

1. Start with `runs/phase0_baseline/REPORT.txt`.
2. Compare with a fresh run from `scripts/repro_ab_initio.sh`.

---

## Contribution guidance (documentation-first)

When adding major features, update in this order:

1. `docs/adr/` (decision + rationale),
2. `docs/architecture.md` (structure and flow),
3. this `README.md` (visitor-facing entry points),
4. any runbooks/scripts affected.

That ordering keeps onboarding quality high while preserving a clear design history.

---

## Additional reading

- Environment details: [`environment_requirements.md`](environment_requirements.md)
- Agda mechanization notes: [`agda/README.md`](agda/README.md)
- Architecture guide: [`docs/architecture.md`](docs/architecture.md)
- Phase evidence contract: [`docs/phase1_evidence_contract.md`](docs/phase1_evidence_contract.md)

