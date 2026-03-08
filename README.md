# PEN Theory: Mechanized from First Principles

This repository is an executable research artifact with two layers:

1. Formal mechanization in Cubical Agda.
2. A reproducible synthesis/verification engine in Haskell.

PEN (Principle of Efficient Novelty) models mathematical library growth by selecting candidates that maximize novelty per construction effort under coherence constraints.

## Quick start

### 1) Type-check core Agda files

```bash
cd agda
agda PEN.agda
agda Test/Fibonacci.agda
agda Test/OpSchemaTest.agda
agda Test/BridgePayloadContract.agda
```

### 2) Build and run the Haskell engine

```bash
cd engine
cabal build all
cabal run ab-initio -- --structural --csv ../runs/quickstart_structural_d2.csv
```

### 3) Run benchmark harness

```bash
cd ..
./scripts/benchmark.sh quickstart_benchmark
```

Read the generated report at `runs/quickstart_benchmark/REPORT.txt`.

## Repository map

- `agda/`: Cubical Agda mechanization and tests.
- `engine/`: Haskell engine and executables.
- `scripts/`: Reproduction and analysis scripts.
- `docs/adr/`: Architecture decision records.
- `docs/reports/`: Implementation and phase reports.
- `runs/`: Run outputs (mostly generated artifacts).
- `*.tex`: Manuscript sources.

See `docs/architecture.md` for system architecture details.

## Prerequisites

See `environment_requirements.txt` for complete setup and dependency details.

Minimum toolchain:

- Agda 2.6.4+ (project currently targets 2.8.0)
- Cubical library v0.9
- GHC compatible with `base` 4.14-4.19
- Cabal 2.4+
- Git
- Optional: `pdflatex`

## Common commands

From `engine/`:

```bash
cabal build all
cabal run ab-initio -- --strict --phase1-shadow --max-steps 7
cabal run acceptance
```

From repo root:

```bash
./scripts/repro_ab_initio.sh my_run
./scripts/compare_runs.sh runs/phase0_baseline runs/my_run
```

## Artifact policy

This repository keeps source and curated reports. Build outputs, temporary run logs, and local experiment CSV/log files are ignored by `.gitignore`.

## Additional reading

- `environment_requirements.txt`
- `docs/architecture.md`
- `docs/phase1_evidence_contract.md`
- `agda/README.md`
