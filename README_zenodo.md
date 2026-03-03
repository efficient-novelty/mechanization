# README for Zenodo Deposition

This repository contains the research artifacts for:

**The Principle of Efficient Novelty (PEN)** — including a narrative manuscript, formal proof mechanization in Cubical Agda, and a reproducible synthesis/verification engine in Haskell.

---

## What this Zenodo release contains

This Zenodo deposition is organized into separate archival bundles for clarity and reproducibility.

### 1) Paper bundle
- `pen_paper_v1.0.zip`

Contents:

- `pen_paper.tex` — source manuscript
- `pen_paper.pdf` — compiled manuscript
- `README_zenodo.md` — this file

### 2) Code bundle
- `pen_code_v1.0.zip`

Contents:

- `agda/` — Cubical Agda mechanization
- `engine/` — Haskell synthesis/verification engine
- `scripts/` — reproducibility scripts
- `README.md`
- `environment_requirements.md`

### 3) Runs/artifacts bundle
- `pen_runs_v1.0.zip`

## Reproducibility quick checks

From the code bundle root:

### Agda checks

```bash
cd agda
agda PEN.agda
agda Test/Fibonacci.agda
agda Test/OpSchemaTest.agda
agda Test/BridgePayloadContract.agda
```

### Haskell engine checks

```bash
cd engine
cabal build all
cabal run ab-initio -- --structural --csv ../runs/quickstart_structural_d2.csv
```

### Benchmark script

```bash
cd ..
./scripts/benchmark.sh quickstart_benchmark
```

The benchmark report is written to:

- `runs/quickstart_benchmark/REPORT.txt`

---

## Environment requirements

See:

- `environment_requirements.md`

Minimum tooling summary:

- Agda 2.6.4+ (project target: 2.8.0)
- Cubical library v0.9
- GHC compatible with `base` 4.14–4.19
- Cabal 2.4+
- Optional: `pdflatex` for manuscript rebuilds


## Suggested citation text (example)

> Lande, H. (2026). *The Principle of Efficient Novelty (PEN): Manuscript, mechanization, and reproducible engine* (Version 1.0) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.18841121

Replace title/version/DOI with the final Zenodo metadata.

## Github
https://github.com/efficient-novelty/mechanization

## Contact
halvor.s.lande@gmail.com
