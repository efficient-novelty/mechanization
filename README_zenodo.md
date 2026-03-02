# README for Zenodo Deposition

This repository contains the research artifacts for:

**The Principle of Efficient Novelty (PEN)** — including a narrative manuscript, formal proof mechanization in Cubical Agda, and a reproducible synthesis/verification engine in Haskell.

---

## What this Zenodo release contains

This Zenodo deposition is organized into separate archival bundles for clarity and reproducibility.

### 1) Paper bundle

Recommended filename:

- `pen_unified_paper_vX.Y.zip`

Expected contents:

- `pen_unified.tex` — source manuscript
- `pen_unified.pdf` — compiled manuscript
- `README_zenodo.md` — this file

### 2) Code bundle

Recommended filename:

- `pen_unified_code_vX.Y.zip`

Expected contents:

- `agda/` — Cubical Agda mechanization
- `engine/` — Haskell synthesis/verification engine
- `scripts/` — reproducibility scripts
- `README.md`
- `environment_requirements.md`

### Optional 3) Runs/artifacts bundle

Recommended filename:

- `pen_unified_runs_vX.Y.zip`

Use this only if you want to archive generated run outputs (CSV/log/report artifacts) as a frozen dataset.

---

## Why split into bundles?

- **Paper readers** can access the manuscript immediately.
- **Reproducibility reviewers** can download only the code package.
- **Metadata quality** improves when each artifact type is clearly labeled.
- **Versioning clarity**: paper and code can evolve together while remaining navigable.

---

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

---

## Recommended exclusions when creating ZIP archives

Do **not** include temporary/build products unless intentionally archiving them:

- LaTeX auxiliaries: `.aux`, `.log`, `.out`, `.toc`
- Cabal build dirs: `dist-newstyle/`
- Editor/OS cruft: `.DS_Store`, swap files, etc.

---

## Suggested citation text (example)

> Lande, H. (2026). *The Principle of Efficient Novelty (PEN): Manuscript, mechanization, and reproducible engine* (Version X.Y) [Data set]. Zenodo. https://doi.org/XX.XXXX/zenodo.XXXXXXX

Replace title/version/DOI with the final Zenodo metadata.

---

## Contact

For questions regarding this deposition, use the contact information in the manuscript metadata and repository documentation.
