# Environment Requirements

This document describes the tooling and packages needed to build and run the PEN project.

## Agda (Formal Verification)

The `agda/` directory contains the Cubical Agda mechanization of the PEN framework.

### Agda compiler

- **Required**: Agda **2.6.4+** (project currently uses **2.8.0**)
- Agda is installed via Cabal (the Haskell package manager): `cabal install Agda`
- Must be built with Cubical support (enabled by default in recent versions)
- Verify: `agda --version`

### Cubical library

- **Required**: cubical library **v0.9** from https://github.com/agda/cubical
- Installation:
  ```bash
  git clone https://github.com/agda/cubical.git ~/.agda/cubical
  echo "$HOME/.agda/cubical/cubical.agda-lib" >> ~/.agda/libraries
  ```
- The project depends on `Cubical.Foundations.Prelude` for path types (`≡`, `refl`, `cong`, `sym`, etc.)

### Known issue (Agda 2.8.0)

Agda 2.8.0 has `InfectiveImport` errors when importing `Cubical.Data.Nat`, `Cubical.Data.Sigma`, `Cubical.Data.Bool`, or any module that transitively depends on them. The project works around this by defining its own types in `Core/Nat.agda`. Do **not** import `Cubical.Data.*` modules.

## Haskell (Information-Theoretic Engine)

The `engine/` directory contains a Haskell executable that computes Kolmogorov complexity (kappa) and Shannon surprise (nu) for Genesis sequence structures.

### GHC (Glasgow Haskell Compiler)

- **Required**: GHC providing `base` in the **4.14 -- 4.19** range (corresponding to roughly **GHC 8.10 -- 9.8**)
- Recommended install method: [GHCup](https://www.haskell.org/ghcup/)
  ```bash
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
  ```
- Verify: `ghc --version`

### Cabal

- **Required**: Cabal **2.4+** (for the `cabal-version: 2.4` spec in `pen-engine.cabal`)
- Installed alongside GHC via GHCup
- Verify: `cabal --version`

### Haskell dependencies

The engine only depends on `base` (no external packages). All library imports are from the standard library:

| Module | Used for |
|--------|----------|
| `Data.List` | List manipulation |
| `Data.Maybe` | Optional values |
| `Text.Printf` | Formatted output |
| `GHC.Generics` | Generic deriving |
| `Data.Map.Strict` | Proof-rank clustering |
| `Data.Set` | Set operations |

### Building the engine

```bash
cd engine
cabal build
cabal run pen-engine
```

## LaTeX (Papers)

The root directory contains `.tex` files (`pen_paper.tex`, `pen_genesis.tex`) for the theoretical papers.

- **Required**: A TeX distribution (e.g., [TeX Live](https://tug.org/texlive/) or [MiKTeX](https://miktex.org/))
- Verify: `pdflatex --version`

## Summary

| Component | Tool | Minimum Version |
|-----------|------|-----------------|
| Agda mechanization | Agda | 2.6.4+ |
| Cubical library | agda/cubical | v0.9 |
| Haskell engine | GHC | 8.10+ |
| Haskell build system | Cabal | 2.4+ |
| Papers | pdflatex | any recent |
| Version control | Git | any recent |
