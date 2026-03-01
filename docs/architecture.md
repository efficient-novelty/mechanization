# PEN Repository Architecture

This document provides the **maintainer-level architecture view** of the PEN repository.
For a quick visitor introduction, start with the root [`README.md`](../README.md).

## 1. System overview

The repository has two primary mechanized pillars:

1. **Proof pillar (Agda):** formal obligations and theorem-level contracts.
2. **Computation pillar (Haskell):** executable candidate generation, scoring, selection, and acceptance checks.

These pillars are connected by reporting and evidence workflows in `scripts/`, `runs/`, and `docs/reports/`.

```text
            ┌──────────────────────────────────────────────┐
            │                PEN Theory                    │
            │     (papers / axioms / model narrative)      │
            └──────────────────────────────────────────────┘
                           │
          ┌────────────────┴────────────────┐
          │                                 │
┌───────────────────────────┐    ┌───────────────────────────┐
│  agda/                    │    │  engine/                  │
│  Cubical mechanization    │    │  executable synthesis     │
│  (proof obligations)      │    │  + audits + acceptance    │
└───────────────────────────┘    └───────────────────────────┘
          │                                 │
          └──────────────┬──────────────────┘
                         │
              ┌─────────────────────────┐
              │ scripts/ + runs/        │
              │ reproducible evidence    │
              └─────────────────────────┘
                         │
              ┌─────────────────────────┐
              │ docs/reports + ADRs     │
              │ signoff + decisions     │
              └─────────────────────────┘
```

## 2. Directory-level responsibilities

## `agda/` — formal mechanization

- Contains Cubical Agda modules for PEN obligations, recurrence properties, schema contracts, and bridge tests.
- `PEN.agda` acts as the central aggregation/entry module.
- `Test/` modules provide type-check based regression checks.

When to modify:
- You are changing a formal statement, invariant, schema, or proof boundary.

## `engine/` — synthesis and validation engine

- `pen-engine.cabal` defines multiple executables (`pen-engine`, `ab-initio`, `acceptance`, etc.).
- `src/` hosts enumeration, scoring (\(\nu\), \(\kappa\)), coherence handling, MBTT-first search, and telemetry/audit modules.
- `scripts/` under `engine/` contains phase-specific checks used by CI and migration/signoff workflows.

When to modify:
- You are changing executable behavior, scoring logic, run modes, or acceptance harnesses.

## `scripts/` (root) — reproducibility entry points

- `benchmark.sh`: external-review benchmark profile runner.
- `repro_ab_initio.sh`: deterministic run harness with standard modes.
- `compare_runs.sh`: CSV-level run comparisons.

When to modify:
- You are changing user-facing reproducibility workflows.

## `docs/`

- `docs/adr/`: architecture decisions and rationale snapshots.
- `docs/reports/`: implementation reports and phase signoffs.
- `docs/phase1_evidence_contract.md`: contract-level expectations for evidence artifacts.

When to modify:
- You are documenting design decisions or milestone evidence.

## `runs/`

- Stores generated artifacts (CSV, logs, reports) from reproducibility runs.
- Includes baseline and test run samples.

When to modify:
- Usually through scripts; direct manual edits should be rare.

## 3. Recommended contributor workflow

1. **Understand intent**
   - Read relevant ADR(s) and report(s) for your area.
2. **Change implementation**
   - Agda proofs and/or Haskell engine code.
3. **Run reproducibility checks**
   - At minimum, targeted run mode(s) + acceptance checks.
4. **Record impact**
   - Update docs/reports or ADRs if design/behavior changed.
5. **Update README only for entry-point changes**
   - Keep root onboarding concise.

## 4. Why architecture is separate from README

Keeping architecture in a dedicated file gives:

- **Cleaner onboarding:** README remains short and task-focused.
- **Lower churn:** deep architecture edits do not overwhelm first-time visitors.
- **Better maintainability:** design evolution can be tracked in ADRs + architecture doc.

The README should always contain a short architecture summary and link here.

## 5. Operational run matrix (quick reference)

| Goal | Command | Primary output |
|---|---|---|
| Type-check main mechanization | `cd agda && agda PEN.agda` | terminal success/failure |
| Build engine executables | `cd engine && cabal build all` | Cabal build output |
| Structural synthesis run | `cd engine && cabal run ab-initio -- --structural --csv ../runs/<tag>.csv` | run CSV + log |
| Acceptance suite | `cd engine && cabal run acceptance` | acceptance terminal output |
| Full benchmark profile | `./scripts/benchmark.sh <tag>` | `runs/<tag>/REPORT.txt` |

