---
name: pen-atomic
description: Donor map and engineering guide for the PEN atomic rewrite. Use when working on a Rust-first repo that aims to rediscover the 15-step genesis sequence from atomic MBTT primitives with an Agda sidecar for export and verification, especially for AST/schema design, search semantics, evaluator parity, checkpointing, donor-module mapping, or migration decisions from this Haskell and Agda repository.
---

# PEN Atomic

## Overview

Use this skill as the portable memory pack for the new `pen-atomic` repository. It captures the executable donor code, the paper-level targets, the Agda verification contract, and the hard lessons from the old search engine so a new Codex instance does not have to rediscover the whole repo from scratch.

The mathematical theory has been distilled into a linked tree under [theory/README.md](theory/README.md), so future work can use compact theorem notes instead of reopening the large TeX manuscripts.

## Working Rules

1. Treat the user-provided frozen Rust architecture as the current rewrite contract unless the user explicitly changes it.
2. Treat `engine/src/*.hs` as the source of truth for executable semantics, not older prose that may lag behind.
3. Keep the hot path name-free. Semantic labels belong in reporting and Agda export layers only.
4. Remember that the current repo completes 15 steps mainly through a molecular lane after step 3. A new atomic-only repo must not mistake that for solved atomic discovery.
5. Separate three targets that the repo sometimes conflates:
   - the paper or aspirational 15-step values,
   - the current strict executable values,
   - older historical 16-step or Lie-group variants that still appear in some Agda and TeX files.

## Reference Map

- Start with [theory/README.md](theory/README.md) when you need the theorem map or want to know which manuscripts still matter.
- Read [theory/genesis.md](theory/genesis.md) when you need the exact strict 15-step target.
- Read [theory/pen-model.md](theory/pen-model.md), [theory/coherence-and-scaling.md](theory/coherence-and-scaling.md), and [theory/novelty-selection-and-rejection.md](theory/novelty-selection-and-rejection.md) for the mathematical contract behind the search objective.
- Read [theory/late-framework-abstraction.md](theory/late-framework-abstraction.md) and [theory/terminal-dct.md](theory/terminal-dct.md) before making claims about steps 10 to 15.
- Start with [01-project-brief](references/01-project-brief.md) for scope, source priority, and stale-doc warnings.
- Read [02-target-sequence](references/02-target-sequence.md) when you need the canonical 15-step target, current strict values, and bar arithmetic.
- Read [03-repo-donor-map](references/03-repo-donor-map.md) when deciding what to port, what to demote to reporting, and what not to copy.
- Read [04-mbtt-kernel](references/04-mbtt-kernel.md) before touching ASTs, encoding, canonicalization, telescope storage, or capability flags.
- Read [05-search-and-selection](references/05-search-and-selection.md) before implementing enumeration, admissibility, ranking, minimality, or resume behavior.
- Read [06-atomic-research-lessons](references/06-atomic-research-lessons.md) before designing any atomic search loop or anti-junk frontier policy.
- Read [07-agda-validation](references/07-agda-validation.md) before building export payloads, witness modules, or verification scripts.
- Read [08-evidence-and-invariants](references/08-evidence-and-invariants.md) when you need test or CI contracts, ablation lessons, or non-interference requirements.
- Read [09-rust-rewrite-blueprint](references/09-rust-rewrite-blueprint.md) when scaffolding crates, schemas, checkpoints, memory limits, and implementation order.
- Read [10-open-questions](references/10-open-questions.md) before making decisions that could accidentally lock in the wrong semantics.

## Task Routing

### If you are defining the new Rust core

Read:
- [theory/pen-model.md](theory/pen-model.md)
- [theory/coherence-and-scaling.md](theory/coherence-and-scaling.md)
- [04-mbtt-kernel](references/04-mbtt-kernel.md)
- [03-repo-donor-map](references/03-repo-donor-map.md)
- [09-rust-rewrite-blueprint](references/09-rust-rewrite-blueprint.md)

Focus on:
- frozen atom schema
- telescope and library representation
- exact kappa and canonicalization contracts
- stable IDs, hashes, and checkpoint compatibility

### If you are implementing the search loop

Read:
- [theory/novelty-selection-and-rejection.md](theory/novelty-selection-and-rejection.md)
- [theory/genesis.md](theory/genesis.md)
- [05-search-and-selection](references/05-search-and-selection.md)
- [06-atomic-research-lessons](references/06-atomic-research-lessons.md)
- [02-target-sequence](references/02-target-sequence.md)

Focus on:
- exact-band search and bar semantics
- admissibility from structural debt, not names
- deterministic dedupe and SCC minimality
- anti-junk frontier shaping for a truly atomic lane

### If you are implementing Agda verification

Read:
- [theory/late-framework-abstraction.md](theory/late-framework-abstraction.md)
- [theory/terminal-dct.md](theory/terminal-dct.md)
- [07-agda-validation](references/07-agda-validation.md)
- [08-evidence-and-invariants](references/08-evidence-and-invariants.md)

Focus on:
- bridge payload schema
- contract witness modules
- deterministic export and replay
- keeping Agda out of the hot loop

### If you are checking whether a design choice is honest

Read:
- [01-project-brief](references/01-project-brief.md)
- [08-evidence-and-invariants](references/08-evidence-and-invariants.md)
- [10-open-questions](references/10-open-questions.md)

Reject designs that:
- smuggle semantic names or target IDs into core crates
- let reporting or decoding influence selection
- hide molecular templates behind supposedly atomic enumeration
- rely on floating-point comparisons for core admissibility or ranking

## Quick Summary

- The reusable donor kernel is real: MBTT AST, telescope representation, canonicalization, checking, native `nu`, structural `nu`, Agda export, and evidence contracts.
- The current 15-step success is also real, but it is mostly a molecular recovery lane, not open-ended atomic invention from scratch.
- The new repo should preserve the kernel and verification split while replacing the generator, frontier management, storage, resume, and memory model with the frozen Rust architecture.
