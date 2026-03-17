# Agda Validation

## Role in the System

The clean split for the new repo is:

- Rust discovers and evaluates anonymous candidates.
- Agda validates exported witnesses and payload claims.

Agda should never be the hot loop.

This matches both the user's frozen architecture and the Phase 6 reporting and verification split already documented in the current repo.

## Current Agda Surfaces That Matter

### Main proof pillar

`agda/PEN.agda` re-exports the major mechanization modules:

- core recurrence and sequence facts
- saturation and schema modules
- adjunction depth modules

This is useful background, but not all of it needs to move into the new repo.

### Bridge contract

The most important actual donor for the rewrite is:

- `agda/Test/BridgePayloadContract.agda`

This file defines the Agda-side contract for exported verification payloads.

### Generated witness stubs

`agda/bridge/PEN/Genesis/Step*.agda` are generated postulate wrappers around the current reference telescopes.

They show the present export shape:

- one Agda module per accepted step
- library imports inferred from `Lib` references
- each telescope entry rendered as a postulate

## Current Payload Schema

`engine/src/AgdaExport.hs` exports verification payloads with these fields:

```text
step
name
canonical_key
kappa_bit
kappa_desugared
anonymous_ast
nu_claim:
  nu_g
  nu_h
  nu_c
  nu_total
```

The Agda bridge contract mirrors this with:

- `NuClaim`
- `BridgePayload`
- `CanonicalKeySound`
- `NuClaimWellFormed`
- `DecodeNonInterference`
- `ContractWitness`

The new repo should preserve this general shape even if the renderer changes.

## Important Agda Environment Caveats

The current Agda setup is messy enough that it should be documented explicitly.

### Cubical import problem

Some current Agda files avoid `Cubical.Data.*` imports due to import and interface issues with Agda 2.8.0 and the Cubical library.

The repo works around this by:

- defining local naturals and simple helpers in `agda/Core/Nat.agda`
- using `Cubical.Foundations.Prelude` selectively

### Reflection versus cubical split

Some oracle modules, especially older `Oracle/*.agda`, use `--without-K` instead of `--cubical` because reflection and the installed Cubical setup conflicted.

That means the new repo should avoid relying on Agda reflection for the sidecar if possible.

## What the New Repo Probably Needs

The new repo does not need the full old Agda universe. It needs a small, stable verification surface.

Suggested minimal sidecar:

- `agda/Prelude.agda`
- `agda/StepWitness.agda`
- `agda/Generated/StepNN.agda`
- `agda/Generated/PayloadNN.agda` or JSON-adjacent generated files
- a verification script that shells out to Agda after acceptance

## Good Validation Targets for the New Repo

Reasonable Agda-side checks include:

- exported AST payload matches the canonical key contract
- exported `nu` claim is structurally well formed
- generated witness modules import the right prior steps
- accepted step manifests and payloads are deterministic across re-export

## What Not to Copy Blindly

- old phase plans that assume Agda will own generation
- reflection-heavy oracle modules if the goal is only sidecar validation
- outdated 16-step narrative from older overview files

## Primary Source Files

- `engine/src/AgdaExport.hs`
- `agda/Test/BridgePayloadContract.agda`
- `agda/bridge/PEN/Genesis/Step1-Universe.agda`
- `agda/bridge/PEN/Genesis/Step15-DCT.agda`
- `docs/reports/p6_v2_bridge_schema_report.md`
- `docs/reports/p6_v3_agda_harness_report.md`
- `docs/reports/p6_v4_verification_split_signoff.md`
