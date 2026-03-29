# PEN: Principle of Efficient Novelty in Cubical Agda

Mechanization of the Principle of Efficient Novelty framework for modeling
mathematical evolution in intensional type theory.

## Setup

### Prerequisites

1. **Agda 2.6.4+** with Cubical support
2. **Cubical library** from https://github.com/agda/cubical

### Installation

```bash
# Clone the cubical library (if not already installed)
git clone https://github.com/agda/cubical.git ~/.agda/cubical

# Add to ~/.agda/libraries:
echo "$HOME/.agda/cubical/cubical.agda-lib" >> ~/.agda/libraries

# Navigate to this project
cd /mnt/c/dev/pen/agda

# Type-check the main module
agda PEN.agda
```

## Project Structure

```
agda/
├── pen.agda-lib          # Agda library file
├── PEN.agda              # Main module (exports all)
├── README.md             # This file
│
├── Core/
│   ├── Nat.agda          # Fibonacci, Δ, τ definitions
│   └── Sequence.agda     # History vectors, windows
│
├── ObligationGraph/
│   ├── Interface.agda    # Schema, Interface definitions
│   └── Recurrence.agda   # Fibonacci recurrence proof
│
├── Oracle/
│   ├── Kappa.agda        # Effort measure (Phase 2)
│   ├── Nu.agda           # Novelty measure (Phase 3)
│   └── Efficiency.agda   # Selection dynamics
│
├── Genesis/              # (Phase 4 - TODO)
│   ├── Candidates.agda
│   ├── Selection.agda
│   └── Trace.agda
│
└── Test/
    └── Fibonacci.agda    # Unit tests
```

## Coherence-Depth Track

The repository now has a dedicated coherence-depth track alongside the
original PEN counting artifact:

- `Core/DepthOneAffine.agda` packages the paper-facing depth-1 corollary
  `cor:d1`: one-layer affine growth with bootstrap indexing, together with the
  closed forms for `Δ` and cumulative `τ` in subtraction-free natural-number
  form.
- `Core/AffineRecurrence.agda` defines a payload-aware affine recurrence and
  proves that a constant shift recovers the homogeneous Fibonacci law.
- `Metatheory/Extensional.agda` packages the extensional/UIP collapse:
  if `isSet A`, then every binary coherence space `p ≡ q` is contractible.
- `Metatheory/KanSubsumption.agda` packages the open-box input consumed by
  cubical composition and, when instantiated at square types, covers the
  arity-3 obligation case. It proves
  `arity3-obligation-syntactically-derivable`,
  `history-beyond-two-algorithmically-subsumed`, and
  `arity3-open-box-hfilled`.
- `Metatheory/AdjunctionBarrier.agda` gives the depth-one lower bound by
  packaging the paper's explicit `\mathbf{2}`/swap promoted-interface
  obstruction: the unary clause `const-left` leaves a residual binary sealing
  obligation along the swap-induced endomap path, the triangle-identity
  corollary is exposed as a theorem-facing record, and `depth1-insufficient`
  / `adjunction-barrier` rule out global depth-1 collapse.
- `Saturation/Axiom.agda` is retained as the older combinatorial modeling
  surface for the payload-free depth-two counting story; it is no longer the
  main justification for the paper's depth-two theorem.
- `Saturation/Decomposition.agda`, `Saturation/AbstractionBarrier.agda`, and
  `Saturation/AbstractionBarrier9.agda` remain as the concrete sealed-interface
  barrier checks that motivate the counting layer.

`PEN.agda` now re-exports the full coherence-depth theorem package together
with the recurrence modules, and `Test/MetatheorySmoke.agda` acts as a small
top-level import regression for the affine, extensional, upper-bound, and
lower-bound theorems.

## Implementation Status

### Phase 1: Complete ✓

- [x] Fibonacci sequence definition
- [x] Integration cost Δₙ = Fₙ
- [x] Realization time τₙ = F_{n+2} - 1
- [x] Fibonacci sum identity proof
- [x] Golden Schedule proof
- [x] Recurrence theorem for d=2
- [x] Paper-facing affine corollary for d=1
- [x] Unit tests matching Genesis table

### Phase 2: Stub

- [ ] Reflection-based constructor counting
- [ ] Path constructor classification
- [ ] κ measurement for standard HoTT types

### Phase 3: Stub

- [ ] Novelty measure definition
- [ ] Type enumeration for ν computation
- [ ] Validation against Genesis table

### Phase 4: TODO

- [ ] Candidate DSL (Genome)
- [ ] Candidate generator (Mutator)
- [ ] Selection loop
- [ ] Trace output

## Key Results

### Theorem 1: Complexity Scaling

For foundations with Coherence Window d=2 (intensional type theory):

```
Δ(n+1) = Δ(n) + Δ(n-1)
```

Given Δ(1) = Δ(2) = 1, this yields Δ(n) = Fₙ.

### Theorem 2: Golden Schedule

Realization time follows:

```
τₙ = Σᵢ₌₁ⁿ Δᵢ = F_{n+2} - 1
```

### Theorem 3: Affine One-Step Growth (d=1)

For depth-1 systems with uniform payload `c` and empty bootstrap:

```
Δ_{n+1} = Δ_n + c
```

with subtraction-free closed forms packaged in `Core/DepthOneAffine.agda`.
The older payload-free stagnation surface remains available in
`ObligationGraph/Recurrence.agda`.

## Running Tests

```bash
# Type-check test file (tests pass if no errors)
agda Test/Fibonacci.agda
```

The tests verify:
- Fibonacci values match expected sequence
- Δ and τ match Genesis table exactly
- Recurrence identities hold
- Infrastructure correspondence (Φ₄ < φ)

## References

1. Univalent Foundations Program, *Homotopy Type Theory*, 2013
2. Cohen et al., "Cubical Type Theory", TYPES 2015
3. Schreiber, "Differential cohomology in a cohesive infinity-topos", 2013
4. Vezzosi et al., "Cubical Agda", ICFP 2019
