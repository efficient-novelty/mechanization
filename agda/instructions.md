# PEN Agda Project: Instructions for AI Assistants

## What This Project Is

This is a **mechanization of the Principle of Efficient Novelty (PEN)** in Cubical Agda. PEN is a theoretical framework that models mathematical discovery as an optimization process, where structures are selected based on their efficiency ratio ρ = ν/κ (novelty over effort).

The key claim: In intensional type theory (Coherence Window d=2), integration costs follow the **Fibonacci sequence**, leading to a "Golden Schedule" for mathematical realization.

## The Goal

**Produce a computation that a skeptical mathematician can inspect.**

Specifically:
1. **Phase 1** (DONE): Prove that d=2 + saturation assumption ⟹ Fibonacci costs
2. **Phase 2**: Build a computable κ-measure (effort) using Agda's reflection API
3. **Phase 3**: Build a computable ν-measure (novelty) - the hardest part
4. **Phase 4**: Wire everything into a generative loop that produces the "Genesis Sequence"

The ultimate test: Can the system generate a sequence of mathematical structures that matches (or interestingly diverges from) the hand-tuned Genesis Sequence in the papers?

## Source Documents

Located in `/mnt/c/dev/pen/`:

| File | Description |
|------|-------------|
| `pen_paper.tex` | The foundational PEN paper - defines Coherence Windows, proves Fibonacci recurrence |
| `pen_genesis.tex` | The Genesis Sequence paper - the 16-structure hierarchy, DCT singularity |
| `pen_agda_implementation_plan.md` | Detailed 4-phase implementation plan with code sketches |

**Read these first** if you need to understand the theory.

## Technical Setup (Already Done)

### Agda Installation
- **Version**: Agda 2.8.0
- **Location**: `/home/halvor/.cabal/bin/agda`
- **Cubical Library**: v0.9 at `~/.agda/cubical`
- **Libraries file**: `~/.agda/libraries` contains path to cubical

### Critical Workaround

**Agda 2.8.0 has a compatibility issue with the cubical library.**

When you import `Cubical.Data.Nat` or `Cubical.Data.Sigma`, you get `InfectiveImport` errors because Agda's built-in modules aren't compiled with `--cubical`.

**The workaround**: We define our own types in `Core/Nat.agda`:
- `data ℕ` (natural numbers)
- `_+_` (addition with proofs)
- `_×_` (pairs)

Only import `Cubical.Foundations.Prelude` for path types (`≡`, `refl`, `cong`, `∙`, `sym`, etc.).

**Do NOT try to import**:
- `Cubical.Data.Nat`
- `Cubical.Data.Sigma`
- `Cubical.Data.Bool`
- Any module that transitively imports these

## Project Structure

```
/mnt/c/dev/pen/agda/
├── pen.agda-lib          # Agda library file (depends on cubical)
├── PEN.agda              # Main module - import this to check everything
├── README.md             # User-facing setup instructions
├── instructions.md       # THIS FILE - AI context
├── progress_tracking.md  # Detailed progress log
│
├── Core/
│   ├── Nat.agda          # ℕ, +, ×, fib, Δ, τ, and all proofs
│   └── Sequence.agda     # Vec, History types
│
├── ObligationGraph/
│   ├── Interface.agda    # Schema, Interface², ObligationNode
│   └── Recurrence.agda   # Fibonacci recurrence, Golden Schedule proofs
│
├── Oracle/               # Phase 2-3 (stubs exist)
│   ├── Kappa.agda        # κ measure via reflection
│   ├── Nu.agda           # ν measure (three candidate definitions)
│   └── Efficiency.agda   # Selection dynamics, Bar computation
│
├── Genesis/              # Phase 4 (empty, not started)
│
└── Test/
    ├── Fibonacci.agda    # Unit tests - all pass
    └── WarmUp.agda       # Fluency exercises
```

## Key Definitions

All in `Core/Nat.agda`:

```agda
-- Fibonacci (0-indexed, starting with 1, 1)
fib : ℕ → ℕ
fib 0 = 1, fib 1 = 1, fib (n+2) = fib(n+1) + fib(n)

-- Integration cost (1-indexed)
Δ : ℕ → ℕ
Δ (suc n) = fib n    -- So Δ 1 = 1, Δ 2 = 1, Δ 3 = 2, ...

-- Realization time (cumulative cost)
τ : ℕ → ℕ
τ 0 = 0
τ (suc n) = Δ (suc n) + τ n
```

## Key Theorems Proved

1. **`fibonacci-recurrence`**: Δ(n+3) = Δ(n+2) + Δ(n+1)
2. **`τ-golden-schedule`**: τ n + 1 = fib (suc n)
3. **`fibSum-identity`**: Σfib(i) + 1 = fib(n+2)
4. **`stagnation-recurrence`**: For d=1 systems, costs are constant

## Common Commands

```bash
cd /mnt/c/dev/pen/agda

# Type-check everything
agda PEN.agda

# Run tests
agda Test/Fibonacci.agda

# Check a specific file
agda Core/Nat.agda
```

## The Genesis Sequence (Reference)

This is what Phase 4 should eventually generate:

| n | τ | Structure | Δ | ν | κ | ρ |
|---|---|-----------|---|---|---|---|
| 1 | 1 | Universe | 1 | 1 | 2 | 0.50 |
| 2 | 2 | Unit | 1 | 1 | 1 | 1.00 |
| 3 | 4 | Witness | 2 | 2 | 1 | 2.00 |
| 4 | 7 | Π/Σ types | 3 | 5 | 3 | 1.67 |
| 5 | 12 | Circle S¹ | 5 | 7 | 3 | 2.33 |
| ... | ... | ... | ... | ... | ... | ... |
| 16 | 2583 | DCT | 987 | 150 | 8 | 18.75 |

The Δ column is Fibonacci. The τ column is cumulative sums. These are verified in `Test/Fibonacci.agda`.

## What Needs To Be Done Next

### Phase 2: κ-Oracle (Effort Measurement)
- Implement `countConstructors : Name → TC ℕ` using reflection
- Classify constructors as point/path/higher
- Test against: Unit (κ=1), Bool (κ=2), S¹ (κ=2), Torus (κ=4)
- **Challenge**: Reflection API may not fully support HITs

### Phase 3: ν-Measure (Novelty Measurement)
- This is the hardest and most important phase
- Three candidate definitions in `Oracle/Nu.agda`
- Must produce values that sustain selection dynamics
- **Be honest**: If computed ν doesn't match paper, report that

### Phase 4: Selection Loop
- Candidate DSL in `Genesis/Candidates.agda`
- Selection loop that generates structures
- Compare output to Genesis Sequence

## Philosophy Note

From the implementation plan:

> "Commit now to publishing whatever you find. The value is in the computation, not in the confirmation."

If the model doesn't work, that's also a result worth reporting. The goal is intellectual honesty, not confirmation of the papers.

## Gotchas

1. **Don't import Cubical.Data.*** - Use our local definitions
2. **BUILTIN NATURAL pragma** - Already set in Core/Nat.agda, don't set it again
3. **Proofs by refl** - Many things are definitionally equal; if `refl` doesn't work, the definitions may need adjustment
4. **Large Fibonacci values** - Agda can compute fib 16 = 987 just fine, but type-checking proofs about large values can be slow

## Contact / Context

This project belongs to Halvor Lande (hsl@awc.no), an investment director and amateur mathematician exploring the intersection of type theory, homotopy theory, and mathematical physics.

The PEN framework proposes that mathematical structures emerge through efficiency optimization in systems with 2-dimensional coherence constraints - and that this explains why the structures of modern physics (gauge theory, general relativity) take the forms they do.
