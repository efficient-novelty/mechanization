# Remaining Paper Improvement Plan for `1_coherence_depth.tex`

## Objective

The paper now presents itself as a theorem about a fixed cubical extension calculus over a chosen CCHM-style core. The remaining work is to supply the missing bridge from cubical syntax to normalized extension signatures, tighten the cost model, and restate the strongest theorems over that bridge rather than over the current synthetic counting layer alone.

## Highest-Priority Remaining Work

## 1. Define the formal theorem object completely

### Remaining tasks

- Give an explicit formal tuple
  - `C` = chosen cubical core
  - `Ext(C)` = surface language of globally acting extensions
  - `Seal` = opacity / controlled-unfolding discipline
  - `mu` = minimal normalized opaque-presentation cost
- Add a compact grammar for surface extensions:
  - payload declarations
  - global action clauses
  - comparison / transport clauses
  - opacity annotations
- Define elaboration from `Ext(C)` into core signatures over `C`.
- Define precisely what is public, opaque, and available to later sealing steps.

### Done criterion

- A reader can see exactly what an admissible extension declaration is and what part of it survives sealing.

## 2. Add the missing bridge section

### Required new section

- `From Cubical Syntax to Normalized Extension Signatures`

### Remaining tasks

- Define elaboration:
  - every admissible surface extension `e` elaborates to `[[e]]`
- Define normalization:
  - `N([[e]])` is the canonical normalized public signature
- Define presentation equivalence modulo:
  - definitional equality
  - telescope isomorphism
  - higher-coherence elimination
- State and prove:
  1. Elaboration soundness
  2. Signature adequacy
  3. Minimality / necessity of irreducible fields
  4. Canonical presentation / canonical isomorphism for equivalent extensions

### Done criterion

- The counting layer is attached to actual cubical syntax rather than standing beside it.

## 3. Replace generatorwise density by basis-sensitive coverage/minimality

### Remaining tasks

- Introduce a chosen finite generating basis for the active interface.
- Redefine coupling sites in terms of basis sites rather than raw exported generators.
- Replace or refine the current maximal-density theorem with:
  - a coverage theorem: missing clauses break admissibility
  - a minimality theorem: redundant clauses are eliminable from canonical normalized presentations
- Update the examples and recurrence discussion so they count irreducible basis clauses rather than assuming a rigid one-clause-per-generator shape.

### Done criterion

- The counting theorem is stable under harmless polymorphic compression of clauses.

## 4. Make the upper bound syntactic via higher-coherence elimination

### Remaining tasks

- Replace the current primitive-cost convention by a minimal-presentation measure:
  - `mu(e) =` least size of a normalized opaque presentation of `e`
- Prove a theorem of the form:

> Any admissible extension whose normalized public signature contains a primitive historical field of arity `k >= 3` is definitionally equivalent to one in which that field is removed and replaced by a term synthesized from lower-arity boundary data using cubical Kan operations.

- Rewrite the upper-bound argument so it proves eliminability from minimal public presentations, not only existence / contractibility of higher fillers.
- Make explicit where `hcomp`, `transp`, and related Kan structure synthesize the replacement term.

### Done criterion

- "Higher fillers cost zero" is a theorem of minimality, not a modeling convention.

## 5. Upgrade telescopic subsumption to recent-history factorization

### Remaining tasks

- Reformulate the chronological-window result as a factorization theorem on normalized public signatures.
- Prove that every surviving binary clause factors through the previous two normalized signatures.
- Make the dependency of the recurrence theorem on this factorization explicit.

### Done criterion

- The paper no longer jumps directly from dimension control to chronology; the two-layer window is justified by a factorization theorem.

## 6. Restate the main recurrence over `mu`

### Remaining tasks

- Rewrite the recurrence section in terms of `mu`, not the current `Delta`-based primitive-cost convention.
- Make the main theorem:

\[
\mu_{n+1} = \mu_n + \mu_{n-1} + \kappa_n + \kappa_{n-1}.
\]

- Keep Fibonacci only as the constant-payload corollary:

\[
U_n = \mu_n + 2c.
\]

### Done criterion

- The depth-two affine law is the headline theorem and shifted Fibonacci is explicitly secondary.

## 7. Tighten the lower-bound section inside the extension discipline

### Remaining tasks

- Keep the swap obstruction as the primary explicit binary lower bound.
- Keep clutching as the geometric exact depth-two family.
- Decide whether adjunction remains:
  - a categorical reading of the same binary obstruction, or
  - a genuine internal theorem of the extension language
- If no internal no-reconstruction theorem is added, rewrite the adjunction material so it clearly plays the first role only.

### Done criterion

- The lower bound reads as a theorem inside the chosen extension discipline, not as a loose collection of analogies.

## 8. Add the syntax-backed modality story if modalities remain central examples

### Remaining tasks

- Replace generic prose about "global cohesive modalities" with an actual modal syntax reference point.
- Add the modal DTT / dependent right adjoint references needed if modalities remain prominent.
- If cubical-modal interaction remains a main motivation, add the corresponding cubical-modal citation support and align the prose with it.

### Done criterion

- Modal examples are justified by a concrete extension syntax, not only by intuition.

## 9. Finish the bibliography upgrade

### Remaining tasks

- Add the remaining references needed for:
  - controlled unfolding / extension types
  - Agda's actual abstraction mechanisms if discussed directly
  - modal dependent type theory / dependent right adjoints if modalities stay central
- Make sure each new citation does argumentative work in the prose:
  - core calculus reference for `C`
  - controlled-unfolding reference for `Seal`
  - modal syntax references for modality examples
  - Cubical Agda citations for mechanization and examples

### Done criterion

- The bibliography supports the strengthened theorem object rather than only the old informal background.

## 10. Align the mechanization section with the final bridge theorem

### Remaining tasks

- Once the bridge and elimination theorems are written, update `sec:mechanization` to say exactly which parts of:
  - elaboration
  - normalized signatures
  - higher-coherence elimination
  - recent-history factorization
  are formalized
- Extend the theorem map if new Agda counterparts are added.
- Keep the trust-boundary paragraph synchronized with the actual formal boundary.

### Done criterion

- The mechanization section exactly matches the final theorem stack and its formal support.

## Final Submission Checklist

- `Ext(C)` and `Seal` are defined explicitly.
- The bridge section exists and is central.
- Adequacy and minimality are stated and proved.
- Higher-coherence elimination is proved over normalized public signatures.
- Recent-history factorization is proved.
- The recurrence is stated and proved over `mu`.
- Fibonacci appears only as the constant-payload corollary.
- The lower bound is clearly internal to the extension discipline.
- The bibliography and mechanization sections match the final claims exactly.

## Suggested Next Execution Order

1. Write the formal `Ext(C)` / `Seal` definitions and the bridge-section skeleton.
2. Replace maximal-interface density with basis-sensitive coverage/minimality.
3. Rework the upper bound into higher-coherence elimination.
4. Upgrade telescopic subsumption into recent-history factorization.
5. Rewrite the recurrence over `mu`.
6. Tighten the lower-bound section and supporting citations.
7. Update the mechanization map to match the completed bridge theorem stack.
