# Remaining Paper Improvement Plan for `1_coherence_depth.tex`

## Objective

The paper now fixes the cubical theorem object explicitly, includes a bridge section from surface extensions to normalized public signatures upgraded to an adequacy/minimality theorem package, and replaces generatorwise density with a basis-sensitive coverage/minimality law. The remaining work is to make the upper bound syntactic via higher-coherence elimination, upgrade chronology to recent-history factorization, restate the main recurrence over the minimal opaque cost `mu`, and align the lower-bound, bibliography, and mechanization sections with that sharper theorem stack.

## Highest-Priority Remaining Work

## 1. Make the upper bound syntactic via higher-coherence elimination

### Remaining tasks

- Replace the current primitive-cost convention by a genuinely presentation-invariant minimality theorem over `mu`.
- Prove a theorem of the form:

> Any admissible extension whose normalized public signature contains a primitive historical field of arity `k >= 3` is definitionally equivalent to one in which that field is removed and replaced by a term synthesized from lower-arity boundary data using cubical Kan operations.

- Rewrite the upper-bound argument so it proves eliminability from minimal public presentations, not only existence / contractibility of higher fillers.
- Make explicit where `hcomp`, `transp`, and related Kan structure synthesize the replacement term.

### Done criterion

- "Higher fillers cost zero" is a theorem of minimality, not a modeling convention.

## 2. Upgrade telescopic subsumption to recent-history factorization

### Remaining tasks

- Reformulate the chronological-window result as a factorization theorem on normalized public signatures.
- Prove that every surviving binary clause factors through the previous two normalized signatures.
- Make the dependency of the recurrence theorem on this factorization explicit.

### Done criterion

- The paper no longer jumps directly from dimension control to chronology; the two-layer window is justified by a factorization theorem.

## 3. Restate the main recurrence over `mu`

### Remaining tasks

- Rewrite the recurrence section in terms of `mu`, not the current `Delta`-based presentation count.
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

## 4. Tighten the lower-bound section inside the extension discipline

### Remaining tasks

- Keep the swap obstruction as the primary explicit binary lower bound.
- Keep clutching as the geometric exact depth-two family.
- Decide whether adjunction remains:
  - a categorical reading of the same binary obstruction, or
  - a genuine internal theorem of the extension language
- If no internal no-reconstruction theorem is added, rewrite the adjunction material so it clearly plays the first role only.

### Done criterion

- The lower bound reads as a theorem inside the chosen extension discipline, not as a loose collection of analogies.

## 5. Add the syntax-backed modality story if modalities remain central examples

### Remaining tasks

- Replace generic prose about "global cohesive modalities" with an actual modal syntax reference point.
- Add the modal DTT / dependent right adjoint references needed if modalities remain prominent.
- If cubical-modal interaction remains a main motivation, add the corresponding cubical-modal citation support and align the prose with it.

### Done criterion

- Modal examples are justified by a concrete extension syntax, not only by intuition.

## 6. Finish the bibliography upgrade

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

- The bibliography supports the strengthened theorem object rather than only the bridge skeleton.

## 7. Align the mechanization section with the final bridge theorem

### Remaining tasks

- Once the strengthened bridge and elimination theorems are written, update `sec:mechanization` to say exactly which parts of:
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

- Higher-coherence elimination is proved over normalized public signatures.
- Recent-history factorization is proved.
- The recurrence is stated and proved over `mu`.
- Fibonacci appears only as the constant-payload corollary.
- The lower bound is clearly internal to the extension discipline.
- The bibliography and mechanization sections match the final claims exactly.

## Suggested Next Execution Order

1. Rework the upper bound into higher-coherence elimination.
2. Upgrade telescopic subsumption into recent-history factorization.
3. Rewrite the recurrence over `mu`.
4. Tighten the lower-bound section and supporting citations.
5. Update the mechanization map to match the completed bridge theorem stack.
