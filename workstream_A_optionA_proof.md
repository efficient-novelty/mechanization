# Workstream A Proof Note: Can Option A (`\R` from internal completion in univalent foundations) be made rigorous?

## Executive result
**Yes, conditionally in standard univalent foundations; no, not from the currently selected PEN library state without extra imports.**

More precisely:
1. In HoTT/UF with a natural numbers object (`\mathbb{N}`), one can construct `\mathbb{Z}`, `\mathbb{Q}`, and then `\mathbb{R}` via Cauchy completion or Dedekind cuts.
2. In the PEN trajectory as currently written, Step 5 rejects `\mathbb{N}` from the selected library branch, so Option A is **not derivable at Step 13** unless arithmetic infrastructure is explicitly imported and cost-accounted.

---

## 1) Formal target
We want to justify the metric signature used at Step 13:
\[
  g : TX \otimes_s TX \to \mathbb{R}
\]
with `\mathbb{R}` obtained internally from type theory (Option A), not postulated as an unexplained primitive.

---

## 2) Standard UF construction chain (proof skeleton)

### Theorem A (conditional derivability in UF)
Assume a univalent universe `\mathcal U` with standard type formers and a natural numbers object `\mathbb N` (plus required truncation/set-level infrastructure). Then there exists a type `\mathbb R : \mathcal U` that satisfies the usual Dedekind/Cauchy real-number interface.

### Proof sketch
1. **From `\mathbb N` to `\mathbb Z`:** construct integers as a quotient/completion of pairs of naturals (or equivalent HIT/set-quotient presentation).
2. **From `\mathbb Z` to `\mathbb Q`:** construct rationals as fractions `(a,b)` with `b \neq 0`, modulo the standard equivalence relation.
3. **From `\mathbb Q` to `\mathbb R`:**
   - **Cauchy route:** define Cauchy approximations/sequences over `\mathbb Q`, quotient by vanishing-distance equivalence, then prove completeness.
   - **Dedekind route:** define lower cuts in `\mathbb Q` with inhabitedness, roundedness, and locatedness properties, then equip with order/algebra structure.
4. By univalence, equivalent constructions can be identified up to equivalence in `\mathcal U`; thus `\mathbb R` is a legitimate internal type object with expected algebraic/order structure.

So Option A is mathematically coherent in UF **if arithmetic/countable infrastructure is present**.

---

## 3) Obstruction in the current PEN branch

### Theorem B (branch-local non-derivability at Step 13)
Given the current PEN narrative where `\mathbb N` is rejected at Step 5 and no alternative countable scalar tower is explicitly selected later, `\mathbb R` cannot be obtained internally by the Cauchy/Dedekind route before Step 13.

### Proof sketch
1. Both Cauchy and Dedekind constructions depend on `\mathbb Q`.
2. `\mathbb Q` depends on `\mathbb Z` and divisibility/nonzero denominator data.
3. `\mathbb Z` depends on `\mathbb N` (or an equivalent countable arithmetic basis).
4. If the selected library branch contains none of these objects (and no explicit import rule), then the dependency chain is absent.
5. Therefore, internal construction of `\mathbb R` is blocked.

Hence Option A is not currently proven in the paper’s selected branch; it requires explicit additional assumptions/cost.

---

## 4) What must be added (without editing paper text yet)
To make Option A actually provable in PEN terms, one of the following has to be made explicit:

### Path A1 — Import arithmetic tower explicitly
Add explicit clauses introducing the arithmetic prerequisites (at minimum a natural numbers object and enough structure to construct `\mathbb Q` and complete it).
- Pros: keeps `\mathbb R` derived, not primitive.
- Cost implication: introduce explicit `\kappa` for arithmetic/completion machinery and recompute Step 13 margin.

### Path A2 — Move `\mathbb R` behind a scalar-interface axiom
Axiomatize a scalar field/line object and treat it as imported API with explicit cost.
- Pros: simpler for metric section.
- Cons: this is closer to Option B than strict Option A.

For strict Option A, Path A1 is required.

---

## 5) Minimal “proof package” checklist for strict Option A
If we proceed with strict Option A later, the eventual manuscript proof package should include:
1. A dependency DAG `\mathbb N \to \mathbb Z \to \mathbb Q \to \mathbb R`.
2. Explicit statement of which real construction is used (Cauchy or Dedekind).
3. The exact PEN cost policy for these prerequisites (where `\kappa` is charged).
4. A recomputed Step 13 efficiency sensitivity showing whether metric still clears bar after charging this cost.

---

## 6) Conclusion for Workstream A (current status)
- **Provability verdict:** Option A is mathematically valid in UF, but **not currently justified by the selected PEN branch as written**.
- **Actionable consequence:** to claim Option A in the paper, PEN must explicitly include (and cost) arithmetic/completion prerequisites, or otherwise abandon strict Option A in favor of an explicitly axiomatized scalar object.

This closes the logical gap at the planning/proof-note level without modifying `pen_unified.tex` yet.
