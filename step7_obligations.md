# Step 7 Obligation Analysis: S² (Δ₇ = 13 = 8 + 5)

## The Type

S² is the 2-sphere, the first HIT with a 2-dimensional cell. Its specification consists of 5 structural schemas: formation, base, surf, S²-elim, β-surf.

## Prior Layers

- **L₆ = PropTrunc** (8 schemas): ∥_∥ formation, |_| constructor, squash, coherence-disc, ∥-∥-rec, β-squash, β-coherence, Π-compatibility
- **L₅ = S¹** (5 schemas): S¹ formation, base, loop, S¹-elim, β-loop

## Obligations from L₆ = PropTrunc (8)

PropTrunc is a type operator (∥_∥ : Type → Type). Its 8 schemas instantiate uniformly at any new type. Each of PropTrunc's 8 schemas, applied to S², yields one obligation:

**Obligation 7.1:** ∥S²∥ : Type.
References L₆ because PropTrunc's formation rule (∥_∥ : Type → Type) applied to S² yields a new type. Not reducible to L₅ because this is the truncation operator, which lives at L₆.

**Obligation 7.2:** |_|_{S²} : S² → ∥S²∥.
References L₆ because PropTrunc's constructor (|_| : A → ∥A∥) instantiated at S² gives the inclusion. Not reducible to L₅ because |_| is PropTrunc infrastructure, not S¹-related.

**Obligation 7.3:** squash_{S²} : (x y : ∥S²∥) → x ≡ y.
References L₆ because squash is PropTrunc's path constructor. At ∥S²∥, squash must collapse all paths — including the image of surf under |_|. Not reducible to L₅ because this is a PropTrunc obligation; S¹'s loop is not directly involved.

**Obligation 7.4:** coherence-disc at ∥S²∥ (2-cell filler for squash).
References L₆ because PropTrunc's coherence disc ensures squash paths compose consistently. At ∥S²∥ this is particularly substantive: S² has a non-trivial 2-cell (surf), so the coherence disc must account for how squash interacts with the image of surf in ∥S²∥. Not reducible to L₅ because this is about PropTrunc's own higher coherence, not S¹'s 1-cells.

**Obligation 7.5:** ∥S²∥-rec : isProp B → (S² → B) → ∥S²∥ → B.
References L₆ because ∥-∥-rec is PropTrunc's eliminator. Recursion out of ∥S²∥ must handle S²'s structure (surf must be sent to a trivial 2-cell in B). Not reducible to L₅ because the recursion principle is PropTrunc's, not S¹'s.

**Obligation 7.6:** β-squash at ∥S²∥.
References L₆ because this is the computation rule for PropTrunc's squash instantiated at S². Not reducible to L₅ because β-rules for squash belong to PropTrunc.

**Obligation 7.7:** β-coherence at ∥S²∥.
References L₆ because this is the computation rule for PropTrunc's coherence disc instantiated at S². Not reducible to L₅ because coherence computation is PropTrunc infrastructure.

**Obligation 7.8:** Π-compatibility for ∥S²∥-rec.
References L₆ because PropTrunc's isProp constraint (isProp B = (b₁ b₂ : B) → b₁ ≡ b₂) involves Π, and this must work for type families involving S². Concretely: functions S² → B for propositional B must factor through ∥S²∥. Not reducible to L₅ because this involves PropTrunc's interaction with Π, not S¹.

### Assessment of L₆ obligations

**All 8 are clean.** The 1-1 correspondence is trivial because PropTrunc is parametric: each PropTrunc schema, applied to S², gives a well-defined datum. This correspondence would hold identically for *any* new type — replace S² with any X and you get the same 8 obligations.

**Honest signal:** These 8 obligations are "free" in the sense that PropTrunc's parametricity handles them automatically. They don't represent mathematical work specific to S². The 8 comes from PropTrunc's complexity, not S²'s.

---

## Obligations from L₅ = S¹ (5)

S¹ is not a type operator — it's a specific type. The obligations here are genuine S²↔S¹ interactions.

**Obligation 7.9:** [S², S¹] — the mapping space S² → S¹.
References L₅ because S¹ formation (S¹ : Type) means maps S² → S¹ exist. The specific mathematical content: [S², S¹] ≃ S¹ × K(ℤ,2), and characterizing this requires S²'s 2-cell interacting with S¹'s 1-cell. Not reducible to L₆ because this concerns S¹ as a target, not propositional truncation.

**Obligation 7.10:** const_{base} : S² → S¹ and its characterization.
References L₅ because base : S¹ provides the constant map, and establishing that const_{base}(surf) = refl_{refl_{base}} links S²'s 2-cell to S¹'s point structure. Not reducible to L₆ because this uses S¹'s specific basepoint, not PropTrunc structure.

**Obligation 7.11:** surf ↔ loop interaction (the suspension/Hopf connection).
References L₅ because loop : base ≡ base is S¹'s 1-cell, and the key relationship is ΣS¹ ≃ S² (suspension): S¹'s loop generates S²'s surf via suspension. Equivalently: loop ∈ Ω(S¹) corresponds to a generator of π₂(S²) ≅ ℤ. This is the most substantive obligation — a genuine homotopy-theoretic fact. Not reducible to L₆ because this is about the dimensional interplay between S¹'s 1-cell and S²'s 2-cell.

**Obligation 7.12:** S¹-elim/S²-elim compatibility.
References L₅ because S¹-elim targeting S²-valued families must interact correctly with S²'s structure. Concretely: if f : S¹ → S² is defined via S¹-elim with b : S² and l : b =_{f ∘ loop} b, then composing f with S²-elim must commute. Not reducible to L₆ because this is about S¹'s eliminator, not PropTrunc's.

**Obligation 7.13:** β-loop ↔ β-surf coherence.
References L₅ because S¹'s computation rule (β-loop: apd (S¹-elim b l) loop ≡ l) must hold when the target involves S²-valued types. The computation rules for the two HITs' eliminators must be compatible when composed. Not reducible to L₆ because this concerns S¹'s β-rule, not PropTrunc's.

### Assessment of L₅ obligations

**7.9–7.11 are genuine.** These represent substantive homotopy-theoretic content:
- 7.9 (mapping space [S², S¹]) — real mathematical structure
- 7.10 (constant map characterization) — real but mild
- 7.11 (suspension/Hopf: loop generates surf) — the deepest obligation

**7.12–7.13 are formulaic.** "Elimination and β-rule compatibility" would appear for any two HITs. These aren't S¹-specific — replace S¹ with any HIT X and you'd get "X-elim/S²-elim compatibility" and "β-X/β-surf coherence." They fill the slots but carry less individual content.

**The honest signal:** There are 3 genuinely S¹-specific obligations (7.9, 7.10, 7.11) and 2 generic "HIT compatibility" obligations (7.12, 7.13). The count reaches 5 because S¹ has 5 schemas and each generates a "response" — but 2 of those responses are template compatibility conditions rather than specific mathematical interactions.

---

## Overall Verdict

### Where the 1-1 principle holds cleanly

- **L₆ = PropTrunc (8/8):** Perfect 1-1 correspondence, trivially, by parametric instantiation. This is not a deep fact about S² — it would hold for any type.

### Where the 1-1 principle is strained

- **L₅ = S¹ (5/5):** The totals match, but the mechanism is a *mixture* of:
  - 3 genuine mathematical interactions (mapping space, basepoint map, suspension)
  - 2 template "elimination compatibility" conditions

### The deeper pattern

The 1-1 principle works for two different reasons at the two layers:

1. **Parametric types** (PropTrunc, and earlier: Π, ≡): Every schema gives an automatic obligation by type-level instantiation. The correspondence is trivially 1-1 but mathematically contentless.

2. **Specific types** (S¹): Some schemas give genuine mathematical obligations (formation → mapping space, constructors → specific interactions), while others give generic compatibility obligations (elim → elim compatibility, β → β coherence).

The total works out because: PropTrunc contributes its full count (8) automatically, and S¹ contributes its count (5) through a mix of genuine interactions (3) and template compatibility (2).

### Critical question for the paper

Is the "one obligation per face" principle a *theorem* or a *design choice*?

**For parametric types:** It's close to a theorem — parametric instantiation always gives one datum per schema.

**For specific types:** It's closer to a design choice. There are 3 genuine S²↔S¹ interactions. The principle assigns 5. The extra 2 are real compatibility conditions, but they're not S¹-*specific* — they'd appear for any HIT predecessor. The 1-1 correspondence holds because S¹ happens to have 5 schemas and there happen to be 5 things to check. Whether this is coincidence or consequence depends on whether you believe the Genesis ordering is constrained enough that each predecessor's schema count matches the number of interactions with its successor.

### Recommendation

The decomposition approach is **solid enough for the paper** — the 13 obligations are all real, and the layer tagging is defensible (no obligation is obviously mis-assigned). But the framing should:

1. Acknowledge that the PropTrunc obligations are automatic (parametric instantiation), which accounts for 8 of the 13
2. Present the S¹ obligations at the level of detail given in 7.9–7.13, rather than claiming a clean 1-1 principle
3. State the "one obligation per face" principle as a *regularity observed in the Genesis sequence*, not as a general theorem about type theory

The mechanism is subtler than strict 1-1: it's parametric instantiation (for type operators) + dimensional correspondence (for specific types) + generic HIT compatibility (filling remaining slots). All three effects are real, but they're not the same mechanism.
