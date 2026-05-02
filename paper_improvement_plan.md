# Paper Improvement Plan: From Interesting to Groundbreaking

**Target paper:** `1_coherence_depth.tex` / `1_coherence_depth.pdf`  
**Target repository:** `efficient-novelty/mechanization`  
**Prepared:** 2026-05-01

## Executive goal

Move the paper from a compelling, carefully scoped theorem proposal to a field-level contribution by eliminating the five likely reviewer doubts:

1. whether raw structural extensions genuinely normalize to the horn-generated obligation language;
2. whether the active-basis contract is mathematically natural or merely assumes the recurrence;
3. whether the fully coupled foundational-core regime is important rather than tailor-made;
4. whether canonical minimality and trace elimination are invariant under admissible presentations;
5. whether the recurrence says something about real extension practice rather than only an idealized worst case.

The strongest revised paper should not claim “all HoTT” or “all Cubical Type Theory.” It should instead establish a sharper and harder-to-dispute result:

> For a precisely specified, independently motivated class of sealed, globally acting foundational extensions over a chosen CCHM-style cubical core, raw structural syntax normalizes into a horn-generated canonical trace interface; the exact cubical metatheory gives obligation depth two; the canonical trace quotient makes higher structural traces derived; and the resulting counted interface law is validated by both mechanization and worked case studies.

That would be much closer to “groundbreaking”: not because it proves a universal law for all libraries, but because it introduces a robust quantitative methodology for coherence debt, proves the central theorem for a nontrivial cubical extension calculus, and explains exactly how ordinary practice deviates from the full-coupling upper envelope.

---

## Current position and main trust boundary

The current paper already has a strong theorem architecture:

- it distinguishes exact obligation depth `d_obl` from minimal-signature depth `d_mu`;
- it separates present obligations, contractible obligations, transparently derived fields, µ-minimal elimination, and absence;
- it gives a depth-two cubical theorem stack, a binary lower bound, and an affine/Fibonacci recurrence;
- it explicitly limits the claim to a fixed fully coupled cubical extension calculus, not arbitrary Cubical Agda libraries or plain Book HoTT.

The repository is also substantially developed. The current Agda tree exposes theorem-facing modules for obligations, interface calculus, refactoring, canonicity density, trace principle, universal recurrence, extensional collapse, Kan subsumption, upper bound, chronological window, exact depth, lower-bound obstruction, 2D-foundations wrappers, and clutching-family witnesses. In particular, `Metatheory/ComputationalReplacement.agda` already formalizes a canonical trace-signature interface and the core replacement result on explicit canonical trace presentations; `Metatheory/KanSubsumption.agda` already provides a horn-extension/structural-obligation surface; and `agda/README.md` describes the current theorem-facing module map.

The main remaining weakness is not the recurrence arithmetic. It is the **end-to-end bridge**:

```text
raw typed structural extension declaration
        ↓ elaboration / sealing / opacity
canonical full tagged normalized trace presentation
        ↓ surface-to-horn image theorem
horn-generated structural obligation language
        ↓ cubical computation / replacement
µ-primitive trace filter and recurrence input
```

The paper currently argues this bridge mathematically, but the repository does not yet mechanize it as a typed raw-calculus-to-canonical-interface theorem. This is the highest-priority gap to close.

---

## What “groundbreaking” should mean here

A revised paper should aim for the following package.

### Minimum publishable strong package

1. A fully formalized **abstract counted-interface theorem stack**:
   - active basis / trace principle / recurrence;
   - exact cubical depth-two wrapper;
   - computational replacement on canonical trace presentations;
   - refactoring invariance.

2. A mechanized or at least theorem-contract-level **surface bridge** for the fixed extension calculus:
   - raw structural grammar;
   - typed/admissible declarations;
   - normalization to canonical trace signatures;
   - image theorem into horn-generated obligations;
   - preservation of support, arity, and primitive-cost tags.

3. A mathematical naturality argument for active-basis coverage, backed by formal examples and nonexamples.

4. At least two worked foundational-core case studies showing that the fully coupled regime captures real phenomena, plus at least one sparse-extension counterexample showing that the scope restriction is principled.

5. A general sparse-dependency recurrence that contains the full-coupling Fibonacci law as a special upper-envelope case.

### Ambitious “groundbreaking” package

Add the following:

1. a small executable checker or Agda DSL that lets a reader write a raw extension declaration and compute/verify its normalized trace footprint;
2. a benchmark/case-study table comparing sparse, partially coupled, and fully coupled extensions;
3. a reusable abstract theorem interface that other cubical systems could instantiate by proving the horn-computational and canonical-trace hypotheses.

---

## Reviewer-objection matrix

| Reviewer question | Current state | Groundbreaking target | Main proof/mechanization work |
|---|---|---|---|
| Does every admissible raw structural extension normalize to the claimed horn-generated form? | Paper-level bridge; abstract horn language is mechanized. | A typed raw extension calculus in Agda with a normalization function and image theorem. | Add raw syntax, admissibility, elaboration target, canonicalization, and `raw-structural-normalizes-to-horn`. |
| Is the active-basis contract natural or does it build in recurrence? | Contract-to-density is formalized abstractly; naturality argued in prose. | Derive active-basis coverage from a separately motivated global-action semantics and show recurrence needs additional independent hypotheses. | Formalize active interfaces as finitely generated signatures and global actions as homomorphism-like structures over basis sites. Add examples/nonexamples. |
| Is the fully coupled regime broad enough to matter? | Examples are described, but not developed enough to silence the “designed calculus” objection. | Worked case studies: universe extension, global modality, promoted generic interface; sparse counterexamples; dependency-graph generalization. | Add case-study modules/docs and sparse recurrence theorem. |
| Are canonical minimality and trace elimination invariant under reasonable presentations? | Refactoring and computational replacement are partially formalized. | A formal admissible-presentation category/setoid, canonical normal form or universal quotient, and invariance theorem for `µ`. | Extend `Refactoring` and `ComputationalReplacement` into a full canonical-telescope/trace-cost normalizer. |
| Does the recurrence describe real practice or only idealized worst-case discipline? | Paper says fully coupled foundational-core regime only. | Present the Fibonacci law as a theorem about the maximal-coupling upper envelope, plus a sparse law for ordinary library practice and empirical/case-study calibration. | Add dependency-hypergraph recurrence and Haskell/Agda annotation workflow for real or synthetic extensions. |

---

# Workstream A: Mechanize the raw-structural-to-horn bridge

## A.1 Strategic answer

The paper should not attempt to mechanize arbitrary Cubical Agda surface syntax. That would be too large and unnecessary. Instead, it should mechanize the **fixed extension calculus** that the paper itself defines.

The claim should become:

> Every admissible raw structural declaration in the paper’s extension calculus normalizes to the horn-generated structural obligation language.

That is enough. It makes the theorem object precise, closes the main trust boundary, and avoids the impossible burden of covering arbitrary Agda programs.

## A.2 New Agda modules

Add these modules under `agda/Metatheory/`:

```text
RawStructuralSyntax.agda
RawStructuralTyping.agda
CanonicalTelescope.agda
SurfaceNormalizationBridge.agda
SurfaceToHornImage.agda
```

Optional supporting modules:

```text
FiniteInterfaceBasis.agda
CanonicalTraceNormalizer.agda
```

## A.3 Raw syntax to formalize

Define a small typed syntax for the paper’s fixed calculus, not a parser for Agda:

```agda
record LayerRef : Type where ...
record BasisSite : Type where ...
record PayloadField : Type where ...
record Boundary : Type where ...

data RawStructuralClause : Type where
  act  : NewPayloadRef → BasisSite → RawStructuralClause
  cmp  : NewPayloadRef → BasisSite → BasisSite → RawStructuralClause
  horn : RawBoundary → RawStructuralClause
```

Then define raw declarations:

```agda
record RawExtension : Type where
  field
    payload      : Telescope PayloadField
    structural   : Telescope RawStructuralClause
    algebraic    : Telescope AlgebraicPayloadField
    exportPolicy : ExportPolicy
```

The crucial design choice is to distinguish:

- structural clauses: `act`, `cmp`, `horn`;
- user algebraic operations: payload or payload-indexed structure;
- public trace fields: what remains after sealing and normalization.

This prevents an arbitrary ternary user operation from being misclassified as a structural integration trace.

## A.4 Typing/admissibility layer

Define `AdmissibleRawExtension` as a record, not as a postulate:

```agda
record AdmissibleRawExtension (B : LibraryState) (e : RawExtension) : Type where
  field
    payloadWellTyped      : PayloadWellTyped B e
    structuralWellTyped   : StructuralClausesWellTyped B e
    algebraicWellTyped    : AlgebraicWellTyped B e
    sealingDerivation     : SealingDerivation B e
    opacityRespected      : OpacityRespected B e
    exportPolicySound     : ExportPolicySound B e
```

This can remain abstract over actual CCHM term syntax. The point is to formalize exactly the paper’s extension-calculus judgments.

## A.5 Normalization function

Define a canonical target:

```agda
record CanonicalNormalizedSignature : Type where
  field
    payloadFields : CanonicalTelescope
    traceFields   : CanonicalTraceSignature ...
```

Then implement:

```agda
normalizeRawExtension :
  (B : LibraryState) →
  (e : RawExtension) →
  AdmissibleRawExtension B e →
  CanonicalNormalizedSignature
```

The normalizer should perform only the quotient operations the paper allows:

- telescope flattening;
- record splitting/rebundling;
- reassociation of Σ-spines;
- currying/uncurrying of Π-telescopes where the sealed judgment is unchanged;
- transport along already exported equalities;
- transparent structural operations;
- deletion or retagging of transparently derived fields.

## A.6 Main theorem statements

The paper’s Theorem 4.9 should be mirrored by an Agda theorem with roughly this shape:

```agda
raw-structural-normalizes-to-horn :
  (B : LibraryState) →
  (e : RawExtension) →
  (adm : AdmissibleRawExtension B e) →
  (φ : RawTraceField e) →
  3 ≤ arity φ →
  Σ[ ∂ ∈ HornBoundary B e ]
    CanonicallyEquivalent
      (normalizeRawTraceField B e adm φ)
      (HornExtensionFiber ∂)
```

Preservation theorem:

```agda
surface-to-horn-preserves-tags :
  ... →
  support normalizedField ≡ support hornField ×
  arity normalizedField ≡ arity hornField ×
  primitiveCost normalizedField ≡ derived
```

Completeness theorem:

```agda
horn-image-complete-for-structural-clauses :
  ... →
  EveryStructuralTraceFieldInCanonicalSignature
    arisesFromRawActCmpOrHorn
```

Non-admissibility theorem for naked higher structural faces:

```agda
naked-higher-structural-face-not-admissible :
  ... →
  Not (AdmissibleRawExtension B e)
```

or, more softly:

```agda
naked-higher-face-classified-as-algebraic-payload :
  ... →
  classifiedAsPayloadOrRejected e
```

## A.7 Proof strategy

Proceed by induction on the raw structural clause.

### `act`

Show it normalizes to a unary trace field. It is primitive unless a transparent derivation already exists.

### `cmp`

Show it normalizes to a binary comparison trace field. It is primitive unless already generated by exported trace data.

### `horn`

Show that the typed boundary determines:

```text
A_∂, φ_∂, u_∂, u0_∂
```

and therefore a `HornExtensionFiber` over the already visible lower boundary.

This should use the existing `Metatheory/KanSubsumption.agda` surface rather than duplicating it.

## A.8 Acceptance criteria

This gap is closed when:

1. `agda --transliterate Metatheory/SurfaceToHornImage.agda` succeeds;
2. the module exports theorem names matching paper Theorem 3.4, Theorem 4.9, Theorem 4.12, and Theorem 4.13;
3. Section 6 of the paper can replace “paper-level bridge” with “mechanized bridge for the fixed raw extension calculus”; 
4. the paper still explicitly says this is not a parser theorem for all Cubical Agda.

## A.9 Paper edits

Add a new subsection before the current bridge discussion:

> The raw extension calculus as a formal syntax

Then state:

- what is formalized;
- what is not formalized;
- why this is the right level of abstraction;
- how arbitrary Agda programs can instantiate the calculus only after a separate elaboration argument.

---

# Workstream B: Make the active-basis contract mathematically natural

## B.1 Strategic answer

The active-basis contract currently risks looking like the recurrence in disguise. The revision should make clear that it is a **global-action totality principle**, not a Fibonacci assumption.

The recurrence requires three independent ingredients:

1. active-basis coverage/minimality;
2. trace export, where discharged obligations become public trace fields;
3. depth/window stabilization.

The active-basis contract supplies only the first ingredient. It does not imply a two-layer window and does not imply Fibonacci growth by itself.

## B.2 Mathematical reframing

Replace rhetoric like “fully coupled means it acts on every active basis site” with a representation theorem:

> A globally acting extension is a structure-preserving action of the new payload on the active interface signature. Since the active interface is generated by basis sites modulo transparent equivalence, giving such an action is equivalent to giving one primitive action/comparison datum on each active basis class, subject to structural coherence.

This makes active-basis coverage analogous to defining a homomorphism by specifying its values on generators.

## B.3 New formal objects

Add or extend:

```text
Metatheory/FiniteInterfaceBasis.agda
Metatheory/GlobalActionSemantics.agda
Metatheory/ActiveBasisContract.agda
```

Core definitions:

```agda
record ActiveInterface : Type where
  field
    fields       : FiniteSet InterfaceField
    transparent : EquivalenceRelation InterfaceField
    basis        : BasisFamily transparent

record GlobalActionPayload (I : ActiveInterface) : Type where
  field
    actionOnObjects : ...
    actionOnPaths   : ...
    advertisedScope : WholeActiveInterface

record ActionTotality (X : GlobalActionPayload I) : Type where
  field
    actsOnEveryBasisSite : ...
```

Then prove:

```agda
basis-action-equivalence :
  GlobalActionOnInterface I X ≃
  ActionOnBasisSites I X
```

and:

```agda
active-basis-contract-from-global-action-totality :
  ActionTotality X → ActiveBasisContract X
```

## B.4 Demonstrate non-circularity

Add a theorem showing that active-basis coverage alone does not yield the recurrence:

```agda
coverage-alone-does-not-fix-window :
  ∃[ model ∈ CountedExtensionModel ]
    ActiveBasisCoverage model ×
    Not (ChronologicalWindowSize model 2)
```

If constructive existential proof is too heavy, add two explicit models:

1. full active-basis coverage with depth-one UIP collapse;
2. full active-basis coverage with a depth-three artificial window.

This demonstrates that the active-basis contract is not smuggling in the depth-two Fibonacci law.

## B.5 Examples and nonexamples

Add a paper table and, where feasible, small formal examples.

### Positive examples

1. **Universe decoding extension**  
   A new code former must specify decoding and interaction with existing codes, transport, and computation principles.

2. **Global modality / dependent right adjoint**  
   A modality advertised as acting on dependent types must specify action on existing type formers and transports.

3. **Promoted generic interface operator**  
   A public generic action over the whole active interface must provide clauses for the active basis.

### Negative examples

1. **Orthogonal datatype addition**  
   `Tree` added next to `Socket` need not supply primitive `Tree-Socket` interaction.

2. **Transparent lemma package**  
   It adds no sealed layer and no active-interface generator.

3. **Local plugin/API extension**  
   It depends only on a small footprint, giving sparse rather than fully coupled recurrence.

## B.6 Acceptance criteria

This gap is closed when a skeptical reader can see:

- active-basis coverage follows from a separately meaningful global-action semantics;
- the contract distinguishes positive and negative examples correctly;
- the recurrence still requires trace export and chronological depth;
- sparse extensions are not forced into the theorem.

## B.7 Paper edits

Add a section titled:

> Why the active-basis contract is not a recurrence assumption

Include:

- a generator/action analogy;
- a theorem dependency diagram;
- examples/nonexamples;
- a statement that the contract is a scope condition, not a growth law.

---

# Workstream C: Show the fully coupled foundational-core regime is important

## C.1 Strategic answer

The paper should stop defending the fully coupled regime only by caveat. It should show that this regime captures an important upper-envelope phenomenon:

> Ordinary library growth is sparse, but foundational-core extensions are often globally advertised. The paper studies the maximal coupling envelope for such extensions, and the sparse case is a principled weakening rather than a counterexample.

## C.2 Add a dependency-graph generalization

Formalize a sparse general law first, then recover the fully coupled theorem as a special case.

Define a dependency footprint:

```agda
record CouplingFootprint (n : Nat) : Type where
  field
    dependsOn : FiniteSubset (PreviousLayers n)
```

Define sparse recurrence:

```agda
µ-sparse-next :
  µ (n + 1) ≡
  sum (λ j → µ j + κ j) (activeFootprint n)
```

Then full coupling is the case:

```agda
activeFootprint n = { n, n - 1 }       -- after cubical window factorization
```

or, before factorization, the whole active interface with depth stabilization reducing it to the last two layers.

This reframing makes the Fibonacci law a theorem about the **complete recent-history footprint**, not a universal law for every library.

## C.3 Add case-study documents

Create:

```text
docs/case_studies/coherence_depth_universe_extension.md
docs/case_studies/coherence_depth_global_modality.md
docs/case_studies/coherence_depth_promoted_interface.md
docs/case_studies/coherence_depth_sparse_datatype.md
```

Each case study should include:

1. payload fields;
2. active interface footprint;
3. unary trace obligations;
4. binary trace obligations;
5. higher horn obligations and their derived status;
6. whether the active-basis contract is satisfied;
7. expected `µ` contribution;
8. whether the full recurrence, sparse recurrence, or no recurrence applies.

## C.4 Formal toy models

Add small Agda toy models under:

```text
agda/CaseStudies/UniverseExtension.agda
agda/CaseStudies/GlobalModality.agda
agda/CaseStudies/PromotedInterface.agda
agda/CaseStudies/SparseDatatype.agda
```

These do not need to encode full CCHM universes or real modal type theory. They should instantiate the abstract counted-interface and active-basis structures with enough detail to show the classification is not arbitrary.

## C.5 Haskell engine support

Add a simple annotation layer in the engine:

```text
engine/src/CoherenceDepth/Footprint.hs
engine/src/CoherenceDepth/Estimate.hs
```

Input schema:

```yaml
layer: GlobalModality
payload: 2
scope: fully-coupled
footprint: [previous, previous-1]
primitive_traces:
  unary: ...
  binary: ...
derived_traces:
  horn: ...
```

Output:

```text
µ_next estimate
sparse/full classification
window-size assumption used
trace of recurrence calculation
```

This makes the recurrence auditable on examples.

## C.6 Acceptance criteria

This gap is closed when the paper can say:

- the fully coupled regime is an important maximal-coupling regime;
- ordinary sparse growth is modeled by a separate sparse law;
- the paper’s Fibonacci theorem is the full-coupling specialization;
- at least three examples and one nonexample are analyzed using the same machinery.

---

# Workstream D: Strengthen canonical minimality and presentation invariance

## D.1 Strategic answer

The phrase “all reasonable presentations” is dangerous unless “reasonable” is formalized. The paper should define a precise class of **admissible presentation equivalences** and prove invariance there.

Do not claim invariance under every conceivable encoding. Claim invariance under:

- canonical telescope isomorphism;
- definitional equality;
- transparent transport;
- record splitting/rebundling;
- Σ reassociation;
- Π currying/uncurrying preserving the sealed judgment;
- deletion of transparently derived fields;
- insertion/removal of transparent aliases;
- duplicate primitive fields within the same transparent-generation class.

## D.2 New formal presentation layer

Extend current `Metatheory/Refactoring.agda` and `Metatheory/ComputationalReplacement.agda` with:

```text
Metatheory/CanonicalTelescope.agda
Metatheory/PresentationEquivalence.agda
Metatheory/TraceCostNormalForm.agda
Metatheory/MuInvariance.agda
```

Core definitions:

```agda
record TelescopePresentation : Type where ...

data PresentationStep : TelescopePresentation → TelescopePresentation → Type where
  reassocΣ       : ...
  splitRecord    : ...
  bundleRecord   : ...
  curryΠ         : ...
  uncurryΠ       : ...
  transportField : ...
  transparentAliasInsert : ...
  transparentAliasDelete : ...
  duplicatePrimitiveDelete : ...

PresentationEquivalent Γ Δ = ReflexiveTransitiveSymmetricClosure PresentationStep Γ Δ
```

Then define canonical trace cost:

```agda
record TaggedTraceField : Type where
  field
    support : HistoricalSupport k
    arity   : Nat
    cost    : PrimitiveCost

µ : CanonicalTracePresentation → Nat
```

## D.3 Prove invariance one generator at a time

For each presentation step, prove:

```agda
µ-preserved-by-step :
  PresentationStep Γ Δ → µ Γ ≡ µ Δ
```

Then derive:

```agda
µ-invariant-under-presentation-equivalence :
  PresentationEquivalent Γ Δ → µ Γ ≡ µ Δ
```

Also prove stronger field correspondence:

```agda
requires-primitive-fields-correspond :
  PresentationEquivalent Γ Δ →
  Iso (RequiresPrimitiveFields Γ) (RequiresPrimitiveFields Δ)
```

## D.4 Essentiality theorem

The paper needs a non-deletion theorem for primitive fields:

```agda
requires-primitive-field-essential :
  (φ : TraceField Γ) →
  cost φ ≡ requiresPrimitive →
  Not (PresentationEquivalent Γ (deleteField Γ φ))
```

This should not be proved by fiat. It should rest on a definition of `requiresPrimitive` as “not generated by the transparent closure.” If full decidability is hard, define `requiresPrimitive` semantically as an essential-field predicate and make the cost tag a theorem rather than a raw label.

Preferred direction:

```agda
TransparentlyGenerated Γ φ : Type
RequiresPrimitive Γ φ = Not (TransparentlyGenerated Γ φ)
```

Then “derived” and “requiresPrimitive” become proof-relevant statuses, not arbitrary tags.

## D.5 Worked refactoring tests

Add test modules:

```text
agda/Test/PresentationInvariance/RebundleRecord.agda
agda/Test/PresentationInvariance/SplitShell.agda
agda/Test/PresentationInvariance/CurryUncurry.agda
agda/Test/PresentationInvariance/TransparentAlias.agda
agda/Test/PresentationInvariance/DuplicateTrace.agda
```

Each test should instantiate two presentations and prove equal `µ`.

## D.6 Acceptance criteria

This gap is closed when:

1. the paper defines “admissible presentation equivalence” precisely;
2. `µ` is proved invariant under that equivalence;
3. higher horn trace elimination is proved as a special case of transparent derivability;
4. primitive field non-deletion is stated and proved for the same equivalence relation;
5. there are explicit tests for the common reviewer worries: rebundling, splitting, currying, aliases, duplicates.

---

# Workstream E: Connect the recurrence to real extension practice

## E.1 Strategic answer

The recurrence should be presented as a theorem about the **maximal coupling envelope of foundational-core extension**, not a universal empirical law of all proof-assistant libraries.

To make it important rather than merely idealized, add two things:

1. a sparse generalization that models ordinary practice;
2. a small evidence suite showing when real-looking extensions land near the full-coupling envelope.

## E.2 Sparse/full recurrence hierarchy

State three levels:

### Level 0: transparent growth

Transparent definitions do not change the active interface and contribute zero integration latency.

### Level 1: sparse sealed extensions

A sealed layer has a finite dependency footprint `F_n`:


default sparse law:

```text
µ_{n+1} = Σ_{j ∈ F_n} (µ_j + κ_j)
```

modulo whatever depth/window theorem applies to that footprint.

### Level 2: fully coupled foundational-core extensions

The footprint is the whole active interface, and the cubical depth/window theorem reduces primitive dependence to the previous two layers:

```text
µ_{n+1} = µ_n + µ_{n-1} + κ_n + κ_{n-1}
```

under constant payload:

```text
µ_{n+1} = µ_n + µ_{n-1} + 2c.
```

This hierarchy gives the paper a convincing answer: the recurrence is not all practice, but the full-coupling endpoint of a broader formal model of practice.

## E.3 Instrumentation plan

Add a small data format for extension events:

```yaml
name: AddGlobalModality
kind: foundational-core
sealed: true
payload_count: 2
active_basis:
  - UniverseCode
  - Decode
  - Transport
  - FunctionCode
trace:
  unary: 4
  binary: 3
  derived_horn: 2
footprint:
  mode: full
  chronological_window: 2
```

Add a script:

```text
scripts/coherence_depth_audit.py
```

or Haskell executable:

```text
engine/app/coherence-depth-audit/Main.hs
```

Outputs:

- `κ_n`, `µ_n`, `κ_n + µ_n` per layer;
- sparse vs full classification;
- whether the recurrence is exact, an upper bound, or not applicable;
- explanation of derived higher traces.

## E.4 Evidence suite

Add curated examples:

```text
docs/reports/coherence_depth_case_study_report.md
runs/coherence_depth_case_studies/*.yaml
```

Minimum examples:

1. **Transparent lemma extension**: zero cost.
2. **Sparse datatype extension**: small finite footprint, no full recurrence.
3. **Promoted interface package**: active-basis coverage applies.
4. **Global modality/universe extension toy model**: full-coupling recurrence applies.
5. **Refactored presentation of the same extension**: same `µ`.

## E.5 Paper framing

Add a paragraph:

> The Fibonacci law is not proposed as a descriptive law of arbitrary repository growth. It is the exact law of the fully coupled endpoint of a more general sparse dependency calculus. Ordinary libraries usually live below this envelope. Foundational-core extensions, however, often advertise global action, and the paper’s theorem identifies the cost law in that maximal-coupling regime.

This phrasing is much more defensible and more interesting.

## E.6 Acceptance criteria

This gap is closed when:

- the sparse law is stated and preferably formalized;
- the full law is clearly a specialization;
- there is at least one sparse counterexample and one full-coupling example;
- the recurrence is no longer rhetorically vulnerable to “real libraries do not grow like Fibonacci.”

---

# Integrated proof and mechanization roadmap

## Phase 0: Repository audit and theorem map cleanup

**Goal:** make the current state auditable before adding new machinery.

Tasks:

1. Update the root `paper_improvement_plan.md` with this plan.
2. Add a `docs/coherence_depth_trust_boundary.md` file listing:
   - formalized theorem;
   - module path;
   - paper theorem number;
   - whether it depends on the raw-surface bridge.
3. Add a search/audit script that checks for `postulate` in theorem-facing modules.
4. Add a theorem-name index in `agda/README.md` or `docs/theorem_index.md`.

Done when:

- a reviewer can tell exactly what is formalized and what remains bridge-level.

## Phase 1: Raw syntax and canonical presentation bridge

**Goal:** close the highest-value gap.

Deliverables:

```text
agda/Metatheory/RawStructuralSyntax.agda
agda/Metatheory/RawStructuralTyping.agda
agda/Metatheory/SurfaceNormalizationBridge.agda
agda/Metatheory/SurfaceToHornImage.agda
agda/Test/SurfaceBridgeSmoke.agda
```

Key theorem names:

```agda
raw-extension-elaborates-to-candidate
raw-trace-normalizes-to-canonical-signature
surface-to-horn-normal-form
surface-to-horn-preserves-support
surface-to-horn-preserves-primitive-cost
higher-raw-structural-traces-derived
```

Paper payoff:

- The current paper-level bridge becomes a mechanized theorem for the fixed extension calculus.

## Phase 2: Active-basis naturality

**Goal:** show active-basis coverage is natural and non-circular.

Deliverables:

```text
agda/Metatheory/FiniteInterfaceBasis.agda
agda/Metatheory/GlobalActionSemantics.agda
agda/Metatheory/ActiveBasisContract.agda
agda/Test/ActiveBasisExamples.agda
```

Key theorem names:

```agda
basis-families-exist
basis-family-cardinality-invariant
global-action-totality-implies-active-basis-contract
active-basis-contract-entails-density
coverage-alone-does-not-imply-depth-two-window
```

Paper payoff:

- The active-basis contract becomes a theorem-backed semantic condition rather than a suspicious assumption.

## Phase 3: Presentation invariance and µ-minimality

**Goal:** make `µ` robust.

Deliverables:

```text
agda/Metatheory/CanonicalTelescope.agda
agda/Metatheory/PresentationEquivalence.agda
agda/Metatheory/TraceCostNormalForm.agda
agda/Metatheory/MuInvariance.agda
agda/Test/PresentationInvariance/*.agda
```

Key theorem names:

```agda
presentation-equivalence-preserves-trace-fields
presentation-equivalence-preserves-primitive-cost
mu-invariant-under-presentation-equivalence
derived-field-deletion-preserves-presentation
requires-primitive-field-essential
```

Paper payoff:

- The phrase “presentation-invariant minimal opaque cost” becomes formally convincing.

## Phase 4: Sparse dependency generalization

**Goal:** connect idealized recurrence to ordinary practice.

Deliverables:

```text
agda/Metatheory/SparseDependencyRecurrence.agda
agda/Metatheory/FullCouplingEnvelope.agda
agda/Test/SparseRecurrenceSmoke.agda
```

Key theorem names:

```agda
sparse-windowed-recurrence
full-coupling-specializes-sparse-recurrence
transparent-growth-zero-footprint
orthogonal-extension-below-full-envelope
```

Paper payoff:

- The Fibonacci theorem becomes a special case of a broader and more realistic theory.

## Phase 5: Case studies

**Goal:** show the fully coupled regime is important.

Deliverables:

```text
docs/case_studies/coherence_depth_universe_extension.md
docs/case_studies/coherence_depth_global_modality.md
docs/case_studies/coherence_depth_promoted_interface.md
docs/case_studies/coherence_depth_sparse_datatype.md
agda/CaseStudies/*.agda
runs/coherence_depth_case_studies/*.yaml
```

Case-study acceptance checklist:

- payload explicitly counted;
- trace obligations explicitly listed;
- active-basis footprint stated;
- higher horn traces classified as derived;
- recurrence applicability stated;
- sparse/full status stated.

Paper payoff:

- The paper looks less like a self-contained invented calculus and more like a reusable analysis of foundational extension patterns.

## Phase 6: Paper rewrite

**Goal:** align prose with the stronger artifact.

Edit targets:

```text
1_coherence_depth.tex
coherence_depth_refs.bib
README.md
agda/README.md
docs/coherence_depth_trust_boundary.md
```

Major paper edits:

1. **Abstract**  
   State the theorem object precisely and mention the now-mechanized fixed-calculus bridge.

2. **Introduction**  
   Add the sparse/full distinction early.

3. **Section 2**  
   Recast active-basis as global-action totality over finite generated interfaces.

4. **Section 3**  
   Replace bridge prose with formal raw syntax and mechanized theorem statements.

5. **Section 4**  
   Keep the depth theorem but explicitly route every surface claim through the new bridge theorem.

6. **Section 5**  
   Present sparse recurrence first, full-coupling Fibonacci second.

7. **Section 6**  
   Update the mechanization audit table: no vague “paper-level bridge” if Phase 1 is complete; otherwise state exactly which bridge components remain.

8. **Conclusion**  
   Emphasize the methodological contribution: quantitative coherence debt for sealed extensions, with full coupling as an exact upper-envelope law.

---

# Detailed responses to the five reviewer questions

## 1. Does every admissible raw structural extension really normalize into the claimed horn-generated form?

### Current likely reviewer concern

The paper’s grammar says structural clauses are `act`, `cmp`, or `horn`, and it argues that higher structural obligations normalize to based horn-extension types. A reviewer may suspect that the result is built into the syntax: if the only higher structural constructor is `horn`, then the theorem may be tautological.

### How to close it

Make the theorem nontrivial by formalizing three distinctions:

1. **Raw surface clause**: what the extension author writes.
2. **Typed structural role**: whether the clause is structural integration trace or payload/algebraic structure.
3. **Canonical normalized trace field**: what remains after sealing and normalization.

Then prove:

- every typed structural higher clause normalizes to a horn-extension package;
- arbitrary higher user operations are classified as payload, not structural trace;
- naked higher remote faces are either rejected or transparently packaged with their filler;
- the normalizer preserves support, arity, and cost tags.

### Mechanization target

`SurfaceToHornImage.agda` should export:

```agda
surface-to-horn-normal-form : ...
higher-structural-fields-derived : ...
raw-syntax-no-naked-higher-structural-projections : ...
```

### Paper target

State explicitly:

> The theorem is not that arbitrary higher operations in a candidate vanish. It is that structural integration clauses of the fixed extension calculus have a normalized horn image. Higher algebraic operations are payload, not trace.

That sentence should appear near the theorem and in the abstract/contribution list.

---

## 2. Is the active-basis contract mathematically natural, or does it build in the recurrence?

### Current likely reviewer concern

If every new layer is required to interact with every active basis site, then the recurrence may look assumed rather than derived.

### How to close it

Prove that active-basis coverage is the generator-level form of global action:

- active interface = finite generated signature modulo transparent equivalence;
- globally acting extension = operation advertised on the whole active interface;
- total global action = action on all generators;
- basis-minimality = no duplicate primitive witnesses per transparent-generation class.

Then separate the dependency chain:

```text
active-basis contract  →  density
trace export           →  previous obligations become public trace
cubical depth/window   →  only previous two layers remain primitive
all three              →  affine/Fibonacci recurrence
```

Add a countermodel or example showing active-basis coverage without depth-two recurrence.

### Mechanization target

`ActiveBasisContract.agda` should export:

```agda
global-action-totality-implies-active-basis-contract
active-basis-contract-entails-density
coverage-alone-does-not-imply-fibonacci
```

### Paper target

Add a theorem dependency diagram and a subsection titled:

> The active-basis contract is not the recurrence

---

## 3. Is the fully coupled foundational-core regime broad enough to be important?

### Current likely reviewer concern

The result may be true only for an artificial calculus designed to produce a Fibonacci recurrence.

### How to close it

Broaden the surrounding theory without weakening the theorem:

1. Define a sparse dependency calculus.
2. Show ordinary library growth lives there.
3. Show fully coupled foundational-core extensions are the maximal-coupling endpoint.
4. Analyze real-looking examples in both categories.

The paper should say:

> Full coupling is not all library growth. It is the regime of globally advertised foundational actions. The theorem identifies the exact cost law for that regime and the upper envelope of the sparse theory.

### Mechanization target

`SparseDependencyRecurrence.agda` and `FullCouplingEnvelope.agda` should export:

```agda
sparse-windowed-recurrence
full-coupling-envelope
orthogonal-extension-zero-or-sparse
```

### Paper target

Add a case-study table:

| Extension | Scope | Contract? | Window? | Law |
|---|---|---:|---:|---|
| Transparent lemma | none | no | none | zero latency |
| Orthogonal datatype | sparse | no full contract | local | sparse/DAG |
| Global modality | full | yes | 2 | affine Fibonacci |
| Universe code former | full | yes | 2 | affine Fibonacci |
| Promoted generic interface | full or partial | depends | 1/2/sparse | classified case by case |

---

## 4. Are canonical minimality and trace-elimination claims invariant under all reasonable presentations?

### Current likely reviewer concern

A clever presentation might hide or expose primitive trace fields differently, undermining `µ`.

### How to close it

Do not rely on informal “reasonable.” Define the admissible presentation groupoid/setoid and prove invariance under its generators.

Then define primitive cost by transparent derivability rather than arbitrary tagging:

```text
cost = derived             iff transparently generated from lower trace data
cost = requiresPrimitive   iff not transparently generated
```

Prove:

- derived fields can be deleted or retained as aliases without changing `µ`;
- requires-primitive fields cannot be deleted under admissible equivalence;
- refactoring steps preserve support and arity;
- computational replacement is a presentation-equivalence step.

### Mechanization target

`MuInvariance.agda` should export:

```agda
mu-invariant-under-presentation-equivalence
derived-field-deletion-preserves-mu
requires-primitive-field-essential
computational-replacement-preserves-mu
```

### Paper target

Replace any broad phrase like “all reasonable presentations” with:

> all presentations related by the admissible presentation equivalence generated by ...

Then list the generators.

---

## 5. Does the recurrence describe real extension practice, or only an idealized worst-case discipline?

### Current likely reviewer concern

Most proof-assistant libraries are sparse dependency DAGs, not monolithic full-coupling sequences.

### How to close it

Agree, then make that part of the theory.

The paper should present a hierarchy:

```text
transparent code      → zero latency
sparse sealed code    → sparse footprint recurrence / DAG law
fully coupled core    → depth-two affine recurrence
constant payload      → shifted Fibonacci
```

Then add case-study evidence and a small audit tool.

### Mechanization target

`SparseDependencyRecurrence.agda` should be enough for the formal law. The Haskell/Python audit tool can support examples but does not need to be part of the theorem stack.

### Paper target

State explicitly:

> The recurrence is not a descriptive law of arbitrary repository growth. It is the exact law of the fully coupled foundational-core envelope. Ordinary practice usually lies below that envelope.

This turns a weakness into a strength.

---

# Suggested theorem dependency diagram for the revised paper

Use this diagram, or something equivalent, in the introduction or mechanization section:

```text
Raw extension calculus
        │
        ▼
Surface normalization bridge
        │
        ├── payload / trace decomposition
        ├── support and arity preservation
        └── higher structural trace ↦ horn-extension fiber
        │
        ▼
Cubical horn-computational package
        │
        ├── exact stabilization O^(k) ≃ O^(2)
        ├── higher trace fields derived
        └── recent-history factorization
        │
        ▼
Canonical trace quotient and µ-invariance
        │
        ▼
Windowed counted-interface recurrence
        │
        ├── sparse footprint law
        └── full-coupling affine/Fibonacci law
```

This makes clear that the recurrence is the last step, not an axiom.

---

# Concrete repository file checklist

## Add

```text
agda/Metatheory/RawStructuralSyntax.agda
agda/Metatheory/RawStructuralTyping.agda
agda/Metatheory/CanonicalTelescope.agda
agda/Metatheory/SurfaceNormalizationBridge.agda
agda/Metatheory/SurfaceToHornImage.agda
agda/Metatheory/FiniteInterfaceBasis.agda
agda/Metatheory/GlobalActionSemantics.agda
agda/Metatheory/ActiveBasisContract.agda
agda/Metatheory/PresentationEquivalence.agda
agda/Metatheory/TraceCostNormalForm.agda
agda/Metatheory/MuInvariance.agda
agda/Metatheory/SparseDependencyRecurrence.agda
agda/Metatheory/FullCouplingEnvelope.agda
agda/CaseStudies/UniverseExtension.agda
agda/CaseStudies/GlobalModality.agda
agda/CaseStudies/PromotedInterface.agda
agda/CaseStudies/SparseDatatype.agda
agda/Test/SurfaceBridgeSmoke.agda
agda/Test/ActiveBasisExamples.agda
agda/Test/SparseRecurrenceSmoke.agda
agda/Test/PresentationInvariance/RebundleRecord.agda
agda/Test/PresentationInvariance/SplitShell.agda
agda/Test/PresentationInvariance/CurryUncurry.agda
agda/Test/PresentationInvariance/TransparentAlias.agda
agda/Test/PresentationInvariance/DuplicateTrace.agda
docs/coherence_depth_trust_boundary.md
docs/theorem_index.md
docs/case_studies/coherence_depth_universe_extension.md
docs/case_studies/coherence_depth_global_modality.md
docs/case_studies/coherence_depth_promoted_interface.md
docs/case_studies/coherence_depth_sparse_datatype.md
docs/reports/coherence_depth_case_study_report.md
scripts/coherence_depth_audit.py
runs/coherence_depth_case_studies/*.yaml
```

## Update

```text
1_coherence_depth.tex
paper_improvement_plan.md
agda/PEN.agda
agda/README.md
agda/progress_tracking.md
README.md
coherence_depth_mechanization_progress.md
```

---

# Verification commands

After implementation, add these to `agda/README.md` and CI where feasible:

```bash
cd agda
agda --transliterate PEN.agda
agda --transliterate Test/MetatheorySmoke.agda
agda --transliterate Test/SurfaceBridgeSmoke.agda
agda --transliterate Test/ActiveBasisExamples.agda
agda --transliterate Test/SparseRecurrenceSmoke.agda
agda --transliterate Test/ClutchingSmoke.agda
agda --transliterate Test/Fibonacci.agda
```

Add a root script:

```bash
./scripts/check_coherence_depth_artifact.sh
```

That script should:

1. run the Agda checks;
2. grep theorem-facing modules for `postulate`;
3. run the case-study audit tool;
4. compile `1_coherence_depth.tex` twice.

---

# Paper-level restructuring plan

## Revised abstract target

The abstract should say, in restrained form:

- the theorem object is a fixed cubical extension calculus;
- raw structural syntax is normalized into canonical horn-generated trace form;
- exact obligation depth is two for the cubical calculus and one under UIP;
- minimal-signature depth is handled separately through canonical trace replacement;
- the recurrence is a full-coupling envelope, with sparse growth handled separately;
- the mechanization covers the fixed-calculus bridge and theorem stack, not arbitrary Agda syntax.

## Revised introduction target

The introduction should answer three questions early:

1. What practical phenomenon is being modeled?  
   Sealed globally acting foundational extensions.

2. What is not being modeled?  
   Ordinary transparent or sparse library growth.

3. Why is the result important anyway?  
   It gives the exact maximal-coupling law and a general method for measuring coherence debt.

## Revised mechanization section target

Replace a long theorem-name inventory with a layered table:

| Layer | Claim | Module | Status |
|---|---|---|---|
| Raw syntax bridge | structural clauses normalize to horn form | `SurfaceToHornImage.agda` | formalized for fixed calculus |
| Canonical trace | µ invariant under presentation equivalence | `MuInvariance.agda` | formalized |
| Cubical upper bound | `O^(k) ≃ O^(2)` | `UpperBound.agda` | formalized |
| Lower bound | binary obstruction | `AdjunctionBarrier.agda`, `Clutching.agda` | formalized |
| Recurrence | sparse and full-coupling laws | `SparseDependencyRecurrence.agda`, `UniversalRecurrence.agda` | formalized |
| Case studies | examples/nonexamples | `CaseStudies/*`, `docs/case_studies/*` | checked / documented |

## Revised conclusion target

The conclusion should avoid claiming a universal law of mathematical growth. Stronger and safer:

> The paper identifies an exact quantitative law for the fully coupled endpoint of sealed foundational extension in a cubical setting, together with a sparse calculus explaining why ordinary libraries need not follow the same law. The result is a method for measuring coherence debt, not a slogan about Fibonacci growth everywhere.

---

# Risk register

## Risk 1: The raw bridge becomes too large

Mitigation:

- formalize only the paper’s raw extension calculus;
- explicitly state that arbitrary Agda elaboration is outside scope;
- add examples showing how real-looking declarations map into the calculus.

## Risk 2: Active-basis naturality remains philosophical

Mitigation:

- prove generator/action equivalence for finite active interfaces;
- add positive and negative examples;
- prove coverage alone does not imply the recurrence.

## Risk 3: Presentation invariance requires too much normalization machinery

Mitigation:

- define presentation equivalence by generators rather than implementing a complete normalizer;
- prove `µ` invariance by induction over equivalence steps;
- keep “canonical” as a theorem about quotient representatives, not as a full algorithm unless necessary.

## Risk 4: Case studies are dismissed as toys

Mitigation:

- label them honestly as schematic foundational-core instances;
- include one or two literature-facing examples in prose;
- show both examples and nonexamples;
- avoid claiming empirical validation beyond the evidence provided.

## Risk 5: The paper becomes overlong

Mitigation:

- move detailed case-study calculations to docs or appendix;
- keep the main paper focused on theorem architecture and one representative example;
- include a compact table for the remaining examples.

---

# Prioritized execution if time is limited

If only one major gap can be closed, close **Workstream A**. The surface-to-horn bridge is the most important trust boundary.

If two can be closed, close **A + D**. Together they make the µ-minimal trace theorem much harder to dispute.

If three can be closed, close **A + D + B**. That establishes the raw syntax, canonical quotient, and active-basis naturality.

If the goal is “groundbreaking,” close **A + B + C + D + E** and add the case studies. The paper will then offer not just a theorem, but a reusable program for measuring coherence debt.

---

# Definition of done

The paper has moved from “interesting” to “groundbreaking candidate” when all of the following are true:

1. The raw extension calculus is formalized.
2. The surface-to-horn image theorem is mechanized for that calculus.
3. Active-basis coverage is derived from a global-action semantics over finite generated interfaces.
4. The fully coupled regime is presented as the maximal endpoint of a sparse dependency theory.
5. `µ` is invariant under an explicitly generated presentation equivalence.
6. Higher structural trace elimination is a theorem about transparent derivability, not a tag convention.
7. The recurrence theorem is presented both in sparse form and full-coupling affine/Fibonacci form.
8. The paper contains worked examples and nonexamples.
9. Section 6’s mechanization table has no ambiguous “paper-level” rows for the main theorem object; any remaining informal bridge is explicitly outside the claimed theorem.
10. The final abstract and conclusion no longer sound like “Fibonacci appears in cubical type theory generally,” but like the sharper and stronger claim: “sealed fully coupled cubical extension has exact depth-two coherence debt, and this produces a quantified maximal-coupling law.”

