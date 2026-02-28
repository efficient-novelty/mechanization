# PEN Paper Improvement Plan for Peer-Review Hardening

## Goal
Produce a revision package for `pen_unified.tex` that directly closes six anticipated reviewer attack surfaces (A–F), while preserving PEN’s core claims (deterministic selection, mechanical scoring, MBTT-first synthesis) and improving auditability.

## Operating Principles for the Revision
1. **No hidden assumptions:** every imported object (especially `\mathbb{R}`) must have an explicit origin and cost treatment.
2. **Algorithmic transparency over mystique:** distinguish typed search, pruning, and ranking from infeasible brute-force enumeration.
3. **Symmetric scoring policy:** apply the same bundling logic to discrete and geometric candidates, then show why topology still dominates.
4. **Syntax/semantics separation:** keep “physics-like” labels as post-hoc interpretation unless equations are explicitly axiomatized.
5. **Type-theoretic necessity over editorial policy:** frame interface-density requirements as normalization/canonicity constraints.
6. **Grammar neutrality statement:** explain why cohesive/temporal tokens are primitive language constructors and not target leakage.

---

## Workstream A — Resolve the `\mathbb{R}` Dependency Paradox

### Problem to close
Step 13 currently uses `\R` in the metric signature without a fully explicit origin under the empty-library premise.

### Planned edits
1. **Add a dedicated “Number Object Provenance” subsection** near Step 13 and cross-reference it from Appendix B.4:
   - State two admissible formalizations and commit to one in the main narrative:
     - **Option A (derived):** `\R` constructed from internal Cauchy/Dedekind completion in univalent foundations.
     - **Option B (axiomatic smooth line):** `\mathbb{R}_\mathrm{sm}` introduced as cohesive primitive.
2. **Add a formal cost-accounting paragraph**:
   - If derived: specify prerequisite chain (`\N, \Z, \Q`, completion machinery) and where those costs appear in PEN (or why they are disallowed under selected branch).
   - If axiomatic: define a new explicit clause budget item (`\kappa_\R`) and re-evaluate Step 13 margins with/without this add-on.
3. **Insert a “consistency with empty-library premise” note** in the assumptions section:
   - Clarify that no scalar type may be silently imported.
4. **Update rejected/alternative branch discussion**:
   - If arithmetic tower is not chosen by PEN, explain how a scalar field can still appear only through an explicit later axiom, never retroactively as hidden infrastructure.

### Deliverables
- Main-text clarification paragraph.
- Appendix proof-note with explicit dependency graph and `\kappa` implications.
- Updated table row or footnote showing adjusted Step 13 sensitivity.

### Validation checks
- Confirm every occurrence of `\R` is either defined or linked to the provenance subsection.
- Confirm no claim remains that implies “free” access to reals.

---

## Workstream B — Make Search Dynamics Credible (No Brute-Force Illusion)

### Problem to close
Section 7.1 may read like practical enumeration over a near-astronomical MBTT space (e.g., 229-bit candidates), which is not believable if interpreted as blind search.

### Planned edits
1. **Add an explicit search-pipeline diagram/text block** in Section 7.1:
   - Grammar generation → type checking → prerequisite gating → canonical quotienting → budget/horizon filters → scoring.
2. **Quantify pruning stages with measurable counters** (to be reported from engine logs or one reproducibility run):
   - raw syntactic proposals,
   - well-typed survivors,
   - admissible-by-prereq survivors,
   - canonicalized uniques,
   - scored finalists.
3. **Clarify autonomy language**:
   - Replace any wording that implies unrestricted brute-force with wording that accurately describes constrained typed synthesis.
4. **Add an explicit “what is and is not curated” paragraph**:
   - If category templates are human-specified, state that clearly.
   - Emphasize that **selection/scoring** is mechanical even if search grammar is engineered.
5. **Add a transparency appendix note** with a short complexity discussion:
   - Why typed inhabitation + constraints dominate practical complexity over raw bitstring space.

### Deliverables
- Revised Section 7.1 prose with pipeline semantics.
- New table with candidate counts per filtering stage.
- Reproducibility pointer to command(s) generating these counters.

### Validation checks
- Ensure no remaining sentence can be interpreted as naive exhaustive search over `2^{229}`.
- Ensure engine claims are backed by either logs, scripts, or explicit caveats.

---

## Workstream C — Neutralize API Bundling Double-Standard Critique

### Problem to close
Reviewers may view `\N` as aggressively minimized while Metric is generously bundled, creating asymmetric scoring.

### Planned edits
1. **Add a steel-man subsection for `\N` in Rejected Candidates**:
   - Introduce an intentionally generous 7-clause arithmetic API bundle (e.g., multiplication, exponentiation, primality, factorization schema, zeta-like operator).
2. **Run comparative scoring envelopes**:
   - Conservative and generous bounds for `\nu` and `\rho` for this boosted `\N` bundle.
3. **Show topological bottleneck explicitly**:
   - Use `\nu_H = 0` for discrete 0-groupoid and prove linear-growth ceiling versus Fibonacci bar growth.
4. **Add a short proposition/corollary**:
   - “Even under bundled arithmetic API assumptions, purely discrete branches eventually fail by step 8/9.”
5. **Cross-link to Constructive Irreducibility Boundary**:
   - Distinguish derivable operations from genuinely new eliminators uniformly across domains.

### Deliverables
- Expanded rejected-candidate analysis with fair-bundling stress test.
- Optional mini-table: `\N` minimal vs `\N` steel-man bundle vs Metric.

### Validation checks
- Confirm argument demonstrates *topological* reason for failure, not preference bias.
- Confirm policy language does not appear asymmetric across examples.

---

## Workstream D — Preempt “Semantic Pareidolia” Objections

### Problem to close
AST labels like “Ricci scalar” may overstate semantic content when only skeletal signatures are encoded.

### Planned edits
1. **Promote Appendix A.5 caveat into main text** (concise but explicit):
   - PEN discovers efficient typed interface skeletons, not full field equations.
2. **Add “semantic level tags” for key outputs**:
   - e.g., *structural analogy*, *axiomatized equation*, *fully constrained law*.
3. **Annotate Table 12 entries** with an “equationally constrained?” column.
4. **Add a one-paragraph interpretation policy**:
   - Human labels are mnemonics for isomorphism classes of signatures.
   - Physical commitments require extra axioms not currently part of selected AST.

### Deliverables
- Main text disclaimer paragraph.
- Appendix/table annotation to separate syntax from physics semantics.

### Validation checks
- Ensure no statement implies PEN derived contracted Bianchi identity unless actually encoded.
- Ensure every physics-facing label has matching qualification.

---

## Workstream E — Recast Maximal Interface Density as Logic Preservation

### Problem to close
Current framing leans on “library quality policy,” which looks optional rather than logically necessary.

### Planned edits
1. **Rewrite Remark 2.10 motivation** toward metatheory:
   - new constructors/modalities must define eliminator interactions to preserve normalization and prevent stuck terms.
2. **Add a formal “stuck-term” toy counterexample**:
   - show failure mode when an operator is added without elimination/computation interaction clauses.
3. **State a theorem-style claim (or proposition)**:
   - Interface closure is required for canonicity/decidable checking in this framework.
4. **Move physics analogy to secondary status**:
   - keep Equivalence Principle analogy as intuition, not primary justification.

### Deliverables
- Strengthened logical justification in foundational section.
- Optional appendix proof sketch linking interface completeness to normalization behavior.

### Validation checks
- Confirm primary justification is syntactic/metatheoretic necessity.
- Confirm no remaining language treats the rule as merely editorial preference.

---

## Workstream F — Defuse “Teleological Grammar” Concern

### Problem to close
Including cohesive/temporal modality tokens in MBTT grammar can appear to pre-wire the desired conclusion.

### Planned edits
1. **Add grammar-design rationale paragraph near Table 6**:
   - Primitive tokens represent accepted foundational operator families (modal type theory), not target theories.
2. **Clarify distinction between capacity and selection**:
   - Grammar grants expressivity; PEN objective determines which combinations survive.
3. **Add ablation statement (or planned experiment)**:
   - compare runs with reduced modality alphabets and report effects on achievable candidates and efficiency.
4. **Document non-selected alternatives built from same grammar**:
   - show many cohesive/temporal combinations are generated but rejected by bar/efficiency criteria.

### Deliverables
- Revised MBTT grammar commentary.
- Optional ablation appendix/table.

### Validation checks
- Ensure narrative no longer sounds pre-ordained.
- Ensure claim is “operators were available,” not “operators were destined.”

---

## Cross-Cutting Execution Plan

### Phase 1 — Textual architecture and definitions
- Add/relocate the key clarifications (A, D, E, F) in main text before appendix deep dives.
- Introduce consistent terminology: **syntactic skeleton**, **equational content**, **interface closure**, **explicit imported primitives**.

### Phase 2 — Quantitative transparency updates
- Produce search-pipeline counts (B).
- Add fair-bundling stress-test for `\N` (C).
- Recompute any impacted margins if `\kappa_\R` is introduced (A).

### Phase 3 — Consistency and claim calibration
- Audit all “autonomous,” “emergence,” and physics-label statements for overreach.
- Ensure every strong claim has either data, proof, or explicit caveat.

### Phase 4 — Reviewer-facing hardening
- Add a concise “anticipated objections and responses” subsection summarizing A–F in one location.
- Ensure all new claims are cross-referenced to equations/tables/appendices.

---

## Concrete File-Level Edit Targets
- **Primary:** `pen_unified.tex`
  - Sections around Remark 2.10, Section 7.1, Step 13 spec, MBTT grammar, and Appendix A.5/C audit.
- **New helper artifacts (optional but recommended):**
  - `appendix_search_transparency_table.tex` (if modular tables are used).
  - `notes/repro_search_counts.md` capturing command outputs used for Section 7.1 transparency table.

---

## Acceptance Criteria (Definition of Done)
1. `\R` provenance is explicit and cost-accounted; no silent import remains.
2. Search process is transparent enough that no reasonable reader infers brute-force over full bitspace.
3. `\N` receives a fair steel-man bundle and still fails for topology-driven reasons.
4. Main text clearly states AST outputs are structural interfaces, not full physical laws.
5. Maximal Interface Density is justified as a canonicity/normalization requirement.
6. MBTT primitive grammar rationale is explicit, with at least one anti-teleology argument (preferably ablation evidence).
7. All modified claims are internally cross-consistent and tied to tables/appendices.

---

## Suggested Commit Sequence for the Paper Revision (future implementation)
1. `clarify-R-provenance-and-cost-model`
2. `add-search-transparency-and-pruning-metrics`
3. `steelman-N-bundle-and-topology-ceiling`
4. `separate-structural-signatures-from-physics-semantics`
5. `justify-interface-density-via-canonicity`
6. `document-mbtt-grammar-rationale-and-ablation`

This sequence keeps controversial conceptual edits isolated, making peer-review responses and regression checks easier.
