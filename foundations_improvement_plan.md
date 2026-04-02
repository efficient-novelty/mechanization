# Foundations Improvement Plan for `foundations_reality_draft.tex`

## End state

Turn the current manuscript from a concise metaphysical architecture statement into a JGPS-ready philosophy-of-science article that argues for a two-level framework of foundational explanation.

- Target journal shape: full article, roughly 8,500-10,500 words in the main text.
- Governing question: why is a space of coherent possibility still not enough to explain an actual, historically articulated world?
- Central thesis: any adequate foundational account of a determinate and historically articulated world must distinguish two irreducible explanatory roles, constitutive constraint on admissible structure and selective actualization among admissible continuations.
- Working title: `Coherent Possibility and Selective Actualization: A Two-Level Framework for Foundational Explanation`.

## Current manuscript snapshot

- Current file: `foundations_reality_draft.tex`.
- Current approximate length: 5,478 words, so the paper is now past the midpoint toward the expected JGPS full-article scale but still needs substantial expansion.
- Current strengths: clear constitutive/selective distinction, good presentation-independent instinct, strong "argued / proposed / deferred" discipline, useful emphasis on local actualization.
- Current obstacles to submission: manifesto-style rhetoric, theorem/proof presentation for conceptual claims, bibliography still needing fuller integration, no toy model, and no JGPS-facing front matter.

## Non-negotiable editorial shift

Do not revise this as a paper about "the necessary foundations of reality."

Revise it as a paper about foundational explanation in philosophy of science.

- The paper should argue for an explanatory architecture, not announce a final ontology.
- "Exactly two irreducible laws" should become "two irreducible explanatory roles."
- "Theorem", "proof", and "derives" should largely become "thesis", "argument", "constraint", "proposal", and "schematic formulation."
- "Interesting universe" should become "nontrivial, dynamically stable, historically articulated world."
- "Future-opening power" should become "local generative affordance."
- "Local actualization" should be explicitly defined as locality within the present continuation cone, not spatial locality.

## Guardrails

Keep:

- the constitutive/selective distinction;
- the localizability fix;
- the argued / proposed / deferred structure;
- the presentation-independent ambition.

Do not add in this paper:

- full PEN machinery;
- cosmology;
- coherence amplitude / Born-rule material;
- Constructive Idealism;
- natural numbers as a major standalone issue;
- consciousness;
- a worked physics application.

This paper should function as the philosophical gateway paper, not the technical system paper.

## Execution order

### Task 1. Revise the constitutive section into a literature-facing account of constitutive constraint

Objective: keep the strongest current material while tying each requirement to a live philosophical issue.

Actions:

- Retain coherent realizability, equivalence-invariance, canonicity, and local satisfiability.
- Reframe the section title from `The Constitutive Law of coherent possibility` to `Constitutive Constraint` or a close variant.
- Connect each requirement to an explicit problem: coherence, identity, equivalence, executability, and local dischargeability.
- Reduce slogan-driven phrasing and make the section sound like a disciplined account of admissibility conditions.

Done when:

- each constitutive requirement has a clear explanatory role;
- the section is no longer just a polished restatement of the original thesis.

### Task 2. Rewrite the selective section so localizability leads the argument

Objective: make the selective role precise, intelligible, and less vulnerable to charges of hand-waving.

Actions:

- Move localizability to the front and make it the organizing constraint for the section.
- Define commitment, admissibility, local generative affordance, stabilization burden, sufficiency, primeness, and integration in the new vocabulary.
- State explicitly that locality concerns the present continuation cone, not spatial locality and not a denial of historical dependence.
- Reduce the current repetition between the positive statement of the selective law and the later necessity-by-failure section.

Done when:

- the selective role reads as a structured argument rather than a list of intuitions;
- localizability is clearly defended and clearly delimited.

### Task 3. Add a one-page toy branching model

Objective: convert the framework from proclamation into article-grade argument by showing how the pieces work on a small example.

Toy model to implement:

- `a`: incoherent, so not in `A(H)`;
- `b`: admissible but insufficient;
- `c`: admissible, sufficient, and prime;
- `d = b \oplus c`: admissible and sufficient, but non-prime because it is a gratuitous bundle.

Actions:

- Walk through the example in prose and simple notation.
- Show that constitutive constraint excludes `a`.
- Show that selective actualization excludes `b`.
- Show that primeness excludes `d`.
- Conclude with `H' = I(H,c)`.

Done when:

- the toy model clarifies admissibility, valuation, sufficiency, primeness, and integration in one place;
- the section can be read independently as the paper's intuitive core example.

### Task 4. Split novelty from repetition by creating a dedicated "What the framework adds" section

Objective: state the paper's contribution positively after the machinery is on the table.

Actions:

- Explain that the paper is not merely contrasting possibility and actuality in the abstract.
- State that the novelty lies in a claim about the architecture of foundational explanation for a determinate and historically articulated world.
- Contrast the framework with familiar distinctions already discussed in the comparison section.
- Keep this section concise and synthetic rather than introducing new major machinery.

Done when:

- the contribution is explicit, not merely implied;
- the section answers the likely referee question "what is new here beyond terminology?"

### Task 5. Preserve and strengthen the "argued / proposed / deferred" section

Objective: keep one of the current draft's best features while making the limits more explicit.

Actions:

- Retain the tripartite structure but rewrite it in the lower-key explanatory register.
- Put genuinely argued claims under the narrowest defensible headings.
- Move stronger canonical formulations into the `proposed` bucket.
- Use the `deferred` bucket to make clear that exact evaluators, thresholds, probabilistic refinements, cosmological applications, and formal uniqueness claims are out of scope here.

Done when:

- the limits of the paper are unambiguous;
- the manuscript reads as disciplined rather than overextended.

### Task 6. Rewrite the conclusion into a restrained submission-ready ending

Objective: end with a clear result instead of a final metaphysical flourish.

Actions:

- Compress the conclusion.
- Restate the paper's contribution in terms of foundational explanation and the distinction between constitutive admissibility and selective actualization.
- Remove slogan-heavy closing lines unless they genuinely sharpen the thesis.
- Make the final paragraph sound open to later technical development rather than presenting the last word on ontology.

Done when:

- the conclusion is shorter, calmer, and more analytic;
- the final sentence points forward without grandiosity.

### Task 7. Complete the remaining JGPS-facing LaTeX and submission formatting

Objective: make the manuscript operationally ready for journal submission rather than just conceptually revised.

Actions:

- Introduce a submission-ready journal template workflow, ideally using the Springer LaTeX setup already present in the repository.
- Keep the existing abstract, keyword block, and author-year bibliography aligned with the eventual journal template.
- Keep displayed heading levels to three or fewer across the full manuscript.
- Add DOI information where available during the final bibliography cleanup.
- Prepare the declarations and other submission-facing front-matter items once the manuscript text is stable.

Done when:

- the manuscript is sitting in a journal-compatible template with the remaining front matter in place;
- the front matter matches the stated JGPS constraints.

### Task 8. Expand the manuscript to full-article scale without importing the wrong material

Objective: reach JGPS length by deepening the argument, not by stuffing in the larger PEN program.

Recommended word-budget growth:

- introduction plus comparison section: add roughly 2,000-2,500 words;
- constitutive and selective sections: add roughly 1,500-2,000 words through clarification and literature engagement;
- toy model and remaining formal clarification: add roughly 1,000-1,500 words;
- framework-adds and limits sections: add roughly 500-1,000 words.

Actions:

- Grow the paper by argument, comparison, formal clarification, and example.
- Refuse the temptation to pad the paper with technical machinery or downstream applications.
- Use the word budget to improve referee legibility, not to widen scope.

Done when:

- the main text is in the 8,500-10,500 word range;
- the added length directly serves the central argument.

### Task 9. Run a final referee-style audit before submission packaging

Objective: catch the remaining places where the paper still sounds stronger, stranger, or less grounded than it should.

Audit checklist:

- search for `exactly two`, `necessary foundations of reality`, `theorem`, `proof`, `interesting universe`, `future-opening`, `derive`, `brute`, and other high-voltage formulations;
- confirm that every major claim is either argued, proposed, or deferred;
- confirm that no section exceeds three heading levels;
- confirm that the formal schema is minimal and that PEN mechanics have not leaked in;
- confirm that comparison with neighboring frameworks is explicit and fair;
- confirm that the bibliography is substantial and body-integrated.

Done when:

- the paper reads like a submission candidate rather than a concept note;
- no obvious reviewer objection is left unaddressed at the level of framing, structure, or scope.

## Section-by-section surgery map

Use this map while rewriting the current file.

- Current `Introduction: the missing half of reality`:
  rewrite around the admissibility/actuality gap and end with the paper's precise contribution.
- Current `Method and scope`:
  compress and reposition so it supports the introduction rather than dominating the front third of the paper.
- Current `Why reality requires two laws`:
  rename and weaken into the framework section.
- Current `The Constitutive Law of coherent possibility`:
  keep the core content, but literature-situate each requirement.
- Current `The Selective Law: local actualization`:
  keep the core architecture, but reorder around localizability and add the formal schema bridge.
- Current `Why the Selective Law must have this form`:
  fold selected necessity arguments into the rewritten selective section and comparison material; remove repetitive restatement.
- Current `The relation between the two laws`:
  split across the comparison section and the contribution section.
- Current `What is argued, what is proposed, and what is deferred`:
  keep and strengthen.
- Current `Conclusion: reality as coherent form plus actualization`:
  shorten and de-dramatize.

## Suggested working sequence inside the file

Use this order when doing the actual rewrite:

1. retitle the paper and rewrite the abstract;
2. rewrite the introduction;
3. create the new section outline and move existing paragraphs into place;
4. draft the comparison section;
5. revise the framework section;
6. revise the constitutive section;
7. revise the selective section;
8. add the toy model;
9. revise the contribution and limits sections;
10. rebuild the bibliography and convert citations;
11. do the final tone and submission audit.

## Submission-prep checklist

- abstract between 150 and 250 words;
- 4-6 keywords present;
- author-year citation style in place;
- no more than three displayed heading levels;
- Springer LaTeX template or equivalent submission-ready format in use;
- declarations section prepared when the manuscript reaches submission stage;
- bibliography includes DOI data where feasible;
- PDF compiles cleanly after the revision.
