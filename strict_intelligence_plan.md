# Honest Strict Lane: Molecular Intelligence Plan

Our goal is still the same:

- in `RunAbInitio`, under `--strict`
- with no seeds, no target labels, no canonical-name steering
- rediscover the PEN sequence with the correct `nu`, `kappa`, and `rho`
- inside explicit machine-time budgets

The target trajectory remains:

\begin{table}[tbp]
\centering
\caption{The PEN Synthesis Trajectory.  $\nu$ is the Generative Capacity (count of atomic inference rules added to the derivation logic).  Every quantity is computable from the five axioms of \S\ref{sec:framework}; the $\nu$ values are mechanically verified by the Haskell synthesis engine's structural AST analysis (\S\ref{sec:inference-nu}).  The DCT analysis in \S\ref{sec:decomposition} consolidates the terminal-step novelty accounting and structural properties, with topological sensitivity summarized in \cref{rem:nuH-sensitivity}.  Structure names are post-hoc human semantic labels for anonymous selected ASTs.}
\label{tab:genesis}
\small
\begin{tabular}{@{}cr l rrrr rrr@{}}
\toprule
$n$ & $\tau$ & Structure & $\Delta_n$ & $\nu$ & $\kappa$ & $\rho$ & $\Phi_n$ & $\Omega_{n-1}$ & Bar \\
\midrule
1  & 1    & Universe $\U_0$              & 1   & 1   & 2 & 0.50  & ---  & ---  & ---  \\
2  & 2    & Unit type $\mathbf{1}$       & 1   & 1   & 1 & 1.00  & 1.00 & 0.50 & 0.50 \\
3  & 4    & Witness $\star : \mathbf{1}$ & 2   & 2   & 1 & 2.00  & 2.00 & 0.67 & 1.33 \\
4  & 7    & $\Pi$/$\Sigma$ types         & 3   & 5   & 3 & 1.67  & 1.50 & 1.00 & 1.50 \\
\midrule
5  & 12   & Circle $S^1$                 & 5   & 7   & 3 & 2.33  & 1.67 & 1.29 & 2.14 \\
6  & 20   & Propositional truncation     & 8   & 8   & 3 & 2.67  & 1.60 & 1.60 & 2.56 \\
7  & 33   & Sphere $S^2$                 & 13  & 10  & 3 & 3.33  & 1.62 & 1.85 & 3.00 \\
8  & 54   & H-space $S^3$                & 21  & 18  & 5 & 3.60  & 1.62 & 2.12 & 3.43 \\
9  & 88   & Hopf fibration               & 34  & 17  & 4 & 4.25  & 1.62 & 2.48 & 4.01 \\
\midrule
10 & 143  & Cohesion                     & 55  & 19  & 4 & 4.75  & 1.62 & 2.76 & 4.46 \\
11 & 232  & Connections                  & 89  & 26  & 5 & 5.20  & 1.62 & 3.03 & 4.91 \\
12 & 376  & Curvature tensors            & 144 & 34  & 6 & 5.67  & 1.62 & 3.35 & 5.42 \\
13 & 609  & Metric + frame bundle        & 233 & 46  & 7 & 6.57  & 1.62 & 3.70 & 5.99 \\
14 & 986  & Hilbert functional           & 377 & 62  & 9 & 6.89  & 1.62 & 4.13 & 6.68 \\
\midrule
15 & 1596 & Dynamical Cohesive Topos     & 610 & 103 & 8 & 12.88 & 1.62 & 4.57 & 7.40 \\
\bottomrule
\end{tabular}
\end{table}

---

## 1. Strategy Shift

The strict plan has changed.

The old plan assumed that foundational discovery could be done by atomic typed AST search plus a smarter prefix beam. The current experiments show the exact boundary where that assumption fails.

The new plan is:

- keep the current strict evaluator
- keep the SCC semantic minimality filter
- keep the honest final selector
- but replace atomic foundational generation with molecular signature generation

In short:

- from atomic concatenation
- to molecular instantiation

The generator should stop trying to guess foundational eliminators node-by-node and instead enumerate mathematically valid signatures whose eliminators are categorically forced.

---

## 2. What We Learned

Current strict experiments established three facts.

### 2.1 Atomic MBTT search is physically bounded but semantically wrong

At `astDepth = 3`, the exact-band strict run is fast enough to prove the runtime envelope is not the main problem.

But the retained frontier is still dominated by generic macro junk.

The measured failure mode is:

- step 4 completes within budget
- step 4 still selects wrong `nu = 6`, `kappa = 3` junk
- the retained frontier still contains no `Pi`
- step 5 still finds nothing

So the issue is no longer:

- time
- raw bit budget
- raw AST depth
- final ranking

It is representational.

### 2.2 Atomic search hits a semantic noise wall

At the raw AST level, almost every typed term at this budget is a meaningless macro:

- `\f \x \y . f y x`
- variable-swapping binders
- shallow self-application skeletons
- generic universe shells

The true dependent eliminator for `Pi` is a deep, sparse, highly structured object. It cannot compete fairly against the combinatorics of generic macros inside a 768-state beam.

The beam cannot reliably tell the difference between:

- a meaningless but well-typed macro
- a profound motive-driven eliminator

until the telescope is almost complete.

By then the true package has already been deleted.

### 2.3 Atomic eliminator guessing contradicts the paper

This is not only an engineering problem. It conflicts with the theory.

`pen_paper.tex` and `pen_paper.pdf` rely on Lemma 4.12, the Adjoint Completion Principle:

- introduction structure categorically forces corresponding elimination/computation structure

That means:

- forcing the generator to blindly guess eliminator syntax node-by-node
- while the theory says those eliminators are determined by the framework

is the wrong search model.

The discovery layer should search over signatures, not over raw elimination AST atoms.

---

## 3. Non-Negotiable Constraints

The strategy shift must still remain:

- paper-independent
- seed-free
- representative-free
- canonical-name-neutral
- auditable
- explicitly bounded

Still forbidden:

- exact reference telescope seeding
- family seed pools
- canonical-name ranking
- target-conditioned scoring
- hand-authored target rescue

Still allowed:

- structural admissibility
- signature-level proof-theoretic generation
- deterministic adjoint completion
- structural evaluation
- SCC semantic minimality
- name-free final ranking

New allowed intelligence:

- molecular signature enumeration
- deterministic compiler expansion from signatures to MBTT telescopes
- conceptual `kappa` cost at the molecule level

---

## 4. The New Paradigm: Molecular Synthesis

Mathematicians do not discover `Pi`, `Sigma`, or `S^1` by throwing `Lam`, `App`, and `Var` atoms into a void.

They define a signature.

The type theory then determines the induced rules.

The strict engine should do the same.

### 4.1 Core idea

Replace the primary strict foundational generator with:

- a molecular signature enumerator
- a deterministic molecule compiler
- then reuse the existing strict evaluator, minimality filter, and final selector unchanged

The compiled telescope is still raw MBTT.

The generator just stops searching at the wrong abstraction level.

---

## 5. Molecular Objects

The new search space should be built from mathematically valid molecules.

Suggested core type:

```text
data MolecularPackage
  = ActivateAmbient PrimitiveNode
  | DefineHIT Int Int
  | DefineAPI APISkeleton
```

Interpretation:

- `ActivateAmbient Pi`
  - turn on the ambient dependent function package
- `ActivateAmbient Sigma`
  - turn on the ambient dependent pair package
- `ActivateAmbient Id`
  - turn on identity/path package
- `ActivateAmbient Susp`
  - turn on suspension package
- `DefineHIT points paths`
  - define a local HIT by point count and path count
- `DefineAPI skeleton`
  - later-stage structural API package for steps 10-14

The crucial design rule is:

- molecules are mathematical packages
- not raw syntax fragments

---

## 6. The Molecule Compiler

Each molecular package should compile deterministically to a raw MBTT telescope.

This compiler is the executable realization of the Adjoint Completion Principle.

### 6.1 Ambient packages

Examples:

```text
ActivateAmbient(Pi)
  -> compile to:
     [ Pi-formation
     , Lam-introduction
     , App-elimination
     ]

ActivateAmbient(Sigma)
  -> compile to:
     [ Sigma-formation
     , Pair-introduction
     , Fst/Snd or equivalent elimination package
     ]

ActivateAmbient(Id)
  -> compile to:
     [ Id-formation
     , Refl-introduction
     , J/elimination package
     ]
```

The compiler does not guess these rules. It emits them because the package semantics demand them.

### 6.2 Local HIT packages

Examples:

```text
DefineHIT(1 point, 0 paths)
  -> one-point type

DefineHIT(1 point, 1 path)
  -> S1-like package:
     [ formation
     , base
     , loop
     , auto-derived eliminator/computation package
     ]

DefineHIT(1 point, 2 paths)
  -> S2-like package
```

The key rule is:

- point/path counts define the signature
- eliminators are derived mechanically from the signature

So the generator never has to fuzz the induction principle AST blindly.

### 6.3 Later API packages

For steps 10-14, molecular generation can be extended with API-level packages such as:

- modal interfaces
- bridge interfaces
- curvature/metric skeletons
- functional-analysis skeletons

These are still anonymous structural packages, not named targets.

---

## 7. Conceptual `kappa` vs. compiled clause length

The molecular plan requires an explicit separation between:

- conceptual discovery cost
- physical compiled telescope length

This is essential.

### 7.1 Why the current atomic model breaks

The true `Pi`/`Sigma` package is conceptually `kappa = 3`, but physically it can expand to more than three MBTT clauses once eliminative payload is represented explicitly.

Similarly, `S^1` is conceptually small but its compiled telescope may contain more physical clauses once eliminator/computation structure is emitted.

### 7.2 New accounting rule

For strict molecular search:

- `kappa` for admissibility and exact-band search is the molecular effort cost
- compiled telescope length is only representation size

Suggested data:

```text
data CompiledMolecule = CompiledMolecule
  { cmPackages :: [MolecularPackage]
  , cmConceptualKappa :: Int
  , cmCompiledTelescope :: Telescope
  }
```

The strict lane should search over `cmConceptualKappa`, not over raw compiled clause count.

This is the clean way to resolve the Step-4 capacity paradox.

---

## 8. New Strict Search Loop

The old atomic prefix beam should no longer be the primary strict discovery engine for foundational steps.

Primary loop:

```text
enumerateMolecularTelescopes(lib, exactBand):
  candidates = []

  for bundle in generateMolecularBundles(maxConceptualKappa = exactBand):
    compiled = compileBundle(bundle)

    if compiled.cmConceptualKappa == exactBand:
      tele = compiled.cmCompiledTelescope

      if checkTelescope(lib, tele) == CheckOK:
        candidates.append(compiled)

  return candidates
```

Then the pipeline stays the same:

```text
for each compiled candidate:
  evaluate with strict evaluator
  apply SCC semantic minimality
  deduplicate canonically
  rank with honestSelectionKey
```

The current evaluator/minimality/ranker are already the strong part of the strict lane. They should be reused, not replaced.

---

## 9. Why this is Honest and Not Cheating

This shift does not inject names or target answers.

### 9.1 No names

The generator is not told:

- build `Pi`
- build `S^1`
- build `Trunc`

It is only told:

- you may activate ambient primitives
- you may define local HIT signatures with point/path counts
- you may instantiate anonymous structural API skeletons

### 9.2 The objective still decides

Bad signatures still lose.

Example:

```text
DefineHIT(2 points, 0 paths)
```

may compile to a Boolean-like package.

The evaluator will still compute:

- its `nu`
- its `rho`
- whether it clears the bar

If it does not clear the strict bar, it dies automatically.

### 9.3 It aligns with the theory

The Adjoint Completion Principle is no longer violated by the generator.

The search layer proposes signatures.

The compiler emits the adjoint-complete telescope.

That is exactly the proof-theoretic story the paper already claims.

---

## 10. Expected Immediate Payoff

This shift is expected to fix the current boundary directly.

### Step 4

The generator should quickly test a bundle like:

```text
[ActivateAmbient(Pi), ActivateAmbient(Sigma)]
```

That bundle compiles to the full former package, not a 3-node macro soup.

It should then be evaluated honestly and ranked by the existing strict selector.

### Step 5

The generator should quickly test:

```text
DefineHIT(1, 1)
```

which compiles to the local circle-like package with derived eliminative structure.

That avoids the need to discover `loop` and the induction principle by blind raw-node fuzzing.

### Performance

The molecular signature space is tiny compared with atomic depth-3 AST space.

So:

- step 4 should collapse from expensive prefix competition to trivial bundle evaluation
- step 5 should become equally small

This is the first strategy that is both honest and aligned with the theory.

---

## 11. New Roadmap

Implement in four phases.

### Phase A. Molecular core types and compiler

Goal:

- define the molecule language and deterministic compiler

Deliverables:

- new module, likely `engine/src/StrictMolecules.hs`
- `MolecularPackage`
- `CompiledMolecule`
- compiler from packages to MBTT telescopes
- conceptual `kappa` accounting

Code touchpoints:

- new: `engine/src/StrictMolecules.hs`
- `engine/src/Telescope.hs`
- `engine/src/TelescopeEval.hs`

Acceptance:

- `ActivateAmbient(Pi)` compiles deterministically
- `ActivateAmbient(Sigma)` compiles deterministically
- `DefineHIT(1,1)` compiles deterministically
- compiler outputs are type-checkable MBTT telescopes

### Phase B. Molecular exact-band enumeration

Goal:

- replace atomic strict foundational enumeration with molecular bundle search

Deliverables:

- molecular bundle generator
- exact-band search over conceptual `kappa`
- strict integration path in `RunAbInitio`

Code touchpoints:

- new or expanded `engine/src/StrictMolecules.hs`
- `engine/src/RunAbInitio.hs`
- possibly reduce `engine/src/MBTTEnum.hs` to fallback/shadow use

Acceptance:

- step 4 reliably produces a viable `kappa = 3` former package
- step 5 reliably produces a viable `kappa = 3` circle-like package
- steps 1-5 should become dramatically faster than the atomic-prefix version

### Phase C. Reuse current evaluator/minimality/ranker unchanged

Goal:

- preserve honesty by keeping final truth tests the same

Deliverables:

- compiled molecular telescopes feed directly into:
  - `evaluateTelescopeWithHistory`
  - `applyStrictSemanticMinimalityFilter`
  - `honestSelectionKey`

Acceptance:

- no name-aware evaluation appears
- no target-conditioned ranking appears
- molecular candidates are judged by the same strict objective as atomic ones

### Phase D. Later-stage molecular API packages

Goal:

- extend the same architecture past foundations and HITs

Deliverables:

- structural API molecules for steps 10-14
- hybrid molecular search for:
  - modal/cohesive packages
  - connection/curvature skeletons
  - metric/Hilbert bundles

Acceptance:

- strict later steps are explored through anonymous structural packages, not target labels

---

## 12. What happens to the atomic prefix beam?

The current plan is:

- retire the atomic prefix beam as the primary strict engine for foundational discovery
- keep it only as:
  - a shadow audit path
  - a regression harness
  - a possible fallback for later free-form API search

Reason:

- it has already mapped the boundary where syntactic fuzzing fails
- it no longer looks like the right abstraction for steps 4-9

This is not a loss. It is a proof.

The atomic beam established the exact point where proof-theoretic synthesis must take over.

---

## 13. Acceptance Tests to Add

### Molecular compiler tests

- `ActivateAmbient(Pi)` compiles to the expected anonymous former package shape
- `ActivateAmbient(Sigma)` compiles to the expected anonymous pair package shape
- `DefineHIT(1,1)` compiles to a circle-like package shape
- compiled outputs are name-invariant

### Conceptual `kappa` tests

- conceptual `kappa` is determined by molecular bundle cost
- compiled clause count may exceed conceptual `kappa`
- strict admissibility uses conceptual `kappa`, not compiled array length

### Strict honesty tests

- molecular search does not inspect canonical names
- evaluator results are invariant to external naming
- ranking remains canonical-name-neutral

### Regression tests

- steps 1-5 still pass strict correctness targets
- SCC minimality still behaves correctly on compiled molecular telescopes

---

## 14. Benchmark Gates

Benchmark the new strict molecular lane with:

1. `cabal run ab-initio -- --strict --max-steps 5`
2. `cabal run ab-initio -- --strict --max-steps 10`
3. `cabal run ab-initio -- --strict --max-steps 14`
4. `cabal run ab-initio -- --strict`

Required gates:

- Gate 1:
  - step 4 discovered
  - step 5 discovered
  - steps 1-5 comfortably faster than the current atomic strict lane

- Gate 2:
  - steps 6-9 discovered with molecular foundations/HITs

- Gate 3:
  - steps 10-14 discovered with molecular API packages

- Gate 4:
  - DCT still discovered under the same honest evaluator and selector

If a gate fails:

- do not widen raw atomic search first
- inspect whether the missing structure should be a new molecule family
- only keep atomic search as a fallback audit, not the primary remedy

---

## 15. Final Deliverable

The end-state strict engine should behave like this:

```text
enumerate exact admissible conceptual-kappa bands
generate anonymous mathematical signatures
compile each signature to a raw MBTT telescope by adjoint completion
evaluate compiled telescopes with the existing strict evaluator
reject composite bundles with SCC semantic minimality
rank surviving candidates by the existing honest selector
```

This is principled because:

- it never injects names
- it never injects the final answer
- it respects the Adjoint Completion Principle in the generation layer

This is pragmatic because:

- it bypasses the raw AST noise barrier
- it reuses the strongest parts of the current strict lane
- it gives the engine the actual language of mathematics instead of the language of syntax fuzzing
