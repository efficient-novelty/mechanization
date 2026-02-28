# Decision Gate A2 Resolution

## Scope
This note closes **Decision gate A2** from `path_A1.md`: verify that each imported item in Path A1 is **strictly required** for the locked Cauchy-completion route (`\N\to\Z\to\Q\to\R`) and decide the minimal prerequisite interface.

---

## Starting point
- A1 is already locked to **Cauchy completion** as the primary route.
- A2 requires pruning the scalar-import bundle to avoid over-importing arithmetic APIs and to keep `\kappa_{\mathrm{scalar}}` defensible.

---

## Method for A2 verification
For each proposed import, classify by:
1. **Constructive necessity:** removing it blocks definition/proof of Cauchy reals.
2. **Substitutability:** can be replaced by a weaker primitive already in bundle.
3. **PEN accounting relevance:** should be charged in scalar import cost.

Classification labels:
- **REQ** = required
- **DER** = derivable from required items (not separately imported)
- **OPT** = optional convenience (exclude from minimal bundle)

---

## Dependency-minimality audit

### Layer N (`\N` foundation)
| Item | Status | Reason |
|---|---|---|
| `\N` type + `0` + `\mathsf{succ}` + induction/recursor | **REQ** | Required to build integer/rational arithmetic and Cauchy indexing/moduli. |
| Primitive addition/multiplication on `\N` as separate axioms | **DER** | Definable from recursor; do not import as separate clauses. |
| Prime factorization / advanced number theory APIs | **OPT** | Not needed for `\Z,\Q,\R` construction. |

### Layer Z (`\Z` construction)
| Item | Status | Reason |
|---|---|---|
| Integer carrier + equivalence from `\N\times\N` (or equivalent quotient/HIT form) | **REQ** | Needed for signed arithmetic underlying rationals. |
| Ring operations/identities on `\Z` sufficient for fractions | **REQ** | Needed to define rational numerator/denominator algebra. |
| Euclidean algorithm / gcd as primitive import | **OPT** | Useful for normalization but not necessary to define `\Q` quotiented fractions. |

### Layer Q (`\Q` construction)
| Item | Status | Reason |
|---|---|---|
| Rational representation `(a,b)` with `b\neq 0` + fraction equivalence | **REQ** | Core definition of rationals. |
| Field operations on `\Q` (+,−,×,÷ with nonzero divisor) | **REQ** | Needed for Cauchy approximation arithmetic and limits. |
| Order/preorder relation on `\Q` | **REQ** | Needed to express Cauchy convergence/modulus bounds. |
| Canonical reduced-form normalizer | **OPT** | Convenience only; equivalence relation already handles representation. |

### Layer R (`\R` via Cauchy completion)
| Item | Status | Reason |
|---|---|---|
| Type of Cauchy approximations/sequences over `\Q` | **REQ** | Core completion carrier. |
| Cauchy equivalence relation | **REQ** | Needed to quotient approximations into real numbers. |
| Quotient/HIT/set-quotient constructor yielding `\R` | **REQ** | Produces actual real-number type. |
| Embedding `\iota: \Q\hookrightarrow\R` | **REQ** | Needed for compatibility with metric/Hilbert scalar usage. |
| Completeness witness (every Cauchy approximation converges) | **REQ** | Distinguishes completion from mere sequence type. |
| Full classical order trichotomy on `\R` | **OPT** | Not required for baseline metric codomain typing in Path A1. |

---

## Minimal bundle decision (A2 output)
The A2-minimal scalar bundle is:
\[
\mathcal{I}_{\mathrm{scalar}}^{\min}
= \mathcal{I}_{\N}^{\min} \cup \mathcal{I}_{\Z}^{\min} \cup \mathcal{I}_{\Q}^{\min} \cup \mathcal{I}_{\R,\mathrm{Cauchy}}^{\min}
\]
where each `\mathcal{I}^{\min}` includes only REQ items above.

Excluded from the minimal bundle (explicitly):
- advanced arithmetic APIs,
- canonical normalization conveniences,
- classical order enrichments not needed for Step 13 typing-level scalar codomain support.

---

## Gate A2 decision
**Decision:** ✅ **A2 complete** — the required-import set is validated and pruned to the minimal REQ bundle for Cauchy completion.

---

## Consequences for next gates

### For A3 (`\kappa` accounting mode)
- `\kappa_{\mathrm{scalar}}` must be computed over `\mathcal{I}_{\mathrm{scalar}}^{\min}` only.
- DER/OPT items are not charged unless explicitly promoted later.

### For A4 (Step 13 viability)
- Sensitivity analysis should use at least:
  1. **Minimal-charge** scenario (`\mathcal{I}_{\mathrm{scalar}}^{\min}`),
  2. **Expanded-charge** stress test (if reviewers demand richer scalar API assumptions).

---

## Recommendation
Proceed to A3 with the A2-minimal bundle as the normative baseline. This gives the strongest reviewer position: no hidden imports and no inflated arithmetic package beyond what Cauchy completion strictly requires.
