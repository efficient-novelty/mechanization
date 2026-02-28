# Decision Gate A4 Resolution (Revised)

## Scope
Determine Step-13 viability under the revised A3 policy: **strict V1 first-use full-charge** with Step-13 bar fixed to **5.99**.

---

## Fixed inputs
- Baseline metric entry: `\nu_0 = 46`, `\kappa_0 = 7`.
- Enforced Step-13 bar: `\mathrm{Bar}_{13}=5.99`.
- Full-charge model: `\rho = \nu/(\kappa_0 + \kappa_{\mathrm{scalar}})`.

---

## V1 result (strict)
For every tested `\kappa_{\mathrm{scalar}} \in [1,12]`, V1 fails the bar.
This is now treated as a real algorithmic finding, not something to smooth over with amortization.

---

## V3 rescue condition under strict V1
To pass under strict V1, required emergent novelty is:
\[
\Delta \nu_{\min} = \left\lceil 5.99\,(7+\kappa_{\mathrm{scalar}})-46 \right\rceil.
\]
Selected examples from the run:
- `\kappa_{\mathrm{scalar}}=1 \Rightarrow \Delta\nu_{\min}=2`
- `\kappa_{\mathrm{scalar}}=4 \Rightarrow \Delta\nu_{\min}=20`
- `\kappa_{\mathrm{scalar}}=5 \Rightarrow \Delta\nu_{\min}=26`

Hence a `+20` interaction explosion only rescues Step 13 when `\kappa_{\mathrm{scalar}}\le4`.

---

## Revised A4 decision
**Decision:** ✅ **A4 complete (revised)**.

1. Under strict V1, current Step-13 metric entry fails unless additional mechanically-derived novelty is included.
2. The project should pursue one of two strict-policy recovery paths:
   - **Route C+V3:** keep Cauchy path but compute emergent `\Delta\nu` mechanically (uniform-nu / typed synthesis), or
   - **Lower-kappa scalar route:** reduce `\kappa_{\mathrm{scalar}}` (e.g., topological arithmetic extraction or synthetic continuum insertion).

---

## Recommendation
Retain strict V1 as canonical policy, and treat the current Step-13 mismatch as a discovery target: either find the missing interaction schemas mechanically or change the scalar construction route/step structure.
