# Decision Gate A3 Resolution (Revised)

## Scope
Finalize the accounting policy for scalar imports at Step 13 under Path A1.

---

## Policy options reconsidered
- **V1 (strict first-use full-charge):** charge `\kappa_{\mathrm{scalar}}` at first use in Step 13.
- **V2 (amortized infrastructure ledger):** off-row ledger for scalar tower.

Given the review constraint that PEN must remain a greedy, uncompromising efficiency maximizer, V2 is no longer acceptable as a normative policy.

---

## Revised A3 decision
**Decision:** ✅ **A3 complete (revised)**.

- **Accepted normative policy:** **V1 strict first-use full-charge**.
- **Rejected policy:** **V2 amortized ledger** (treated as non-canonical and removed from normative accounting).

---

## Rationale
1. Step-local selection must pay full local dependency cost at first admissible use.
2. Off-balance-sheet ledgers undermine the parameter-free greedy interpretation of Axiom 3.5.
3. If a candidate fails under full charging, that is an empirical signal about algorithmic inefficiency of the current construction path.

---

## Consequence for subsequent analysis
A4 and all future sequence claims must be computed under strict V1 charging. Any rescue must come from mechanically computed novelty (`\nu`) gains, not accounting conventions.
