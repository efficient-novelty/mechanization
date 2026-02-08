# Paper Update Plan — Status Report

## Overview

Three papers exist: `pen_paper.tex` (theoretical framework), `pen_genesis.tex` (computational reconstruction), and `DCT.tex` (DCT deep-dive). All three have been completely rewritten (February 2026) to be pedagogical, economical, and precise, with all identified discrepancies resolved.

**Current status: ALL FIXES COMPLETE. All three papers compile cleanly with 0 errors, 0 undefined references, and 0 uncited bibliography entries.**

| Paper | Pages | Status |
|---|---|---|
| `pen_paper.tex` | 12 | Complete rewrite, clean compile |
| `pen_genesis.tex` | 11 | Complete rewrite, clean compile |
| `DCT.tex` | 7 | Complete rewrite, clean compile |

---

## 1. pen_paper.tex — COMPLETED

### 1.1 Structural Inflation Definition — FIXED
Phi_n = Delta_n / Delta_{n-1} throughout. Remark 3.9 explains why tau_n/tau_{n-1} is wrong.

### 1.2 Section 7: Mechanization — FIXED (MAJOR REWRITE)
Removed ~180 lines of fake Cubical Agda code (Genome.agda, Mutator.agda, TypeChecker.agda, Main.agda). Replaced with accurate description of the two-component system:
- Haskell engine (~3,000 lines, 17 modules): candidate generation, novelty computation, selection loop
- Cubical Agda mechanization: Fibonacci recurrence theorem (Phase 1 complete)

### 1.3 Genesis Table — FIXED
Single canonical table (Section 2) with all correct values. Duplicate table removed. Engine comparison table added in Section 7 showing paper vs. engine values.

### 1.4 Selection Bar Values — FIXED
Both paper and engine bar values presented; divergence explained (S3 kappa=3 vs kappa=5 question).

### 1.5 Proof-Rank Clustering — ADDED
Section 7 describes proof-rank clustering methodology as used for HITs/suspensions.

### 1.6 Candidate Taxonomy — ADDED
Section 7 presents the 9-type taxonomy (Foundation, Former, HIT, Suspension, Map, Algebra, Modal, Axiom, Synthesis) with gates and example kappa values.

### 1.7 Absorption Mechanism — ADDED
Lie groups (kappa=6, nu=9, rho=1.50) discussed as concrete absorption example in Section 2.

### 1.8 References — FIXED
Cross-references to \cite{pen-genesis} throughout. All bibliography entries cited: hott, cubical, schreiber, lawvere, nakano, cubical-agda.

### 1.9 Section Ordering — FIXED
Conclusion moved to final position (was incorrectly between Verification and Mechanization).

### 1.10 Axiom Environment — FIXED
Added \newtheorem{axiom} to preamble. Five axioms now use proper axiom environment.

---

## 2. pen_genesis.tex — COMPLETED

### 2.1 Structural Inflation Definition — FIXED
Changed from tau_n/tau_{n-1} to Delta_n/Delta_{n-1}. Added Remark explaining why the distinction matters (Phi_4 = 1.50 vs 1.75).

### 2.2 DCT Section with Engine Results — INCLUDED
Section 4 presents the Lattice Tensor Product computation (14×11-4=150) with note that the engine implements this concretely.

### 2.3 Proof-Rank Methodology — ADDED
Section 5 describes the complete proof-rank clustering algorithm with S1 worked example.

### 2.4 Kappa Discussion — ADDRESSED
S3 kappa divergence (paper=5, engine=3) discussed in Section 5 with both interpretations noted as producing the same sequence.

### 2.5 Genesis Table — CONSISTENT
Identical to pen_paper.tex table. Engine comparison table in Section 5.

### 2.6 Cross-References — ADDED
\cite{pen-paper} throughout. All bibliography entries now cited: pen-paper, hott, cubical, schreiber, lawvere, kuratowski, pnueli, wigner, nakano, cubical-agda.

### 2.7 Terminology — HARMONIZED
"coherence depth" changed to "Coherence Window" to match pen_paper.tex (3 instances).

---

## 3. DCT.tex — COMPLETED

### 3.1 Numbering Fix — FIXED
Changed "tau=987 (R15: Hilbert)" to "tau=986 (R14: Hilbert)".

### 3.2 Realization Numbering — FIXED
All R-numbering corrected: R10=Cohesion, R11=Connections, R12=Curvature, R13=Metric, R14=Hilbert, R15=DCT.

### 3.3 Kappa Decomposition — HARMONIZED
Now matches pen_genesis.tex: Import Cohesion(1) + Import Dynamics(1) + Temporal primitives(2) + Infinitesimals(1) + Compatibility triad(3) = 8.

### 3.4 Nu=150 List — REPLACED
Removed ~200-line speculative itemized list. Replaced with concise semantic audit table (14 domains) and the structural computation (14×11-4=150).

### 3.5 Compatibility Axioms — RESOLVED
Reduced to 3 axioms (C1-C3) matching pen_genesis.tex. Remark 2.2 explains that C4 (eventually-flat) and C5 (connection compatibility) are derivable from C1-C3.

### 3.6 Standalone Document — FIXED
Converted from section fragment to proper standalone LaTeX document with \documentclass, abstract, table of contents, and bibliography.

### 3.7 Key Theorems — ADDED
Three internal theorems: Internal Tangent Bundle, Temporal Type Dynamics, Hamiltonian Flows.

### 3.8 Cross-References — ADDED
\cite{pen-paper,pen-genesis} in abstract. All bibliography entries cited: pen-paper, pen-genesis, lawvere, schreiber, kuratowski, pnueli, nakano, kock, woodhouse.

---

## 4. Cross-Paper Consistency — ALL RESOLVED

| Item | pen_paper | pen_genesis | DCT | Status |
|---|---|---|---|---|
| Phi definition | Delta_n/Delta_{n-1} | Delta_n/Delta_{n-1} | Delta_n/Delta_{n-1} | **CONSISTENT** |
| Genesis table | 15 rows, 9 cols | Identical | N/A (references) | **CONSISTENT** |
| DCT kappa decomposition | N/A (table only) | 1+1+2+1+3=8 | 1+1+2+1+3=8 | **CONSISTENT** |
| Compatibility axioms | N/A | C1-C3 | C1-C3 | **CONSISTENT** |
| DCT nu computation | N/A | 14×11-4=150 | 14×11-4=150 | **CONSISTENT** |
| R-numbering | R1-R15 (table) | R10-R15 (text) | R10-R15 (text) | **CONSISTENT** |
| Bibliography titles | Match | Match | Match | **CONSISTENT** |
| Uncited bibitems | 0 | 0 | 0 | **CLEAN** |
| Terminology (Coherence Window) | "Coherence Window" | "Coherence Window" | N/A | **CONSISTENT** |
| Cross-references | cites pen-genesis | cites pen-paper | cites both | **CONSISTENT** |

---

## 5. Recommended Update Order — COMPLETED

1. ~~Fix the Phi definition in pen_genesis.tex~~ **DONE**
2. ~~Fix DCT.tex numbering~~ **DONE**
3. ~~Rewrite pen_paper.tex Section 7 (Mechanization)~~ **DONE**
4. ~~Add proof-rank clustering methodology~~ **DONE**
5. ~~Add candidate taxonomy~~ **DONE**
6. ~~Update Genesis tables with engine comparison~~ **DONE**
7. ~~Harmonize DCT kappa decomposition~~ **DONE**
8. ~~Add absorption discussion~~ **DONE**
9. ~~Cross-reference papers~~ **DONE**
10. ~~Update references~~ **DONE**
11. ~~Fix uncited bibliography entries~~ **DONE**
12. ~~Harmonize terminology (Coherence Window)~~ **DONE**
