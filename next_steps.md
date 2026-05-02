# Next Turn Plan for `1_coherence_depth.tex`

## Goal

Finish final sign-off by running the first available TeX build and checking that the bridge-boundary cleanup caused no layout or reference regressions.

## Ordered Tasks

1. Check again whether `latexmk`, `pdflatex`, `xelatex`, or `lualatex` is available in the shell.
2. If a TeX engine is available, rebuild `1_coherence_depth.tex` and inspect warnings, undefined references, overfull boxes, and any bibliography fallout.
3. Spot-check the generated PDF around the abstract, claim-map table, `rem:bridge-target`, foundation-scope table, `sec:mechanization`, and the conclusion.
4. Fix only mechanical fallout from that build pass: broken references, bad line breaks, table overflow, or wording adjustments forced by pagination.
5. If no TeX toolchain is available, record that explicitly again and leave the paper text unchanged unless a concrete new blocker appears.
6. Once build verification is complete, collapse `checklist.md` and `paper_improvement_plan.md` to the final completed state.

## Acceptance Criteria

- The paper is compiled and spot-checked if tooling is available, or the missing-toolchain blocker is re-confirmed explicitly.
- No build warning or layout issue remains in the bridge-sensitive summary and mechanization sections.
- The planning files describe only the final sign-off state.
