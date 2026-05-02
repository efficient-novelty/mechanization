# Coherence-Depth Case Study: Promoted Interface

## Classification

The promoted interface case records active-basis coverage for an operation that
is intentionally not used as a proof of a chronological window or Fibonacci
growth. The module also contains the transparent lemma extension, whose
footprint and `mu` contribution are both zero.

## Counted Interface

| Field | Value |
|---|---:|
| Payload fields | 1 |
| Active interface footprint | 2 |
| Unary trace obligations | 2 |
| Binary trace obligations | 0 |
| Higher horn obligations | 0 |
| Derived higher horn obligations | 0 |
| Active-basis totality | true |
| Expected `mu` contribution | 2 |
| Recurrence classification | promoted-active-basis |

## Transparent Lemma Subcase

| Field | Value |
|---|---:|
| Payload fields | 0 |
| Active interface footprint | 0 |
| Unary trace obligations | 0 |
| Binary trace obligations | 0 |
| Higher horn obligations | 0 |
| Derived higher horn obligations | 0 |
| Active-basis totality | false |
| Expected `mu` contribution | 0 |
| Recurrence classification | transparent-zero |

## Mechanized Witness

`CaseStudies.PromotedInterface` exposes the active-basis contract and density
package obtained from global action totality, plus the non-circularity witnesses
showing that coverage alone does not imply the depth-two window or Fibonacci
law.
