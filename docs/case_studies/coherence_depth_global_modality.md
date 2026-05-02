# Coherence-Depth Case Study: Global Modality

## Classification

The global modality case models a modality-like operation advertised over the
whole active basis. Its dependency footprint is the full three-field envelope,
so the case exercises the full-coupling recurrence branch.

## Counted Interface

| Field | Value |
|---|---:|
| Payload fields | 1 |
| Active interface footprint | 3 |
| Unary trace obligations | 1 |
| Binary trace obligations | 2 |
| Higher horn obligations | 1 |
| Derived higher horn obligations | 1 |
| Active-basis totality | true |
| Expected `mu` contribution | 3 |
| Recurrence classification | full-coupling |

## Mechanized Witness

`CaseStudies.GlobalModality` constructs a three-field active interface, proves
global action totality as active-basis coverage, and instantiates the full
coupling envelope over all three sites.
