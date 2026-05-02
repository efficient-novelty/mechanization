# Coherence-Depth Case Study: Sparse Datatype

## Classification

The sparse datatype case has a finite local dependency footprint. It uses one
previous-window site inside a three-site possible window and does not claim the
full-coupling recurrence.

## Counted Interface

| Field | Value |
|---|---:|
| Payload fields | 1 |
| Active interface footprint | 1 |
| Unary trace obligations | 1 |
| Binary trace obligations | 0 |
| Higher horn obligations | 0 |
| Derived higher horn obligations | 0 |
| Active-basis totality | false |
| Expected `mu` contribution | 1 |
| Recurrence classification | sparse-local |

## Mechanized Witness

`CaseStudies.SparseDatatype` constructs a `CouplingFootprint three` with one
dependency, packages the sparse windowed context, and proves that the footprint
stays below the full previous-window envelope.
