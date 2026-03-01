# Missing Schemas Mechanical Run Summary

run_id: 20260301T072134Z
policy: strict V1, bar=5.99

## Route verdicts
- cauchy_minimal: kappa_full=11, delta_nu_min=20, delta_nu_mech=1, nu_scalar=6, nu_metric=46, nu_combined=53, rho_new=4.272727, margin=-1.717273, pass=false
- topological_arithmetic: kappa_full=10, delta_nu_min=14, delta_nu_mech=-6, nu_scalar=11, nu_metric=46, nu_combined=51, rho_new=4.0, margin=-1.99, pass=false
- synthetic_continuum: kappa_full=9, delta_nu_min=8, delta_nu_mech=0, nu_scalar=4, nu_metric=46, nu_combined=50, rho_new=5.111111, margin=-0.878889, pass=false

Best route by margin: synthetic_continuum (margin=-0.878889).

## Discussion
- Witnesses are now engine-native MBTT node traces from computeNativeNu (nnTrace node=...|ctor=...).
- delta_nu_mech uses inclusion-exclusion over native nu totals: nu_combined - nu_metric - nu_scalar.
- Next refinement: tie node traces to inhabited-type deltas in uniform-nu replay lane.
