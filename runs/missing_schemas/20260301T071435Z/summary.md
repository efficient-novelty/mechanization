# Missing Schemas Mechanical Run Summary

run_id: 20260301T071435Z
policy: strict V1, bar=5.99

## Route verdicts
- cauchy_minimal: kappa_full=11, delta_nu_min=20, delta_nu_mech=64, rho_new=10.0, margin=4.01, pass=true
- topological_arithmetic: kappa_full=10, delta_nu_min=14, delta_nu_mech=64, rho_new=11.0, margin=5.01, pass=true
- synthetic_continuum: kappa_full=9, delta_nu_min=8, delta_nu_mech=64, rho_new=12.222222, margin=6.232222, pass=true

Best route by margin: synthetic_continuum (margin=6.232222).

## Discussion
- All routes passed under the symbolic mechanical interaction generator.
- Higher-kappa routes required larger delta_nu_min but also yielded sufficient interaction counts in this run.
- This run is algorithmically mechanical but symbolic; next stage should bind schema families to engine-native witness traces.
