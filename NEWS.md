# spikeyglass (development version)

## Dependencies

* spikeyglass no longer depends on JGL, which was archived from CRAN on
  2025-07-17. The few JGL 2.3.2 routines it used (MIT licence, Patrick
  Danaher) are vendored in `R/jgl-vendored.R` or ported to C++
  (`flsa.general` is now `flsa_general_update()`). Patrick Danaher is listed
  as a copyright holder; see `inst/COPYRIGHTS`. The `Remotes:` field is
  removed.

* igraph is no longer a dependency. Block detection in the R EM path uses
  the same C++ breadth-first search as the C++ path.

## Performance

* The fused penalty with K > 2 now runs entirely in C++ (EM loop and ADMM)
  instead of in R with `JGL:::flsa.general`. It is about 4 times faster
  (35 s to 9 s for p = 20, K = 3, doubly). Results match the previous
  version to 1e-10.

## Bug fixes

* `lambda2 = 0` (separate estimation) no longer sets every posterior inclusion
  probability to 0. The E-step evaluated `log(lambda2 / v)`, which is `-Inf`
  for both spike and slab, so the posterior was `NaN` and was replaced by 0.
  A penalty with `lambda = 0` now contributes no prior terms to the E-step.
  The same applies to `lambda1 = 0`. Fixed in both the R and C++ E-steps.

* Imputation no longer fails when a group has exactly one missing cell
  (`getmissing()` dropped the missing-cell index matrix to a vector).

* `fit$imputed` is no longer all `NA` when `normalize = FALSE`, and imputed
  values are always returned on the original data scale. The conditional-mean
  imputation assumes mean-zero data, so the data are now always centred
  internally when there are cells to impute. Previously, with
  `normalize = FALSE`, imputations were shrunk towards 0 rather than towards
  the variable means. Fits without missing data are unaffected, because the
  sample covariance is centred anyway.

* With missing data, the M-step now uses the expected sample covariance given
  the observed data, S(imputed) + (sum of conditional covariances) / n, as in
  Li et al. (2019), supplement B. Previously this correction was used only for
  block screening, and the ADMM recomputed S from the imputed data alone. As a
  result, the internal ADMM now takes covariance matrices rather than data, and
  the singleton-node solver is vendored from JGL
  (`JGL:::admm.iters.unconnected` is no longer called).

* The non-doubly R E-step (`gete()`) now uses the log-sum-exp trick, matching
  the C++ version. It no longer returns `NaN` when both the spike and slab
  log-densities underflow.

* In the R EM path (fused penalty with K > 2, or imputation),
  `pi2list` is no longer updated when `doubly = FALSE`. It now stays at
  `a / (a + b)`, matching the C++ path. It was previously reported as
  `(a - 1) / (a + b + n_edges - 2)`, which is meaningless without a similarity
  indicator.

## Breaking changes

* The `warm` and `warm.connected` arguments of `ssjgl()` have been removed.
  They were accepted but never used.

## Testing

* Reference fits saved before JGL was vendored (fused/group, single/doubly,
  K = 2 and 3, and two imputation cases) are reproduced to 1e-10
  (`tests/testthat/test-reference-fits.R`). The C++ `flsa.general` port is
  checked against saved JGL outputs.

* New regression tests for each fix above, plus a test that the C++ and R EM
  paths agree to 1e-10 for fused/group penalties with and without the doubly
  prior (K = 2).

# spikeyglass 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
