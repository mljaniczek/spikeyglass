# spikeyglass 0.1.0

## CRAN preparation

* `ssjgl()` gains `verbose = FALSE`. Progress (each EM iteration on the R
  path, each ladder step, re-initialisation notices) is reported with
  `message()` only when `verbose = TRUE`. It was printed unconditionally
  with `cat()`. `SSJGL_select_v0_cv()`, `SSJGL_final_with_pcor_CI()` and
  `SSJGL_CV_final_pcorCI()` now default to `verbose = FALSE` and use
  `message()`.
* `simulate_ssjgl_data()`, `SSJGL_select_v0_cv()` and
  `SSJGL_final_with_pcor_CI()` no longer change the caller's random-number
  stream: the RNG state is restored on exit after using `seed`.
* `plot_path()` restores `par()` on exit, has dead code removed, and its
  example now runs. Its legend now says what is drawn (the old "Edge" /
  "Non-edge" legend referred to a true graph the function never had).
* `simdat` is regenerated reproducibly with `simulate_ssjgl_data()`
  (`data-raw/simdat.R`) and now also contains the true precision matrices
  (`Omega`). The previous version came from a script that could not be run.
* The unused `fit1.Rdata` files are removed, so the package no longer
  depends on R >= 3.5.0 through serialized data. It now depends on
  R >= 4.1.0.
* New `inst/CITATION` (method paper and package), `CITATION.cff` and
  `cran-comments.md`. The DESCRIPTION has a fuller description citing Li et
  al. (2019), `BugReports`, and the GitHub URL. The Title is in title case.
* GitHub Actions: R CMD check on macOS, Windows and Ubuntu (devel, release,
  oldrel-1), plus test coverage.
* The profiling script moved from `inst/profiling/` to `scripts/` (not
  installed).

# spikeyglass (development version, before 0.1.0)

## Shared tools with multiGGMr: ggmtools

* `precision_to_pcor()`, `confusion_at_threshold()`, `roc_auc()` and
  `plot_roc()` now come from the new package ggmtools (Imports) and are
  re-exported. multiGGMr re-exports the same functions, so attaching both
  packages no longer gives conflicting versions with different signatures.
* `roc_auc()` now returns `auc` (lower case; was `AUC`). `compute_metrics()`
  is updated; `plot_roc()` accepts either.
* `confusion_at_threshold()` returns `NA` (was 0) for rates with a zero
  denominator.
* New methods for the ggmtools generics `pip_table()` (PIPs as a long edge
  table; the shared P(delta = 1) is repeated for each group) and
  `pcor_estimate()` (MAP partial correlations, one matrix per group).
  Scripts written with these work on multiGGMr fits too.
* The TCGA BRCA PI3K dataset is available as `ggmtools::brca_pi3k`.

## New features

* New `diff_edges()` tests which edges differ between two groups.
  `method = "permutation"` refits `ssjgl()` on label-permuted data (pooled
  null of |Δ| across edges, BH-adjusted p-values); `method = "bootstrap"`
  gives percentile intervals for Δ. Δ is on the partial-correlation
  (default) or precision scale. It returns a tidy edge table (`i`, `j`,
  `node1`, `node2`, `est_g1`, `est_g2`, `diff`, `pvalue`/`p_adj` or
  `lower`/`upper`, `selected`, and `prob_dissimilar` = P(ξ = 1) for doubly
  fits), with `print()` and `plot()` methods (network coloured by direction,
  and a scatter of group 1 vs group 2 estimates). Refits run through
  future.apply when it is installed (new Suggests), so they are parallel
  under any `future::plan()`. Results depend only on `set.seed()`.

* The fit stores `args`, every `ssjgl()` argument except the data, so it can
  be refitted with the same settings.

* `ssjgl()` gains `standardize = TRUE`, which centres and scales every
  variable within each group (`normalize = TRUE` only centres, and is kept
  for back-compatibility). Imputed values are returned on the original scale.

* The fitted `ssjgl` object now stores `v0s`, the hyperparameters (`lambda0`,
  `lambda1`, `lambda2`, `v1`, `a`, `b`, `penalty`, `doubly`), the dimensions
  (`n`, `p`, `K`), `varnames`, `groupnames`, the preprocessing (`normalize`,
  `standardize`, `center`, `scale`) and the `call`. `fit$v0s` was
  previously `NULL`.

* Variable names (from `colnames(Y[[1]])`, or `V1, ..., Vp`) are now set as
  dimnames on every p x p output, on both the C++ and the R path; the C++
  path previously dropped them. Group names are taken from `names(Y)`.

* `plot_stability()` defaults to `fit$v0s`.

## Changes to defaults

* One recommended v0 ladder, chosen by a simulation study (see
  `vignette("parameter-exploration")` and `data-raw/v0-ladder-study.R`):
  three steps with `lambda1/v0 = 10, 31.6, 100`.
  * `ssjgl()` now defaults to `v0s = NULL`, meaning `make_v0_ladder(lambda1)`.
    At `lambda1 = 1` this is `c(0.1, 0.0316, 0.01)`, essentially the old
    default `c(0.1, 0.03, 0.01)`. For other `lambda1` the default ladder now
    scales with `lambda1`, because only `lambda1/v0` enters the model.
  * `make_v0_ladder()` defaults change from `n_steps = 10, min_ratio = 5,
    max_ratio = 500` to `n_steps = 3, min_ratio = 10, max_ratio = 100`.
    `n_steps = 1` is now allowed.

## Documentation

* The documentation of `v0` is corrected. The spike and slab are Laplace
  priors on the precision entries with rates `lambda1/v0` and `lambda1/v1`
  (scale `v0/lambda1`), not normal priors with standard deviation
  `sqrt(v0)` on the partial-correlation scale. Both vignettes, `?ssjgl` and
  `?make_v0_ladder` were rewritten. They now explain what `lambda1/v0` does
  in the E-step and the M-step, and that `normalize = TRUE` only centres.

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
