# Full SSJGL workflow: CV selection + final fit with bootstrap CIs

Combines
[`SSJGL_select_v0_cv`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_select_v0_cv.md)
and
[`SSJGL_final_with_pcor_CI`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_final_with_pcor_CI.md)
into a single call. First selects the best v0 via cross-validation, then
fits the final model at that v0 and computes bootstrap confidence
intervals for partial correlations.

## Usage

``` r
SSJGL_CV_final_pcorCI(
  Y,
  v0s,
  folds = 5,
  B = 200,
  ci_level = 0.95,
  seed = 1,
  penalty = "fused",
  lambda0,
  lambda1,
  lambda2,
  v1 = 1,
  doubly = FALSE,
  rho = 1,
  a = 1,
  b = 1,
  maxitr.em.cv = 200,
  tol.em = 1e-04,
  maxitr.jgl.cv = 200,
  tol.jgl = 1e-05,
  maxitr.em = 500,
  maxitr.jgl = 500,
  truncate = 1e-05,
  normalize = FALSE,
  c = 0.1,
  impute = TRUE,
  verbose = TRUE
)
```

## Arguments

- Y:

  List of K data matrices.

- v0s:

  Numeric vector of v0 values to search over.

- folds:

  Integer number of CV folds. Default 5.

- B:

  Integer number of bootstrap samples. Default 200.

- ci_level:

  Numeric confidence level. Default 0.95.

- seed:

  Integer random seed. Default 1.

- penalty, lambda0, lambda1, lambda2, v1, doubly, rho, a, b:

  Arguments passed to
  [`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md).

- maxitr.em.cv, maxitr.jgl.cv:

  Max iterations for CV fits (smaller for speed). Defaults 200.

- tol.em, tol.jgl:

  Convergence tolerances.

- maxitr.em, maxitr.jgl:

  Max iterations for final fit. Defaults 500.

- truncate, normalize, c, impute:

  Additional `ssjgl` arguments.

- verbose:

  Logical. Default TRUE.

## Value

A list with elements:

- cv:

  Output of
  [`SSJGL_select_v0_cv`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_select_v0_cv.md).

- final:

  Output of
  [`SSJGL_final_with_pcor_CI`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_final_with_pcor_CI.md).

## Examples

``` r
if (FALSE) { # \dontrun{
sim <- simulate_ssjgl_data(K = 2, p = 15, n = 100, seed = 42)
res <- SSJGL_CV_final_pcorCI(
  Y = sim$data_list,
  v0s = c(0.05, 0.01, 0.005),
  folds = 3, B = 20,
  penalty = "fused",
  lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
  normalize = TRUE, impute = FALSE
)
res$cv$v0_best
res$final$pcor_hat[[1]][1:5, 1:5]
} # }
```
