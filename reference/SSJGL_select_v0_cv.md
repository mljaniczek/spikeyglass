# Select v0 via K-fold cross-validation

Evaluates each v0 value in the ladder using K-fold cross-validation with
Gaussian negative log-likelihood as the scoring criterion. Selects the
v0 that minimizes the total CV loss.

## Usage

``` r
SSJGL_select_v0_cv(
  Y,
  v0s,
  folds = 5,
  seed = 1,
  penalty,
  lambda0,
  lambda1,
  lambda2,
  v1 = 1,
  doubly = FALSE,
  rho = 1,
  a = 1,
  b = 1,
  maxitr.em = 200,
  tol.em = 1e-04,
  maxitr.jgl = 200,
  tol.jgl = 1e-05,
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

  Numeric vector of v0 values to evaluate.

- folds:

  Integer number of CV folds. Default 5.

- seed:

  Integer random seed. Default 1.

- penalty, lambda0, lambda1, lambda2, v1, doubly, rho, a, b, maxitr.em,
  tol.em, maxitr.jgl, tol.jgl, truncate, normalize, c, impute:

  Arguments passed to
  [`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md).

- verbose:

  Logical. If TRUE, prints fold progress. Default TRUE.

## Value

A list with elements:

- v0_best:

  The selected v0 value.

- i_best:

  Index of the best v0 in `v0s`.

- cv_score:

  Named numeric vector of total CV scores per v0.

- folds:

  Number of folds used.

- seed:

  Random seed used.

## Examples

``` r
if (FALSE) { # \dontrun{
sim <- simulate_ssjgl_data(K = 2, p = 15, n = 100, seed = 42)
cv_res <- SSJGL_select_v0_cv(
  Y = sim$data_list,
  v0s = c(0.1, 0.05, 0.01, 0.005, 0.001),
  folds = 3,
  penalty = "fused",
  lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
  maxitr.em = 50, maxitr.jgl = 50,
  normalize = TRUE, impute = FALSE
)
cv_res$v0_best
cv_res$cv_score
} # }
```
