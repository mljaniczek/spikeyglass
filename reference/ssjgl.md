# Bayesian Spike-and-Slab Joint Graphical Lasso

Estimates multiple related precision matrices (inverse covariance
matrices) across K groups using an EM algorithm with spike-and-slab
priors. The method encourages shared sparsity across groups via either
fused or group penalties, while allowing group-specific differences
through adaptive, edge-specific penalization.

## Usage

``` r
ssjgl(
  Y,
  penalty = "fused",
  lambda0,
  lambda1,
  lambda2,
  v1 = 1,
  v0s = NULL,
  doubly = FALSE,
  rho = 1,
  a = 1,
  b = 1,
  maxitr.em = 500,
  tol.em = 1e-04,
  maxitr.jgl = 500,
  tol.jgl = 1e-05,
  truncate = 1e-05,
  normalize = FALSE,
  c = 0.1,
  impute = TRUE,
  standardize = FALSE,
  verbose = FALSE
)
```

## Arguments

- Y:

  List of K data matrices, each n_k x p. Missing values (`NA`) are
  supported when `impute = TRUE`.

- penalty:

  Character: `"fused"` (penalizes pairwise differences between groups)
  or `"group"` (penalizes L2 norm across groups).

- lambda0:

  Scalar penalty on diagonal entries of precision matrices. The method
  is relatively insensitive to this value; `lambda0 = 1` is recommended
  in most cases.

- lambda1:

  Scalar (or matrix) base penalty on off-diagonal entries (edge-wise
  sparsity). In the prior, each off-diagonal precision entry has a
  Laplace prior with rate `lambda1/v0` in the spike and `lambda1/v1` in
  the slab, so sparsity is governed by the ratio `lambda1/v0` (see
  `v0s`). Li et al. (2019) use `lambda1 = 1` with variables on unit
  scale, and that is the recommended starting point with
  `standardize = TRUE`. If the data are not standardised, the meaning of
  `lambda1` depends on the variables' scales, because the penalty acts
  on precision entries.

- lambda2:

  Scalar (or matrix) base penalty for the cross-group similarity term.
  Controls borrowing of strength across groups:

  - `lambda2 = 0`: no cross-group borrowing (separate estimation)

  - `lambda2 = lambda1`: equal weight on sparsity and similarity

  - `lambda2 > lambda1`: encourage more similar graphs across groups

  - `lambda2 < lambda1`: allow groups to differ more

- v1:

  Numeric slab variance parameter. Default 1. Should generally be left
  at 1.

- v0s:

  Numeric vector of spike variances, **decreasing**: the ladder for the
  dynamic posterior exploration of Li et al. (2019), each step
  warm-started from the previous one. The spike is a Laplace prior on
  the precision entries with rate `lambda1/v0`, i.e. scale `v0/lambda1`;
  it is not a normal prior, and `sqrt(v0)` has no partial-correlation
  interpretation. Only `lambda1/v0` matters. As a rough guide, a
  non-edge's precision entry is soft-thresholded in the M-step at about
  `lambda1/(n v0)` on the covariance scale (the correlation scale for
  standardised data), for group sample size `n`. Default `NULL` uses
  `make_v0_ladder(lambda1)`, the 3-step ladder
  `lambda1/v0 = 10, 31.6, 100`, i.e. `v0s = c(0.1, 0.0316, 0.01)` at
  `lambda1 = 1`. This default was chosen by a simulation study; see
  [`vignette("parameter-exploration")`](https://mljaniczek.github.io/spikeyglass/articles/parameter-exploration.md)
  and
  [`make_v0_ladder`](https://mljaniczek.github.io/spikeyglass/reference/make_v0_ladder.md).
  It requires a positive scalar `lambda1`; otherwise supply `v0s`.

- doubly:

  Logical. If `TRUE`, uses doubly spike-and-slab prior with separate
  indicators for edge existence (delta) and cross-group similarity (xi).
  Default `FALSE`.

- rho:

  Numeric ADMM step-size parameter. Default 1.

- a:

  Numeric Beta prior shape1 for inclusion probabilities. Default 1.

- b:

  Numeric Beta prior shape2. Default 1. Setting `b = p` gives a sparse
  prior.

- maxitr.em:

  Integer max EM iterations per v0 step. Default 500.

- tol.em:

  Numeric EM convergence tolerance. Default 1e-4.

- maxitr.jgl:

  Integer max ADMM iterations for M-step. Default 500.

- tol.jgl:

  Numeric ADMM convergence tolerance. Default 1e-5.

- truncate:

  Numeric threshold below which entries are zeroed. Default 1e-5.

- normalize:

  Logical. If `TRUE`, mean-centres each variable within each group. It
  does *not* scale. Default `FALSE`. Centring does not change the
  estimates, because the sample covariance is centred anyway. When
  missing values are imputed the data are always centred internally (the
  conditional-mean imputation assumes mean zero), and the imputed values
  are returned on the original scale. Kept for back-compatibility; see
  `standardize`.

- c:

  Numeric diagonal regularization for initial precision estimate.
  Default 0.1.

- impute:

  Logical. If `TRUE`, imputes `NA`s via conditional MVN at each EM
  iteration. The M-step then uses the expected sample covariance given
  the observed data, i.e. the covariance of the imputed data plus the
  conditional covariance of the missing cells (Li et al., 2019,
  supplement B). Default `TRUE`.

- standardize:

  Logical. If `TRUE`, centres *and* scales each variable to unit
  variance within each group before fitting, so the precision matrices
  are estimated on the correlation scale and `lambda1`, `lambda2` and
  `v0s` have the same meaning whatever the variables' units. Recommended
  unless the variables are already on a common, meaningful scale.
  Default `FALSE` (for back-compatibility). Imputed values are returned
  on the original scale.

- verbose:

  Logical. If `TRUE`, report progress (each EM iteration on the R path,
  and each ladder step) with
  [`message()`](https://rdrr.io/r/base/message.html). Default `FALSE`.

## Value

An object of class `"ssjgl"`. With `standardize = TRUE` the precision
matrices are those of the standardised variables. It is a list with
elements:

- thetalist:

  List (length = length(v0s)) of lists of K precision matrices (p x p)
  at each v0 step.

- pi1list:

  List of pi_delta values (edge inclusion probability) at each v0 step.

- pi2list:

  List of pi_xi values (non-similarity probability) at each v0 step.

- fitlist:

  List of raw JGL fit objects at each v0 step.

- itrlist:

  Integer vector of EM iterations at each v0 step.

- problist1:

  List of p x p edge inclusion probability matrices P(delta=1) at each
  v0 step.

- penlist1:

  List of p x p adaptive penalty weight matrices for lambda1 at each v0
  step.

- problist2:

  List of p x p non-similarity probability matrices P(xi=1). NULL if
  `doubly = FALSE`.

- penlist2:

  List of p x p adaptive penalty weights for lambda2. NULL if
  `doubly = FALSE`.

- timelist:

  Numeric vector of wall-clock seconds per v0 step.

- imputed:

  Numeric vector of imputed values on the original data scale (one per
  row of `missed`), or NULL.

- missed:

  Matrix of (group, row, col) for missing values, or NULL.

- v0s, v1, lambda0, lambda1, lambda2, penalty, doubly, a, b:

  The hyperparameters used.

- n, p, K:

  Sample sizes (length K), number of variables and number of groups.

- varnames, groupnames:

  Variable names (from `colnames(Y[[1]])`, or `V1, ..., Vp`) used as
  dimnames of every p x p output, and group names (from `names(Y)`,
  possibly NULL).

- normalize, standardize, center, scale:

  The preprocessing used: `center` is the K x p matrix of column means
  subtracted (NULL if the data were not centred) and `scale` the K x p
  matrix of standard deviations divided by (NULL unless
  `standardize = TRUE`).

- args:

  Named list of every argument except `Y`, as used (with the default
  `v0s` filled in); used to refit with the same settings, e.g. by
  [`diff_edges`](https://mljaniczek.github.io/spikeyglass/reference/diff_edges.md).

- call:

  The matched call.

## Details

The algorithm follows the dynamic posterior exploration strategy of Li
et al. (2019), iterating over a decreasing ladder of spike variances
`v0s` (increasing spike penalty `lambda1/v0`) with warm-starting between
steps.

## References

Li, Z. R., McCormick, T. H., & Clark, S. J. (2019). Bayesian Joint
Spike-and-Slab Graphical Lasso. *ICML 2019*.

## See also

[`make_v0_ladder()`](https://mljaniczek.github.io/spikeyglass/reference/make_v0_ladder.md),
[`plot_path()`](https://mljaniczek.github.io/spikeyglass/reference/plot_path.md),
[`plot_stability()`](https://mljaniczek.github.io/spikeyglass/reference/plot_stability.md),
[`SSJGL_select_v0_cv()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_select_v0_cv.md),
[`compute_metrics()`](https://mljaniczek.github.io/spikeyglass/reference/compute_metrics.md)

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 1, lambda2 = 1,
             standardize = TRUE, maxitr.em = 50, impute = FALSE)
fit$v0s  # the default ladder, make_v0_ladder(lambda1 = 1)
#> [1] 0.10000000 0.03162278 0.01000000
print(fit)
#> Spike-and-Slab Joint Graphical Lasso (SSJGL)
#>   Groups (K): 2
#>   Variables (p): 10
#>   v0 ladder steps: 3
#>   Total EM iterations: 24
#>   Total time: 0.2 seconds
#>   Group 1: 10 non-zero edges (from precision)
#>   Group 2: 10 non-zero edges (from precision)
#>   Edges with P(inclusion) > 0.5: 10
adj <- extract_adjacency(fit, threshold = 0.5)
sum(adj[[1]][upper.tri(adj[[1]])])  # estimated edge count
#> [1] 10
```
