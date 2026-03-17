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
  v0s = c(0.1, 0.03, 0.01),
  doubly = FALSE,
  rho = 1,
  a = 1,
  b = 1,
  maxitr.em = 500,
  tol.em = 1e-04,
  maxitr.jgl = 500,
  tol.jgl = 1e-05,
  warm = NULL,
  warm.connected = NULL,
  truncate = 1e-05,
  normalize = FALSE,
  c = 0.1,
  impute = TRUE
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
  sparsity). The E-step produces adaptive weights that multiply this
  value, so the effective penalty is edge-specific. The method is
  relatively insensitive to the absolute value of `lambda1` (Li et al.,
  2019); what matters most is the ratio `lambda1/v0`. Guidance:

  - Normalized data (`normalize = TRUE`): use 0.01–0.1

  - Raw data with moderate variance: use 0.5–1

  - p \>\> n settings: use smaller values (0.01)

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

  Numeric vector of spike variance parameters, typically **decreasing**.
  Smaller v0 = stronger shrinkage for unlikely edges. The spike standard
  deviation `sqrt(v0)` sets the scale below which partial correlations
  are considered noise. For normalized data where partial correlations
  live in -1, 1, `v0 = 0.01` (spike SD = 0.1) provides a natural
  noise/signal boundary. The default `c(0.1, 0.03, 0.01)` provides a
  short warm-start ladder ending at this target. See
  [`vignette("parameter-exploration")`](https://mljaniczek.github.io/spikeyglass/articles/parameter-exploration.md)
  for guidance on choosing v0 and the
  [`make_v0_ladder`](https://mljaniczek.github.io/spikeyglass/reference/make_v0_ladder.md)
  helper for generating longer exploration ladders.

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

- warm:

  List of K warm-start precision matrices. Default `NULL`.

- warm.connected:

  Logical vector for warm-starting block structure. Default `NULL`.

- truncate:

  Numeric threshold below which entries are zeroed. Default 1e-5.

- normalize:

  Logical. If `TRUE`, mean-centers each variable. Default `FALSE`.

- c:

  Numeric diagonal regularization for initial precision estimate.
  Default 0.1.

- impute:

  Logical. If `TRUE`, imputes `NA`s via conditional MVN at each EM
  iteration. Default `TRUE`.

## Value

An object of class `"ssjgl"`, a list with elements:

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

  Numeric vector of imputed values, or NULL.

- missed:

  Matrix of (group, row, col) for missing values, or NULL.

## Details

The algorithm follows the dynamic posterior exploration strategy of Li
et al. (2019), iterating over a decreasing ladder of spike variance
parameters `v0s` with warm-starting between steps.

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
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = c(0.1, 0.03, 0.01),
             maxitr.em = 50, impute = FALSE)
#> Ladder= 1 v0 = 0.1 done. Time: 0
#> Ladder= 2 v0 = 0.03 done. Time: 0
#> Ladder= 3 v0 = 0.01 done. Time: 0
print(fit)
#> Spike-and-Slab Joint Graphical Lasso (SSJGL)
#>   Groups (K): 2
#>   Variables (p): 10
#>   v0 ladder steps: 3
#>   Total EM iterations: 25
#>   Total time: 0.3 seconds
#>   Group 1: 11 non-zero edges (from precision)
#>   Group 2: 11 non-zero edges (from precision)
#>   Edges with P(inclusion) > 0.5: 11
adj <- extract_adjacency(fit, threshold = 0.5)
sum(adj[[1]][upper.tri(adj[[1]])])  # estimated edge count
#> [1] 11
```
