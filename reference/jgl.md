# Joint graphical lasso

Fits the joint graphical lasso (JGL) of Danaher, Wang & Witten (2014): K
precision matrices estimated together with a lasso penalty on each and a
fused or group penalty across them. This is a reimplementation of
`JGL::JGL()` (JGL 2.3.2, archived from CRAN in 2025) with the same
arguments, defaults and return value, using the C++ ADMM that
[`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
uses for its M-step. It reproduces `JGL::JGL()` to numerical precision
(see the package tests).

## Usage

``` r
jgl(
  Y,
  penalty = "fused",
  lambda1,
  lambda2,
  rho = 1,
  weights = "equal",
  penalize.diagonal = FALSE,
  maxiter = 500,
  tol = 1e-05,
  warm = NULL,
  return.whole.theta = FALSE,
  screening = "fast",
  truncate = 1e-05
)
```

## Arguments

- Y:

  List of K data matrices (n_k x p), same variables in the same column
  order. Each column is centred.

- penalty:

  `"fused"` (default) or `"group"`.

- lambda1:

  Lasso penalty: a scalar or a symmetric p x p matrix.

- lambda2:

  Fused or group penalty: a scalar or a symmetric p x p matrix.

- rho:

  ADMM step size. Default 1.

- weights:

  `"equal"` (default; \\w_k = 1\\), `"sample.size"` (\\w_k = n_k / \sum
  n_k\\), or a numeric vector of length K.

- penalize.diagonal:

  Logical. Whether the lasso penalty applies to the diagonal (and, for
  the group penalty, the group penalty too). Default `FALSE`.

- maxiter:

  Maximum ADMM iterations per block. Default 500.

- tol:

  ADMM convergence tolerance. Default 1e-5.

- warm:

  Optional list of K p x p warm-start precision matrices.

- return.whole.theta:

  Logical. If `TRUE`, `theta` holds the full p x p matrices; if `FALSE`
  (default), only the connected variables, with the unconnected ones'
  diagonal entries in `theta.unconnected`.

- screening:

  Only `"fast"` is supported. (JGL's `"memory.efficient"` option gives
  the same estimates with less memory.)

- truncate:

  Off-diagonal entries with absolute value below this are set to 0.
  Default 1e-5.

## Value

An object of class `"jgl"`, as from `JGL::JGL()`: a list with `theta`
(list of K precision matrices, with variable names, `V1, ..., Vp` if `Y`
has none), `connected` (logical vector; `FALSE` for variables with no
edges in any group) and, if `return.whole.theta = FALSE`,
`theta.unconnected` (list of K vectors, or `NULL`). Print and plot
methods for this class come from the JGL package, if it is loaded.

## Details

The objective is \$\$\sum_k w_k \[\mathrm{tr}(S_k\Theta_k) -
\log\det\Theta_k\] + \lambda_1 \sum_k \sum\_{i \ne j}
\|\theta^{(k)}\_{ij}\| + P_2(\Theta),\$\$ where \\S_k\\ is the (biased)
sample covariance of group k and \\P_2\\ is \\\lambda_2 \sum\_{k \< k'}
\sum\_{ij} \|\theta^{(k)}\_{ij} - \theta^{(k')}\_{ij}\|\\ (fused) or
\\\lambda_2 \sum\_{i \ne j} \sqrt{\sum_k (\theta^{(k)}\_{ij})^2}\\
(group). As in JGL, the fused penalty also applies to the diagonal, and
the lasso penalty applies to the diagonal only if
`penalize.diagonal = TRUE`. The problem is split into independent blocks
by JGL's exact screening rule before solving.

## References

Danaher, P., Wang, P. and Witten, D. M. (2014). The joint graphical
lasso for inverse covariance estimation across multiple classes.
*Journal of the Royal Statistical Society: Series B*, 76(2), 373–397.
[doi:10.1111/rssb.12033](https://doi.org/10.1111/rssb.12033)

## See also

[`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
for the spike-and-slab version with adaptive, edge-specific penalties.

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 100, seed = 1)
fit <- jgl(sim$data_list, penalty = "fused", lambda1 = 0.1, lambda2 = 0.05,
           return.whole.theta = TRUE)
round(fit$theta[[1]][1:4, 1:4], 2)
#>      V1    V2    V3    V4
#> V1 0.92  0.16  0.13  0.00
#> V2 0.16  0.89  0.27 -0.06
#> V3 0.13  0.27  0.80 -0.06
#> V4 0.00 -0.06 -0.06  0.88
# partial correlations, one matrix per group
pc <- lapply(fit$theta, precision_to_pcor)
```
