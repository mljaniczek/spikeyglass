# Differential edges between two groups

Tests which edges differ between two groups of an
[`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
fit, by refitting
[`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
with the same settings on permuted or bootstrapped data.

## Usage

``` r
diff_edges(
  fit,
  groups = c(1, 2),
  score = c("pcor", "precision"),
  method = c("permutation", "bootstrap"),
  B = 100,
  alpha = 0.05,
  Y = NULL,
  v0_index = NULL,
  ...
)

# S3 method for class 'ssjgl_diff_edges'
print(x, n = 10, ...)
```

## Arguments

- fit:

  An `ssjgl` object.

- groups:

  Two distinct group indices (or group names) to compare. The difference
  is group 1 minus group 2.

- score:

  Scale of the comparison: `"pcor"` (partial correlations, default) or
  `"precision"` (precision-matrix entries).

- method:

  `"permutation"`

  :   (default) Rows of the two groups are pooled and the group labels
      shuffled (group sizes kept; any other groups are left as they
      are), and
      [`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
      is refitted `B` times. The null scores \\\|\Delta\|\\ are pooled
      across all edges and permutations. Each edge's p-value is \\(1 +
      \\\\null \ge \|\Delta\_{obs}\|\\) / (1 + \\null)\\, and the
      p-values are adjusted by Benjamini-Hochberg. `selected` is
      `p_adj < alpha`.

  `"bootstrap"`

  :   Rows are resampled with replacement within every group,
      [`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
      is refitted `B` times, and `lower`/`upper` are percentile \\1 -
      \alpha\\ intervals for \\\Delta\\. `selected` means the interval
      excludes 0. This gives no multiplicity control.

- B:

  Number of permutations or bootstrap resamples. Default 100.

- alpha:

  BH level (permutation) or 1 - interval coverage (bootstrap). Default
  0.05.

- Y:

  The list of data matrices `fit` was estimated from. If `NULL`
  (default), the `Y` argument of `fit$call` is evaluated in the calling
  environment; supply it explicitly if that object has changed or is not
  available.

- v0_index:

  Ladder step whose estimates are compared. Default: the last step.

- ...:

  Arguments passed to the
  [`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
  refits, overriding the settings stored in `fit` (e.g. `maxitr.em` to
  make the refits faster).

- x:

  An `ssjgl_diff_edges` object.

- n:

  Number of selected edges to print. Default 10.

## Value

A data frame of class `"ssjgl_diff_edges"`, one row per edge \\i \< j\\
in [`upper.tri()`](https://rdrr.io/r/base/lower.tri.html) order, with
columns

- i, j:

  Variable indices.

- node1, node2:

  Variable names.

- est_g1, est_g2:

  Estimates in the two groups on the `score` scale.

- diff:

  `est_g1 - est_g2`.

- pvalue, p_adj:

  Permutation p-value and its BH adjustment (`method = "permutation"`).

- lower, upper:

  Bootstrap percentile interval for `diff` (`method = "bootstrap"`).

- selected:

  Logical: called differential.

- prob_dissimilar:

  \\P(\xi = 1)\\ (only if the fit is doubly).

Attributes: `method`, `score`, `groups`, `group_names`, `B` (successful
refits), `alpha`, `v0_index`, and `null_scores` (the pooled null,
permutation) or `boot_diffs` (a `B` x edges matrix, bootstrap).

## Details

Refits use the arguments stored in `fit$args`, so they repeat the whole
v0 ladder of the original fit. The resampling indices are drawn in the
calling R session, so results are reproducible with
[`set.seed()`](https://rdrr.io/r/base/Random.html) and do not depend on
how the refits are run.

**Parallel refits.** If the future.apply package is installed, the
refits are run with
[`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html),
so they are parallel under any
[`future::plan()`](https://future.futureverse.org/reference/plan.html)
other than sequential, e.g.
`future::plan(future::multisession, workers = 4)`.

**`prob_dissimilar`.** With `doubly = TRUE` the table also has
`prob_dissimilar`, the EM's posterior probability \\P(\xi\_{jk} = 1)\\
that edge \\(j, k)\\ is in the slab of the cross-group similarity
penalty, i.e. is only weakly shrunk towards equality across groups. It
is shared by all groups, comes from the point-estimating EM, and is
*not* a calibrated probability that the edge differs between `groups`.
In practice it can be uninformative: in one real fit (TCGA PI3K pathway,
p = 28, group penalty), every edge with \\P(\delta = 1) \> 0.5\\ also
had \\P(\xi = 1) \> 0.5\\. Use the permutation p-values for inference.

**Column names.** Where the concepts match, the columns follow
multiGGMr's `diff_summary()` and `select_diff_edges()` (`i`, `j`,
`lower`, `upper`), so the shared columns of the two packages' tables can
be bound together.

## See also

[`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md),
[`plot.ssjgl_diff_edges()`](https://mljaniczek.github.io/spikeyglass/reference/plot.ssjgl_diff_edges.md)

## Examples

``` r
# \donttest{
sim <- simulate_ssjgl_data(K = 2, p = 8, n = 60, seed = 1)
fit <- ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
             standardize = TRUE, impute = FALSE)
set.seed(1)
de <- diff_edges(fit, Y = sim$data_list, B = 20)
de
#> <ssjgl_diff_edges>
#>   group 1 vs group 2, pcor scale, permutation (B = 20), v0 step 3
#>   4 of 28 edges selected at BH-adjusted p < 0.05
#>  i j node1 node2 est_g1  est_g2   diff  pvalue  p_adj selected
#>  1 2    V1    V2  0.579 -0.0574  0.636 0.00178 0.0125     TRUE
#>  4 5    V4    V5  0.193 -0.3917  0.584 0.00178 0.0125     TRUE
#>  2 3    V2    V3 -0.269  0.2658 -0.535 0.00178 0.0125     TRUE
#>  2 4    V2    V4 -0.182  0.2619 -0.444 0.00178 0.0125     TRUE
plot(de)

# }
```
