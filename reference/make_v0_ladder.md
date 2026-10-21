# Generate a v0 ladder

Creates a decreasing sequence of spike variances `v0` for
[`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md),
log-spaced in the ratio `lambda1/v0`. With the defaults it returns the
ladder that
[`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
uses when `v0s` is not given: three steps from `lambda1/v0 = 10` to
`100`, i.e. `lambda1 / c(10, 31.6, 100)`.

## Usage

``` r
make_v0_ladder(
  lambda1,
  n_steps = 3,
  min_ratio = 10,
  max_ratio = 100,
  start_sparse = TRUE
)
```

## Arguments

- lambda1:

  Positive scalar; the off-diagonal penalty used in
  [`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md).
  The ladder is scaled to this value.

- n_steps:

  Integer number of v0 values in the ladder. Default 3.

- min_ratio:

  Ratio `lambda1/v0` at the first (weakest-spike, densest) step. Default
  10.

- max_ratio:

  Ratio `lambda1/v0` at the last (strongest-spike, sparsest) step.
  Default 100.

- start_sparse:

  Logical. If `TRUE` (default), the sequence is decreasing in v0
  (increasing in `lambda1/v0`), which is the direction the warm starts
  of the dynamic posterior exploration need. If `FALSE`, returns an
  increasing sequence.

## Value

A numeric vector of `n_steps` v0 values, decreasing by default.

## Details

In SSJGL the spike and slab are *Laplace* priors on each off-diagonal
precision entry \\\omega\_{jk}\\, with rates `lambda1/v0` (spike) and
`lambda1/v1` (slab); the spike's scale is `v0/lambda1`. So `v0` only
matters through `lambda1/v0`, and this function is parameterised by that
ratio. (`sqrt(v0)` is not a standard deviation of anything in the
model.)

Along the ladder, the spike penalty on a non-edge grows from
`lambda1 * min_ratio` to `lambda1 * max_ratio`. In the M-step it
soft-thresholds sample covariances (sample correlations, for
standardised data) at roughly `lambda1/(n v0)`, for group sample size
`n`.

The default end point `lambda1/v0 = 100` was chosen by a simulation
study (see
[`vignette("parameter-exploration")`](https://mljaniczek.github.io/spikeyglass/articles/parameter-exploration.md)):
by that point the solution path has stabilised and F1 at PIP \> 0.5 has
reached its plateau, while longer ladders (up to 500) took longer and
gave no better graphs. For exploring the whole path, use e.g.
`n_steps = 10, min_ratio = 5, max_ratio = 500` with
[`plot_stability`](https://mljaniczek.github.io/spikeyglass/reference/plot_stability.md)
and
[`plot_path`](https://mljaniczek.github.io/spikeyglass/reference/plot_path.md).

## See also

[`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md),
[`plot_stability()`](https://mljaniczek.github.io/spikeyglass/reference/plot_stability.md)

## Examples

``` r
# Default ladder (what ssjgl() uses) for lambda1 = 1
make_v0_ladder(lambda1 = 1)
#> [1] 0.10000000 0.03162278 0.01000000
# A longer exploration ladder
v0s <- make_v0_ladder(lambda1 = 1, n_steps = 10, min_ratio = 5, max_ratio = 500)
data.frame(v0 = v0s, ratio = 1 / v0s, spike_scale = v0s / 1)
#>             v0      ratio spike_scale
#> 1  0.200000000   5.000000 0.200000000
#> 2  0.119896850   8.340503 0.119896850
#> 3  0.071876273  13.912797 0.071876273
#> 4  0.043088694  23.207944 0.043088694
#> 5  0.025830993  38.713184 0.025830993
#> 6  0.015485274  64.577483 0.015485274
#> 7  0.009283178 107.721735 0.009283178
#> 8  0.005565119 179.690683 0.005565119
#> 9  0.003336201 299.742125 0.003336201
#> 10 0.002000000 500.000000 0.002000000
```
