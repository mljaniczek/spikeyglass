# Generate a v0 ladder for exploring sparsity levels

Creates a decreasing sequence of spike variance values (`v0`) for use
with
[`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md).
This is primarily useful for **exploring** how sparsity changes across
v0 values, for diagnostics, or for replicating the full dynamic
posterior exploration strategy of Li et al. (2019).

## Usage

``` r
make_v0_ladder(
  lambda1,
  n_steps = 10,
  min_ratio = 5,
  max_ratio = 500,
  start_sparse = TRUE
)
```

## Arguments

- lambda1:

  Numeric scalar; the off-diagonal penalty used in
  [`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md).
  The v0 ladder is scaled to this value.

- n_steps:

  Integer number of v0 values in the ladder. Default 10.

- min_ratio:

  Numeric minimum effective penalty ratio `lambda1/v0` at the first
  (densest) step. Default 5.

- max_ratio:

  Numeric maximum effective penalty ratio `lambda1/v0` at the last
  (sparsest) step. Default 500.

- start_sparse:

  Logical. If `TRUE` (default), the first v0 is the largest (weakest
  penalty) and the sequence is decreasing, which is the correct
  direction for warm-starting. If `FALSE`, returns an increasing
  sequence.

## Value

A numeric vector of length `n_steps` with v0 values, decreasing by
default.

## Details

For routine use, a short ladder like `v0s = c(0.1, 0.03, 0.01)` is
recommended (see
[`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
defaults and
[`vignette("parameter-exploration")`](https://mljaniczek.github.io/spikeyglass/articles/parameter-exploration.md)
for details).

The effective penalty for edge sparsity is `lambda1/v0`. This function
creates a log-spaced ladder of v0 values such that the effective penalty
ranges from `min_ratio` to `max_ratio`. Log-spacing concentrates more
steps in the sparse (small v0) end where the model is most sensitive.

**Interpreting v0**: The spike standard deviation `sqrt(v0)` sets the
scale below which partial correlations are treated as noise. For
normalized data where partial correlations live in -1, 1:

- `v0 = 0.1` (spike SD = 0.32): weak sparsity, broad spike

- `v0 = 0.01` (spike SD = 0.10): moderate sparsity, good default

- `v0 = 0.001` (spike SD = 0.03): aggressive sparsity

- `v0 = 0.0001` (spike SD = 0.01): very aggressive, may over-sparsify

## See also

[`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md),
[`plot_stability()`](https://mljaniczek.github.io/spikeyglass/reference/plot_stability.md)

## Examples

``` r
# Exploration ladder for lambda1 = 0.5
v0s <- make_v0_ladder(lambda1 = 0.5, n_steps = 10)
data.frame(v0 = v0s, spike_sd = sqrt(v0s),
           eff_penalty = 0.5 / v0s)
#>             v0   spike_sd eff_penalty
#> 1  0.100000000 0.31622777    5.000000
#> 2  0.059948425 0.24484367    8.340503
#> 3  0.035938137 0.18957357   13.912797
#> 4  0.021544347 0.14677993   23.207944
#> 5  0.012915497 0.11364637   38.713184
#> 6  0.007742637 0.08799225   64.577483
#> 7  0.004641589 0.06812921  107.721735
#> 8  0.002782559 0.05274997  179.690683
#> 9  0.001668101 0.04084239  299.742125
#> 10 0.001000000 0.03162278  500.000000
```
