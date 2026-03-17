# Plot stability of graph structure across the v0 ladder

Visualizes how the estimated graph changes across v0 steps to help
identify when the solution has stabilized. Shows edge counts, mean
inclusion probability, and (optionally) change in edges between
consecutive steps.

## Usage

``` r
plot_stability(
  fit,
  v0s,
  threshold = 0.5,
  what = c("edges", "prob", "change"),
  ...
)
```

## Arguments

- fit:

  An object of class `ssjgl`.

- v0s:

  Numeric vector of v0 values used in the fit (same as passed to
  [`ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)).

- threshold:

  Numeric threshold for counting edges from inclusion probabilities.
  Default 0.5.

- what:

  Character vector of panels to plot. Options: `"edges"` (edge count per
  group), `"prob"` (mean inclusion probability), `"change"` (absolute
  change in edges between steps). Default plots all three.

- ...:

  Additional arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisible list with computed stability data:

- v0s:

  The v0 values.

- edge_counts:

  Matrix of edge counts (n_steps x K).

- mean_prob:

  Numeric vector of mean inclusion probabilities.

- edge_changes:

  Numeric vector of edge count changes between steps (length n_steps -
  1).

## See also

[`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md),
[`make_v0_ladder()`](https://mljaniczek.github.io/spikeyglass/reference/make_v0_ladder.md),
[`plot_path()`](https://mljaniczek.github.io/spikeyglass/reference/plot_path.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sim <- simulate_ssjgl_data(K = 2, p = 15, n = 100, seed = 42)
v0s <- make_v0_ladder(lambda1 = 0.5, n_steps = 10)
fit <- ssjgl(Y = sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = v0s, normalize = TRUE)
plot_stability(fit, v0s)
} # }
```
