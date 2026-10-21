# Plot the solution path

Plots each off-diagonal precision entry (or, with `normalize = TRUE`,
partial correlation) against v0 along the ladder, one panel per group.
Entries whose PIP is below `thres` at a step are drawn as 0.

## Usage

``` r
plot_path(
  v0s,
  obj,
  thres = 0.5,
  normalize = FALSE,
  xlab = "",
  ylab = "",
  main = "",
  par = c(1, 2),
  ylim = NULL,
  reverse = FALSE,
  position = "bottomright",
  ...
)
```

## Arguments

- v0s:

  The v0 values of the fit (`obj$v0s`).

- obj:

  An `ssjgl` object.

- thres:

  PIP threshold. Default 0.5.

- normalize:

  Logical. If `TRUE`, plot partial correlations instead of precision
  entries. Default `FALSE`.

- xlab:

  String for xlab. Default empty.

- ylab:

  string for y-axis label. Default empty.

- main:

  string for main title. Default empty.

- par:

  Layout `c(rows, cols)` for the K panels, passed to `par(mfrow = )` and
  restored on exit. Default `c(1, 2)`; `NULL` leaves the layout alone.

- ylim:

  default NULL and will calculate from range

- reverse:

  Default FALSE. If want to reverse direction of plot.

- position:

  Legend position, or `"none"`. Default `"bottomright"`.

- ...:

  other parameters passed in to base plot

## Value

Invisible NULL. Called for side effect (plot).

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 8, n = 100, seed = 42)
fit <- ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
             v0s = make_v0_ladder(1, n_steps = 5, min_ratio = 5, max_ratio = 200),
             standardize = TRUE, maxitr.em = 50, impute = FALSE)
plot_path(fit$v0s, fit, thres = 0.5,
          xlab = expression(v[0]), ylab = "Precision entries",
          main = c("Group 1", "Group 2"))
```
