# Plot the solution path

Plots the solution paths from ssjgl().

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

  list of v0 input

- obj:

  object created using ssjgl

- thres:

  default threshold for probability of edge inclusion

- normalize:

  True or False (default False) on if normalized covariance should be
  returned

- xlab:

  String for xlab. Default empty.

- ylab:

  string for y-axis label. Default empty.

- main:

  string for main title. Default empty.

- par:

  default c(1,2) to list layout of output plots

- ylim:

  default NULL and will calculate from range

- reverse:

  Default FALSE. If want to reverse direction of plot.

- position:

  Default "bottomright". For position of legend.

- ...:

  other parameters passed in to base plot

## Value

Invisible NULL. Called for side effect (plot).

## Examples

``` r
if (FALSE) { # \dontrun{
sim <- simulate_ssjgl_data(K = 2, p = 15, n = 100, seed = 42)
v0s <- make_v0_ladder(lambda1 = 0.5, n_steps = 5)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = v0s, maxitr.em = 50, impute = FALSE)
plot_path(v0s, fit, thres = 0.5,
          xlab = expression(v[0]), ylab = "Precision entries",
          main = c("Group 1", "Group 2"))
} # }
```
