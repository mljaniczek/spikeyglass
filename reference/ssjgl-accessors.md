# Shared-format accessors for ssjgl fits

Methods for the ggmtools generics
[`ggmtools::pip_table()`](https://rdrr.io/pkg/ggmtools/man/pip_table.html)
and
[`ggmtools::pcor_estimate()`](https://rdrr.io/pkg/ggmtools/man/pcor_estimate.html),
so that downstream code written for those generics works on `ssjgl` fits
as well as on fits from other packages (e.g. multiGGMr).

## Usage

``` r
# S3 method for class 'ssjgl'
pip_table(fit, v0_index = NULL, ...)

# S3 method for class 'ssjgl'
pcor_estimate(fit, v0_index = NULL, ...)
```

## Arguments

- fit:

  An `ssjgl` object.

- v0_index:

  Ladder step. Default: the last step.

- ...:

  Ignored.

## Value

[`pip_table()`](https://rdrr.io/pkg/ggmtools/man/pip_table.html): a data
frame with columns `i`, `j`, `node1`, `node2`, `group`, `pip`.
[`pcor_estimate()`](https://rdrr.io/pkg/ggmtools/man/pcor_estimate.html):
a named list of K partial-correlation matrices.

## Details

In SSJGL the edge indicator \\\delta\_{jk}\\ is shared by all groups, so
[`pip_table()`](https://rdrr.io/pkg/ggmtools/man/pip_table.html) gives
every group the same `pip` (P(delta = 1)).
[`pcor_estimate()`](https://rdrr.io/pkg/ggmtools/man/pcor_estimate.html)
returns the MAP partial correlations (the EM estimate), not a posterior
mean.

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 6, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, lambda0 = 1, lambda1 = 1, lambda2 = 1,
             standardize = TRUE, maxitr.em = 20, impute = FALSE)
head(pip_table(fit))
#>   i j node1 node2   group          pip
#> 1 1 2    V1    V2 Group_1 8.750011e-07
#> 2 1 3    V1    V3 Group_1 8.750011e-07
#> 3 2 3    V2    V3 Group_1 1.000000e+00
#> 4 1 4    V1    V4 Group_1 8.750011e-07
#> 5 2 4    V2    V4 Group_1 1.000000e+00
#> 6 3 4    V3    V4 Group_1 1.000000e+00
pcor_estimate(fit)[[1]][1:3, 1:3]
#>    V1        V2        V3
#> V1  1  0.000000  0.000000
#> V2  0  1.000000 -0.191912
#> V3  0 -0.191912  1.000000
```
