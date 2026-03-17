# Extract partial correlation matrices from an ssjgl fit

Equivalent to `fitted(fit)`.

## Usage

``` r
extract_pcor(fit, v0_index = NULL)
```

## Arguments

- fit:

  An object of class `ssjgl`.

- v0_index:

  Integer index into the v0 ladder. Default `NULL` uses the last step.

## Value

A list of K partial correlation matrices (p x p).

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
#> Ladder= 1 v0 = 0.01 done. Time: 0
pcor <- extract_pcor(fit)
range(pcor[[1]])  # values in [-1, 1]
#> [1] -0.4336362  1.0000000
```
