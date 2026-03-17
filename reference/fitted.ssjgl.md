# Extract partial correlations from an ssjgl fit

Equivalent to
[`extract_pcor`](https://mljaniczek.github.io/spikeyglass/reference/extract_pcor.md).

## Usage

``` r
# S3 method for class 'ssjgl'
fitted(object, v0_index = NULL, ...)
```

## Arguments

- object:

  An object of class `ssjgl`.

- v0_index:

  Integer index into the v0 ladder. Default `NULL` uses the last step.

- ...:

  Additional arguments (ignored).

## Value

A list of K partial correlation matrices (p x p).

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
#> Ladder= 1 v0 = 0.01 done. Time: 0
pcor <- fitted(fit)
str(pcor)  # list of K partial correlation matrices
#> List of 2
#>  $ : num [1:10, 1:10] 1 -0.205 -0.257 -0.128 0.244 ...
#>  $ : num [1:10, 1:10] 1 -0.229 -0.32 0.113 0.177 ...
```
