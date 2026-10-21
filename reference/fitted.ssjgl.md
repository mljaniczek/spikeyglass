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
pcor <- fitted(fit)
str(pcor)  # list of K partial correlation matrices
#> List of 2
#>  $ : num [1:10, 1:10] 1 -0.236 -0.262 -0.258 0 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
#>  $ : num [1:10, 1:10] 1 -0.236 -0.326 0.097 0 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
```
