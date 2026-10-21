# Extract precision matrices from an ssjgl fit

Equivalent to
[`extract_precision`](https://mljaniczek.github.io/spikeyglass/reference/extract_precision.md).

## Usage

``` r
# S3 method for class 'ssjgl'
coef(object, v0_index = NULL, ...)
```

## Arguments

- object:

  An object of class `ssjgl`.

- v0_index:

  Integer index into the v0 ladder. Default `NULL` uses the last step
  (most sparse).

- ...:

  Additional arguments (ignored).

## Value

A list of K precision matrices (p x p).

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
theta <- coef(fit)
str(theta)  # list of K precision matrices
#> List of 2
#>  $ : num [1:10, 1:10] 0.939 0.215 0.196 0.103 -0.309 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
#>  $ : num [1:10, 1:10] 1.016 0.245 0.285 -0.133 -0.154 ...
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
#>   .. ..$ : chr [1:10] "V1" "V2" "V3" "V4" ...
```
