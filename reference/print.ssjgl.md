# Print an ssjgl object

Print an ssjgl object

## Usage

``` r
# S3 method for class 'ssjgl'
print(x, ...)
```

## Arguments

- x:

  An object of class `ssjgl`.

- ...:

  Additional arguments (ignored).

## Value

Invisible `x`.

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
print(fit)
#> Spike-and-Slab Joint Graphical Lasso (SSJGL)
#>   Groups (K): 2
#>   Variables (p): 10
#>   v0 ladder steps: 1
#>   Total EM iterations: 11
#>   Total time: 0.1 seconds
#>   Group 1: 24 non-zero edges (from precision)
#>   Group 2: 24 non-zero edges (from precision)
#>   Edges with P(inclusion) > 0.5: 24
```
