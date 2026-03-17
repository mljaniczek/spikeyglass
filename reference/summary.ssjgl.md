# Summarize an ssjgl object

Summarize an ssjgl object

## Usage

``` r
# S3 method for class 'ssjgl'
summary(object, ...)
```

## Arguments

- object:

  An object of class `ssjgl`.

- ...:

  Additional arguments (ignored).

## Value

A list of class `summary.ssjgl` with summary information.

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
#> Ladder= 1 v0 = 0.01 done. Time: 0
summary(fit)
#> SSJGL Summary
#>   K = 2 groups, p = 10 variables, 1 v0 steps
#>   Total time: 0.1 s (mean 0.1 s per step)
#> 
#> Edge counts per v0 step:
#>      Group1 Group2
#> v0_1     26     26
#> 
#> EM iterations per step: 10 
#> Pi_delta per step: 0.5778 
```
