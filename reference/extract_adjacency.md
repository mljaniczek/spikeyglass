# Extract binary adjacency matrices from an ssjgl fit

Thresholds the edge inclusion probabilities to produce binary adjacency
matrices.

## Usage

``` r
extract_adjacency(fit, v0_index = NULL, threshold = 0.5)
```

## Arguments

- fit:

  An object of class `ssjgl`.

- v0_index:

  Integer index into the v0 ladder. Default `NULL` uses the last step.

- threshold:

  Numeric threshold for edge inclusion. Default 0.5.

## Value

A list of K binary adjacency matrices (p x p, 0 diagonal).

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
#> Ladder= 1 v0 = 0.01 done. Time: 0
adj <- extract_adjacency(fit, threshold = 0.5)
sum(adj[[1]][upper.tri(adj[[1]])])  # edge count
#> [1] 26
```
