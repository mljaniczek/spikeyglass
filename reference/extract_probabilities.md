# Extract edge inclusion probabilities from an ssjgl fit

Extract edge inclusion probabilities from an ssjgl fit

## Usage

``` r
extract_probabilities(fit, v0_index = NULL)
```

## Arguments

- fit:

  An object of class `ssjgl`.

- v0_index:

  Integer index into the v0 ladder. Default `NULL` uses the last step.

## Value

A list with elements:

- prob1:

  p x p matrix of edge inclusion probabilities P(delta=1).

- prob2:

  p x p matrix of non-similarity probabilities P(xi=1), or NULL if not
  doubly spike-and-slab.

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
#> Ladder= 1 v0 = 0.01 done. Time: 0
probs <- extract_probabilities(fit)
# Edges with > 50% inclusion probability
sum(probs$prob1[upper.tri(probs$prob1)] > 0.5)
#> [1] 26
```
