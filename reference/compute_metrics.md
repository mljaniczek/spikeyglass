# Compute comprehensive evaluation metrics for an ssjgl fit

Given an ssjgl fit object and the true adjacency/precision matrices,
computes edge-level metrics (TPR, FPR, AUC) and estimation error
(Frobenius norm, KL divergence).

## Usage

``` r
compute_metrics(
  fit,
  true_adj,
  true_omega = NULL,
  v0_index = NULL,
  threshold = 0.5
)
```

## Arguments

- fit:

  An object of class `ssjgl`.

- true_adj:

  List of K binary adjacency matrices (true graph structure).

- true_omega:

  List of K true precision matrices. Default `NULL`.

- v0_index:

  Integer index into the v0 ladder. Default `NULL` uses the last step.

- threshold:

  Threshold for binarizing edge probabilities. Default 0.5.

## Value

A list with elements:

- per_group:

  List of K sublists, each with TP, FP, TN, FN, TPR, FPR, precision, F1,
  frobenius_norm, and KL_divergence.

- roc:

  List of K ROC objects (from
  [`roc_auc`](https://mljaniczek.github.io/spikeyglass/reference/roc_auc.md)).

- overall:

  Named list with mean TPR, mean FPR, mean AUC across groups.

## Examples

``` r
sim <- simulate_ssjgl_data(K = 2, p = 10, n = 50, seed = 1)
fit <- ssjgl(sim$data_list, penalty = "fused",
             lambda0 = 1, lambda1 = 0.5, lambda2 = 0.5,
             v0s = 0.01, maxitr.em = 10, impute = FALSE)
#> Ladder= 1 v0 = 0.01 done. Time: 0
m <- compute_metrics(fit, sim$adj_list, sim$Omega_list)
m$overall
#> $mean_TPR
#> [1] 0.9428105
#> 
#> $mean_FPR
#> [1] 0.3452381
#> 
#> $mean_AUC
#> [1] 0.8783112
#> 
```
