# Compute graph recovery metrics (single group)

Compares an estimated precision matrix to the truth, computing
edge-level metrics (TP, FP, TN, FN), sum of squared errors, L1 norm, and
Kullback-Leibler divergence.

## Usage

``` r
getmetric(est, truth, graph)
```

## Arguments

- est:

  Estimated precision matrix (p x p).

- truth:

  True precision matrix (p x p).

- graph:

  Binary adjacency matrix of the true graph (p x p, 0 diagonal).

## Value

A named list with elements:

- SSE:

  Sum of squared errors between est and truth (off-diagonal).

- L1:

  L1 norm of the estimated precision (off-diagonal).

- nedges:

  Number of non-zero edges in the estimate.

- tp:

  True positives (edges correctly identified).

- fp:

  False positives (spurious edges).

- fn:

  False negatives (missed edges).

- tn:

  True negatives (correctly absent edges).

- dKL:

  Kullback-Leibler divergence KL(est \|\| truth).

## Details

This is a legacy function from the reference implementation. For new
code, use
[`compute_metrics`](https://mljaniczek.github.io/spikeyglass/reference/compute_metrics.md)
which handles multi-group fits directly.

## See also

[`compute_metrics`](https://mljaniczek.github.io/spikeyglass/reference/compute_metrics.md)
for the recommended multi-group interface.

## Examples

``` r
# Single-group metric computation
p <- 5
truth <- diag(p)
truth[1,2] <- truth[2,1] <- -0.3
est <- truth + matrix(rnorm(p*p, 0, 0.01), p, p)
est <- (est + t(est)) / 2
graph <- (truth != 0) * 1L; diag(graph) <- 0L
getmetric(est, truth, graph)
#> $SSE
#> [1] 0.0008671386
#> 
#> $L1
#> [1] 0.6981802
#> 
#> $nedges
#> [1] 10
#> 
#> $tp
#> [1] 1
#> 
#> $fp
#> [1] 9
#> 
#> $fn
#> [1] 0
#> 
#> $tn
#> [1] 0
#> 
#> $dKL
#> [1] 2.500406
#> 
```
