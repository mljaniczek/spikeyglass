# Compute differential edge metrics across groups

Evaluates how well the method recovers edges that differ between groups.
Computes both graph-level differences (binary edge presence) and
continuous-value differences (precision matrix entries).

## Usage

``` r
getdiffmetric(est, truth, graph, tol)
```

## Arguments

- est:

  List of K estimated precision matrices.

- truth:

  List of K true precision matrices.

- graph:

  List of K true binary adjacency matrices.

- tol:

  Numeric tolerance for declaring continuous differences. Entries with
  `abs(diff) > tol` are considered different.

## Value

A named list with elements:

- tp.gdiff:

  True positives for graph-level differences.

- fp.gdiff:

  False positives for graph-level differences.

- fn.gdiff:

  False negatives for graph-level differences.

- tp.diff:

  True positives for continuous-value differences.

- fp.diff:

  False positives for continuous-value differences.

- fn.diff:

  False negatives for continuous-value differences.

## Details

This is a legacy function from the reference implementation.

## Examples

``` r
# Compare differential edges between two groups
p <- 5
est1 <- diag(p); est1[1,2] <- est1[2,1] <- -0.3
est2 <- diag(p); est2[1,2] <- est2[2,1] <- 0  # edge absent in group 2
graph1 <- (est1 != 0) * 1L; diag(graph1) <- 0L
graph2 <- (est2 != 0) * 1L; diag(graph2) <- 0L
getdiffmetric(list(est1, est2), list(est1, est2),
              list(graph1, graph2), tol = 0.01)
#> $fn.diff
#> [1] 0
#> 
#> $fp.diff
#> [1] 0
#> 
#> $tp.diff
#> [1] 1
#> 
#> $fn.gdiff
#> [1] 0
#> 
#> $fp.gdiff
#> [1] 0
#> 
#> $tp.gdiff
#> [1] 1
#> 
```
