# Compute confusion matrix metrics at a threshold

Given a score matrix (e.g., edge inclusion probabilities) and a binary
truth matrix, computes TP, FP, TN, FN, TPR, FPR, precision, and F1 at a
given threshold. Only the upper triangle is used by default.

## Usage

``` r
confusion_at_threshold(
  score_mat,
  truth_mat,
  threshold = 0.5,
  upper_only = TRUE
)
```

## Arguments

- score_mat:

  Numeric matrix of scores (e.g., probabilities in \[0,1\]).

- truth_mat:

  Binary matrix of true edges (1 = edge, 0 = no edge).

- threshold:

  Numeric threshold; scores \>= threshold are predicted as edges.
  Default 0.5.

- upper_only:

  Logical; if TRUE (default), only use upper triangle.

## Value

A named list with elements: TP, FP, TN, FN, TPR (sensitivity), FPR
(1-specificity), precision, F1.

## Examples

``` r
# Simulated score and truth matrices
truth <- matrix(c(0,1,0, 1,0,1, 0,1,0), 3, 3)
scores <- matrix(c(0,0.9,0.1, 0.9,0,0.8, 0.1,0.8,0), 3, 3)
confusion_at_threshold(scores, truth, threshold = 0.5)
#> $TP
#> [1] 2
#> 
#> $FP
#> [1] 0
#> 
#> $TN
#> [1] 1
#> 
#> $FN
#> [1] 0
#> 
#> $TPR
#> [1] 1
#> 
#> $FPR
#> [1] 0
#> 
#> $precision
#> [1] 1
#> 
#> $F1
#> [1] 1
#> 
```
