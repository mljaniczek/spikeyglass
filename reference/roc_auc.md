# Compute ROC curve and AUC

Computes the ROC curve (TPR vs FPR at varying thresholds) and the area
under the curve (AUC) for edge selection. Only the upper triangle of the
matrices is used.

## Usage

``` r
roc_auc(score_mat, truth_mat)
```

## Arguments

- score_mat:

  Numeric matrix of continuous scores (e.g., edge inclusion
  probabilities or absolute partial correlations).

- truth_mat:

  Binary matrix of true edges (1 = edge, 0 = no edge).

## Value

A list with elements:

- FPR:

  Numeric vector of false positive rates.

- TPR:

  Numeric vector of true positive rates.

- thresholds:

  Numeric vector of thresholds used.

- AUC:

  Scalar area under the ROC curve (trapezoidal rule).

## Examples

``` r
truth <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
scores <- matrix(c(0,.9,.1,.05, .9,0,.8,.1, .1,.8,0,.7, .05,.1,.7,0), 4, 4)
roc <- roc_auc(scores, truth)
roc$AUC
#> [1] 1
```
