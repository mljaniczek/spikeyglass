# Plot ROC curve

Plots the ROC curve from a
[`roc_auc`](https://mljaniczek.github.io/spikeyglass/reference/roc_auc.md)
result.

## Usage

``` r
plot_roc(roc_obj, main = "ROC Curve", ...)
```

## Arguments

- roc_obj:

  A list returned by
  [`roc_auc`](https://mljaniczek.github.io/spikeyglass/reference/roc_auc.md).

- main:

  Title for the plot. Default `"ROC Curve"`.

- ...:

  Additional arguments passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisible NULL. Called for side effect (plot).

## Examples

``` r
truth <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), 4, 4)
scores <- matrix(c(0,.9,.1,.05, .9,0,.8,.1, .1,.8,0,.7, .05,.1,.7,0), 4, 4)
roc <- roc_auc(scores, truth)
plot_roc(roc)
```
