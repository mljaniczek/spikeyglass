
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spikeyglass

<!-- badges: start -->
<!-- badges: end -->

The goal of spikeyglass is to implement the method Bayesian
Spike-and-Slab Joint Graphical Lasso (SSJGL) in an easy-to-use R
package.

Method paper:

Li ZR, McCormick TH, Clark SJ. Bayesian Joint Spike-and-Slab Graphical
Lasso. Proc Mach Learn Res. 2019 Jun;97:3877-3885. PMID: 33521648;
PMCID: PMC7845917.

Code based on: <https://github.com/richardli/SSJGL>

Acknowledgement The code to implement GGL and FGL are adapted from the R
package [JGL](https://cran.r-project.org/web/packages/JGL/index.html) by
Patrick Danaher.

## Installation

You can install the development version of spikeyglass from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mljaniczek/spikeyglass")
```

## Basic usage

Input list of sample by feature matrices (one matrix for each group),
specify group or fused penalty, and specify priors for simultaneous
shrinkage and model selection.

``` r
fit1 <- ssjgl(data,penalty,lambda0, lambda1,lambda2, doubly=TRUE, normalize=TRUE)
```
