# spikeyglass: Bayesian Joint Spike-and-Slab Graphical Lasso

Estimates multiple related Gaussian graphical models (one precision
matrix per group) with the Bayesian joint spike-and-slab graphical lasso
of Li, McCormick and Clark (2019)
[arXiv:1805.07051](https://arxiv.org/abs/1805.07051). An EM algorithm
with spike-and-slab Laplace priors turns the fused or group graphical
lasso into an edge-adaptive penalty, explored along a ladder of spike
variances, and returns posterior edge inclusion probabilities. Also
provides missing-data imputation, permutation and bootstrap tests for
differential edges, cross-validation, and evaluation tools.

## See also

Useful links:

- <https://mljaniczek.github.io/spikeyglass/>

- <https://github.com/mljaniczek/spikeyglass>

- Report bugs at <https://github.com/mljaniczek/spikeyglass/issues>

## Author

**Maintainer**: Margaret Janiczek <mjaniczek@umass.edu>

Authors:

- Zehang Richard Li <lizehang@gmail.com>

Other contributors:

- Patrick Danaher (Author of JGL; vendored and ported routines, see
  inst/COPYRIGHTS) \[copyright holder\]
