# CLAUDE.md — spikeyglass

## Package Overview

`spikeyglass` implements the **Bayesian Spike-and-Slab Joint Graphical Lasso (SSJGL)** from Li, McCormick & Clark (2019, ICML). The method estimates multiple related Gaussian precision matrices (inverse covariance) across K groups simultaneously, using an EM algorithm with spike-and-slab priors for adaptive, edge-specific penalization.

**Paper**: Li et al. (2019) "Bayesian Joint Spike-and-Slab Graphical Lasso" (PMC7845917)
**Reference code**: github.com/richardli/SSJGL
**Sibling package**: github.com/mljaniczek/multiGGMr (Peterson et al. 2015, Bayesian GGM with MRF priors)

## Method Summary

### Algorithm: EM with dynamic posterior exploration

1. **Outer loop**: Iterate over a decreasing ladder of spike variance `v0s` with warm-starting between steps (dynamic posterior exploration, Section 3.3 of paper). Smaller v0 → stronger shrinkage for unlikely edges.

2. **E-step** (`gete()` / `gete.doubly()`): Compute posterior probabilities of edge inclusion P(delta=1) and (if doubly) similarity P(xi=1). Produces adaptive penalty weights `d = (1-prob)/v0 + prob/v1` for the M-step. Uses log-sum-exp trick for numerical stability.

3. **M-step** (`JGL.adaptive()` + `admm.iters.adaptive()`): ADMM-based Joint Graphical Lasso with matrix-valued lambda1/lambda2 penalties (edge-specific weights from E-step). Theta update via eigendecomposition, Z update via JGL's `flsa2`/`dsgl`.

4. **Pi update**: Beta posterior mode: `pi_delta = (a + sum_edges(prob1) - 1) / (a + b + n_edges - 2)`.

5. **Missing data**: Optional conditional MVN imputation at each EM iteration (`getmissing()`).

### Two penalty types
- **Fused** (`penalty="fused"`): Penalizes pairwise differences |Theta_k - Theta_l| across groups. Encourages similar precision matrices.
- **Group** (`penalty="group"`): Penalizes L2 norm sqrt(sum_k Theta_k^2) across groups. Encourages shared sparsity pattern.

### Doubly spike-and-slab
When `doubly=TRUE`, separate indicators for edge existence (delta) and cross-group similarity (xi), giving three configurations: (0,0), (1,0), (1,1) with corresponding penalty weights for lambda1 and lambda2.

## Package Architecture

### Core algorithm
- `R/ssjgl.R` — Main `ssjgl()` function: EM algorithm with v0 ladder
- `R/utils-gete.R` — `gete()`, `gete.doubly()`: E-step computation; `getmissing()`: MVN imputation
- `R/utils-admm.R` — `admm.iters.adaptive()`: ADMM iterations with matrix-valued penalties
- `R/utils-ssjgl.R` — `JGL.adaptive()`: Adapted JGL with block screening; `negloglik_Gaussian()`, `precision_to_pcor()`, `SSJGL_select_v0_cv()`, `SSJGL_final_with_pcor_CI()`, `SSJGL_CV_final_pcorCI()`

### S3 methods and helpers
- `R/methods.R` — `print.ssjgl`, `summary.ssjgl`, `coef.ssjgl`, `fitted.ssjgl`
- `R/helpers.R` — `extract_precision()`, `extract_adjacency()`, `extract_pcor()`, `extract_probabilities()`

### Evaluation and visualization
- `R/utils-eval.R` — `confusion_at_threshold()`, `roc_auc()`, `compute_metrics()`, `plot_roc()`
- `R/plot_path.R` — `plot_path()`: Solution path visualization
- `R/getmetric.R` — `getmetric()`: Legacy single-group metrics
- `R/getdiffmetric.R` — `getdiffmetric()`: Differential edge metrics

### Data simulation
- `R/utils-datagen.R` — `simulate_ssjgl_data()`: Generate K-group data with band/random/hub/scale-free graphs

### Package infrastructure
- `R/spikeyglass-package.R` — Package-level imports
- `R/data.R` — Dataset documentation (simdat)

## Key Hyperparameter Defaults

| Parameter | Default | Notes |
|-----------|---------|-------|
| `v0s` | `seq(0.0001, 0.01, len=10)` | Spike variance ladder (decreasing = more sparse) |
| `v1` | 1 | Slab variance |
| `lambda0` | — | Diagonal penalty (user must specify) |
| `lambda1` | — | Off-diagonal sparsity penalty (user must specify) |
| `lambda2` | — | Cross-group similarity penalty (user must specify) |
| `a`, `b` | 1, 1 | Beta prior for pi_delta. Use b=p for sparse prior |
| `doubly` | FALSE | Whether to use doubly spike-and-slab |
| `maxitr.em` | 500 | Max EM iterations per v0 step |
| `tol.em` | 1e-4 | EM convergence tolerance (max abs change in Theta) |

## Dependencies

- `JGL` — Core ADMM routines (flsa2, dsgl, penalty.as.matrix, admm.iters.unconnected)
- `igraph` — Block structure detection in screening step
- `MASS` — `mvrnorm()` for data simulation
- `graphics`, `stats` — Base R plotting and statistics

## Build & Check Commands

```bash
# Generate documentation
Rscript -e "devtools::document()"

# Run tests
Rscript -e "devtools::test()"

# Full R CMD check
Rscript -e "devtools::check()"

# Build vignettes
Rscript -e "devtools::build_vignettes()"

# Install locally
R CMD INSTALL .
```

## Known Issues & Bug Fixes

1. **Pi_delta double-counting (fixed)**: Original code used `sum(prob1)` which double-counts symmetric entries. Fixed to `sum(prob1) / 2`. Note: the original reference code (richardli/SSJGL) has the same issue.

2. **Convergence criterion (fixed)**: Original used `max(diff, (theta_last[[k]] - theta[[k]])^2)` which computes element-wise squared differences. Fixed to `max(abs(...))` for consistent tolerance semantics.

3. **JGL internal access**: Package uses `JGL:::` to access unexported functions (flsa2, dsgl, gcrit, crit, penalty.as.matrix, admm.iters.unconnected). These may break if JGL changes internals.

## Cross-project Notes (multiGGMr)

Shared utility functions adapted between packages (keeping consistent signatures):
- `precision_to_pcor()` — Precision to partial correlation conversion
- `confusion_at_threshold()` / `roc_auc()` — Evaluation metrics
- `simulate_ssjgl_data()` — Data simulation (adapted from multiGGMr's `simulate_multiggm()`)

Both packages are for multiple GGM estimation but use different methods:
- `spikeyglass`: EM + spike-and-slab + JGL (Li et al. 2019)
- `multiGGMr`: MCMC + MRF priors + G-Wishart (Peterson et al. 2015)
