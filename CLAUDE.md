# CLAUDE.md — spikeyglass

## Package Overview

`spikeyglass` implements the **Bayesian Spike-and-Slab Joint Graphical Lasso (SSJGL)** from Li, McCormick & Clark (2019, ICML). The method estimates multiple related Gaussian precision matrices (inverse covariance) across K groups simultaneously, using an EM algorithm with spike-and-slab priors for adaptive, edge-specific penalization.

**Paper**: Li et al. (2019) "Bayesian Joint Spike-and-Slab Graphical Lasso" (PMC7845917)
**Reference code**: github.com/richardli/SSJGL
**Sibling package**: github.com/mljaniczek/multiGGMr (Peterson et al. 2015, Bayesian GGM with MRF priors)

## Method Summary

### Algorithm: EM with dynamic posterior exploration

1. **Outer loop**: Iterate over a decreasing ladder of spike variance `v0s` with warm-starting between steps (dynamic posterior exploration, Section 3.3 of paper). Smaller v0 → larger spike rate `lambda1/v0` → stronger shrinkage for unlikely edges. M-step soft-thresholds spike edges at ~`lambda1/(n v0)` on the covariance scale.

2. **E-step** (`gete()` / `gete.doubly()`): Compute posterior probabilities of edge inclusion P(delta=1) and (if doubly) similarity P(xi=1). Produces adaptive penalty weights `d = (1-prob)/v0 + prob/v1` for the M-step. Uses log-sum-exp trick for numerical stability.

3. **M-step** (`JGL.adaptive()` + `admm.iters.adaptive()`): ADMM-based Joint Graphical Lasso with matrix-valued lambda1/lambda2 penalties (edge-specific weights from E-step). Theta update via eigendecomposition, Z update via C++ ports of JGL's `flsa2`/`flsa.general`/`dsgl`.

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
- `R/jgl-vendored.R` — `penalty.as.matrix()`, `admm.iters.unconnected.S()` (from JGL 2.3.2, MIT)
- `R/utils-ssjgl.R` — `JGL.adaptive()`: Adapted JGL with block screening; `negloglik_Gaussian()`, `precision_to_pcor()`, `SSJGL_select_v0_cv()`, `SSJGL_final_with_pcor_CI()`, `SSJGL_CV_final_pcorCI()`

### S3 methods and helpers
- `R/methods.R` — `print.ssjgl`, `summary.ssjgl`, `coef.ssjgl`, `fitted.ssjgl`
- `R/helpers.R` — `extract_precision()`, `extract_adjacency()`, `extract_pcor()`, `extract_probabilities()`

### Differential edges
- `R/diff_edges.R` — `diff_edges()`: permutation (pooled null, BH) or bootstrap inference on group differences; refits via `fit$args`, optionally parallel through future.apply; `print`/`plot` methods. Edge-table columns follow multiGGMr where concepts match (`i`, `j`, `lower`, `upper`).

### Evaluation and visualization
- `R/utils-eval.R` — `compute_metrics()`
- `R/reexports.R` — re-exports from ggmtools: `precision_to_pcor()`, `confusion_at_threshold()`, `roc_auc()`, `plot_roc()`, `pip_table()`, `pcor_estimate()`, `edge_index()`
- `R/methods.R` also has `pip_table.ssjgl()` / `pcor_estimate.ssjgl()` (ggmtools generics)
- `R/plot_path.R` — `plot_path()`: Solution path visualization
- `R/getmetric.R` — `getmetric()`: Legacy single-group metrics
- `R/getdiffmetric.R` — `getdiffmetric()`: Differential edge metrics

### Data simulation
- `R/utils-datagen.R` — `simulate_ssjgl_data()`: Generate K-group data with band/random/hub/scale-free graphs

### Package infrastructure
- `R/spikeyglass-package.R` — Package-level imports
- `R/data.R` — Dataset documentation (simdat: 3 groups, p = 50, with true graphs; built by data-raw/simdat.R)

## Key Hyperparameter Defaults

| Parameter | Default | Notes |
|-----------|---------|-------|
| `v0s` | `NULL` = `make_v0_ladder(lambda1)` = `lambda1/c(10, 31.6, 100)` | Decreasing spike ladder. Spike is Laplace with rate `lambda1/v0` on precision entries (scale `v0/lambda1`); only `lambda1/v0` matters. Chosen by `data-raw/v0-ladder-study.R` |
| `v1` | 1 | Slab variance |
| `lambda0` | — | Diagonal penalty (user must specify) |
| `lambda1` | — | Off-diagonal sparsity penalty (user must specify; 1 with `standardize = TRUE`, as in Li et al.) |
| `lambda2` | — | Cross-group similarity penalty (user must specify) |
| `a`, `b` | 1, 1 | Beta prior for pi_delta. Use b=p for sparse prior |
| `doubly` | FALSE | Whether to use doubly spike-and-slab |
| `standardize` | FALSE | Centre and scale columns per group (recommended); `normalize` only centres |
| `maxitr.em` | 500 | Max EM iterations per v0 step |
| `tol.em` | 1e-4 | EM convergence tolerance (max abs change in Theta) |

## Dependencies

- `Rcpp`, `RcppArmadillo` — C++ EM loop and ADMM (JGL routines vendored/ported; JGL and igraph are no longer dependencies)
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

3. **JGL vendored (2026-10)**: JGL was archived from CRAN, so it is no longer a dependency. `R/jgl-vendored.R` holds `penalty.as.matrix()` and `admm.iters.unconnected.S()`; `src/admm_adaptive.cpp` ports flsa2, dsgl and flsa.general (`flsa_general_update()`). Attribution is in `inst/COPYRIGHTS`. igraph has also been dropped: `connected_components_cpp()` does block detection. `tests/testthat/fixtures/` holds the pre-vendoring reference fits and the JGL flsa.general outputs.

4. **Imputation meanj indexing (fixed)**: `ssjgl.R` line 356 used `meanj[missed[, 3]]` which indexes a (K x p) matrix with a single vector (treated as linear indexing). Fixed to `meanj[cbind(missed[, 1], missed[, 3])]` for correct group-specific mean restoration.

6. **PIP collapse / pi handling (2026-10)**: ladders ending at small lambda1/v0 (e.g. DiNA's 16) make PIPs collapse (`data-raw/pip-collapse-diagnostic.md`). pi is now clamped to [1/N, 1-1/N], convergence also checks log(pi) (`tol.pi`), `ssjgl()` warns when no PIP > 0.5 at the last step, and `carry_pi` is optional. Same logic in the C++ (`ssjgl_em_inner_cpp`) and R EM loops; `tests/testthat/test-pi-fixes.R`.

5. **Correctness fixes (2026-10, see NEWS.md)**: lambda = 0 drops its prior terms in the E-step (`laplace_logprior()`); `getmissing()` uses `drop = FALSE`; data are always centred internally when imputing, and `imputed` is returned on the original scale; the M-step ADMM takes S matrices (S + addvar/n with missing data, per Li et al. 2019 supplement B); `warm`/`warm.connected` were removed from `ssjgl()`. `tests/testthat/test-bugfixes.R` checks that the C++ and R EM paths agree.

## TODO (low priority)

- **SSVS timing at p >= 200 (on hold, user decision 2026-10-21)**: `inst/scripts/bench_runtime.R --multiggm --multiggm-large` times multiGGM SSVS at p = 200/400 (hours per fit; results are cached in bench-cache/). Needed before the paper makes any EM-vs-MCMC speed claim: at p <= 100 SSVS (5000 + 1000 iterations) was as fast as ssjgl() (`inst/extdata/bench_runtime.csv`).

## Cross-project Notes (multiGGMr, ggmtools)

Functions shared with multiGGMr (github.com/mljaniczek/multiGGMr) live in **ggmtools** (github.com/mljaniczek/ggmtools; local checkout `../ggmtools`), and both packages re-export them:
- `precision_to_pcor()`, `confusion_at_threshold()`, `roc_auc()` (tie-correct; returns `auc`), `plot_roc()`
- generics `pip_table()` (long edge table: i, j, node1, node2, group, pip) and `pcor_estimate()` (list of K pcor matrices); each package registers methods
- `edge_index()` / `as_pip_table()` for the shared edge-table format; dataset `brca_pi3k`

Change shared behaviour in ggmtools, not here. Until ggmtools is on CRAN, DESCRIPTION has `Remotes: mljaniczek/ggmtools`.

Both packages are for multiple GGM estimation but use different methods:
- `spikeyglass`: EM + spike-and-slab + JGL (Li et al. 2019)
- `multiGGMr`: MCMC + MRF priors + G-Wishart (Peterson et al. 2015)
