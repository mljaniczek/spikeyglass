# Changelog

## spikeyglass 0.1.0

### PIP collapse (pi handling in the EM)

A diagnostic of the PIP collapse seen in the DiNA simulation is in
`data-raw/pip-collapse-diagnostic.md`. It was caused by ladders ending
at a small `lambda1/v0` (e.g. 16): an edge needs roughly
`sum_g |theta_g| > (G log r - logit(pi)) / (r - 1)`, `r = lambda1/v0`,
to reach PIP 0.5. Below that bar the pi update shrinks pi, which raises
the bar again. The following changes make the EM robust to this and make
it visible:

- pi_delta and pi_xi are kept inside `[1/N, 1 - 1/N]`, `N = p(p-1)/2`.
  pi_xi could previously reach exactly 0 and stay there (`log(0)` in the
  E-step), so `problist2` was identically 0.
- EM convergence now also requires the change in `log(pi)` to be below
  the new `tol.pi` (default 1e-3). Checking theta alone could stop a
  ladder step while pi was still falling by orders of magnitude per
  iteration.
- [`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
  warns when the last ladder step has no edge with PIP \> 0.5 or
  pi_delta is at its floor.
- New `carry_pi = FALSE`. If `TRUE`, pi starts each ladder step from the
  previous step’s value instead of the prior mean.
- [`?ssjgl`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
  gains a section on what the PIPs are (EM conditional probabilities
  that rank edges, not calibrated posterior probabilities) and on the
  collapse condition.

For fits that did not collapse, the final estimates are unchanged (to
1e-14 on the reference fits); some first ladder steps take one iteration
more or fewer.

### CRAN preparation

- [`ssjgl()`](https://mljaniczek.github.io/spikeyglass/reference/ssjgl.md)
  gains `verbose = FALSE`. Progress (each EM iteration on the R path,
  each ladder step, re-initialisation notices) is reported with
  [`message()`](https://rdrr.io/r/base/message.html) only when
  `verbose = TRUE`. It was printed unconditionally with
  [`cat()`](https://rdrr.io/r/base/cat.html).
  [`SSJGL_select_v0_cv()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_select_v0_cv.md),
  [`SSJGL_final_with_pcor_CI()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_final_with_pcor_CI.md)
  and
  [`SSJGL_CV_final_pcorCI()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_CV_final_pcorCI.md)
  now default to `verbose = FALSE` and use
  [`message()`](https://rdrr.io/r/base/message.html).
- [`simulate_ssjgl_data()`](https://mljaniczek.github.io/spikeyglass/reference/simulate_ssjgl_data.md),
  [`SSJGL_select_v0_cv()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_select_v0_cv.md)
  and
  [`SSJGL_final_with_pcor_CI()`](https://mljaniczek.github.io/spikeyglass/reference/SSJGL_final_with_pcor_CI.md)
  no longer change the caller’s random-number stream: the RNG state is
  restored on exit after using `seed`.
- [`plot_path()`](https://mljaniczek.github.io/spikeyglass/reference/plot_path.md)
  restores [`par()`](https://rdrr.io/r/graphics/par.html) on exit, has
  dead code removed, and its example now runs. Its legend now says what
  is drawn (the old “Edge” / “Non-edge” legend referred to a true graph
  the function never had).
- `simdat` is regenerated reproducibly with
  [`simulate_ssjgl_data()`](https://mljaniczek.github.io/spikeyglass/reference/simulate_ssjgl_data.md)
  (`data-raw/simdat.R`) and now also contains the true precision
  matrices (`Omega`). The previous version came from a script that could
  not be run.
- The unused `fit1.Rdata` files are removed, so the package no longer
  depends on R \>= 3.5.0 through serialized data. It now depends on R
  \>= 4.1.0.
- New `inst/CITATION` (method paper and package), `CITATION.cff` and
  `cran-comments.md`. The DESCRIPTION has a fuller description citing Li
  et al. (2019), `BugReports`, and the GitHub URL. The Title is in title
  case.
- GitHub Actions: R CMD check on macOS, Windows and Ubuntu (devel,
  release, oldrel-1), plus test coverage.
- The profiling script moved from `inst/profiling/` to `scripts/` (not
  installed).

## spikeyglass 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
